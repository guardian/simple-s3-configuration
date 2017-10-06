# simple-s3-configuration

[ ![Download](https://api.bintray.com/packages/guardian/platforms/simple-s3-configuration/images/download.svg) ](https://bintray.com/guardian/platforms/simple-s3-configuration/_latestVersion)

_A configuration library without any magic_

## Goal
This library will help you load the configuration of your application from s3.

It relies on [lightbend's configuration library](https://github.com/typesafehub/config), AWS' s3 sdk and AWS' ec2 sdk.

## Usage

In your `build.sbt`:
```scala
resolvers += "Guardian Platform Bintray" at "https://dl.bintray.com/guardian/platforms"
libraryDependencies += "com.gu" %% "simple-s3-configuration" % "1.0"
```

Then in your code:

```scala
import com.gu.AppIdentity
import com.gu.conf.ConfigurationLoader

val identity = AppIdentity.whoAmI(defaultAppName = "mobile-apps-api")
val config = ConfigurationLoader.load(identity)()
```

Let's look in detail at what's happening here.

### AppIdentity.whoAmI (optional)
The `AppIdentity.whoAmI` function is a helper that will try to identify your application via the tags (`App`, `Stack`, `Stage`) set on the ec2 instance you are running. It will need the appropriate IAM permission to be able to query the ec2 API (see [IAM paragraph below](#iam-permissions))

If you are running your application on an ec2 instance, the function will return an AppIdentity subtype: AwsIdentity defined as follows:

```scala
case class AwsIdentity(
  app: String,
  stack: String,
  stage: String,
  region: String
) extends AppIdentity
```

If you are not running on an ec2 instance, for instance when testing locally, the function will return an AppIdentity subtype: DevIdentity initialised with the defaultAppName you provided. It is defined as follows:

```scala
case class DevIdentity(app: String) extends AppIdentity
```

If you don't need to auto-detect the identity of your application, you can instantiate an AppIdentity yourself and provide the values you want.

You can optionally provide your [own AWS credentials](#examples) rather than relying on the defaults if you were to prefer controlling that aspect. It is defined liked that:

```scala
def whoAmI(
    defaultAppName: String,
    credentials: => AWSCredentialsProvider = DefaultAWSCredentialsProviderChain.getInstance
  ): AppIdentity
```

### ConfigurationLoader.load

This function will load your configuration from S3, or locally if you are in dev mode.
It will use the identity to understand where the app is running, and load the configuration accordingly. It will of course need the appropriate IAM permission, as defined in the [paragraph bellow](#iam-permissions).

By default the configuration are loaded from the following locations:

`~/.gu/${identity.app}.conf` for the local file if you are in dev mode (AppIdentity is of type DevIdentity)

`s3://${identity.app}-dist/${identity.stage}/${identity.stack}/${identity.app}/${identity.app}.conf` once running on an EC2 instance

`ConfigurationLoader.load` is defined like that:
```scala
def load(
    identity: AppIdentity,
    credentials: => AWSCredentialsProvider = DefaultAWSCredentialsProviderChain.getInstance
  )(locationFunction: PartialFunction[AppIdentity, ConfigurationLocation] = PartialFunction.empty): Config
```

The only parameter you need to provide is the identity, other parameters will use a sensible default.

`identity`: identity is a parameter of type `AppIdentity` that describe your application (name, stack, stage, awsRegion). See [above paragraph](#appidentitywhoami-optional) about AppIdentity.whoAmI

`credentials`: These are the AWS credentials that will be used to load your configuration from S3. The default behaviour should be enough, but if you wanted to customise the way the credentials are picked you could pass it as a parameter. Note that it's a pass-by-name parameter so the content won't be evaluated unless needed. The default behaviour when running locally is to load the configuration from a local file, so credentials won't be evaluated in that case.

`locationFunction`: This function is a way to customise where to load the configuration depending on the environment. For instance if your configuration is in the same place for two different stacks or if you're using the same configuration file for multiple apps (multi-module project) you could override the path that will be used. It's a partial function, so it's thought to be used as pattern matching on the `AppIdentity` you provided. You can see an [example below](#examples) or you can see what return types are possible in the [Location Types paragraph](#location-types).

## Examples

**provide your own credentials**
```scala
val identity = AppIdentity.whoAmI(defaultAppName = "mobile-apps-api", credentials = myOwnCredentials)
val config = ConfigurationLoader.load(identity, credentials = myOwnCredentials)()
```

**custom location**
See [Location Types](#location-types) for a list of all the location types.

```scala
val config = ConfigurationLoader.load(identity) {
  case AwsIdentity(app, "stack1", stage, _) => S3ConfigurationLocation("mybucket", s"somepath/$stage/$app.conf")
  case DevIdentity(myApp) => ResourceConfigurationLocation(s"localdev-${myApp}.conf")
}
```

**Play application with Compile time Dependency Injection**
```scala
import play.api.Configuration
import com.gu.AppIdentity
import com.gu.conf.ConfigurationLoader


class MyApplicationLoader extends ApplicationLoader {
  override def load(context: Context): Application = {
    LoggerConfigurator(context.environment.classLoader) foreach { _.configure(context.environment) }
    val identity = AppIdentity.whoAmI(defaultAppName = "myApp")
    val loadedConfig = Configuration(ConfigurationLoader.load(identity)())
    val newContext = context.copy(initialConfiguration = context.initialConfiguration ++ loadedConfig)
    (new BuiltInComponentsFromContext(newContext) with AppComponents).application
  }
}
```

Here's what we're doing above:
 - Initialise the logs (standard behaviour when using compile time dependencies with Play)
 - Auto detect the application identity
 - Load the Lightbend (Typesafe) Config, then wrap it in a Play Configuration
 - Concatenate the initial play configuration (application.conf) with what has been loaded from S3 or locally
 - Use that new configuration to instantiate the Play app

## Location types

When providing your own mapping between `AppIdentity` and location, you can specify three location types:

- `S3ConfigurationLocation(bucket: String, path: String, region: String)`
- `FileConfigurationLocation(file: File)`
- `ResourceConfigurationLocation(resourceName: String)`

### S3ConfigurationLocation
This will help `ConfigurationLoader.load` locate the file on an S3 bucket. You must provide the bucket name and the complete path to the file. The region defaults to the autodetected one, but you can override it if you please.

### FileConfigurationLocation
This will be useful when loading a file ouside of your classpath. Typically, a configuration that can contain secrets and that shouldn't be committed on the repository. This is used by default when in DEV mode and points to `~/.gu/${identity.app}.conf`

### ResourceConfigurationLocation
This will load a configuration file from within your classpath. Typically a file under the `resource` folder of your project. It is useful if your configuration can be committed in your repo and is directly accessible from the classpath. 

## IAM permissions
- if you use `AppIdentity.whoAmI`
```json
{
    "Effect": "Allow",
    "Action": "ec2:DescribeTags",
    "Resource": "*"
}
```
- for `ConfigurationLoader.load`
```json
{
    "Effect": "Allow",
    "Action": "s3:GetObject",
    "Resource": "arn:aws:s3:::mybucket/*"
}
```
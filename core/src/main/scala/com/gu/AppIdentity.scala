package com.gu

import com.amazonaws.auth.{AWSCredentialsProvider, DefaultAWSCredentialsProviderChain}
import com.amazonaws.regions.Regions
import com.amazonaws.services.autoscaling.model.{DescribeAutoScalingGroupsRequest, DescribeAutoScalingInstancesRequest}
import com.amazonaws.services.autoscaling.{AmazonAutoScaling, AmazonAutoScalingClientBuilder}
import com.amazonaws.util.EC2MetadataUtils
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

sealed trait AppIdentity

case class AwsIdentity(
  app: String,
  stack: String,
  stage: String,
  region: String
) extends AppIdentity

case class DevIdentity(
  app: String
) extends AppIdentity

object AppIdentity {
  private val logger = LoggerFactory.getLogger(this.getClass)

  private def safeAwsOperation[A](errorMessage: => String)(operation: => A): Option[A] = Try(operation) match {
    case Success(value) => Option(value)
    case Failure(e) =>
      logger.error(errorMessage, e)
      None
  }

  private def fromASGTags(credentials: => AWSCredentialsProvider): Option[AwsIdentity] = {
    // We read tags from the AutoScalingGroup rather than the instance itself to avoid problems where the
    // tags have not been applied to the instance before we start up (they are eventually consistent)
    def withOneOffAsgClient[T](f: AmazonAutoScaling => T): T = {
      val asgClient: AmazonAutoScaling = AmazonAutoScalingClientBuilder
        .standard()
        .withRegion(region)
        .withCredentials(credentials)
        .build()
      val returned = f(asgClient)
      asgClient.shutdown()
      returned
    }

    def getTags(asgClient: AmazonAutoScaling, instanceId: String): Map[String, String] = {
      val describeAutoScalingInstancesRequest = new DescribeAutoScalingInstancesRequest().withInstanceIds(instanceId)
      val describeAutoScalingInstancesResult = asgClient.describeAutoScalingInstances(describeAutoScalingInstancesRequest)
      val autoScalingGroupName = describeAutoScalingInstancesResult.getAutoScalingInstances.asScala.head.getAutoScalingGroupName

      val describeAutoScalingGroupsRequest = new DescribeAutoScalingGroupsRequest().withAutoScalingGroupNames(autoScalingGroupName)
      val describeAutoScalingGroupsResult = asgClient.describeAutoScalingGroups(describeAutoScalingGroupsRequest)
      val autoScalingGroup = describeAutoScalingGroupsResult.getAutoScalingGroups.asScala.head

      autoScalingGroup.getTags.asScala.map { t => t.getKey -> t.getValue }.toMap
    }

    Option(EC2MetadataUtils.getInstanceId).map { instanceId =>
      val tags = withOneOffAsgClient(client => getTags(client, instanceId))
      AwsIdentity(
        app = tags("App"),
        stack = tags("Stack"),
        stage = tags("Stage"),
        region = Regions.getCurrentRegion.getName
      )
    }
  }


  private def getEnv(variableName: String): Option[String] = Option(System.getenv(variableName))

  private def fromLambdaEnvVariables(): Option[AppIdentity] = {
    for {
      app <- getEnv("App")
      stack <- getEnv("Stack")
      stage <- getEnv("Stage")
      region <- getEnv("AWS_DEFAULT_REGION")
    } yield AwsIdentity(
      app = app,
      stack = stack,
      stage = stage,
      region = region
    )
  }

  private def fromTeamcityEnvVariables(defaultAppName: String): Option[AppIdentity] = {
    for {
      _ <- getEnv("TEAMCITY_VERSION")
    } yield DevIdentity(defaultAppName)
  }

  def region: String = {
    lazy val ec2Region = safeAwsOperation("Failed to identify the regionName of the instance") {
      Option(Regions.getCurrentRegion).map(_.getName)
    }.flatten
    lazy val lambdaRegion = Option(System.getenv("AWS_DEFAULT_REGION"))
    lambdaRegion orElse ec2Region getOrElse "eu-west-1"
  }

  def whoAmI(
    defaultAppName: String,
    credentials: => AWSCredentialsProvider = DefaultAWSCredentialsProviderChain.getInstance
  ): AppIdentity = {
    val result = fromTeamcityEnvVariables(defaultAppName)
      .orElse(fromLambdaEnvVariables())
      .orElse(fromASGTags(credentials))
      .getOrElse(DevIdentity(defaultAppName))
    logger.info(s"Detected the following AppIdentity: $result")
    result
  }
}

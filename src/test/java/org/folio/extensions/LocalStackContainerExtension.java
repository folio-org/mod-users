package org.folio.extensions;

import static org.folio.support.TestConstants.TENANT_NAME;

import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

import org.folio.s3.client.FolioS3Client;
import org.folio.s3.client.S3ClientFactory;
import org.folio.s3.client.S3ClientProperties;
import org.testcontainers.containers.localstack.LocalStackContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

public class LocalStackContainerExtension implements BeforeAllCallback, AfterAllCallback {

  @Container
  private static final LocalStackContainer localStackContainer = new LocalStackContainer(
    DockerImageName.parse("localstack/localstack:s3-latest")
  )
    .withServices(LocalStackContainer.Service.S3)
    .withStartupAttempts(3);

  public static FolioS3Client S3_CLIENT;

  @Override
  public void beforeAll(ExtensionContext context) throws Exception {
    if (!localStackContainer.isRunning()) {
      localStackContainer.start();
    }

    System.setProperty("AWS_URL", localStackContainer.getEndpoint().toString());
    System.setProperty("AWS_REGION", localStackContainer.getRegion());
    System.setProperty("AWS_ACCESS_KEY_ID", localStackContainer.getAccessKey());
    System.setProperty("AWS_SECRET_ACCESS_KEY", localStackContainer.getSecretKey());
    System.setProperty("AWS_BUCKET", TENANT_NAME);

    S3_CLIENT = S3ClientFactory.getS3Client(
      S3ClientProperties
        .builder()
        .endpoint(localStackContainer.getEndpoint().toString())
        .accessKey(localStackContainer.getAccessKey())
        .secretKey(localStackContainer.getSecretKey())
        .bucket("diku")
        .awsSdk(false)
        .region(localStackContainer.getRegion())
        .build()
    );
    S3_CLIENT.createBucketIfNotExists();
  }

  @Override
  public void afterAll(ExtensionContext context) {
    System.clearProperty("AWS_URL");
    System.clearProperty("AWS_REGION");
    System.clearProperty("AWS_ACCESS_KEY_ID");
    System.clearProperty("AWS_SECRET_ACCESS_KEY");
    System.clearProperty("AWS_BUCKET");
  }
}

package org.folio.extensions;

import static org.folio.support.TestConstants.TENANT_NAME;

import org.jspecify.annotations.NonNull;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

import org.folio.s3.client.FolioS3Client;
import org.folio.s3.client.S3ClientFactory;
import org.folio.s3.client.S3ClientProperties;
import org.testcontainers.containers.MinIOContainer;
import org.testcontainers.utility.DockerImageName;

public class MinioContainerExtension implements BeforeAllCallback, AfterAllCallback {

  private static final DockerImageName IMAGE_NAME = DockerImageName.parse("minio/minio:RELEASE.2025-09-07T16-13-09Z");

  private static final MinIOContainer minioContainer = new MinIOContainer(IMAGE_NAME)
    .withUserName("minio_admin")
    .withPassword("minio_password")
    .withStartupAttempts(3);

  public static FolioS3Client s3Client;

  @Override
  public void beforeAll(@NonNull ExtensionContext context) throws Exception {
    if (!minioContainer.isRunning()) {
      minioContainer.start();
    }

    System.setProperty("AWS_URL", minioContainer.getS3URL());
    System.setProperty("AWS_REGION", "us-east-1");
    System.setProperty("AWS_ACCESS_KEY_ID", minioContainer.getUserName());
    System.setProperty("AWS_SECRET_ACCESS_KEY", minioContainer.getPassword());
    System.setProperty("AWS_BUCKET", TENANT_NAME);

    s3Client = S3ClientFactory.getS3Client(
      S3ClientProperties.builder()
        .endpoint(minioContainer.getS3URL())
        .accessKey(minioContainer.getUserName())
        .secretKey(minioContainer.getPassword())
        .bucket("diku")
        .awsSdk(false)
        .region("us-east-1")
        .build()
    );

    s3Client.createBucketIfNotExists();
  }

  @Override
  public void afterAll(@NonNull ExtensionContext context) {
    System.clearProperty("AWS_URL");
    System.clearProperty("AWS_REGION");
    System.clearProperty("AWS_ACCESS_KEY_ID");
    System.clearProperty("AWS_SECRET_ACCESS_KEY");
    System.clearProperty("AWS_BUCKET");
  }
}

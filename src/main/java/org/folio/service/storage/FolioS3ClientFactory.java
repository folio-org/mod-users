package org.folio.service.storage;

import org.folio.rest.utils.OkapiConnectionParams;
import org.folio.s3.client.FolioS3Client;
import org.folio.s3.client.S3ClientFactory;
import org.folio.s3.client.S3ClientProperties;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class FolioS3ClientFactory {

  @Value("${minio.endpoint:http://localhost:9000}")
  private String endpoint;

  @Value("${minio.accessKey:minioadmin}")
  private String accessKey;

  @Value("${minio.secretKey:minioadmin}")
  private String secretKey;

  private String bucket = OkapiConnectionParams.OKAPI_TENANT_HEADER;

  @Value("${minio.region:us-east-1}")
  private String region;

  private FolioS3Client folioS3Client;

  public FolioS3ClientFactory() {
    this.folioS3Client = null;
  }

  public FolioS3Client getFolioS3Client() {
    if (folioS3Client != null) {
      return folioS3Client;
    }
    folioS3Client = createFolioS3Client();
    return folioS3Client;
  }

  private FolioS3Client createFolioS3Client() {

    return S3ClientFactory.getS3Client(
      S3ClientProperties
        .builder()
        .endpoint(endpoint)
        .accessKey(accessKey)
        .secretKey(secretKey)
        .bucket(bucket)
        .region(region)
        .build()
    );
  }
}

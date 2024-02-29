package org.folio.service.storage;

import org.apache.commons.lang3.StringUtils;
import org.folio.rest.utils.OkapiConnectionParams;
import org.folio.s3.client.FolioS3Client;
import org.folio.s3.client.S3ClientFactory;
import org.folio.s3.client.S3ClientProperties;

import java.util.HashMap;
import java.util.Map;

public class FolioS3ClientFactory {
  private Map<String, FolioS3Client> tenantS3ClientMap;

  public FolioS3ClientFactory() {
    this.tenantS3ClientMap = new HashMap<>();
  }

  public FolioS3Client getFolioS3Client(Map<String, String> okapiHeaders) {
    String tenant = okapiHeaders.get(OkapiConnectionParams.OKAPI_TENANT_HEADER);
    return tenantS3ClientMap.computeIfAbsent(tenant, k -> createFolioS3Client());
  }

  private FolioS3Client createFolioS3Client() {
    return S3ClientFactory.getS3Client(
      S3ClientProperties
        .builder()
        .endpoint(getValues("AWS_URL"))
        .accessKey(getValues("AWS_ACCESS_KEY_ID"))
        .secretKey(getValues("AWS_SECRET_ACCESS_KEY"))
        .bucket(getValues("AWS_BUCKET"))
        .region(getValues("AWS_REGION"))
        .build()
    );
  }

  private String getValues(String key) {
    return StringUtils.firstNonBlank(System.getenv(key), System.getProperty(key));
  }
}

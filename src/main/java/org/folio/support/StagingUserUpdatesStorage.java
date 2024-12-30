package org.folio.support;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class StagingUserUpdatesStorage<T> {
    @Builder.Default
    Boolean isUpdated = false;
    T oldEntity;
    T newEntity;
}

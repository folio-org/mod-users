package org.folio.support;

public class StagingUserUpdatesStorage<T> {
    private Boolean isUpdated = false;
    private T oldEntity;
    private final T newEntity;

    public StagingUserUpdatesStorage(Boolean isUpdated, T oldEntity, T newEntity) {
        this.isUpdated = isUpdated;
        this.oldEntity = oldEntity;
        this.newEntity = newEntity;
    }

    public StagingUserUpdatesStorage(T newEntity) {
        this.isUpdated = false;
        this.newEntity = newEntity;
    }

    public T getOldEntity() {
        return oldEntity;
    }

    public Boolean getUpdated() {
        return isUpdated;
    }

    public T getNewEntity() {
        return newEntity;
    }
}

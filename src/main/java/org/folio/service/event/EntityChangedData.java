package org.folio.service.event;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class EntityChangedData<E> {

  public EntityChangedData(E oldEntity, E newEntity) {
    this.oldEntity = oldEntity;
    this.newEntity = newEntity;
  }

  @JsonProperty("old")
  E oldEntity;
  @JsonProperty("new")
  E newEntity;

  public E getOldEntity() {
    return oldEntity;
  }

  public void setOldEntity(E oldEntity) {
    this.oldEntity = oldEntity;
  }

  public E getNewEntity() {
    return newEntity;
  }

  public void setNewEntity(E newEntity) {
    this.newEntity = newEntity;
  }
}

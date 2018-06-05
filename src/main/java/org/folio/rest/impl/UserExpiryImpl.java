/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.Vertx;
import org.folio.rest.resource.interfaces.PeriodicAPI;
import org.folio.rest.utils.ExpirationTool;

/**
 *
 * @author kurt
 */
public class UserExpiryImpl implements PeriodicAPI {


  @Override
  public long runEvery() {
    return 5000 * 60;
  }

  @Override
  public void run(Vertx vertx, Context context) {
    context.runOnContext(v -> {      
      ExpirationTool.doExpiration(vertx, context);
    });
  }

}

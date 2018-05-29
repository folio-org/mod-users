package org.folio.rest.deserializer;

import java.io.IOException;
import java.util.Date;
import java.util.Optional;

import org.folio.rest.jaxrs.model.User;
import org.folio.rest.tools.utils.ObjectMapperTool;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class UserDeserializer extends JsonDeserializer<User> {

  @Override
  public User deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
    ObjectMapper mapper = ObjectMapperTool.getDefaultMapper();
    ObjectCodec objectCodec = parser.getCodec();
    JsonNode node = objectCodec.readTree(parser);
    User user = mapper.treeToValue(node, User.class);
    Optional<Date> expirationDate = Optional.ofNullable(user.getExpirationDate());
    if (expirationDate.isPresent()) {
      Date now = new Date();
      if (now.compareTo(expirationDate.get()) > 0) {
        user.setActive(false);
      }
    }
    return user;
  }

}

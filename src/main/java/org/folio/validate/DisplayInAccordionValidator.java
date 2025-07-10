package org.folio.validate;

import java.util.List;

import org.apache.commons.lang3.Validate;
import org.folio.rest.jaxrs.model.CustomField;
import org.folio.validate.definition.Validatable;
import org.springframework.stereotype.Component;

@Component
public class DisplayInAccordionValidator implements Validatable {

  private final static List<String> ALLOWED_VALUES = List.of(
    "user_information",
    "extended_information",
    "contact_information",
    "default",
    "fees_fines",
    "loans",
    "requests"
  );

  public static final String INVALID_DISPLAY_IN_ACCORDION_MESSAGE =
    "Display in accordion value must be one of: %s";

  @Override
  public void validateDefinition(CustomField fieldDefinition) {
    var displayInAccordionValue = fieldDefinition.getDisplayInAccordion();
    Validate.isTrue(ALLOWED_VALUES.contains(displayInAccordionValue),
      INVALID_DISPLAY_IN_ACCORDION_MESSAGE, ALLOWED_VALUES);
  }

  @Override
  public boolean isApplicable(CustomField fieldDefinition) {
    return fieldDefinition.getDisplayInAccordion() != null;
  }
}

import {
  restore,
  popover,
  mockSessionProperty,
  filterWidget,
  editDashboard,
  saveDashboard,
  setFilter,
} from "__support__/e2e/cypress";

import { DASHBOARD_LOCATION_FILTERS } from "./helpers/e2e-dashboard-filter-data-objects";
import { addWidgetStringFilter } from "../native-filters/helpers/e2e-field-filter-helpers";

Object.entries(DASHBOARD_LOCATION_FILTERS).forEach(
  ([filter, { value, representativeResult }]) => {
    describe("scenarios > dashboard > filters > location", () => {
      beforeEach(() => {
        restore();
        cy.signInAsAdmin();

        mockSessionProperty("field-filter-operators-enabled?", true);

        cy.visit("/dashboard/1");

        editDashboard();
        setFilter("Location", filter);

        cy.findByText("Select…").click();
        popover()
          .contains("City")
          .click();
      });

      it(`should work for "${filter}" when set through the filter widget`, () => {
        saveDashboard();

        filterWidget().click();
        addWidgetStringFilter(value);

        cy.get(".Card").within(() => {
          cy.contains(representativeResult);
        });
      });

      it(`should work for "${filter}" when set as the default filter`, () => {
        cy.findByText("Default value")
          .next()
          .click();

        addWidgetStringFilter(value);

        saveDashboard();

        cy.get(".Card").within(() => {
          cy.contains(representativeResult);
        });
      });
    });
  },
);

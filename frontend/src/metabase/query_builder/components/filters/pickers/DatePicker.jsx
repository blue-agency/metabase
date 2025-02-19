/* eslint-disable react/prop-types */
import React, { Component } from "react";
import PropTypes from "prop-types";
import { t } from "ttag";
import cx from "classnames";
import moment from "moment";
import _ from "underscore";

import SpecificDatePicker from "./SpecificDatePicker";
import RelativeDatePicker, { DATE_PERIODS } from "./RelativeDatePicker";
import DateOperatorSelector from "../DateOperatorSelector";
import DateUnitSelector from "../DateUnitSelector";
import Calendar from "metabase/components/Calendar";

import { FieldDimension } from "metabase-lib/lib/Dimension";

const singleDatePickerPropTypes = {
  className: PropTypes.string,
  filter: PropTypes.object,
  onFilterChange: PropTypes.func,
  hideTimeSelectors: PropTypes.func,
};

const multiDatePickerPropTypes = {
  className: PropTypes.string,
  filter: PropTypes.object,
  onFilterChange: PropTypes.func,
  hideTimeSelectors: PropTypes.func,
};

const SingleDatePicker = ({
  className,
  filter: [op, field, value],
  onFilterChange,
  hideTimeSelectors,
}) => (
  <SpecificDatePicker
    className={className}
    value={value}
    onChange={value => onFilterChange([op, field, value])}
    hideTimeSelectors={hideTimeSelectors}
    calendar
  />
);

SingleDatePicker.propTypes = singleDatePickerPropTypes;

const MultiDatePicker = ({
  className,
  filter: [op, field, startValue, endValue],
  onFilterChange,
  hideTimeSelectors,
}) => (
  <div className={className}>
    <div className="Grid Grid--1of2 Grid--gutters">
      <div className="Grid-cell">
        <SpecificDatePicker
          value={startValue}
          hideTimeSelectors={hideTimeSelectors}
          onChange={value => onFilterChange([op, field, value, endValue])}
        />
      </div>
      <div className="Grid-cell">
        <SpecificDatePicker
          value={endValue}
          hideTimeSelectors={hideTimeSelectors}
          onChange={value => onFilterChange([op, field, startValue, value])}
        />
      </div>
    </div>
    <div className="Calendar--noContext">
      <Calendar
        initial={startValue ? moment(startValue) : moment()}
        selected={startValue && moment(startValue)}
        selectedEnd={endValue && moment(endValue)}
        onChange={(startValue, endValue) =>
          onFilterChange([op, field, startValue, endValue])
        }
      />
    </div>
  </div>
);

MultiDatePicker.propTypes = multiDatePickerPropTypes;

const PreviousPicker = props => (
  <RelativeDatePicker {...props} formatter={value => value * -1} />
);

PreviousPicker.horizontalLayout = true;

const NextPicker = props => <RelativeDatePicker {...props} />;

NextPicker.horizontalLayout = true;

class CurrentPicker extends Component {
  state = {
    showUnits: false,
  };

  static horizontalLayout = true;

  render() {
    const {
      className,
      filter: [operator, field, intervals, unit],
      onFilterChange,
    } = this.props;
    return (
      <DateUnitSelector
        className={className}
        value={unit}
        open={this.state.showUnits}
        onChange={value => {
          onFilterChange([operator, field, intervals, value]);
          this.setState({ showUnits: false });
        }}
        togglePicker={() => this.setState({ showUnits: !this.state.showUnits })}
        formatter={val => val}
        periods={DATE_PERIODS}
      />
    );
  }
}

const getIntervals = ([op, field, value, unit]) =>
  op === "time-interval" && typeof value === "number" ? Math.abs(value) : 30;
const getUnit = ([op, field, value, unit]) =>
  op === "time-interval" && unit ? unit : "day";
const getOptions = ([op, field, value, unit, options]) =>
  (op === "time-interval" && options) || {};

const getDate = value => {
  if (typeof value !== "string" || !moment(value).isValid()) {
    value = moment().format("YYYY-MM-DD");
  }
  return value;
};

const hasTime = value =>
  typeof value === "string" && /T\d{2}:\d{2}:\d{2}$/.test(value);

/**
 * Returns MBQL :field clause with temporal bucketing applied.
 * @deprecated -- just use FieldDimension to do this stuff.
 */
function getDateTimeField(field, bucketing) {
  const dimension = FieldDimension.parseMBQLOrWarn(field);
  if (dimension) {
    if (bucketing) {
      return dimension.withTemporalUnit(bucketing).mbql();
    } else {
      return dimension.withoutTemporalBucketing().mbql();
    }
  }
  return field;
}

export function getDateTimeFieldTarget(field) {
  const dimension = FieldDimension.parseMBQLOrWarn(field);
  if (dimension && dimension.temporalUnit()) {
    return dimension.withoutTemporalBucketing().mbql();
  } else {
    return field;
  }
}

// add temporal-unit to fields if any of them have a time component
function getDateTimeFieldAndValues(filter, count) {
  const values = filter
    .slice(2, 2 + count)
    .map(value => value && getDate(value));
  const bucketing = _.any(values, hasTime) ? "minute" : null;
  const field = getDateTimeField(filter[1], bucketing);
  return [field, ...values];
}

const ALL_TIME_OPERATOR = {
  name: "all",
  displayName: t`All Time`,
  init: () => null,
  test: op => op === null,
};

export const DATE_OPERATORS = [
  {
    name: "previous",
    displayName: t`Previous`,
    init: filter => [
      "time-interval",
      getDateTimeField(filter[1]),
      -getIntervals(filter),
      getUnit(filter),
      getOptions(filter),
    ],
    test: ([op, field, value]) =>
      (op === "time-interval" && value < 0) || Object.is(value, -0),
    widget: PreviousPicker,
    options: { "include-current": true },
  },
  {
    name: "next",
    displayName: t`Next`,
    init: filter => [
      "time-interval",
      getDateTimeField(filter[1]),
      getIntervals(filter),
      getUnit(filter),
      getOptions(filter),
    ],
    test: ([op, field, value]) => op === "time-interval" && value >= 0,
    widget: NextPicker,
    options: { "include-current": true },
  },
  {
    name: "current",
    displayName: t`Current`,
    init: filter => [
      "time-interval",
      getDateTimeField(filter[1]),
      "current",
      getUnit(filter),
    ],
    test: ([op, field, value]) => op === "time-interval" && value === "current",
    widget: CurrentPicker,
  },
  {
    name: "before",
    displayName: t`Before`,
    init: filter => ["<", ...getDateTimeFieldAndValues(filter, 1)],
    test: ([op]) => op === "<",
    widget: SingleDatePicker,
  },
  {
    name: "after",
    displayName: t`After`,
    init: filter => [">", ...getDateTimeFieldAndValues(filter, 1)],
    test: ([op]) => op === ">",
    widget: SingleDatePicker,
  },
  {
    name: "on",
    displayName: t`On`,
    init: filter => ["=", ...getDateTimeFieldAndValues(filter, 1)],
    test: ([op]) => op === "=",
    widget: SingleDatePicker,
  },
  {
    name: "between",
    displayName: t`Between`,
    init: filter => ["between", ...getDateTimeFieldAndValues(filter, 2)],
    test: ([op]) => op === "between",
    widget: MultiDatePicker,
  },
];

export const EMPTINESS_OPERATORS = [
  {
    name: "empty",
    displayName: t`Is Empty`,
    init: filter => ["is-null", getDateTimeField(filter[1])],
    test: ([op]) => op === "is-null",
  },
  {
    name: "not-empty",
    displayName: t`Not Empty`,
    init: filter => ["not-null", getDateTimeField(filter[1])],
    test: ([op]) => op === "not-null",
  },
];

export const ALL_OPERATORS = DATE_OPERATORS.concat(EMPTINESS_OPERATORS);

export function getOperator(filter, operators = ALL_OPERATORS) {
  return _.find(operators, o => o.test(filter));
}

export default class DatePicker extends Component {
  state = {
    operators: [],
  };

  static propTypes = {
    filter: PropTypes.array.isRequired,
    onFilterChange: PropTypes.func.isRequired,
    className: PropTypes.string,
    hideEmptinessOperators: PropTypes.bool,
    hideTimeSelectors: PropTypes.bool,
    isSidebar: PropTypes.bool,
    operators: PropTypes.array,
  };

  UNSAFE_componentWillMount() {
    let operators = this.props.operators || DATE_OPERATORS;
    if (!this.props.hideEmptinessOperators) {
      operators = operators.concat(EMPTINESS_OPERATORS);
    }

    const operator = getOperator(this.props.filter, operators) || operators[0];
    this.props.onFilterChange(operator.init(this.props.filter));

    this.setState({ operators });
  }

  render() {
    const {
      className,
      filter,
      onFilterChange,
      includeAllTime,
      isSidebar,
    } = this.props;

    let { operators } = this.state;

    if (includeAllTime) {
      operators = [ALL_TIME_OPERATOR, ...operators];
    }

    const operator = getOperator(this.props.filter, operators);
    const Widget = operator && operator.widget;

    return (
      <div
        // apply flex to align the operator selector and the "Widget" if necessary
        className={cx(className, {
          "flex align-center": Widget && Widget.horizontalLayout,
          "PopoverBody--marginBottom": !isSidebar,
        })}
        style={{ minWidth: 300 }}
      >
        <DateOperatorSelector
          className={cx({
            mr2: Widget && Widget.horizontalLayout,
            mb2: Widget && !Widget.horizontalLayout,
          })}
          operator={operator && operator.name}
          operators={operators}
          onOperatorChange={operator => onFilterChange(operator.init(filter))}
        />
        {Widget && (
          <Widget
            {...this.props}
            className="flex-full"
            filter={filter}
            hideHoursAndMinutes={this.props.hideTimeSelectors}
            onFilterChange={filter => {
              if (operator && operator.init) {
                onFilterChange(operator.init(filter));
              } else {
                onFilterChange(filter);
              }
            }}
          />
        )}
      </div>
    );
  }
}

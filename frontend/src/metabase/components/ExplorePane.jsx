/* eslint-disable react/prop-types */
import React from "react";
import { Link } from "react-router";

import Icon from "metabase/components/Icon";
import MetabotLogo from "metabase/components/MetabotLogo";
import Select, { Option } from "metabase/components/Select";
import { Grid, GridItem } from "metabase/components/Grid";
import Card from "metabase/components/Card";
import { Flex } from "grid-styled";
import { color } from "metabase/lib/colors";

import { t } from "ttag";
import _ from "underscore";

const DEFAULT_TITLE = t`Hi, Metabot here.`;
const DEFAULT_DESCRIPTION = "";

const DEFAULT_VISIBLE_ITEMS = 4;

export class ExplorePane extends React.Component {
  state = {
    schemaName: null,
    visibleItems: DEFAULT_VISIBLE_ITEMS,
  };
  static defaultProps = {
    title: DEFAULT_TITLE,
    description: DEFAULT_DESCRIPTION,
    withMetabot: true,
    gridColumns: 1 / 2,
    asCards: false,
  };

  render() {
    const {
      candidates,
      title,
      description,
      withMetabot,
      gridColumns,
      asCards,
    } = this.props;
    let { schemaName, visibleItems } = this.state;

    let schemaNames;
    let tables;
    const hasMore = false;
    if (candidates && candidates.length > 0) {
      schemaNames = candidates.map(schema => schema.schema);
      if (schemaName == null) {
        schemaName = schemaNames[0];
      }
      const schema = _.findWhere(candidates, { schema: schemaName });
      tables = (schema && schema.tables) || [];
    }

    return (
      <div>
        {title && (
          <div className="flex align-center mb2">
            {withMetabot && <MetabotLogo />}
            <h3 className="ml2">
              <span className="block" style={{ marginTop: 8 }}>
                {title}
              </span>
            </h3>
          </div>
        )}
        {description && (
          <div className="mb2 text-paragraph">
            <span>{description}</span>
          </div>
        )}
        {schemaNames && schemaNames.length > 1 && (
          <div className="flex align-center ml-auto">
            <div className="mr1">{t`Based on the schema`}</div>
            <Select
              value={schemaName}
              onChange={e =>
                this.setState({
                  schemaName: e.target.value,
                  visibleItems: DEFAULT_VISIBLE_ITEMS,
                })
              }
            >
              {schemaNames.map(schemaName => (
                <Option key={schemaName} value={schemaName}>
                  {schemaName}
                </Option>
              ))}
            </Select>
          </div>
        )}
        {tables && (
          <ExploreList
            candidates={tables}
            gridColumns={gridColumns}
            asCards={asCards}
          />
        )}
        {hasMore && (
          <div
            className="border-top cursor-pointer text-brand-hover flex layout-centered text-light px2 pt2 mt4"
            onClick={() => this.setState({ visibleItems: visibleItems + 4 })}
          >
            <Icon name="chevrondown" size={20} />
          </div>
        )}
      </div>
    );
  }
}

export const ExploreList = ({ candidates, gridColumns, asCards }) => (
  <Grid>
    {candidates &&
      candidates.map((option, index) => (
        <GridItem width={gridColumns} key={index}>
          {asCards ? (
            <Card hoverable p={2}>
              <ExploreOption option={option} />
            </Card>
          ) : (
            <ExploreOption option={option} />
          )}
        </GridItem>
      ))}
  </Grid>
);

export const ExploreOption = ({ option }) => (
  <Link
    to={option.url}
    className="flex align-center no-decoration text-medium text-brand-hover"
  >
    <Flex
      align="center"
      justify="center"
      style={{
        backgroundColor: color("accent4"),
        borderRadius: 6,
        width: 42,
        height: 42,
      }}
      mr={1}
    >
      <Icon name="bolt" size={20} className="flex-no-shrink text-white" />
    </Flex>
    <div>
      {t`A look at your`}{" "}
      <span className="text-bold text-wrap">{option.title}</span>
    </div>
  </Link>
);

export default ExplorePane;

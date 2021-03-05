(ns metabase.util.visualization-settings-test
  (:require [clojure.test :refer :all]
            [metabase.util.visualization-settings :as viz]
            [java-time :as t]))

(deftest name-for-column
  (let [viz-settings {:column_settings {(keyword "[\"ref\",[\"field\",14,null]]") {:column_title "Renamed Column"
                                                                                   :date_style   "YYYY/MM/D"
                                                                                   :time_enabled "minutes"
                                                                                   :time_style   "k:mm"}}}
        col          {:id 14}]
    (testing "name-from-col-settings works as expected"
             (is (= "Renamed Column"
                    (viz/name-from-col-settings viz-settings col))))
    (testing "date-format-from-col-settings works as expected"
      (is (= "YYYY/MM/D k:mm"
             (viz/date-format-from-col-settings viz-settings col))))))


(deftest format-overrides-test
  (let [col-ref-id   "[\"ref\",[\"field\",143,null]]"
        col-ref-expr "[\"ref\",[\"expression\",\"CREATED_AT_MINUS_ONE_DAY\"]]"
        viz-settings {:column_settings {(keyword col-ref-id) {:column_title "Grand Total"}
                                        ;; TODO: figure out how to deal with the fact that "D" is day-of-year in Java
                                        (keyword col-ref-expr) {:date_style   "YYYY/MM/D"
                                                                :time_enabled "minutes"
                                                                :time_style   "k:mm"}}}
        id-col       {:description     "The total billed amount."
                      :semantic_type   nil,
                      :table_id        37,
                      :name            "TOTAL",
                      :settings        nil,
                      :source          :fields,
                      :field_ref       [:field 143 nil],
                      :parent_id       nil,
                      :id              143,
                      :visibility_type :normal,
                      :display_name    "Total",
                      :fingerprint     {:global {:distinct-count 4426, :nil% 0.0}}
                      :type            {:type/Number {:min 8.93914247937167, :q1 51.34535490743823,
                                                      :q3 110.29428389265787, :max 159.34900526552292,
                                                      :sd 34.26469575709948, :avg 80.35871658771228}},
                      :base_type       :type/Float}
        expr-col     {:base_type       :type/DateTime,
                      :semantic_type   :type/CreationTimestamp,
                      :name            "CREATED_AT_MINUS_ONE_DAY",
                      :display_name    "CREATED_AT_MINUS_ONE_DAY",
                      :expression_name "CREATED_AT_MINUS_ONE_DAY",
                      :field_ref       [:expression "CREATED_AT_MINUS_ONE_DAY"],
                      :source          :fields}
        test-date    (t/local-date-time 2021 1 21 13 5)]
    (testing "correct format function for dates created for column"
      (let [overrides (viz/make-format-overrides viz-settings expr-col)]
        (is (contains? overrides :format-fn))
        (is (= "2021/01/21 13:05" ((:format-fn overrides) test-date)))))
    (testing ":column_title picked up for expression"
      (is (= {:column_title "Grand Total"}
             (viz/make-format-overrides viz-settings id-col))))))


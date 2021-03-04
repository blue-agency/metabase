(ns metabase.util.visualization-settings-test
  (:require [clojure.test :refer :all]
            [metabase.util.visualization-settings :as viz]))

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


(ns metabase.util.visualization-settings
  "Utility functions for dealing with visualization settings on the backend."
  (:require [schema.core :as s]
            [metabase.util.schema :as su]))

(def Column (su/open-schema
              {(s/required-key :id) su/IntGreaterThanZero}))

;; TODO: tighten this up
(def ColSetting s/Any)

(def ColSettings {s/Keyword ColSetting})

(def VizSettings (s/maybe (su/open-schema
                            {(s/required-key :column_settings) ColSettings})))

(s/defn col-settings-key
  "Gets the key that would be mapped under :column_settings for the given col (a Column domain object)."
  [col :- Column]
  (keyword (format "[\"ref\",[\"field\",%d,null]]" (:id col))))

(s/defn col-settings :- (s/maybe ColSetting)
  "Gets the column_settings value mapped by the given col (a Column domain object) as a key (a Column domain object)."
  [{:keys [column_settings] :as visualization-settings} :- VizSettings col :- Column]
  (get column_settings (col-settings-key col)))

(s/defn name-from-col-settings [visualization-settings :- VizSettings col  :- Column] :- (s/maybe s/Str)
  (let [settings (col-settings visualization-settings col)]
    (:column_title settings)))

(s/defn date-format-from-col-settings [visualization-settings :- VizSettings col  :- Column] :- (s/maybe s/Str)
  (let [settings (col-settings visualization-settings col)]
    (if-let [date-style (:date_style settings)]
      (str date-style (if-let [time-style (:time_style settings)] (str " " time-style) "")))))

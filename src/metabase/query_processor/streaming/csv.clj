(ns metabase.query-processor.streaming.csv
  (:require [clojure.data.csv :as csv]
            [java-time :as t]
            [metabase.query-processor.streaming.common :as common]
            [metabase.query-processor.streaming.interface :as i]
            [metabase.util.date-2 :as u.date]
            [metabase.util.visualization-settings :as viz])
  (:import [java.io BufferedWriter OutputStream OutputStreamWriter]
           java.nio.charset.StandardCharsets))

(defmethod i/stream-options :csv
  [_]
  {:content-type              "text/csv"
   :status                    200
   :headers                   {"Content-Disposition" (format "attachment; filename=\"query_result_%s.csv\""
                                                             (u.date/format (t/zoned-date-time)))}
   :write-keepalive-newlines? false})

(defn- format-metadata [visualization-settings col]
  (if-let [date-fmt-str (viz/date-format-from-col-settings visualization-settings col)]
    {:date-format-str date-fmt-str}
    nil))

(defmethod i/streaming-results-writer :csv
  [_ ^OutputStream os]
  (let [writer (BufferedWriter. (OutputStreamWriter. os StandardCharsets/UTF_8))]
    (reify i/StreamingResultsWriter
      (begin! [_ {{:keys [cols visualization_settings]} :data}]
        (csv/write-csv writer [(map (some-fn (partial viz/name-from-col-settings visualization_settings) :display_name :name) cols)])
        (.flush writer))

      (write-row! [_ row row-num {{:keys [cols visualization_settings]} :data}]
        (letfn [(fmt-row [idx val]
                  (let [col     (nth cols idx)
                        fmt-md  (format-metadata visualization_settings col)]
                    (common/format-value val fmt-md)))]
          (csv/write-csv writer [(map-indexed fmt-row row)])
          (.flush writer)))

      (finish! [_ _]
         ;; TODO -- not sure we need to flush both
        (.flush writer)
        (.flush os)
        (.close writer)))))

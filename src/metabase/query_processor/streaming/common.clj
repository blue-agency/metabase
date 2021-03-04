(ns metabase.query-processor.streaming.common
  "Shared util fns for various export (download) streaming formats."
  (:require [java-time :as t]
            [metabase.util.date-2 :as u.date]))

(defprotocol FormatValue
  "Protocol for specifying how objects of various classes in QP result rows should be formatted in various download
  results formats (e.g. CSV, as opposed to the 'normal' API response format, which doesn't use this logic)."
  (format-value [this metadata]
    "Format this value in a QP result row appropriately for a results download, such as CSV."))

(extend-protocol FormatValue
  nil
  (format-value [_ _] nil)

  Object
  (format-value [this _] this)

  java.time.temporal.Temporal
  (format-value [this metadata]
    (if-let [date-fmt-str (:date-format-str metadata)]
      (u.date/format date-fmt-str this)
      (u.date/format this)))

  java.time.LocalDateTime
  (format-value [this metadata]
    (let [val (if (= (t/local-time this) (t/local-time 0))
                (t/local-date this)
                this)]
      (if-let [date-fmt-str (:date-format-str metadata)]
        (u.date/format date-fmt-str val)
        (u.date/format val))))

  java.time.OffsetDateTime
  (format-value [this metadata]
    (let [val (if (= (t/local-time this) (t/local-time 0))
                (t/local-date this)
                this)]
      (if-let [date-fmt-str (:date-format-str metadata)]
        (u.date/format date-fmt-str val)
        (u.date/format val))))

  java.time.ZonedDateTime
  (format-value [this metadata]
    (format-value (t/offset-date-time this) metadata)))

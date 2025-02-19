(ns metabase.public-settings-test
  (:require [clj-http.fake :as http-fake]
            [clojure.core.memoize :as memoize]
            [clojure.test :refer :all]
            [metabase.models.setting :as setting]
            [metabase.public-settings :as public-settings]
            [metabase.public-settings.premium-features :as premium-features]
            [metabase.test :as mt]
            [metabase.test.fixtures :as fixtures]
            [metabase.util.i18n :as i18n :refer [tru]]))

(use-fixtures :once (fixtures/initialize :db))

(deftest site-url-settings
  (testing "double-check that setting the `site-url` setting will automatically strip off trailing slashes"
    (mt/discard-setting-changes [site-url]
      (public-settings/site-url "http://localhost:3000/")
      (is (= "http://localhost:3000"
             (public-settings/site-url))))))

(deftest site-url-settings-prepend-http
  (testing "double-check that setting the `site-url` setting will prepend `http://` if no protocol was specified"
    (mt/discard-setting-changes [site-url]
      (public-settings/site-url "localhost:3000")
      (is (= "http://localhost:3000"
             (public-settings/site-url))))))

(deftest site-url-settings-with-no-trailing-slash
  (mt/discard-setting-changes [site-url]
    (public-settings/site-url "http://localhost:3000")
    (is (= "http://localhost:3000"
           (public-settings/site-url)))))

(deftest site-url-settings-https
  (testing "if https:// was specified it should keep it")
  (mt/discard-setting-changes [site-url]
    (public-settings/site-url "https://localhost:3000")
    (is (= "https://localhost:3000"
           (public-settings/site-url)))))

(deftest site-url-settings-validate-site-url
  (testing "we should not be allowed to set an invalid `site-url` (#9850)"
    (mt/discard-setting-changes [site-url]
      (is (thrown?
           clojure.lang.ExceptionInfo
           (public-settings/site-url "http://https://www.camsaul.com"))))))

(deftest site-url-settings-set-valid-domain-name
  (mt/discard-setting-changes [site-url]
    (is (some? (public-settings/site-url "https://www.camsaul.x")))))

(deftest site-url-settings-nil-getter-when-invalid
  (testing "if `site-url` in the database is invalid, the getter for `site-url` should return `nil` (#9849)"
    (mt/discard-setting-changes [site-url]
      (setting/set-string! :site-url "https://&")
      (is (= "https://&"
             (setting/get-string :site-url)))
      (is (= nil
             (mt/suppress-output (public-settings/site-url)))))))

(deftest site-url-settings-normalize
  (testing "We should normalize `site-url` when set via env var we should still normalize it (#9764)"
    (mt/with-temp-env-var-value [mb-site-url "localhost:3000/"]
      (mt/with-temporary-setting-values [site-url nil]
        (is (= "localhost:3000/"
               (setting/get-string :site-url)))
        (is (= "http://localhost:3000"
               (public-settings/site-url)))))))

(deftest invalid-site-url-env-var-test
  (testing (str "If `site-url` is set via an env var, and it's invalid, we should return `nil` rather than having the"
                " whole instance break")
    (mt/suppress-output
      (mt/with-temp-env-var-value [mb-site-url "asd_12w31%$;"]
        (mt/with-temporary-setting-values [site-url nil]
          (is (= "asd_12w31%$;"
                 (setting/get-string :site-url)))
          (is (= nil
                 (public-settings/site-url))))))))

(deftest site-url-should-update-https-redirect-test
  (testing "Changing `site-url` to non-HTTPS should disable forced HTTPS redirection"
    (mt/with-temporary-setting-values [site-url                       "https://example.com"
                                       redirect-all-requests-to-https true]
      (is (= true
             (public-settings/redirect-all-requests-to-https)))
      (public-settings/site-url "http://example.com")
      (is (= false
             (public-settings/redirect-all-requests-to-https)))))

  (testing "Changing `site-url` to non-HTTPS should disable forced HTTPS redirection"
    (mt/with-temporary-setting-values [site-url                       "https://example.com"
                                       redirect-all-requests-to-https true]
      (is (= true
             (public-settings/redirect-all-requests-to-https)))
      (public-settings/site-url "https://different.example.com")
      (is (= true
             (public-settings/redirect-all-requests-to-https))))))

(deftest translate-public-setting
  (mt/with-mock-i18n-bundles {"zz" {"Host" "HOST"}}
    (mt/with-user-locale "zz"
      (is (= "HOST"
             (str (:display-name (first (get-in (setting/properties :public) [:engines :postgres :details-fields])))))))))

(deftest tru-translates
  (mt/with-mock-i18n-bundles {"zz" {"Host" "HOST"}}
    (mt/with-user-locale "zz"
      (is (= true
             (= (i18n/locale "zz")
                (i18n/user-locale))))
      (is (= "HOST"
             (tru "Host"))))))

(deftest query-caching-max-kb-test
  (testing (str "Make sure Max Cache Entry Size can be set via with a string value, which is what comes back from the "
                "API (#9143)")
    (mt/discard-setting-changes [query-caching-max-kb]
      (is (= "1000"
             (public-settings/query-caching-max-kb "1000")))))

  (testing "query-caching-max-kb should throw an error if you try to put in a huge value"
    (mt/discard-setting-changes [query-caching-max-kb]
      (is (thrown-with-msg?
           IllegalArgumentException
           #"Values greater than 204,800 \(200\.0 MB\) are not allowed"
           (public-settings/query-caching-max-kb (* 1024 1024)))))))

(deftest site-locale-test
  (testing "site-locale Setting"
    (testing "should validate input"
      (testing "invalid format"
        (testing "blank string"
          (mt/with-temporary-setting-values [site-locale "en_US"]
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"Invalid locale \"\""
                 (public-settings/site-locale "")))
            (is (= "en_US"
                   (public-settings/site-locale)))))

        (testing "non-existant locale"
          (mt/with-temporary-setting-values [site-locale "en_US"]
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"Invalid locale \"en_EN\""
                 (public-settings/site-locale "en_EN")))
            (is (= "en_US"
                   (public-settings/site-locale)))))))

    (testing "should normalize input"
      (mt/discard-setting-changes [site-locale]
        (public-settings/site-locale "en-us")
        (is (= "en_US"
               (public-settings/site-locale)))))

    (testing "should be able to unset site locale"
      (mt/discard-setting-changes [site-locale]
        (public-settings/site-locale "es")
        (public-settings/site-locale nil)
        (is (= "en"
               (public-settings/site-locale))
            "should default to English")))))

(deftest redirect-all-requests-to-https-test
  (testing "Shouldn't be allowed to set `redirect-all-requests-to-https` to `true` unless `site-url` is HTTPS"
    (doseq [v [true "true"]]
      (testing (format "\nSet value to ^%s %s" (.getCanonicalName (class v)) (pr-str v))
        (testing "\n`site-url` *is* HTTPS"
          (mt/with-temporary-setting-values [site-url                       "https://example.com"
                                             redirect-all-requests-to-https false]
            (public-settings/redirect-all-requests-to-https v)
            (is (= true
                   (public-settings/redirect-all-requests-to-https)))))

        (testing "\n`site-url` is not HTTPS"
          (mt/with-temporary-setting-values [site-url                       "http://example.com"
                                             redirect-all-requests-to-https false]
            (is (thrown?
                 AssertionError
                 (public-settings/redirect-all-requests-to-https v)))
            (is (= false
                   (public-settings/redirect-all-requests-to-https)))))))))

(deftest cloud-gateway-ips-test
  (with-redefs [premium-features/is-hosted? (constantly true)]
    (testing "Setting calls Store URL to fetch IP addresses"
      (memoize/memo-clear! @#'public-settings/fetch-cloud-gateway-ips-fn)
      (http-fake/with-fake-routes-in-isolation
        {{:address (public-settings/cloud-gateway-ips-url)}
         (constantly {:status 200 :body "{\"ip_addresses\": [\"127.0.0.1\"]}"})}
        (is (= ["127.0.0.1"] (public-settings/cloud-gateway-ips))))

      (testing "Getter is memoized to avoid frequent HTTP calls"
        (http-fake/with-fake-routes-in-isolation
          {{:address (public-settings/cloud-gateway-ips-url)}
           (constantly {:status 200 :body "{\"ip_addresses\": [\"0.0.0.0\"]}"})}
          (is (= ["127.0.0.1"] (public-settings/cloud-gateway-ips))))))

    (testing "Setting returns nil if URL is unreachable"
      (memoize/memo-clear! @#'public-settings/fetch-cloud-gateway-ips-fn)
      (http-fake/with-fake-routes-in-isolation
        {{:address (public-settings/cloud-gateway-ips-url)}
         (constantly {:status 500})}
        (is (= nil (public-settings/cloud-gateway-ips))))))

  (testing "Setting returns nil in self-hosted environments"
    (memoize/memo-clear! @#'public-settings/fetch-cloud-gateway-ips-fn)
    (http-fake/with-fake-routes-in-isolation
      {{:address (public-settings/cloud-gateway-ips-url)}
       (constantly {:status 200 :body "{\"ip_addresses\": [\"127.0.0.1\"]}"})}
      (is (= nil (public-settings/cloud-gateway-ips))))))

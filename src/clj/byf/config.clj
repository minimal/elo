(ns byf.config
  (:require [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [aero.core :as aero]))

(defn load-config
  []
  (let [profile (:environment env)]
    ;; failling on uberjar otherwise
    #_(assert (some? profile) "Could not detect profile")
    (log/info "loading configuration for profile " profile)
    (aero/read-config "config.edn" {:profile profile})))

(defn value
  [k]
  (let [config (load-config)]
    (assert (contains? config k))
    (get config k)))
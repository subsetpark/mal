(def Env @{:get! (fn [self key]
                   (match (in self key)
                     nil (error [:notfound key])
                     val val))})

(defn new-env [proto & kvs]
  (table/setproto (table ;kvs) proto))

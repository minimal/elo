(ns byf.algorithms.elo
  "Implementation of the BYF algorithm"
  (:require [byf.shared-config :as shared]))

(defn expected
  [diff]
  (/ 1.0 (inc (Math/pow 10 (/ diff 400)))))

(defn new-rating
  [old expected score {:keys [k] :as config}]
  (+ old (* k (- score expected))))

(defn invert-score
  [score]
  (cond (zero? score) 1
        (= 1 score) 0
        :else score))

(defn new-rankings
  ([rankings result]
   (new-rankings rankings result shared/default-game-config))

  ([rankings [p1 p2 score t] config]
   (let [ra (get-in rankings [p1 :ranking])
         rb (get-in rankings [p2 :ranking])]
     (assoc rankings
            p1 {:ranking (new-rating ra
                                     (expected (- rb ra))
                                     score
                                     config)
                :last-game t}

            p2 {:ranking (new-rating rb
                                     (expected (- ra rb))
                                     (invert-score score)
                                     config)
                :last-game t}))))

;;TODO: this should return the whole history of the rankings instead
;;of simply the last one??
(defn update-rankings
  [rankings games config]
  (if (empty? games)
    rankings
    (recur (new-rankings rankings (first games) config)
           (rest games)
           config)))

(defn initial-rankings
  ([players]
   (initial-rankings players shared/default-game-config))

  ([players {:keys [initial-ranking] :as config}]
   (zipmap players (repeat {:ranking initial-ranking :last-game nil}))))

(defn extract-players
  [games]
  (vec (set (apply concat
                   (for [[f s] games]
                     [f s])))))

(defn normalize-game
  "Normalize the game identifying winner and loser (or draw) from the number of goals.
  With this approach the goal difference doesn't matter, but with
  changes to this normalizatione that could also be taken into account."

  ;;TODO: if we return both scores in one go we don't need the extra
  ;;normalizationq done above
  [{:keys [p1 p2 p1_points p2_points played_at] :as game}]
  (cond
    (= p1_points p2_points) [p1 p2 0.5 played_at]
    (> p1_points p2_points) [p1 p2 1 played_at]
    (> p2_points p1_points) [p1 p2 0 played_at]))

(defn compute-rankings
  ([games players]
   (compute-rankings games players shared/default-game-config))

  ([games players config]
   (update-rankings (initial-rankings players config)
                    games
                    (merge shared/default-game-config config))))

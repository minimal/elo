(ns elo.league-detail.views
  (:require [accountant.core :as accountant]
            [cljsjs.moment]
            [clojure.string :as str]
            [elo.common.views :refer [drop-down]]
            [elo.date-picker-utils :refer [date-time-picker]]
            [elo.league-detail.handlers :as handlers]
            [elo.routes :as routes]
            [elo.shared-config :as config]
            [elo.utils :as utils]
            [elo.vega :as vega]
            [re-frame.core :as rf]))

(def timestamp-format "YYYY-MM-DDZhh:mm:SS")
(def form-size 5)

(defn drop-down-players
  [opts dispatch-key value]
  [drop-down opts dispatch-key value :value-fn :id :display-fn :name])

(defn- translate
  [term]
  (let [league (rf/subscribe [::handlers/league])]
    ;;XXX: is there a way to avoid all this extra safety?
    (config/term (or (:game_type @league) :fifa) term)))

(defn now-format
  []
  (.format (js/moment) timestamp-format))

(defn date-range-picker
  []
  (let [game (rf/subscribe [::handlers/game])]
    [:div.filter-panel--range__inputs.date-range__inputs
     [date-time-picker {:name "datetime-widget"
                        :selected (:played_at @game)
                        :react-key "date-picker"
                        :date (js/moment)
                        :min-date "2018-08-01"
                        :max-date (js/moment)
                        :placeholder "When was it played"
                        :on-change #(rf/dispatch [::handlers/played_at %])
                        :class "date-picker-class"}]]))

(defn- enable-button
  [valid-game? opts]
  (if valid-game?
    opts
    (assoc opts :disabled "{true}")))

(defn game-form
  []
  (let [players (rf/subscribe [::handlers/players])
        valid-game? (rf/subscribe [::handlers/valid-game?])
        game (rf/subscribe [::handlers/game])
        league (rf/subscribe [::handlers/league])
        game-type (or (:game_type @league) :fifa)
        points-range (map str (config/opts game-type :points))
        sorted-players (sort-by :name @players)]

    [:div.game__form
     [:div.form-group.player1__group
      [:label.form__label "Player 1"]
      [:div.form__row.form-control
       [drop-down-players sorted-players ::handlers/p1 (:p1 @game)
        {:caption "Name"}]

       [drop-down points-range ::handlers/p1_points (:p1_points @game)
        {:caption (translate :points)}]

       [:input.form-control
        {:type "text"
         :placeholder (str (translate :using) " Name")
         :value (:p1_using @game)
         :on-change (utils/set-val ::handlers/p1_using)}]]]

     [:div.form-group.player2__group
      [:label.form__label "Player 2"]
      [:div.form__row.form-control
       [drop-down-players sorted-players ::handlers/p2 (:p2 @game)
        {:caption "Name"}]

       [drop-down points-range ::handlers/p2_points (:p2_points @game)
        {:caption (translate :points)}]

       [:input.form-control {:type "text"
                             :placeholder (str (translate :using) " Name")
                             :value (:p2_using @game)
                             :on-change (utils/set-val ::handlers/p2_using)}]]]

     [:div.form__row.form-group
      [:label.form__label {:for "played_at"} "Played at"]
      [:div.form-control {:id "played_at"} [date-range-picker]]]

     [:div.form__row.form-group
      [:button.submit__game
       (enable-button @valid-game?
                      {:on-click (if @valid-game?
                                   #(rf/dispatch [::handlers/add-game])
                                   #(js/alert "Invalid results or incomplete form"))})

       "Add Game"]]]))

(defn- enumerate
  [xs]
  ;; without sorting it only works up to 30 !!
  (sort (zipmap (map inc (range (count xs))) xs)))

(defn- format-date
  [timestamp]
  (.format (js/moment timestamp) "YYYY-MM-DD"))

(defn games-table
  []
  (let [games (rf/subscribe [::handlers/games-live-players])
        name-mapping (rf/subscribe [::handlers/name-mapping])
        up-to (rf/subscribe [::handlers/up-to-games])
        show-all? (rf/subscribe [::handlers/show-all?])]

    (fn []
      (let [first-games (if (some? @up-to)
                          (take @up-to @games)
                          @games)

            header [:tr
                    [:th "game #"]
                    [:th "player 1"]
                    [:th (translate :using)]
                    [:th (translate :points)]
                    [:th "player 2"]
                    [:th (translate :using)]
                    [:th (translate :points)]
                    [:th "played At"]]
            rev-games (-> first-games enumerate reverse)
            filtered-games (if @show-all? rev-games (take 10 rev-games))]

        [:div
         [:button {:on-click #(rf/dispatch [::handlers/toggle-show-all])}
          (if @show-all? "SHOW LAST 10" "SHOW ALL")]

         [:table.table.table-striped
          [:thead header]
          (into [:tbody]
                (for [[idx {:keys [p1 p2 p1_using p2_using p1_points p2_points played_at]}]
                      filtered-games]

                  [:tr
                   [:td idx]
                   [:td (get @name-mapping p1)]
                   [:td p1_using]
                   [:td p1_points]
                   [:td (get @name-mapping p2)]
                   [:td p2_using]
                   [:td p2_points]
                   [:td (format-date played_at)]]))]]))))

(defn el-result
  [idx result]
  [:span {:key idx :class (str "result__element result__" (name result))}
   (-> result name str/capitalize)])

(defn results-boxes
  [results]
  (map-indexed el-result (take-last form-size results)))

(defn game-slider
  []
  (let [games (rf/subscribe [::handlers/games-live-players])
        up-to-games (rf/subscribe [::handlers/up-to-games])]

    (fn []
      (let [up-to-current (if (some? @up-to-games) @up-to-games (count @games))]
        [:div.form-group
         [:input.form-control.up-to-range-slider
          {:type "range"
           :min 0
           :max (count @games)
           :value up-to-current
           :class "slider"
           :on-change (utils/set-val ::handlers/up-to-games js/parseInt)}]

         [:span.rankings-chevrons.form-control
          [:i.fas.fa-chevron-left {:on-click #(rf/dispatch [::handlers/prev-game])}]
          [:span.up-to-current-games up-to-current]
          [:i.fas.fa-chevron-right {:on-click #(rf/dispatch [::handlers/next-game])}]]]))))

(def hide-show-all
  [:span.hide__show__all
   [:i.fas.fa-eye-slash
    {:title "Hide All"
     :on-click #(rf/dispatch [::handlers/hide-all])}]

   [:i.fas.fa-eye
    {:title "Show All"
     :on-click #(rf/dispatch [::handlers/show-all])}]])

(def kill-revive-all
  [:span
   [:i.fas.fa-skull
    {:title "Kill All"
     :on-click #(rf/dispatch [::handlers/kill-all])}]

   [:i.fas.fa-life-ring
    {:title "Revive All"
     :on-click #(rf/dispatch [::handlers/revive-all])}]])

(defn- tag
  [t]
  (fn [v] [t (if (float? v) (int v) v)]))

(defn- stats-table
  [header data]
  [:table.table.table-striped.table__stats
   [:thead
    (into [:tr] (map (tag :th) (map :v header)))]

   (into [:tbody]
         (for [row data]
           (into [:tr]
                 (map (tag :td)
                      (vals (select-keys row (map :k header)))))))])

(defn rankings-table
  []
  (let [name-mapping @(rf/subscribe [::handlers/name-mapping])
        results @(rf/subscribe [::handlers/results])
        stats @(rf/subscribe [::handlers/stats])
        sorted-rankings @(rf/subscribe [::handlers/rankings])
        header [:tr
                [:th hide-show-all]
                [:th kill-revive-all]
                [:th "position"]
                [:th "player"]
                [:th "ranking"]
                [:th "# of games"]
                [:th "form"]
                [:th "# W/L/D"]]]

    [:div
     [game-slider]
     [:table.table.table-striped
      [:thead header]
      (into [:tbody]
            (for [[idx {:keys [id ranking ngames]}] (enumerate sorted-rankings)
                  :let [{:keys [wins losses draws]} (get stats id)
                        player-name (get name-mapping id)
                        hidden? @(rf/subscribe [::handlers/hidden? id])
                        dead? @(rf/subscribe [::handlers/dead? id])]]

              [:tr {:class (if dead? "dead__ranking__row" "alive__ranking__row")}
               [:td [:span
                     (if hidden?
                       [:i.fas.fa-eye
                        {:title (str "Show " player-name)
                         :on-click #(rf/dispatch [::handlers/show id])}]

                       [:i.fas.fa-eye-slash
                        {:title (str "Hide " player-name)
                         :on-click #(rf/dispatch [::handlers/hide id])}])]]

               [:td [:span
                     (if dead?
                       [:i.fas.fa-life-ring
                        {:title (str "Revive " player-name)
                         :on-click #(rf/dispatch [::handlers/revive id])}]

                       [:i.fas.fa-skull
                        {:title (str "Kill " player-name)
                         :on-click #(rf/dispatch [::handlers/kill id])}])]]

               [:td idx]
               [:td player-name]
               [:td (int ranking)]
               [:td ngames]
               [:td (results-boxes (get results id))]
               [:td (str wins "/" losses "/" draws)]]))]]))

(defn show-error
  []
  (let [error @(rf/subscribe [::handlers/error])]
    (when error
      [:div.section.alert.alert-danger
       [:pre (:status-text error)]
       [:pre (:original-text error)]])))

(defn navbar
  []
  (let [league @(rf/subscribe [::handlers/league])]
    [:ul.navbar__root
     [:li.navbar__element
      [:a {:href "#"
           :on-click #(accountant/navigate! (routes/path-for :league-list))}
       "Home"]]
     [:li.navbar__element
      [:a.active {:href "#"} (:game_type league)]]
     [:li.navbar__element.fork_me
      [:a {:href "http://github.com/AndreaCrotti/elo"}
       "Fork Me"]]]))

(defn vega-outer
  []
  (let [history (rf/subscribe [::handlers/rankings-history-vega])
        rankings-domain (rf/subscribe [::handlers/rankings-domain])]

    (fn []
      [vega/vega-inner @history @rankings-domain])))

(defn highest-rankings
  []
  (let [highest-rankings (rf/subscribe [::handlers/highest-rankings-best])]
    (fn []
      [:div.highest__rankings__block
       [stats-table
        [{:k :player :v "name"} {:k :ranking :v "ranking"} {:k :time :v "time"}]
        (take 3 @highest-rankings)]])))

(defn longest-streaks
  []
  (let [longest-streaks (rf/subscribe [::handlers/longest-streaks])]
    (fn []
      [:div.longest__streaks__block
       [stats-table
        [{:k :player :v "name"} {:k :streak :v "streak"}]
        (take 3 @longest-streaks)]])))

(defn highest-increase
  []
  (let [highest (rf/subscribe [::handlers/highest-increase])]
    (fn []
      [:div.highest__increase__block
       [stats-table
        [{:k :player :v "name"} {:k :points :v "points"}]
        (take 3 @highest)]])))

(defn highest-percent

  []
  (let [best (rf/subscribe [::handlers/best-percents])]
    (fn []
      [:div.best__percent__block
       [stats-table
        [{:k :player :v "name"} {:k :w :v "wins %"}
         {:k :d :v "draws %"} {:k :l :v "loss %"}]
        (take 3 @best)]])))

(defn root
  []
  (rf/dispatch [::handlers/load-league])
  (rf/dispatch [::handlers/load-games])
  (rf/dispatch [::handlers/load-players])

  (fn []
    [:div.league_detail__root
     [navbar]
     [show-error]
     [:div.section.players__form_container [game-form]]
     [:div.section.players__stats
      [:div.players__highest_scores [highest-rankings]]
      [:div.players__highest_increase [highest-increase]]
      [:div.players__longest_streak [longest-streaks]]
      [:div.players__highest_percent [highest-percent]]]

     [:div.section.vega__table [vega-outer]]
     [:div.section.rankings__table [rankings-table]]
     [:div.section.games__table [games-table]]]))

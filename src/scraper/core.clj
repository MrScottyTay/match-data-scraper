(ns scraper.core
  (:require [net.cgrand.enlive-html :as html]
            [org.httpkit.client :as http]))
(use 'clojure.set)

;page data getter
(defn get-dom [url]
  (html/html-snippet (:body @(http/get url {:insecure? true}))))

;fixes table of numbers
(declare fix-floats-)
(defn fix-floats [values]
  (fix-floats- (rest values) (first values) []))
(defn fix-floats- [values current-value fixed-values]
  (if (empty? values) ;base case
    (into [] (reverse fixed-values))
    (fix-floats- (rest values) (first values) (cons (Float/parseFloat (str current-value)) fixed-values))
  ))

;--------------------------------------------------
;nba players table related functions
(defn get-player-name [raw-player-name]
  (clojure.string/lower-case
    (clojure.string/replace (clojure.string/replace (first (:content raw-player-name)) #"[^a-zA-Z\d\s]" "") " " "-")))

(defn parse-nba-player-stats [raw-player-stats]
  (let [flattened-stats (flatten (map :content raw-player-stats))]
    (hash-map (keyword (get-player-name (second flattened-stats))) (fix-floats (rest (rest (rest flattened-stats))) ))))

(declare parse-nba-player-stats-table-)
(defn parse-nba-player-stats-table [nba-stats-table]
  (let [nba-stats-table (rest nba-stats-table)] ;remove header row
    (parse-nba-player-stats-table- (rest (rest nba-stats-table))
                                   (second nba-stats-table)
                                   (parse-nba-player-stats (first nba-stats-table)))))
(defn parse-nba-player-stats-table- [nba-stats-table current-player parsed-stats]
  (if (empty? nba-stats-table)
    parsed-stats ;basecase
    (parse-nba-player-stats-table- (rest nba-stats-table)
                                   (first nba-stats-table)
                                   (merge (parse-nba-player-stats current-player) parsed-stats))))

(defn get-nba-player-stats []
  (merge
    (parse-nba-player-stats-table
      (map (comp :content)
        (html/select (get-dom "https://basketball.realgm.com/nba/stats/2018/Averages/Qualified/points/All/desc/1/Regular_Season") [:tr]))))
    (parse-nba-player-stats-table
      (map (comp :content)
        (html/select (get-dom "https://basketball.realgm.com/nba/stats/2018/Averages/Qualified/points/All/desc/2/Regular_Season" [:tr])))))

;--------------------------------------------------
;nba teams table related functions

(defn get-nba-team-link [team-row]
  (println team-row)
  )

(declare parse-nba-team-table-)
(defn parse-nba-team-table [nba-team-table]
  (let [nba-team-table (rest nba-team-table)] ;remove header row
    (parse-nba-team-table- (rest (rest nba-team-table)) (second nba-team-table) (get-nba-team-link (first nba-team-table)))))
(defn parse-nba-team-table- [nba-team-table current-team parsed-teams]
  (if (empty? nba-team-table)
    parsed-teams ;base case
    (parse-nba-team-table- (rest (nba-team-table)) (first nba-team-table) (cons (get-nba-team-link current-team) parsed-teams))))

(defn get-nba-teams []
  (parse-nba-team-table
    (map (comp :content) 
      (html/select (get-dom "https://basketball.realgm.com/nba/team-stats") [:tr]))))

(defn -main
  [& args]
  (println (get-nba-teams)))

(-main)
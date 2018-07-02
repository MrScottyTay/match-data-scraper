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

;turns trings into a suitable keywordable name
(defn get-keywordable-name [input]
  (clojure.string/lower-case
    (clojure.string/replace (clojure.string/replace input #"[^a-zA-Z\d\s]" "") " " "-")))

;--------------------------------------------------
;nba players table related functions
(defn parse-nba-player-stats [raw-player-stats]
  (let [flattened-stats (flatten (map :content raw-player-stats))]
    (hash-map (keyword (get-keywordable-name (first (:content (second flattened-stats))))), (fix-floats (rest (rest (rest flattened-stats)))) )))

(declare parse-nba-player-stats-table-)
(defn parse-nba-player-stats-table [nba-stats-table]
  (let [nba-stats-table (rest nba-stats-table)] ;remove header row
    (parse-nba-player-stats-table- (rest (rest nba-stats-table)), (second nba-stats-table),
                                   (parse-nba-player-stats (first nba-stats-table)))))
(defn parse-nba-player-stats-table- [nba-stats-table current-player parsed-stats]
  (if (empty? nba-stats-table)
    parsed-stats ;basecase
    (parse-nba-player-stats-table- (rest nba-stats-table), (first nba-stats-table),
                                   (merge (parse-nba-player-stats current-player) parsed-stats))))

(defn get-nba-player-stats [] ;redundant but staying in case it wants to be used
  (merge
    (parse-nba-player-stats-table
      (map (comp :content)
        (html/select (get-dom "https://basketball.realgm.com/nba/stats/2018/Averages/Qualified/points/All/desc/1/Regular_Season") [:tr]))))
    (parse-nba-player-stats-table
      (map (comp :content)
        (html/select (get-dom "https://basketball.realgm.com/nba/stats/2018/Averages/Qualified/points/All/desc/2/Regular_Season" [:tr])))))

;--------------------------------------------------
;nba teams table related functions

(defn parse-nba-team-split [raw-team-split]
  (let [flattened-split (flatten (map :content raw-team-split))]
    (let [vs-team (get-keywordable-name (first flattened-split))]
      (hash-map (keyword (.substring vs-team 2 (count vs-team))), (fix-floats (rest flattened-split)) ))))

(declare parse-nba-team-splits-table-)
(defn parse-nba-team-splits-table [nba-team-splits-table]
  (let [nba-team-splits-table (rest nba-team-splits-table)] ;remove header row
    (parse-nba-team-splits-table- (rest (rest nba-team-splits-table)), (second nba-team-splits-table),
                                  (parse-nba-team-split (first nba-team-splits-table)))))
(defn parse-nba-team-splits-table- [nba-team-splits-table current-team-split parsed-team-splits]
  (if (empty? nba-team-splits-table)
    parsed-team-splits
    (parse-nba-team-splits-table- (rest nba-team-splits-table), (first nba-team-splits-table),
                                  (merge (parse-nba-team-split current-team-split) parsed-team-splits))))

(defn parse-nba-team-stats [nba-team-link]
  (let [tables (html/select (get-dom (str "https://basketball.realgm.com" nba-team-link)) [:tbody])]
    (hash-map :players (parse-nba-player-stats-table (map (comp :content) (html/select (first tables) [:tr]))),
              :vs (parse-nba-team-splits-table (map (comp :content) (html/select (nth tables 2) [:tr]))))))

(defn parse-nba-team [team-row]
  (let [raw-team-name (first (:content (second team-row)))]
    (hash-map (keyword (get-keywordable-name (first (:content raw-team-name)))), (parse-nba-team-stats (:href (:attrs raw-team-name))))))

(declare parse-nba-team-table-)
(defn parse-nba-team-table [nba-team-table]
  (let [nba-team-table (rest nba-team-table)] ;remove header row
    (parse-nba-team-table- (rest (rest nba-team-table)) (second nba-team-table) (parse-nba-team (first nba-team-table)))))
(defn parse-nba-team-table- [nba-team-table current-team parsed-teams]
  (if (empty? nba-team-table)
    parsed-teams ;base case
    (parse-nba-team-table- (rest nba-team-table) (first nba-team-table) (cons (parse-nba-team current-team) parsed-teams))))

(defn get-nba-teams []
  (parse-nba-team-table
    (map (comp :content) 
      (html/select (get-dom "https://basketball.realgm.com/nba/team-stats") [:tr]))))

(defn -main
  [& args]
  (println (get-nba-teams))
  (println "DONE"))

(-main)

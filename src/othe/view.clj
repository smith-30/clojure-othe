(ns othe.view
  (:require [clojure.string :refer [join]]
            [othe.model :refer [b-size count-blacks count-whites first-row
                                is-game-over? last-row retrieve-board]]))

(def code-a 97) ; a の文字コード
(def code-curly 123) ; z の次の文字コード
(def col-headers
  (take b-size
        (map (comp str char)
             (range code-a code-curly))))
(def col-header-str
  (str " " (join " " col-headers)))

(defn- st-str
  "マスの状態を表す文字列"
  [st]
  (cond (= st :b) "x"
        (= st :w) "⚪︎"
        :else " "))

(defn- board-strs
  "文字列シーケンス. ボードの各行をレンダリングしたもの"
  [brd]
  (for [row (partition b-size brd)]
    (join " " (map st-str row))))

; map は要素の数を合わせて実行される
(defn- board-strs-with-row
  "board-strs に行番号を付与したもの"
  [brd]
  (map str
       (range (inc first-row) (inc last-row)); ex. (0 1 2 3 4 5 6 7)
       (repeat b-size " ") ; ex. (" " " " ...)
       (board-strs brd))) ; ex. ("x x x x x ⚪︎ x x"). 1回目のループ 0 + " " + "x x x x x ⚪︎ x x" => (...)

(def separator (join (repeat 50 \-)))

(defn- score-str
  "スコア文字列"
  [bs ws]
  (let [s (str "BLACK(x): p:" bs ", WHITE(⚪︎):" ws)]
    (format "%50s" s))); 文字列を50文字の幅で右寄せするフォーマット指定

(defn- winner-str
  "勝者文字列"
  [bs ws]
  (cond
    (> bs ws) "Winner is BLACK. Conguratulations"
    (> ws bs) "Year, WHITE won!!"
    :else "It's a draw game."))

(defn- redraw-board
  "盤面を表示"
  []
  (print col-header-str)
  (dorun
   (map println
        (board-strs-with-row (retrieve-board)))))

(defn on-state-changed
  "Model変化時のハンドラ"
  [& e]
  (if e
    (print "You can't move there. Input again: ")
    (let [bs (count-blacks)
          ws (count-whites)]
      (println  separator)
      (println  (score-str bs ws))
      (redraw-board)
      (when (is-game-over?)
        (println (str "GAME OVER: " (winner-str bs ws)))
        #_{:clj-kondo/ignore [:unresolved-symbol]}
        (command-handler [:exit])))))

(defn init-view
  "View を初期化。handler はユーザコマンドのハンドラ"
  [handler]
  (println "Welcome to the Battle zone!")
  (println "'x' is Black and '⚪︎' is White.")
  (println "Input the column name first, like 'a1' or 'b2'")
  (println "Just hit Enter to exit.")
  (def command-handler handler))
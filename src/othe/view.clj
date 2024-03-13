(ns othe.view
  (:require [clojure.string :refer [join]]
            [othe.model :refer [b-size count-blacks count-whites first-row
                                is-black-turn? is-game-over? last-row
                                pos-from-rowcol retrieve-board]]))

(def code-a 97) ; a の文字コード
(def code-curly 123) ; z の次の文字コード
(def col-headers
  (take b-size
        (map (comp str char)
             (range code-a code-curly))))
(def col-header-str
  (str "  " (join " " col-headers)))

(defn- st-str
  "マスの状態を表す文字列"
  [st]
  (cond (= st :b) "x"
        (= st :w) "○"
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
  (let [s (str "BLACK(x):" bs ", WHITE(⚪︎):" ws)]
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
  (println)
  (dorun
   (map println
        (board-strs-with-row (retrieve-board)))))

(defn init-view
  "View を初期化。handler はユーザコマンドのハンドラ"
  [handler]
  (println "Welcome to the Battle zone!")
  (println "'x' is Black and '⚪︎' is White.")
  (println "Input the column name first, like 'a1' or 'b2'")
  (println "Just hit Enter to exit.")
  (def command-handler handler))

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
        (command-handler [:exit])))))

(defn- read-cmd
  "stdin から、コマンドを読む"
  []
  (print (if (is-black-turn?)
           "It's BLACK's turn: "
           "Hey WHITE, your turn: "))
  (flush)
  (read-line))


(defn- col-from-line
  "ユーザ入力から桁を解読"
  [line]
  (.indexOf col-headers (subs line 0 1)))

(defn- row-from-line
  "ユーザ入力から行を解読"
  [line]
  (dec (read-string (subs line 1))))

(defn- pos-from-line
  "ユーザ入力からposを解読。不正な入力値なら nil"
  [line]
  (when (re-find #"^[a-h][1-8]" line)
    (let [r (row-from-line line)
          c (col-from-line line)]
      (pos-from-rowcol r c))))

(defn- wait-for-cmd
  "ユーザ入力を待ち、nil か pos を返す"
  []
  (loop [line (read-cmd)]
    (if (empty?  line)
      (println "Exiting....")
      (if-let [pos (pos-from-line line)]
        pos
        (do
          (print "Input should be like a1 or b2. Or Enter to exit:")
          (flush)
          (recur (read-cmd)))))))

(defn- view-thread
  "ユーザ入力を監視するスレッド。入力が空だったら終了"
  []
  (loop [pos (wait-for-cmd)]
    (when pos
      (command-handler [:move pos])
      (recur (wait-for-cmd))))
  (command-handler [:exit]))

(defn start-ui
  "ユーザとのインタラクションを開始"
  []
  (.start (Thread. view-thread)))
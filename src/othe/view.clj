(ns othe.view
  (:use othe.model
        [clojure.string :only (join)]))

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
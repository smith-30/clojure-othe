(ns othe.model)

(def b-size 8)
(def first-pos 0)
(def last-pos (* b-size b-size))
(def all-pos (range first-pos last-pos))

(def first-col 0)
(def last-col b-size)
(def first-row 0)
(def last-row b-size)

(defn col-from-pos [pos] (mod pos b-size))
(defn row-from-pos [pos] (quot pos b-size))
(defn pos-from-rowcol [r c] (+ (* r b-size) c))

(def dirs #{:n :ne :e :se :s :sw :w :nw})

(def board (ref [])) ; 盤面状態ベクタ
(def player (ref nil)) ; 次の手番。:b か :w
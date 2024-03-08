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

(def successor (let [north (fn [pos] (- pos b-size))
                     east inc
                     south (fn [pos] (+ pos b-size))
                     west dec]
                 {:n north
                  :ne (comp north east); 関数合成 east -> north の順に処理する
                  :e east
                  :se (comp south east)
                  :s south
                  :sw (comp south west)
                  :w west
                  :nw (comp north west)}))

(def not-wrapped?
  (let [east? (fn [pos] (> (col-from-pos pos) first-col))
        west? (fn [pos] (< (col-from-pos pos) (dec last-col)))]
    {:n identity
     :ne east?
     :e east?
     :se east?
     :s identity
     :sw west?
     :w west?
     :nw west?}))

(defn- in-board? [pos] (and (>= pos first-pos) (< pos last-pos)))

(defn- posline-for-dir
  "pos における dir 方向への posline"
  [pos dir]
  (let [suc (successor dir)
        nwrap? (not-wrapped? dir)]
    (take-while
     (fn [pos]
       (and (nwrap? pos) (in-board? pos)))
     (iterate suc (suc pos)))))

(defn- free?
  "空きマスかどうか"
  [brd pos]
  (= (brd pos) :free))

(defn- self?
  "自陣のマスかどうか"
  [brd pos bw]
  (and (not (free? brd pos)) (= (brd pos) bw)))

(defn- opponent?
  "posline の最初の要素が敵陣かどうか調べる"
  [brd pos bw]
  (and (not (free? brd pos)) (not= (brd pos) bw)))

(defn- all-poslines
  "pos における、各方角への posline を集めたシーケンス"
  [pos]
  (filter not-empty
          (for [dir dirs] (posline-for-dir pos dir))))

(defn- clamping?
  "bw にとって、posline は挟めるか?"
  [brd posline bw]
  (and
   (opponent? brd (first posline) bw)
   (if-let
    [fst
     (first
      (filter
       (fn [pos] (not (opponent? brd pos bw)))
       (rest posline)))]
     (self? brd fst bw)
     nil)))

(defn- playable?
  "bw にとって、pos は打てる場所か?"
  [brd pos bw]
  (and
   (free? brd pos)
   (some
    (fn [pl] (clamping? brd pl bw))
    (all-poslines pos))))
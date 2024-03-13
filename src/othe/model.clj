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
(defn pos-from-rowcol
  "row & col から pos を計算"
  [r c] (+ (* r b-size) c))

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

(def initial-oprs
  "ゲームの初期状態(:b :wが2個ずつ)を表す opr のマップ"
  (let [cntr (dec (quot b-size 2))
        pos (pos-from-rowcol cntr cntr)]
    {pos :b
     ((successor :se) pos) :b
     ((successor :e) pos) :w
     ((successor :s) pos) :w}))

(defn- board-manipulator
  "opr のマップに基づいて、盤面を変更するラムダ"
  [oprs]
  (fn [pos st] (if-let [s (oprs pos)]
                 s; then
                 st; else
                 )))

; map-indexed で brd の [index elem(board の状態)] が manip に適用される
(defn- manipulated-board
  "manipulator を盤面に対して呼んだ後の新しい盤面"
  [brd manip]
  (vec (map-indexed manip brd)))

(let [blank (vec (repeat (- last-pos first-pos) :free))
      manip (board-manipulator initial-oprs)]
  (manipulated-board blank manip))

(defn- make-oprs
  "posline に関して、bw にとっての opr を計算する"
  [brd posline bw]
  (reduce (fn [m pos] (assoc m pos bw)) {}
          (take-while
           (fn [pos] (opponent? brd pos bw))
           posline)))

(defn- make-all-oprs
  "pos における全 posline に関して、bw にとっての opr を計算する"
  [brd pos bw]
  (apply merge
         (cons {pos bw}
               (for [posline
                     (filter
                      (fn [pos] (clamping? brd pos bw))
                      (all-poslines pos))]
                 (make-oprs brd posline bw)))))

(defn- opponent [bw] (if (= bw :b) :w :b))

(defn- has-pos-to-play?
  "bw にとって、打てる場所はあるか?"
  [brd bw]
  (not-empty
   (filter
    (fn [pos] (playable? brd pos bw))
    all-pos)))

(defn- next-player
  "bw の次は誰の番か決める"
  [bw brd]
  (let [nbw (opponent bw)]
    (if (has-pos-to-play? brd nbw) nbw bw)))

(defn init-game
  "新しいゲームを始める"
  [ob]
  (dosync
   (let [blank (vec
                (repeat (- last-pos first-pos) :free))
         manip
         (board-manipulator initial-oprs)]
     (ref-set board
              (manipulated-board blank manip)))
   (ref-set player :b)
   (def observer ob))
  (observer))

(def board (ref []))
(def player (ref nil))

(defn play-move
  "pos へ打つ"
  [pos]
  (dosync
   (if (not (playable? @board pos @player))
     (observer :err)
     (do
       (let
        [manip
         (board-manipulator
          (make-all-oprs @board pos @player))]
         (alter board manipulated-board manip))
       (alter player next-player @board)
       (observer)))))

(defn- retrieve-game-state
  "ゲーム状態(黒番なら:b、白番なら:w、ゲーム終了なら:over)"
  []
  (let [brd @board
        bw @player]
    (if (empty?
         (filter
          (fn [pos] (free? brd pos))
          all-pos))
      :over
      bw)))

(defn- occupacy
  "bw の陣地の広さ"
  [brd bw]
  (count
   (filter
    (fn [pos] (= (brd pos) bw))
    all-pos)))

(defn is-game-over?
  []
  (= (retrieve-game-state) :over))

(defn is-black-turn?
  []
  (= (retrieve-game-state) :b))

(defn count-blacks [] (occupacy @board :b))

(defn count-whites [] (occupacy @board :w))

(defn retrieve-board [] @board)
(ns othe.core
  (:require [othe.model :refer [init-game play-move]]
            [othe.view :refer [init-view on-state-changed]]))

(defn on-command
  "Viewからのコマンド通知を処理するハンドラ."
  [[cmd pos]]
  (cond
    (= cmd :move) (play-move pos)
    (= cmd :exit) (System/exit 0)
    :else nil))

(defn -main
  "entry point"
  [& args]
  (init-view on-command)
  (init-game on-state-changed))
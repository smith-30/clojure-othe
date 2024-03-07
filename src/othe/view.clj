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
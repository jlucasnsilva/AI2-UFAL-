(ns risco-de-ectasia.core
  (:require [seesaw.core :as ss]
            [clojure.java.io :as io])
  (:import [weka.core Instances Utils]
           [weka.classifiers Evaluation]
           [weka.classifiers.trees J48]
           [weka.filters Filter]
           [weka.filters.unsupervised.attribute Remove Normalize])
  (:gen-class))

(defn j48-tree
  [path-to-dataset class-index]
  (with-open [rdr (io/reader path-to-dataset)]
    (let [data (doto (Instances. rdr)
                 (.setClassIndex ,,, class-index))
          new-data (Filter/useFilter
                    data
                    (doto (Remove.)
                      (.setOptions (Utils/splitOptions "-R 1"))
                      (.setInputFormat data)))
          new-data (Filter/useFilter
                    new-data
                    (doto (Normalize.)
                      (.setInputFormat new-data)))
          classifier (doto (J48.)
                       (.setUnpruned ,,, true)
                       (.buildClassifier ,,, new-data))]
      classifier)))

(defn input-panel
  []
  (let [risk-text (ss/text :multi-line? false :editable? false :text "<Risco>")
        a-text (ss/text :multi-line? false :editable? false :text "0.0")
        b-text (ss/text :multi-line? false :editable? false :text "0.0")
        c-text (ss/text :multi-line? false :editable? false :text "0.0")
        d-text (ss/text :multi-line? false :editable? false :text "0.0")
        e-text (ss/text :multi-line? false :editable? false :text "0.0")
        f-text (ss/text :multi-line? false :editable? false :text "0.0")
        g-text (ss/text :multi-line? false :editable? false :text "0.0")
        h-text (ss/text :multi-line? false :editable? false :text "0.0")
        i-text (ss/text :multi-line? false :editable? false :text "0.0")
        j-text (ss/text :multi-line? false :editable? false :text "0.0")
        l-text (ss/text :multi-line? false :editable? false :text "0.0")
        m-text (ss/text :multi-line? false :editable? false :text "0.0")
        n-text (ss/text :multi-line? false :editable? false :text "0.0")
        button-listener (fn [_]
                          (ss/alert "WORKS!"))]
    (ss/grid-panel :columns 2
                   :items ["Risco" (ss/text :multi-line? false
                                            :editable? false
                                            :text "<Risco>")
                           "A" a-text
                           "B" b-text
                           "C" c-text
                           "D" d-text
                           "E" e-text
                           "F" f-text
                           "G" g-text
                           "H" h-text
                           "I" i-text
                           "J" j-text
                           "L" l-text
                           "M" m-text
                           "N" n-text
                           (ss/button :text "Classificar"
                                      :listen [:mouse-clicked button-listener])])))

(defn classifier-info-panel
  [classifier]
  (let [text (str classifier)]
    (ss/scrollable
     (ss/text :multi-line? true
              :editable? false
              :text text))))

(defn -main
  [& args]
  (let [tree (j48-tree "data/dataset.arff" 1)]
    (-> (ss/frame :title "Risco de Ectasia"
                  :size [1280 :by 768]
                  :on-close :exit
                  :content (ss/left-right-split
                            (input-panel)
                            (classifier-info-panel tree)
                            :divider-location 0.3
                            :resize-weight 0.3))
        ss/show!)))

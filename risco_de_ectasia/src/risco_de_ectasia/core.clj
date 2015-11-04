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
  (ss/grid-panel :columns 2
                 :items ["Risco" (ss/text :multi-line? false
                                       :editable? false
                                       :text "<Risco>")
                         "A" (ss/text :multi-line? false :columns 10)
                         "B" (ss/text :multi-line? false :columns 10)
                         "C" (ss/text :multi-line? false :columns 10)
                         "D" (ss/text :multi-line? false :columns 10)
                         "E" (ss/text :multi-line? false :columns 10)
                         "F" (ss/text :multi-line? false :columns 10)
                         "G" (ss/text :multi-line? false :columns 10)
                         "H" (ss/text :multi-line? false :columns 10)
                         "I" (ss/text :multi-line? false :columns 10)
                         "J" (ss/text :multi-line? false :columns 10)
                         "L" (ss/text :multi-line? false :columns 10)
                         "M" (ss/text :multi-line? false :columns 10)
                         "N" (ss/text :multi-line? false :columns 10)
                         (ss/button :text "Classificar")]))

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

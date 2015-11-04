(ns river-crossing.core
  (:require [clojure.pprint :refer [pprint]])
  (:gen-class))

;; Transição de estado
;;   Uma transição de estados ocorre em dois passos:
;;   (1) Um ou dois indivíduos saem de :side-a ou :side-b
;;       para :boat (função board).
;;   (2) Um dois indivíduos saem de :boat para side-a ou
;;       para :side-b (função land).

(defrecord Place [cannibals missionaries])

(def init-state {:side-a (->Place 3 3)
                 :side-b (->Place 0 0)
                 :boat   (->Place 0 0)})

(def final-state {:side-a (->Place 0 0)
                  :side-b (->Place 3 3)
                  :boat   (->Place 0 0)})

(def boarding-possibilities
  [[2 0]   ;; 2 cannibals, 0 missionaries
   [1 0]   ;; 1 cannibals, 0 missionaries
   [1 1]   ;; 1 cannibals, 1 missionaries
   [0 1]   ;; 0 cannibals, 1 missionaries
   [0 2]]) ;; 0 cannibals, 2 missionaries

(def landing-possibilities
  [[1 0]   ;; 1 cannibals, 0 missionaries
   [0 1]]) ;; 0 cannibals, 1 missionaries

(defn valid-side?
  "Verifica se um lado é válido, ou seja,
  se o número de missionários é maior ou
  igual ao número de canibais."
  [{nc :cannibals nm :missionaries}]
  (if (and (> nm 0) (> nc 0))
    (>= nm nc)
    (and (>= nm 0) (>= nc 0))))

(defn valid-boat?
  "Verifica se o barco tem no máximo duas
  pessoas."
  [{nc :cannibals nm :missionaries}]
  (and (<= (+ nc nm) 2)
       (>= nc 0)
       (>= nm 0)))

(defn someone-left-behind?
  [{c :cannibals m :missionaries}]
  (pos? (+ c m)))

(defn move
  "Move uma quatidade de canibais igual a <ncannibals> e uma
  quantidade de missionários igual a <nmissionaries> de um
  lado para o barco ou do barco para um lado, dependendo do
  valor de <tag>. Retorna um par de lugares [new-form new-to].
  <tag> = :to-boat || :from-boat"
  [[ncannibals nmissionaries] tag from to]
  (let [{fc :cannibals fm :missionaries} from
        {tc :cannibals tm :missionaries} to
        from (->Place (- fc ncannibals) (- fm nmissionaries))
        to   (->Place (+ tc ncannibals) (+ tm nmissionaries))]
    (condp = tag
      :to-boat   (when (and (valid-side? from) (valid-boat? to))
                   [from to])
      :from-boat (when (and (valid-boat? from) (valid-side? to))
                   [from to]))))

(defn board
  "Retira indivíduos do lado a e os coloca
  no barco."
  [{:keys [side-a boat] :as state}]
  (if (someone-left-behind? side-a)
    (let [step (fn [pair]
                 (when-let [[new-side new-boat :as both] (move pair :to-boat side-a boat)]
                   (-> state
                       (assoc ,,, :boat new-boat)
                       (assoc ,,, :side-a new-side))))]
      (->> boarding-possibilities
           (map step ,,,)
           (filter some? ,,,)))
    [state]))

(defn land
  "Retira indivíduos do barco e os coloca
  no lado b."
  [{:keys [side-b boat] :as state}]
  (let [step (fn [pair]
               (when-let [[new-boat new-side] (move pair :from-boat boat side-b)]
                 (-> state
                     (assoc ,,, :boat new-boat)
                     (assoc ,,, :side-b new-side))))]
    (->> landing-possibilities
         (map step ,,,)
         (filter some? ,,,))))

(defn cross
  "Faz indivíduos cruzarem o rio."
  [state]
  (mapcat land (board state)))

(defn expand
  "Expande a árvore de busca"
  [state prev-states]
  (filter #(not (contains? prev-states %)) (cross state)))

(defn checking-dfs
  "Checking depth-first search: faz busca na árvore
  desconsiderando estados onde já esteve."
  ([state target expantion-fn]
   (checking-dfs state target expantion-fn []))
  ([state target expantion-fn path]
   (when-not (nil? state)
     (if (.equals state target)
       (conj path state)
       (when-not (contains? path state)
         (let [path (conj path state)
               branches (expantion-fn state path)
               df-search #(checking-dfs % target expantion-fn path)]
           (->> branches
                (map df-search ,,,)
                (filter (comp not nil?) ,,,)
                first)))))))

(defn safe-and-happy-trip-for-all
  []
  (loop [states (checking-dfs init-state final-state expand)
         step   0]
    (when-let [st (first states)]
      (println (str step "º") "passo:")
      (pprint st)
      (recur (next states) (inc step)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (safe-and-happy-trip-for-all))

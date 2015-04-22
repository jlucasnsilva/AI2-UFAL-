(ns tic-tac-toe.core
  (:import [java.util Scanner])
  (:gen-class))

(declare max-step)
(declare min-step)

(def cpu-tag 1)
(def player-tag 2)

(def positions
  (for [i (range 0 3) j (range 0 3)] [i j]))

(defn enemy
  [player]
  (if (= player cpu-tag)
    player-tag
    cpu-tag))

(def init-board
  [[0 0 0]
   [0 0 0]
   [0 0 0]])

(def verification-fns
  [;; looks for an winner in the lines
   (fn [board tag]
     (some true? (map #(apply = (conj % tag)) board)))
   ;; looks for an winner in the columns
   (fn [board tag]
     (some true? (apply #(map = %1 %2 %3 [tag tag tag]) board)))
   ;; looks for an winner in the main diagonal
   (fn [board tag]
     (= ((board 0) 0)
        ((board 1) 1)
        ((board 2) 2)
        tag))
   ;; looks for an winner in the secondary diagonal
   (fn [board tag]
     (= ((board 2) 0)
        ((board 1) 1)
        ((board 0) 2)
        tag))])

(defn available?
  "Retorna <true> se a posição [row col] do
  tabuleiro de tic-tac-toe estiver disponível
  (= 0), <false> caso contrário."
  [board [row col]]
  (zero? (get-in board [row col])))

(defn won?
  "Verifica se o jogador identificado por
  <tag> venceu o jogo."
  [board tag]
  (some true? (map #(% board tag) verification-fns)))

(defn draw?
  "Verifica se o jogo terminou em empate."
  [board]
  (and (not (won? board player-tag))
       (not (won? board cpu-tag))
       (not (some true? (map #(available? board %) positions)))))

(defn expand
  "Lazily expands the search tree in one level."
  ([board player]
   (expand board player (for [i (range 0 3) j (range 0 3)] [i j])))
  ([board player coords]
   (lazy-seq
    (when-let [ij (first (seq coords))]
      (if (available? board ij)
        (cons (assoc-in board ij player) (expand board player (next coords)))
        (expand board player (next coords)))))))

(defn min-max
  [board player player-enemy]
  (cond (won? board player)       [1 board]
        (won? board player-enemy) [-1 board]
        (draw? board)             [0 board]
        :else (let [reduce-fn (fn [[rx _ :as x] [ry _ :as y]]
                                (if (>= rx ry) x y))]
                (->> player
                     (expand board ,,,)
                     (pmap #(min-step % player-enemy player) ,,,)
                     (reduce reduce-fn ,,,)))))

(defn max-step
  "Passo \"max\" do min-max."
  [board player player-enemy]
  (cond (draw? board)             [ 0 board]
        (won? board player)       [ 1 board]
        (won? board player-enemy) [-1 board]
        :else (let [reduce-fn (fn [[xg _] [yg _]]
                                (if (>= xg yg) [xg board] [yg board]))]
                (->> player
                     (expand board ,,,)
                     (map #(min-step % player-enemy player) ,,,)
                     (reduce reduce-fn ,,,)))))

(defn min-step
  "Passo \"min\" do min-max."
  [board player player-enemy]
  (cond (draw? board)             [ 0 board]
        (won? board player)       [-1 board]
        (won? board player-enemy) [ 1 board]
        :else (let [reduce-fn (fn [[xg _] [yg _]]
                                (if (<= xg yg) [xg board] [yg board]))]
                (->> player
                     (expand board ,,,)
                     (map #(max-step % player-enemy player) ,,,)
                     (reduce reduce-fn ,,,)))))

(defn min-max*
  [board player player-enemy]
  (second (min-max board player player-enemy)))

(def input-reader (Scanner. *in*))

(defrecord Player [name strategy])

;; (assoc-in board ij player)
(defn player-input
  [board player]
  (loop []
    (let [i (.nextInt input-reader)
          j (.nextInt input-reader)]
      (if (and (>= i 0) (>= j 0) (< i 3)  (< j 3))
          (if (available? board [i j])
            (assoc-in board [i j] player)
            (do (println "Posição não disponível.")
                (print ">: ")
                (recur)))
          (do (println "Entrada consiste de dois números maiores ou iguais a 0 e menores que 3.")
              (print ">: ")
              (recur))))))

(defn draw-board
  [board]
  (do (println)
      (println "     [0] [1] [2]  ")
      (println "    +---+---+---+")
      (println "[0] |" ((board 0) 0) "|" ((board 0) 1) "|" ((board 0) 2) "|")
      (println "    |---|---|---|")
      (println "[1] |" ((board 1) 0) "|" ((board 1) 1) "|" ((board 1) 2) "|")
      (println "    |---|---|---|")
      (println "[2] |" ((board 2) 0) "|" ((board 2) 1) "|" ((board 2) 2) "|")
      (println "    +---+---+---+")))

(defn game []
  (let [board (atom init-board)]
    (loop []
      (do (draw-board @board)
          (println "Tic-Tac-Toe Kasparov joga:")
          (swap! board min-max* cpu-tag player-tag)
          (draw-board @board)
          (println "==============================")
          (cond (won? @board cpu-tag) (println "Você perdeu... LOSER!")
                (won? @board player-tag)  (println "Você ganhou? COMO!?")
                (draw? @board) (println "Deu empate =(. Ah! Você não ganhou =D!")
                :else (do (println "Você joga:")
                          (swap! board player-input player-tag)
                          (cond (won? @board cpu-tag) (println "Você perdeu... LOSER!")
                                (won? @board player-tag)  (println "Você ganhou? COMO!?")
                                (draw? @board) (println "Deu empate =(. Ah! Você não ganhou =D!")
                                :else (recur))))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (game))

(ns travelling-salesman.core
  (:gen-class))

(declare path?)

(defn gen-swaps-list
  "Gera um lista com todas as trocas de dois a dois
  dos elementos de um caminho. <len> é comprimento
  do caminho."
  [len]
  (for [i (range 1 len) j (range (inc i) len)]
    [i j]))

(def num-of-cities 10)
(def swaps-list (gen-swaps-list num-of-cities))

(defn gen-random-tuple
  "Gera uma <num-of-cities>-tupla cujo primeiro 
  elemento é <fst-cty> e os seguintes são escolhidos
  aleatoriamente no intervalo [1, num-of-cities - 1]."
  [fst-city]
  (loop [route [fst-city]
         past #{fst-city}]
    (if (= (count route) num-of-cities)
      route
      (let [n (rand-int num-of-cities)]
        (if (past n)
          (recur route past)
          (recur (conj route n) (conj past n)))))))

(defn gen-random-route
  "Roda <gen-random-tuple> <attempts> vezes em
  busca de uma rota/caminho iniciado em <fst-city>
  em <roads-map>."
  [roads-map fst-city attempts]
  (loop [c attempts]
    (when (pos? c)
      (let [p (gen-random-tuple fst-city)]
        (if (path? roads-map p)
          p
          (recur (dec c)))))))

;; Infinity
(def inf 9000000)

(def roads-map
  [[  0  30  84  56 inf inf inf  75 inf  80]
   [ 30   0  65 inf inf inf  70 inf inf  40]
   [ 84  65   0  74  52  55 inf  60 143  48]
   [ 56 inf  74   0 135 inf inf  20 inf inf]
   [inf inf  52 135   0  70 inf 122  98  80]
   [ 70 inf  55 inf  70   0  63 inf  82  35]
   [inf  70 inf inf inf  63   0 inf 120  57]
   [ 75 inf 135  20 122 inf inf   0 inf inf]
   [inf inf 143 inf  98  82 120 inf   0 inf]
   [ 80  40  48 inf  80  35  57 inf inf   0]])

(defn connected?
  "Retorna <true> se <p1> está conectado à <p2>
  no grafo <roads-map>, <falase> caso contrário"
  [roads-map p1 p2]
  (and (>= p1 0)
       (>= p2 0)
       (< p1 num-of-cities)
       (< p2 num-of-cities)
       (< ((roads-map p1) p2) inf)))

(defn path?
  "Retorna <true> se <route> foi uma rota/caminho
  no grafo <roads-map> e <false> se não for."
  [roads-map route]
  (let [step (fn [r]
               (if (> (count r) 1)
                 (and (connected? roads-map (first r) (second r))
                      (recur (next r)))
                 (connected? roads-map (first r) (first route))))]
    (step route)))

(defn path-len
  "Retorna o comprimento do caminho <route>."
  [roads-map route]
  (loop [r   route
         len 0]
    (if (= (count r) 1)
      (+ ((roads-map (first r)) (first route)) len)
      (recur (next r) (+ ((roads-map (first r)) (second r)) len)))))

(defn gen-swaps
  "Gera todas as permutações sobre <route> com primeiro elemento
  fixo."
  [route]
  (let [swap (fn [[i j]]
               (assoc route i (route j) j (route i)))]
    (map swap swaps-list)))

(defn gen-swaps-paths
  "Retorna todos os caminhos gerados a partir de
  permutações em <route>."
  [roads-map route]
  (filter #(path? roads-map %) (gen-swaps route)))

(defn filter-best-route
  "Retorna o caminho mais curto entre <routes>."
  [roads-map routes]
  (->> routes
       (map (fn [p] {:path p :path-len (path-len roads-map p)}) ,,,,)
       (reduce (fn [{pl1 :path-len :as p1} {pl2 :path-len :as p2}]
                 (if (<= pl1 pl2) p1 p2)) ,,,,)))

(defn get-best-son-of
  "Gera todos caminhos de permutações de <route>
  e retorna o mais curto dentre eles."
  [roads-map route]
  (filter-best-route roads-map (gen-swaps-paths roads-map route)))

(defn hill-climbing
  "Algoritmo de hill climbing para encontrar o caminho
  mais curto em <roads-map> a partir de sucessivos
  melhoramentos ao caminho <route-1>."
  [roads-map route-1]
  (loop [best-route {:path route-1 :path-len (path-len roads-map route-1)}
         new-route  (get-best-son-of roads-map (:path best-route))
         history []]
    (if (>= (:path-len new-route) (:path-len best-route))
      {:best-path best-route :history history}
      (recur new-route
             (get-best-son-of roads-map (:path new-route))
             (conj history best-route)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [rv (rand-int 5)
        rr (gen-random-route roads-map rv 10000)]
    (do (println "Gerando um caminho aleatoriamente iniciando em" rv)
        (println "Caminho gerado:")
        (println "\t" rr)
        (println "Hill climbing...")
        (clojure.pprint/pprint (hill-climbing roads-map rr)))))

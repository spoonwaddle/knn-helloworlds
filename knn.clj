(ns clojure-noob.core
  (:gen-class))

(defn manhattan-distance
  [vec1 vec2]
  (reduce +
    (map #(Math/abs (- %1 %2)) vec1 vec2)))

(defn square [n] (Math/pow n 2))

(defn euclidean-distance
  [vec1 vec2]
  (Math/sqrt
    (reduce + (map #(square (- %1 %2)) vec1 vec2))))

(defn nearest-neighbors
  [training-samples query-sample k]
  (take k
    (sort-by :score
      (map
        (fn [{:keys [sample] :as neighbor}]
          (assoc neighbor :score (euclidean-distance query-sample sample)))
        training-samples))))

(defn knn
  [training-samples query-sample k]
  (let [votes (nearest-neighbors training-samples query-sample k)
        vote-counts (frequencies (map :label votes))]
  (key (apply max-key val vote-counts))))

(def training-data
  [{:sample [  1   2] :label "cat"}
   {:sample [2.6   3] :label "cat"}
   {:sample [3.4   4] :label "cat"}
   {:sample [5.3 4.2] :label "cat"}
   {:sample [5.4 7.7] :label "cat"}
   {:sample [87   88] :label "dog"}
   {:sample [90   91] :label "dog"}
   {:sample [100 105] :label "dog"}
   {:sample [104 105] :label "dog"}
   {:sample [110 110] :label "dog"}])

(defn -main
  [& args]
  (let [query1 [10 10] query2 [100 100]]
    (println query1 " is closest to... " (knn training-data query1 5))
    (println query2 " is closest to... " (knn training-data query2 5))))

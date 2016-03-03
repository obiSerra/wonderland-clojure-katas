(ns wonderland-number.finder)
(defn same-digit?
  "checks if 2 nums has the same digits"
  [num-a num-b]
  (= (sort (vec (str num-a))) (sort (vec (str num-b)))))

(defn wonderland-number []
  ;; calculate me
  (first
    (filter
      #(and (same-digit? % (* % 2)) (same-digit? % (* % 3)) (same-digit? % (* % 4)) (same-digit? % (* % 5)) (same-digit? % (* % 6)))
      (range 99999 999999))))

(wonderland-number)

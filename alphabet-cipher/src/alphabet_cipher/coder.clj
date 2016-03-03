(ns alphabet-cipher.coder)

(defn to-char
  [letter]
  (get (char-array letter) 0))

(defn letter-position
    [letter]
    (- (int (to-char letter)) 97))

(defn fill-array [from to start]
  ""
  (concat
    (range start (+ to 1))
    (range (+ from 0) start)))

(defn alphabet-int-from [start]
  (->>
    (fill-array (int \a) (int \z) (int (to-char start)))
    (map char)
    (map str)))

(defn convert-letter
  [letter-col letter-row]
  (get
    (vec (alphabet-int-from letter-col))
    (letter-position letter-row)))

(defn repeat-keyword
  [keyword message]
  (if (< (count message) (count keyword))
    (subs keyword 0 (count message))
    (repeat-keyword (str keyword keyword) message)))

(defn add-one-letter
  [target pool]
  (clojure.string/join [target (str (get pool (count target)))]))

(defn reduce-keyword
  [repeated-k keyword]
  (if (= (repeat-keyword keyword repeated-k) repeated-k)
    keyword
    (reduce-keyword
      repeated-k
      (add-one-letter keyword repeated-k))))


(defn repeat-keyword-vec
  [keyword message]
  (->>
    (repeat-keyword keyword message)
    (vec)
    (map str)))


(defn join-solution
  [seq]
  (->>
    (vec seq)
    (clojure.string/join)))


(defn decode-letter
  [letter-alph letter-idx]
  (get (vec (alphabet-int-from "a")) (.indexOf (vec (alphabet-int-from (str letter-alph))) letter-idx)))

(defn encode [keyword message]
  "encodeme"
  (join-solution
      (let [x (repeat-keyword-vec keyword message)]
          (map-indexed
              (fn [idx lt] (convert-letter lt (str (get message idx))))
              x))))

(defn decode
  [keyword message]
  "decodeme"
  (join-solution
      (let [x (repeat-keyword-vec keyword message)]
        (map-indexed
          (fn [idx lt] (decode-letter lt (str (get message idx))))
          x))))

(defn decipher [cipher message]
  "decypherme"
  (let [d (decode message cipher)]
    (reduce-keyword d (str (get d 0)))))

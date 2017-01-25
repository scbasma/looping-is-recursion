(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base k]
                 (if (zero? k)
                   acc
                   (recur (* acc base) base (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (not= (count seq1) (count seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))


(defn find-first-index [pred a-seq]
  (loop [index 0 rec-seq a-seq]
    (cond
      (empty? rec-seq) nil
      (pred (first rec-seq)) index
      :else (recur (inc index) (rest rec-seq)))))

(defn avg [a-seq]
  (loop [acc 0 n 0 rec-seq a-seq]
    (if (empty? rec-seq)
      (/ acc n)
      (recur (+ acc (first rec-seq)) (inc n) (rest rec-seq)))))

(defn parity [a-seq]
  (loop [odd-set #{} rec-seq a-seq]
    (cond
      (empty? rec-seq) odd-set
      (contains? odd-set (first rec-seq)) (recur (disj odd-set (first rec-seq)) (rest rec-seq))
      :else (recur (conj odd-set (first rec-seq)) (rest rec-seq)))))

(defn fast-fibo [n]
  (loop [fun-1 0 fun 1 counter 0]
    (if (>= counter n)
      fun-1
      (recur fun (+ fun fun-1) (inc counter)))))

(defn cut-at-repetition [a-seq]
  (loop [no-rep-set #{} no-rep-vec [] rec-seq a-seq]
    (cond
      (empty? rec-seq) no-rep-vec
      (contains? no-rep-set (first rec-seq)) no-rep-vec
      :else (recur (conj no-rep-set (first rec-seq)) (conj no-rep-vec (first rec-seq)) (rest rec-seq)))))


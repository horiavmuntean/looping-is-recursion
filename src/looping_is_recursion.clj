(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (and (not (empty? seq1)) (empty? seq2)) false
   (and (not (empty? seq2)) (empty? seq1)) false
   (not (= (first seq1) (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         crt-seq a-seq]
    (cond
     (empty? crt-seq) nil
     (pred (first crt-seq)) idx
     :else (recur (inc idx) (rest crt-seq)))))

(defn avg [a-seq]
  (when-not (empty? a-seq)
    (loop [idx 0
           sum 0
           crt-seq a-seq]
      (cond
       (empty? crt-seq) (/ sum idx)
       :else (recur (inc idx) (+ sum (first crt-seq)) (rest crt-seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc-set #{}
         crt-seq a-seq]
    (if (empty? crt-seq)
      acc-set
      (recur (toggle acc-set (first crt-seq)) (rest crt-seq)))))

(defn fast-fibo [n]
  (cond
   (< n 2) n
   :else (loop [c 1
                fn-1 0
                fn 1]
           (if (= c n)
             fn
             (recur (inc c) fn (+ fn-1 fn))))))

(defn cut-at-repetition [a-seq]
  (loop [ctrl-set #{}
         acc-vec  []
         crt-seq a-seq
         fi (first a-seq)]
    (cond
     (empty? crt-seq) acc-vec
     (contains? ctrl-set fi) acc-vec
     :else (recur (conj ctrl-set fi)
                  (conj acc-vec fi)
                  (rest crt-seq)
                  (first (rest crt-seq))))))




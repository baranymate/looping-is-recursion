(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [res exp]
                  (if (zero? exp)
                    res
                    (recur (* res base) (dec exp))
                  ))]
      (helper 1 exp))
  )

(defn last-element [a-seq]
  (let [helper (fn [head tail]
                (if (empty? tail)
                  head
                  (recur (first tail) (rest tail))
                ))]
      (helper (first a-seq) (rest a-seq))
    )
  )

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (and (zero? (count seq1)) (not (zero? (count seq2)))) false
    (and (not (zero? (count seq1))) (zero? (count seq2)) ) false
    (not (= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))
    )
  )

(defn find-first-index [pred a-seq]
  (loop [my-seq a-seq
         idx 0]
    (cond
      (empty? my-seq) nil
      (pred (first my-seq)) idx
      :else (recur (rest my-seq) (inc idx))
      )))

(defn avg [a-seq]
  (loop [sum 0
         nrelems 0
         avg 0
         loop-seq a-seq]

    (cond
      (empty? loop-seq) avg
      :else (let [elem (first loop-seq)
            help-sum (+ sum elem)
            help-elems (inc nrelems)
            help-avg (/ help-sum help-elems)]
        (recur help-sum help-elems help-avg (rest loop-seq))

            )
    )
  ))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn parity [a-seq]
  (loop [help-set #{}
         help-seq a-seq]
    (if (empty? help-seq) help-set
      (recur (toggle help-set (first help-seq)) (rest help-seq))
     )
    )
  )

(defn fast-fibo [n]
  (loop [ct 2
         fib1 0
         fib2 1]
    (cond
      (= n 0) 0
      (= n 1) 1
      (= ct n) (+ fib1 fib2)
      :else (recur (inc ct) fib2 (+ fib1 fib2))
      )
    )
  )

(defn cut-at-repetition [a-seq]
  (loop [loop-seq a-seq
         elems #{}
         res   []]
     (let [head (first loop-seq)]
      (cond
       (empty? loop-seq) res
       (contains? elems head) res
       :else (recur (rest loop-seq) (conj elems head) (conj res head))
      ))
  ))

(ns looping-is-recursion)

(defn power [base exp]
	(let [helper (fn [acc base exp]
		(if (zero? exp)
			acc
			(recur (* acc base) base (dec exp))))]
	(helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [seq1]
  	(if (empty? (rest seq1))
  		(first seq1)
  		(recur (rest seq1))))]
  (helper a-seq)))

(defn seq= [seq1 seq2]
	(let [helper (fn [a-seq b-seq]
                 (cond (and (empty? a-seq) (empty? b-seq)) true
                 	     (or (empty? a-seq) (empty? b-seq))  false
                       (= (first a-seq) (first b-seq))     (recur (rest a-seq) (rest b-seq))
                       :else false))]
    (helper seq1 seq2)))


(defn find-first-index [pred a-seq]
	(loop [i    0
         seq1 a-seq]
    (cond
  	  (empty? seq1) 			 nil
      (pred (first seq1)) i
      :else 					    (recur (inc i) (rest seq1)))))

(defn avg [a-seq]
  (loop [s 0
         n 0
         a a-seq]
    (if (empty? a) 
      (if (zero? n) nil (/ s n))
      (recur (+ s (first a)) (inc n) (rest a)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
	(let [toggle (fn [acc elem]
		  (if (contains? acc elem)
    		(disj acc elem)
   		  (conj acc elem)))]
  (loop [acc #{}
         seq1 a-seq]
    (if (empty? seq1)
      acc
      (recur (toggle acc (first seq1)) (rest seq1))))))

(defn fast-fibo [n]
	(loop [fn1 0
         fn2 1
         i   1]
  	(cond
      (< n 2) n
      (== i n) fn2
      :else (recur fn2 (+ fn1 fn2) (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [a a-seq
         b []]
    (cond
      (empty? a) b
      (contains? (set b) (first a)) b
      :else (recur (rest a) (conj b (first a))))))


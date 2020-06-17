;Construct vocabulary from text
(defn get-vocabulary [word-tokens vocab]
  (if (empty? word-tokens)
    (reverse vocab)
    (if (in-L? (first word-tokens) vocab)
      (get-vocabulary (rest word-tokens) vocab)
      (get-vocabulary (rest word-tokens) (cons (first word-tokens) vocab)))))


;Get number of occurances of word w in text
(defn get-count-of-word [w word-tokens]
  (if (empty? word-tokens)
    0
    (if ( = (first word-tokens) w)
      (+ 1 (get-count-of-word w (rest word-tokens)))
      (get-count-of-word w (rest word-tokens)))))


;Compute a uniform bag of words (BOW) sentence
  (defn flip [p] (if (< (rand 1) p) true false))
  (defn normalize [params] 2 (let [sum (apply + params)]
      (map (fn [x] (/ x sum)) params)))
  (defn sample-categorical [outcomes params]
    (if (flip (first params))
      (first outcomes)
      (sample-categorical (rest outcomes) (normalize (rest params)))))
  (defn create-uniform-distribution [outcomes]
    (let [num-outcomes (count outcomes)]
      (map (fn [x] (/ 1 num-outcomes)) outcomes)))

  (defn sample-uniform-BOW-sentence [n vocab]
    (if (= n 0)
      '()
      (cons (sample-categorical vocab (create-uniform-distribution vocab))
            (sample-uniform-BOW-sentence (- n 1) vocab))))


;Compute uniform distribution probability
(defn compute-uniform-BOW-prob [vocab sentence]
  (if (empty? sentence)
     1
     (* (/ 1 (count vocab)) (compute-uniform-BOW-prob vocab (rest sentence)))))


;Find probablity of word given outcomes
(defn lookup-probability [w outcomes probs]
  (if (= w (first outcomes))
    (first probs)
    (lookup-probability w (rest outcomes) (rest probs))))


;Compute BOW sentence occurance probabilitiy
(defn product [l] (apply * l))

(defn compute-BOW-prob-helper [sentence vocabulary probabilities]
  (if (empty? sentence)
     '()
     (cons (lookup-probability (first sentence) vocabulary probabilities)
      (compute-BOW-prob-helper (rest sentence) vocabulary probabilities))))

(defn compute-BOW-prob [sentence vocabulary probabilities]
  (product (compute-BOW-prob-helper sentence vocabulary probabilities)))


;Computes log of marginal liklihood given theta, theta-prior, corpus, etc
(defn theta-corpus-joint [theta corpus theta-probs]
   (* (score-corpus corpus theta) (first theta-probs)))

(defn compute-marginal-helper [thetas corpus theta-probs]
  (if (empty? thetas)
    nil
    (cons (theta-corpus-joint (first thetas) corpus theta-probs)
          (compute-marginal-helper (rest thetas) corpus theta-probs))))

(defn compute-marginal [corpus theta-probs]
  (logsumexp (compute-marginal-helper thetas corpus theta-probs)))


;Builds corpus based on theta
(defn sample-BOW-corpus [theta sent-len corpus-len]
  (if (= corpus-len 0)
    nil
    (cons (sample-BOW-sentence sent-len theta)
          (sample-BOW-corpus theta sent-len (- corpus-len 1)))))

(defn sample-theta-corpus [sent-len corpus-len theta-probs]
  (let [theta (if (flip (first theta-probs)) theta1 theta2)]
    (list theta (sample-BOW-corpus theta sent-len corpus-len))))

;Builds probability of target corpus based on multiple function calls
(defn get-theta [theta-corpus] (first theta-corpus))
(defn get-corpus [theta-corpus] (first (rest theta-corpus)))
(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
  (repeat (fn []
    (sample-theta-corpus sent-len corpus-len theta-probs)) sample-size))

(defn sum-array [l] (reduce + l))

(defn indicator [corpus]
  (if (= my-corpus (get-corpus corpus)) 1 0 ))

(defn indicator-func [theta-corpus-pairs]
  (if (empty? theta-corpus-pairs)
      nil
      (cons (indicator (first theta-corpus-pairs))
            (indicator-func (rest theta-corpus-pairs)))))

(defn estimate-corpus-marginal [corpus sample-size sent-len corpus-len theta-probs]
  (let [l (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)]
    (double (/ (sum-array (indicator-func l)) sample-size))))


;Builds probablity of values in corpus by rejecting samples where the observed
;corpus does not much the expected corpus
(defn get-count [obs observation-list count]
  (if (empty? observation-list)
    count
    (if (= obs (first observation-list))
      (get-count obs (rest observation-list) (+ 1 count))
      (get-count obs (rest observation-list) count))))
(defn get-counts [outcomes observation-list]
  (let [count-obs (fn [obs] (get-count obs observation-list 0))]
    (map count-obs outcomes)))

(defn theta-switch [corpus]
  (if (= my-corpus (get-corpus corpus)) (get-theta corpus) nil))

(defn conditional-func [theta-corpus-pairs]
  (if (empty? theta-corpus-pairs)
      nil
      (cons (theta-switch (first theta-corpus-pairs))
            (conditional-func (rest theta-corpus-pairs)))))

(defn rejection-sampler [theta observed-corpus sample-size sent-len corpus-len theta-probs]
  (let [l (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)]
    (let [probs (remove nil? (conditional-func l))]
      (let [matches (first (get-counts (list theta) probs))]
        (double (/ matches (count probs)))))))

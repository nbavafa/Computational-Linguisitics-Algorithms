;Return third value in list
(defn third [x] (first (rest (rest x ))))

;Return first two elements of list
(defn first-two [l] (list (first l) (first (rest l))))

;Remove second element of list
(defn remove-second [l] (cons (first l) (rest (rest l))))

;Append to the end of a list
(defn add-to-end [l x]
  (if (empty? l)
     (cons x l)
     (cons (first l) (add-to-end (rest l) x))))

;Reverse a list
 (defn reverse [l]
   (if (empty? l)
      '()
      (add-to-end (reverse (rest l)) (first l))))

;Remove last element of list
(defn remove-last-element [l] (reverse (rest (reverse l))))

;Construct a list containing numbers n to 1
(defn count-to-1-helper [l n]
  (if (= n 0) '()
    (cons n (count-to-1-helper l (- n 1)))))

(defn count-to-1 [n] (count-to-1-helper '() n))

;Return maximum value of a list
(defn get-max-helper [m l]
  (if (empty? l) m
    (if (> (first l) m)
      (if (empty? l)m
        (get-max-helper (first l) (rest l)))
      (if (empty? l) m
        (get-max-helper m (rest l))))))

(defn get-max [l] (get-max-helper (first l) (rest l)))

;Returns a list corresponding to if value at index in l is greater than n
(defn greater-than-n? [l n]
  (if (empty? l) '()
    (cons (if (> (first l) n) true false)
          (greater-than-five? (rest l) n))))

;Combines three lists to form 1 list
(defn concat-three [x y z]
  (if (empty? x)
    (if (empty? y)
      (if (empty? z)
        '()
        (cons (first z) (concat-three x y (rest z))))
      (cons (first y) (concat-three x (rest y) z)))
    (cons (first x) (concat-three (rest x) y z))))

;Checks to see if value x exists in list l
(defn in-L? [l x]
  (if (empty? l)
    false
    (if (= x (first l))
      true
      (in-L? (rest l) x))))

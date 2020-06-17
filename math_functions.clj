;Get absolute Value
(defn abs [x] (Math/sqrt (* x x)))

;Get square of number
(defn take-square [x] (* x x))

;Get the sum of squares of two numbers
(defn sum-of-squares [x y] (+ (take-square x) (take-square y)))

(ns vm-translator.context)

(defn initialize
  []
  {:line-number 1
   :instruction-number 0})

(defn inc-line
  [ctx]
  (update ctx :line-number inc))

(defn inc-instruction
  ([ctx]
   (inc-instruction ctx 1))
  ([ctx count]
   (update ctx :instruction-number (partial + count))))

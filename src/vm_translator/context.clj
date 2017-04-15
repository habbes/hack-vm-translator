(ns vm-translator.context)

(defn initialize
  []
  {:line-number 0
   :instruction-number -1})

(defn inc-line
  [ctx]
  (update ctx :line-number inc))

(defn inc-instruction
  ([ctx]
   (inc-instruction ctx 1))
  ([ctx count]
   (update ctx :instruction-number (partial + count))))

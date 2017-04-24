(ns vm-translator.context)

(defn initialize
  "Initializes new context"
  ([]
   {:line-number 0
    :instruction-number -1})
  ([class-name]
   (assoc (initialize)
     :class class-name)))

(defn inc-line
  "Increments the line number"
  [ctx]
  (update ctx :line-number inc))

(defn inc-instruction
  "Increments the instruction number by the specifed count"
  ([ctx]
   (inc-instruction ctx 1))
  ([{ic :instruction-number :or {ic -1}
     :as ctx} count]
   (assoc ctx :instruction-number
     (+ ic count))))

(defn set-class
  "Sets the current class for the context"
  [ctx class-name]
  (assoc ctx :class class-name))

(defn unset-class
  "Removes the current class from the context"
  [ctx]
  (dissoc ctx :class))

(defn set-function
  "Sets the current function for the context"
  [ctx function]
  (assoc ctx :function function))

(defn unset-function
  "Removes the current function from the context"
  [ctx]
  (dissoc ctx :function))

(ns cljstyle.format.vertical-alignment
  (:require [cljstyle.format.zloc :as zl]
            [clojure.string :as str]
            [rewrite-clj.zip :as z]
            [clojure.spec.alpha :as s]
            [rewrite-clj.node :as node]))


(def ^:private binding-form-symbols
  #{'binding 'doseq 'for 'if-let 'if-some 'let 'loop
    'when-let 'when-some 'with-local-vars 'with-open 'with-redefs})

(defn- binding-form?
  [zloc]
  (and (z/list? zloc)
       (contains? binding-form-symbols (zl/form-symbol (z/down zloc)))))

(def ^:private cond-form-skips
  {'are     2
   'case    1
   'cond    0
   'cond->  1
   'cond->> 1
   'condp   1})

(def ^:private cond-form-symbols
  (set (keys cond-form-skips)))

(defn- cond-form?
  [zloc]
  (and (z/list? zloc)
       (contains? cond-form-symbols (zl/form-symbol (z/down zloc)))))

(defn- align-form-type
  [zloc]
  (cond
    (z/map? zloc)        :map
    (binding-form? zloc) :binding
    (cond-form? zloc)    :cond
    :else                nil))

(defn- should-align?
  [zloc]
  (boolean
   (align-form-type zloc)))

(defmulti ^:private align-form
  align-form-type)

(def ^:private node-identity
  (juxt z/node z/position))

(defn- empty-line-between?
  [start-zloc end-zloc]
  (if (or (nil? start-zloc) (nil? end-zloc))
    false
    (loop [cur-zloc start-zloc
           lines    0]
      (cond
        (> lines 1)                  true
        (= (node-identity cur-zloc)
           (node-identity end-zloc)) false

        :else
        (let [next-zloc  (z/next* cur-zloc)
              next-lines (cond
                           (z/linebreak? next-zloc)
                           (+ lines (z/length next-zloc))

                           (z/whitespace-or-comment? next-zloc)
                           lines

                           :else
                           0)]
          (recur next-zloc next-lines))))))

(defn- element-width
  [zloc]
  (let [node-text (z/string zloc)]
    (cond
      (nil? zloc)                          0
      (not (str/includes? node-text "\n")) (count node-text)

      :else
      (let [last-line (last (str/split node-text #"\n"))
            left-offset (dec (last (z/position zloc)))]
        (- (count last-line) left-offset)))))

(defn- max-element-width
  [zloc]
  (loop [cur-zloc  zloc
         max-width (element-width cur-zloc)]
    (let [next-zloc (-> cur-zloc (z/right) (z/right))]
      (if (or (nil? next-zloc)
              (empty-line-between? cur-zloc next-zloc))
        max-width
        (recur next-zloc
               (max max-width (element-width next-zloc)))))))

(defn- pad-element
  [left-element-zloc padding-width]
  (let [whitespace (apply str (repeat padding-width " "))]
    (-> (z/find left-element-zloc z/right* z/whitespace?)
        (z/replace (node/whitespace-node whitespace))
        (z/left))))

(defn- align-pairs
  [first-pair-left-zloc]
  (loop [cur-zloc      first-pair-left-zloc
         cur-max-width (max-element-width cur-zloc)]
    (let [padding-width   (inc (- cur-max-width (element-width cur-zloc)))
          padded-cur-zloc (pad-element cur-zloc padding-width)
          next-zloc       (-> padded-cur-zloc (z/right) (z/right))]
      (if next-zloc
        (recur next-zloc (if (empty-line-between? cur-zloc next-zloc)
                           (max-element-width next-zloc)
                           cur-max-width))
        padded-cur-zloc))))

(defmethod align-form :map
  [zloc]
  (if-not (empty? (z/sexpr zloc))
    (align-pairs (z/down zloc))
    zloc))

(defmethod align-form :binding
  [zloc]
  (let [pair-vector (-> zloc (z/down) (z/right))]
    (if-not (empty? (z/sexpr pair-vector))
      (align-pairs (z/down pair-vector))
      zloc)))

(defn- cond-first-pair-left
  [zloc skip-n]
  (->> zloc
       (z/down)
       (z/right)
       (iterate z/right)
       (take (inc skip-n))
       (last)))

(defmethod align-form :cond
  [zloc]
  (let [cond-symbol     (-> zloc (z/down) (zl/form-symbol))
        skip-n          (get cond-form-skips cond-symbol)
        first-pair-left (cond-first-pair-left zloc skip-n)]
    (if first-pair-left
      (align-pairs first-pair-left)
      zloc)))

(defmethod align-form :default
  [zloc]
  zloc)

(defn realign
  [form opts]
  (let [zloc (z/edn* form {:track-position? true})]
    (z/root
     (zl/edit-walk zloc should-align? align-form))))


(ns cljstyle.format.vertical-alignment-test
  (:require
   [cljstyle.config :as config]
   [cljstyle.format.core :refer [reformat-string]]
   [clojure.test :refer [deftest testing is]]))


(defn realign-string
  ([form-string]
   (realign-string form-string config/default-indents 2))
  ([form-string indents]
   (realign-string form-string indents 2))
  ([form-string indents list-indent-size]
   (reformat-string form-string {:indentation?        true
                                 :indents             indents
                                 :list-indent-size    list-indent-size
                                 :vertical-alignment? true})))

(deftest map-alignment
  (is (= "{:foo  :bar\n :quux :baz}"
         (realign-string "{:foo :bar\n :quux :baz}")))

  (testing "alignment groups"
    (is (= "{:foo :bar\n :baz :buzz\n\n :quux :quuz\n :quuk :quuw}"
           (realign-string "{:foo :bar\n :baz   :buzz\n\n :quux :quuz\n :quuk   :quuw}"))))

  (testing "multiline form as key"
    (is (= "{(str \"foo\"\n      \"bar\") :baz\n :quux       :buzz}"
           (realign-string "{(str \"foo\"\n\"bar\") :baz\n :quux :buzz}")))

    (is (= "{(str \"foo\"\n      \"bar\") :baz\n :quux       :buzz\n\n :quuwkz :quuw}"
           (realign-string "{(str \"foo\"\n\"bar\") :baz\n :quux :buzz\n\n :quuwkz :quuw}")))))

(deftest binding-alignment
  (is (= "(let [foo  :bar\n      buzz :baz])"
         (realign-string "(let [foo :bar\n     buzz :baz])")))

  (testing "alignment groups"
    (is (= "(let [foo  :bar\n      buzz :baz\n\n      bun :bux])"
           (realign-string "(let [foo :bar\n      buzz :baz\n\n      bun :bux])")))))

(deftest cond-alignment
  (is (= "(case foo\n  :foo  1\n  :buzz 2)"
         (realign-string "(case foo\n  :foo 1\n  :buzz 2)")))

  (testing "alignment groups"
    (is (= "(case foo\n  :foo  1\n  :buzz 2\n\n  :bar 3)"
           (realign-string "(case foo\n  :foo 1\n  :buzz 2\n\n  :bar 3)"))))

  (testing "multiline form as first element"
    (is (= "(cond\n  (every? true? [false\n                 true\n                 true]) :foo\n  :else                 :bar)"
           (realign-string "(cond\n  (every? true? [false\n                 true\n                 true]) :foo\n  :else :bar)")))

    (is (= "(cond\n  (every? true? [false\n                 true\n                 true]) :foo\n  (true? baz)           :baz\n\n  :else :bar)"
           (realign-string "(cond\n  (every? true? [false\n                 true\n                 true]) :foo\n  (true? baz)           :baz\n\n  :else     :bar)")))))


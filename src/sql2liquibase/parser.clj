(ns sql2liquibase.parser
  (:require [instaparse.core :as insta]
            [clojure.string :as clj-str]))


(def nd-transform (atom nil))


(defn mk-parser
  [delimiter]
  (insta/parser
   (str
   "<PROGRAM>      ::= <SPACE*> STATEMENT <SPACE*> | PROGRAM <SPACE*> STATEMENT <SPACE*>
    STATEMENT      ::= EXP | M_COMMENT | S_COMMENT | ND_EXP;
    EXP            ::= START_TOKEN SPACE+ BODY  <DELIMITER>;
    ND_EXP         ::= <ND_START> <SPACE> WORD <SPACE*> BODY
    ND_START       ::= \"DELIMITER\"
    M_COMMENT      ::= MCOMMENT_START SPACE* BODY  MCOMMENT_END ?[DELIMITER];
    S_COMMENT      ::= '-' SPACE* SC_BODY <#'[\n]'>;
    START_TOKEN    ::= '/*!' | 'SET'
                    | 'DROP' | 'CREATE'
                    | 'ALTER'| 'INSERT';
    MCOMMENT_START ::= #'/\\*';
    MCOMMENT_END   ::= #'\\*/';
    BODY           ::= WORD | BODY SPACE+ WORD;
    SC_BODY        ::= WORD | SC_BODY NB_SPACE+ WORD;
    WORD           ::= #'^((?!" delimiter "|\\s|\\*/).)*' SPACE*;
    <DELIMITER>    ::= \"" delimiter "\";
    SPACE          ::=  #'[ \t\n,]+';
    NB_SPACE       ::=  #'[ \t,]+'; "
    )))



(defn to-string
  [& args]
  (.toString (reduce str args)))


(defn mk-new-parse
  [parser]
  (fn [input]
          (let [parsed (parser input)]
            (if (insta/failure? parsed)
              (throw (ex-info (pr-str (insta/get-failure parsed)) {}))
              parsed))))

(defn do-new-delim
  [word body]
  (println ":word " word " :body " (class body) ":nd-transform " @nd-transform)
  (let [new-parser (mk-parser word)
        new-parse (mk-new-parse new-parser)]
    (first (@nd-transform (new-parse body)))))

(def transform-options
  {
    :STATEMENT      to-string
    :EXP            to-string
    :ND_EXP         do-new-delim
    :ND_START       to-string
    :M_COMMENT      to-string
    :S_COMMENT      to-string
    :START_TOKEN    to-string
    :MCOMMENT_START to-string
    :MCOMMENT_END   to-string
    :BODY           to-string
    :SC_BODY        to-string
    :WORD           to-string
    :SPACE          to-string
    :NB_SPACE       to-string })



(defn transform
  [p-tree]
  (insta/transform transform-options p-tree))



(def dfault-parser (mk-parser ";"))


(defn parse
  ([input loc]
     (reset! nd-transform transform)
     ;(map (partial transform loc)
          (let [parsed (dfault-parser input)]
            (if (insta/failure? parsed)
              (throw (ex-info (pr-str (insta/get-failure parsed)) {}))
              parsed)))
  ([input] (parse input nil)))



(defn parse-file [filename]
  (parse (slurp filename) filename))





(ns sql2liquibase.parser
  (:require [instaparse.core :as insta]
            [clojure.string :as clj-str]))



(defn mk-parser
  [delimiter]
  (insta/parser
   (str
   "<PROGRAM>      ::= SPACE* STATEMENT SPACE* | PROGRAM SPACE* STATEMENT SPACE*
    STATEMENT      ::= EXP | M_COMMENT | S_COMMENT | ND_EXP;
    EXP            ::= START_TOKEN SPACE+ BODY  DELIMITER;
    ND_EXP         ::= ND_START BODY
    ND_START       ::= \"DELIMITER\"
    M_COMMENT      ::= MCOMMENT_START SPACE* BODY  MCOMMENT_END ?[DELIMITER];
    S_COMMENT      ::= '-' SPACE* SC_BODY #'[\n]';
    START_TOKEN    ::= '/*!' | 'SET'
                    | 'DROP' | 'CREATE'
                    | 'ALTER'| 'INSERT';
    MCOMMENT_START ::= #'/\\*';
    MCOMMENT_END   ::= #'\\*/';
    BODY           ::= WORD | BODY SPACE+ WORD;
    SC_BODY        ::= WORD | SC_BODY NB_SPACE+ WORD;
    WORD           ::= #'^((?!" delimiter "|\\s|\\*/).)*' SPACE*;
    DELIMITER      ::= \"" delimiter "\";
    <SPACE>        ::= <#'[ \t\n,]+'>;
    <NB_SPACE>     ::= <#'[ \t,]+'>; "
    )))

(def parser (mk-parser ";"))


(defn parse
  ([input loc]
     ;(map (partial transform loc)
          (let [parsed (parser input)]
            (if (insta/failure? parsed)
              (throw (ex-info (pr-str (insta/get-failure parsed)) {}))
              parsed)))
  ([input] (parse input nil)))



(defn parse-file [filename]
  (parse (slurp filename) filename))





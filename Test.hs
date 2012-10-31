{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Math.InfoRetrieval.TFIDF
       
corpus =
    foldl' (\c (d,t)->addDocument d t c) emptyCorpus
    $ zip (map Doc [1..])
    [ ["hello", "world", "T", "thing"]
    , ["hello", "scope", "plug", "box"]
    , ["hello", "box", "T", "T"]
    , ["world", "T", "thing", "cord"]
    ]
    
main = print $ search LinearScaling corpus "T"

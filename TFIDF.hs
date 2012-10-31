{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module TFIDF ( Document(..)
             , Term(..)
             , Corpus
             , emptyCorpus
             , addDocument
             , removeDocument
             , tfidf
             , search
             ) where            

import Debug.Trace       
import           Control.Lens
import           Data.Function (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import           Data.Hashable
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import Data.String (IsString)       

type Score = Double
                 
newtype Document = Doc Int
                 deriving (Show, Eq, Hashable)
                 
newtype Term = Term T.Text
             deriving (Show, Eq, Hashable, IsString)
             
data Corpus = Corpus { _cDocuments :: HashSet Document
                     , _cTerms :: HashMap Term (HashMap Document Int)
                     }
makeLenses ''Corpus
     
emptyCorpus :: Corpus
emptyCorpus = Corpus S.empty M.empty
            
addDocument :: Document -> [Term] -> Corpus -> Corpus
addDocument d ts c = addDocument' d ts' c
    where ts' = foldl' (flip $ \t->M.insertWith (+) t 1) M.empty ts

addDocument' :: Document -> HashMap Term Int -> Corpus -> Corpus
addDocument' d ts c =
    cDocuments %~ S.insert d
    $ M.foldlWithKey' (\c term n->cTerms %~ M.insertWith (<>) term (M.singleton d n) $ c) c ts
    
removeDocument :: Document -> Corpus -> Corpus
removeDocument d =
    (cDocuments %~ S.delete d)
    . (cTerms %~ M.map (M.delete d))

idf :: Corpus -> Term -> Double
idf c t = log $ docs / docsWithTerm
    where docs = realToFrac $ views cDocuments S.size c
          docsWithTerm = realToFrac $ M.size $ M.lookupDefault M.empty t (c^.cTerms)
    
data Scaling = BoolScaling | LogScaling | LinearScaling | NormedScaling

tf :: Scaling -> Corpus -> Term -> Document -> Score
tf LinearScaling c t d = 
    realToFrac $ M.lookupDefault 0 d $ M.lookupDefault M.empty t (c^.cTerms)
tf LogScaling c t d = log $ tf LinearScaling c t d
tf BoolScaling c t d = if tf LinearScaling c t d > 0 then 1 else 0

tr s x = traceShow (s,x) x

tfidf :: Corpus -> Term -> Document -> Score
tfidf c t d = tf LinearScaling c t d * idf c t
      
search :: Corpus -> Term -> [(Document, Score)]
search c t =
    sortBy (flip (compare `on` snd))
    $ map (\d->(d, tfidf c t d))
    $ views cDocuments S.toList c


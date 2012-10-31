{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Math.InfoRetrieval.TFIDF
    ( Document(..)
    , Term(..)
    , Corpus
    , emptyCorpus
    , addDocument
    , removeDocument
    , FreqScaling(..)
    , tfidf
    , search
    ) where            

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
    
data FreqScaling = BoolScaling | LogScaling | LinearScaling | NormedScaling

tf :: FreqScaling -> Corpus -> Term -> Document -> Score
tf LinearScaling c t d = 
    realToFrac $ M.lookupDefault 0 d $ M.lookupDefault M.empty t (c^.cTerms)
tf LogScaling c t d = log $ tf LinearScaling c t d
tf BoolScaling c t d = if tf LinearScaling c t d > 0 then 1 else 0

tfidf :: FreqScaling -> Corpus -> Term -> Document -> Score
tfidf scaling c t d = tf scaling c t d * idf c t
      
search :: FreqScaling -> Corpus -> Term -> [(Document, Score)]
search scaling c t =
    sortBy (flip (compare `on` snd))
    $ map (\d->(d, tfidf scaling c t d))
    $ views cDocuments S.toList c


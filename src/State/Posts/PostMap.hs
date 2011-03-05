module State.Posts.PostMap where

import Data.Ord (Ordering(..))
import Data.ByteString (ByteString)

import State.Users

type PostId = Int
type PostTime = Int
type PostScore = Float

class (Eq a) => Post a where
  postId :: a -> PostId
  postAuthor :: a -> Username
  postScore :: a -> PostScore

data Post a => IdMap a
     = IdNode a (AuthorMap a) (AuthorMap a)
     | IdEmpty

data Post a => AuthorMap a
     = AuthorNode a (ScoreMap a) (ScoreMap a)
     | AuthorEmpty

data Post a => ScoreMap a
     = ScoreNode a (IdMap a) (IdMap a)
     | ScoreEmpty

data (Eq a, Ord a) => PostIndex a = Index a | Ignore

instance Ord a => Eq (PostIndex a) where
  Ignore == _ = True
  _ == Ignore = True
  (Index i) == (Index i') = i == i'

instance Ord a => Ord (PostIndex a) where
  compare Ignore _ = EQ
  compare _ Ignore = EQ
  compare (Index i) (Index i') = compare i i'

data PostLookup =
  PostLookup { lookupId :: PostIndex PostId
             , lookupAuthor :: PostIndex Username
             , lookupScore :: PostIndex PostScore
             }

instance Eq PostLookup where
  (PostLookup id author score) == (PostLookup id' author' score') =
    id == id' && author == author' && score == score'

getPostLookup :: Post a => a -> PostLookup
getPostLookup p =
  PostLookup (Index $ postId p) (Index $ postAuthor p) (Index $ postScore p)

insertId :: Post a => a -> IdMap a -> IdMap a
insertId p IdEmpty = IdNode p AuthorEmpty AuthorEmpty
insertId p (IdNode p' l r)
  | p == p' = IdNode p' l r
  | postId p <= postId p' = IdNode p' (insertAuthor p l) r
  | otherwise = IdNode p' l (insertAuthor p r)

insertAuthor :: Post a => a -> AuthorMap a -> AuthorMap a
insertAuthor p AuthorEmpty = AuthorNode p ScoreEmpty ScoreEmpty
insertAuthor p (AuthorNode p' l r)
  | p == p' = AuthorNode p' l r
  | postAuthor p <= postAuthor p' = AuthorNode p' (insertScore p l) r
  | otherwise = AuthorNode p' l (insertScore p r)

insertScore :: Post a => a -> ScoreMap a -> ScoreMap a
insertScore p ScoreEmpty = ScoreNode p IdEmpty IdEmpty
insertScore p (ScoreNode p' l r)
  | p == p' = ScoreNode p' l r
  | postScore p <= postScore p' = ScoreNode p' (insertId p l) r
  | otherwise = ScoreNode p' l (insertId p r)
                


fromListId :: Post a => [a] -> IdMap a
fromListId = foldr insertId IdEmpty

fromListAuthor :: Post a => [a] -> AuthorMap a
fromListAuthor = foldr insertAuthor AuthorEmpty

fromListScore :: Post a => [a] -> ScoreMap a
fromListScore = foldr insertScore ScoreEmpty



toListId :: Post a => IdMap a -> [a]
toListId IdEmpty = []
toListId (IdNode p l r) = toListAuthor l ++ [p] ++ toListAuthor r

toListAuthor :: Post a => AuthorMap a -> [a]
toListAuthor AuthorEmpty = []
toListAuthor (AuthorNode p l r) = toListScore l ++ [p] ++ toListScore r

toListScore :: Post a => ScoreMap a -> [a]
toListScore ScoreEmpty = []
toListScore (ScoreNode p l r) = toListId l ++ [p] ++ toListId r


lookupIdIndex :: Post a => PostLookup -> IdMap a -> [a]
lookupIdIndex _ IdEmpty = []
lookupIdIndex pl (IdNode p l r)
  | pl == pl'  = left ++ [p] ++ right
  | otherwise = left ++ right
  where
    pl' = getPostLookup p
    (left, right) = case lookupId pl of
      Ignore   -> (lookupAuthorIndex pl l, lookupAuthorIndex pl r)
      Index id -> if id <= postId p
                  then (lookupAuthorIndex pl l, [])
                  else ([], lookupAuthorIndex pl r)

lookupAuthorIndex :: Post a => PostLookup -> AuthorMap a -> [a]
lookupAuthorIndex _ AuthorEmpty = []
lookupAuthorIndex pl (AuthorNode p l r)
  | pl == pl'  = left ++ [p] ++ right
  | otherwise = left ++ right
  where
    pl' = getPostLookup p
    (left, right) = case lookupAuthor pl of
      Ignore       -> (lookupScoreIndex pl l, lookupScoreIndex pl r)
      Index author -> if author <= postAuthor p
                      then (lookupScoreIndex pl l, [])
                      else ([], lookupScoreIndex pl r)

lookupScoreIndex :: Post a => PostLookup -> ScoreMap a -> [a]
lookupScoreIndex _ ScoreEmpty = []
lookupScoreIndex pl (ScoreNode p l r)
  | pl == pl'  = left ++ [p] ++ right
  | otherwise = left ++ right
  where
    pl' = getPostLookup p
    (left, right) = case lookupScore pl of
      Ignore      -> (lookupIdIndex pl l, lookupIdIndex pl r)
      Index score -> if score <= postScore p
                     then (lookupIdIndex pl l, [])
                     else ([], lookupIdIndex pl r)
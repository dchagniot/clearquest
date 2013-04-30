{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module ClearQuestHelper where

import ClearQuestOleServer
import System.Win32.Com
--import System.Win32.Com.Automation as Auto
--import Control.Exception
import Control.Monad (MonadPlus, mzero, mplus)
import Data.Int

-- | commit or revert the entity after validation
comOrRev :: IOAdEntity a0 -> IO (Maybe (IOAdEntity a0))
comOrRev entity = do
    resString <- entity # validate
    case resString of
         "" -> do
            _ <- entity # commit
            return (Just entity)
         str -> do
            putStrLn str
            entity # revert
            return Nothing

-- | execute the query and return the results
exeQuery :: String -> (IOAdQueryDef () -> IO a) -> IOAdSession a0 -> IO [[String]]
exeQuery queryString queDef cqApp = coRun $ do
    myQuery <- cqApp # buildQuery queryString
    _ <- myQuery # queDef
    myResSet <- cqApp # buildResultSet myQuery
    myResSet # execute
    myNbCol <- myResSet # getNumberOfColumns
    unfoldrM (unfoldFunc myResSet myNbCol) 1

-- | a base query who returns an id, and matches Layer with Integration
-- | best used with exeQuery like this
-- | result <- cqApp # exeQuery "ReleaseItem" (\queryObj -> queryRiBase queryObj    >>=
-- |                                         "Subsystem"   `eqOp` "mySubSysILookFor" >>=
-- |                                         "VersionName" `eqOp` "x.y.z")
queryRiBase :: IOAdQueryDef a0 -> IO (IOAdQueryFilterNode ())
queryRiBase quer =  quer # bField "id"
            >>= andOp
            >>= "Layer" `eqOp` "Integration"

-- | helper function in order to have a signature
-- | a -> IO (a) instead of a -> IO ()
bField :: String -> IOAdQueryDef a0 -> IO (IOAdQueryDef a0)
bField str qDef = do
    qDef # buildField str
    return qDef

andOp, orOp :: (QueryDefFilterNod a) => a -> IO (IOAdQueryFilterNode ())
andOp qDef = qDef # baseOp 1
orOp qDef  = qDef # baseOp 2

-- | a typeclass is defined in order to allow andOp and orOp operate
-- | on both IOAdQueryDef and IOAdQueryFilterNode
class QueryDefFilterNod a where
    baseOp :: Int32 -> a -> IO (IOAdQueryFilterNode ())
instance QueryDefFilterNod (IOAdQueryDef a0) where
    baseOp = buildFilterOperator
instance QueryDefFilterNod (IOAdQueryFilterNode a0) where
    baseOp = buildFilterOperator0

eqOp,neqOp,ltOp,lteOp,gtOp,gteOp,likeOp,notLikeOp,betweenOp,notBetweenOp,isNullOp,isNotNullOp,inOp,notInOp :: String -> String -> IOAdQueryFilterNode a -> IO (IOAdQueryFilterNode a)
eqOp         = compOpBase 1
neqOp        = compOpBase 2
ltOp         = compOpBase 3
lteOp        = compOpBase 4
gtOp         = compOpBase 5
gteOp        = compOpBase 6
likeOp       = compOpBase 7
notLikeOp    = compOpBase 8
betweenOp    = compOpBase 9
notBetweenOp = compOpBase 10
isNullOp     = compOpBase 11
isNotNullOp  = compOpBase 12
inOp         = compOpBase 13
notInOp      = compOpBase 14

-- | helper function in order to lift the comparison operators
compOpBase :: Int32 -> String -> String -> IOAdQueryFilterNode a -> IO (IOAdQueryFilterNode a)
compOpBase magicNum str str' qDef = do
    qDef # buildFilter str magicNum str'
    return qDef

--getReleaseItemField 
unfoldFunc :: IOAdResultSet a0 -> Int32 -> Int32 -> IO (Maybe ([String],Int32))
unfoldFunc resSet nbCol inc = coRun $ do
    res <- resSet # moveNext
    case res of
         1 -> do
             val <- mapM (`getColumnValue` resSet) [1..nbCol]
             return (Just (val, inc+1))
         _ -> return Nothing

-- | took from Monad-loop
{-# SPECIALIZE unfoldrM  :: (a -> IO (Maybe (b,a))) -> a -> IO [b] #-}
{-# SPECIALIZE unfoldrM' :: (Monad m) => (a -> m (Maybe (b,a))) -> a -> m [b] #-}
{-# SPECIALIZE unfoldrM' :: (a -> IO (Maybe (b,a))) -> a -> IO [b] #-}
-- |See 'Data.List.unfoldr'.  This is a monad-friendly version of that.
unfoldrM :: (Monad m) => (a -> m (Maybe (b,a))) -> a -> m [b]
unfoldrM = unfoldrM'
-- |See 'Data.List.unfoldr'.  This is a monad-friendly version of that, with a
-- twist.  Rather than returning a list, it returns any MonadPlus type of your
-- choice.
unfoldrM' :: (Monad m, MonadPlus f) => (a -> m (Maybe (b,a))) -> a -> m (f b)
unfoldrM' f z = do
        x <- f z
        case x of
                Nothing         -> return mzero
                Just (a, b)     -> do
                        xs <- unfoldrM' f b
                        return (return a `mplus` xs)


{-- clearquest API constants

REM **  -------------------  BoolOp  -------------------
 
Public Const AD_BOOL_OP_AND = 1
Public Const AD_BOOL_OP_OR = 2
 
REM **  -------------------  CompOp  -------------------
 
Public Const AD_COMP_OP_EQ          = 1
Public Const AD_COMP_OP_NEQ         = 2
Public Const AD_COMP_OP_LT          = 3
Public Const AD_COMP_OP_LTE         = 4
Public Const AD_COMP_OP_GT          = 5
Public Const AD_COMP_OP_GTE         = 6
Public Const AD_COMP_OP_LIKE        = 7
Public Const AD_COMP_OP_NOT_LIKE    = 8
Public Const AD_COMP_OP_BETWEEN     = 9
Public Const AD_COMP_OP_NOT_BETWEEN = 10
Public Const AD_COMP_OP_IS_NULL     = 11
Public Const AD_COMP_OP_IS_NOT_NULL = 12
Public Const AD_COMP_OP_IN          = 13
Public Const AD_COMP_OP_NOT_IN      = 14
 
REM **  -------------------  FetchStatus  -------------------
 
Public Const AD_SUCCESS = 1
Public Const AD_NO_DATA_FOUND = 2
Public Const AD_MAX_ROWS_EXCEEDED = 3
Public Const AD_ROW_DELETED = 4
--}

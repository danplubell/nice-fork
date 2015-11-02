module NiceFork
       ( ThreadManager
       , newManager
       , forkManaged
       , getStatus
       , waitFor
       , waitAll) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.Map           as M
import           Data.Typeable

data ForkException = ThrewException deriving (Show, Typeable, Eq)
instance Exception ForkException

data ThreadStatus = Running
                  | Finished -- terminated normally
                  | Threw ForkException -- killed by uncaught exception
                  deriving  (Eq, Show)

-- | ThreadManager contains an MVar that contains a map of thread id to thread status
newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))

-- | Create a new thread manager.
newManager:: IO ThreadManager
newManager= Mgr <$> newMVar M.empty

-- | Create a new managed thread.
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
            modifyMVar mgr $ \m -> do
                       state <- newEmptyMVar
                       tid   <- forkIO $ do
                         result <- try body
                         putMVar state (either Threw (const Finished) result)
                       return (M.insert tid state m, tid)


-- | Immediately return the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m,Nothing)
      Just st -> tryTakeMVar st >>= \mst -> case mst of
                  Nothing -> return (m, Just Running)
                  Just sth -> return (M.delete tid m, Just sth)

-- | Block until a specific managed thread terminates
waitFor:: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor  (Mgr mgr) tid =
  join.modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
     (Nothing, _) -> (m, return Nothing)
     (Just st, m') -> (m',Just <$> takeMVar st)



-- | Block until all managed threads terminate
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
  where elems m = return (M.empty, M.elems m)




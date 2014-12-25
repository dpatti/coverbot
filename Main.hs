import qualified Data.Map as Map
import           Control.Concurrent (threadDelay)

type Token = String
type Key = String
data Auth = Auth { token :: Token, key :: Key }

data QueryId = ObjectId | Me
type ObjectId = String

data Member = Member
data Action = Action
data Board = Board

-- Map of board id -> last action id scanned
type TailMarker = ObjectId
type PollState = Map.Map ObjectId TailMarker

getNewActions :: Auth -> Board -> IO (Board, [Action])
getNewActions = undefined

getMember :: Auth -> QueryId -> IO Member
getMember = undefined

-- Get list of boards you are a member of
getBoardsList :: Auth -> IO [Board]
getBoardsList = undefined

updateState :: (Board, TailMarker) -> IO (ObjectId, TailMarker)
updateState = undefined

extractStates :: PollState -> [Board] -> [TailMarker]
extractStates = undefined

mergeState :: PollState -> PollState -> PollState
mergeState = undefined

commitState :: PollState -> IO ()
commitState = undefined

-- Main loop
pollBoards :: Auth -> PollState -> IO ()
pollBoards auth state = do
  -- Get the boards we are on
  boards <- getBoardsList auth
  -- Pull out the states for those boards
  let states = extractStates state boards
  -- Using the existing state, perform the query on each board that returns new
  -- pairs
  updatedPairs <- mapM updateState (zip boards states)
  -- Make sure we keep old states around by merging the new ones in
  let newState = mergeState state (Map.fromList updatedPairs)
  -- Write to disk, wait, and iterate
  commitState newState
  threadDelay 2000000
  pollBoards auth newState

-- Load configuration from disk
conf :: String -> IO String
conf = undefined

-- Load poll state from disk
loadLastPoll :: IO (PollState)
loadLastPoll = undefined

main :: IO ()
main = do
  token <- conf "token"
  key <- conf "key"
  let auth = Auth token key
  pollState <- loadLastPoll
  pollBoards auth pollState

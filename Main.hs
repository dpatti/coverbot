{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (threadDelay)
import           Control.Monad
import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Configured, Name, Value)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Text (Text)
import           System.Directory (doesFileExist)

type Token = String
type Key = String
data Auth = Auth { token :: Token, key :: Key }

data QueryId = ObjectId | Me
type ObjectId = String

data Resource = Resource { oid :: ObjectId }
type Member = Resource
type Action = Resource
type Board = Resource

-- Map of board id -> last action id scanned
type TailMarker = ObjectId
type PollState = Map.Map ObjectId TailMarker

-- request stuff
data Verb = GET | POST | PUT | DELETE deriving (Show)
type Path = String
data Response = Response

requestTrello :: Auth -> Verb -> Path -> IO Response
requestTrello _ verb path = do
  putStrLn $ (show verb) ++ " " ++ path
  return Response

getNewActions :: Auth -> Board -> TailMarker -> IO [Action]
getNewActions auth board last = do
  response <- requestTrello auth GET url
  return []
  where url = "/1/boards/" ++ (oid board) ++ "/actions" ++ qs
        qs = "?actions_since=" ++ last

-- Get list of boards you are a member of
getBoardsList :: Auth -> IO [Board]
getBoardsList auth = do
  response <- requestTrello auth GET "/1/members/me/boards?fields=id"
  return []

updateState :: Auth -> (Board, TailMarker) -> IO TailMarker
updateState auth (board, last) = do
  actions <- getNewActions auth board last
  case null actions of
    True  -> return last
    False ->
      -- See if we take action
      -- Return the new last read
      return . oid . List.last $ actions

-- Turn a state object and board list into a list of tail markers
extractStates :: PollState -> [Board] -> [TailMarker]
extractStates ps boards = map lookup boards where
  lookup board = case Map.lookup (oid board) ps of
                  Just last -> last
                  Nothing -> (oid board)

-- Merge states together. Note: the first argument is given precedence.
mergeState :: PollState -> PollState -> PollState
mergeState = Map.union

stateFile :: String
stateFile = ".state"

-- Save poll state to disk
commitState :: PollState -> IO ()
commitState ps = do
  writeFile stateFile (show ps)

-- Load poll state from disk
restoreState :: IO (PollState)
restoreState = do
  exists <- doesFileExist stateFile
  case exists of
    True -> do
      contents <- readFile stateFile
      return (read contents)
    False -> return Map.empty

-- Main loop
pollBoards :: Auth -> PollState -> IO ()
pollBoards auth state = do
  putStrLn "Polling..."
  -- Get the boards we are on
  boards <- getBoardsList auth
  -- Pull out the states for those boards
  let states = extractStates state boards
  -- Using the existing state, perform the query on each board that returns new
  -- pairs
  newTails <- mapM (updateState auth) (zip boards states)
  let updatedPairs = zip (map oid boards) newTails
  -- Make sure we keep old states around by merging the new ones in
  let newState = mergeState (Map.fromList updatedPairs) state
  -- Write to disk, wait, and iterate
  commitState newState
  threadDelay 2000000
  pollBoards auth newState

-- Load configuration from disk
conf :: Configured a => Name -> IO a
conf key = do
  config <- Conf.load [Conf.Required "conf"]
  Conf.require config key

main :: IO ()
main = do
  auth <- liftM2 Auth (conf "token") (conf "key")
  pollState <- restoreState
  pollBoards auth pollState

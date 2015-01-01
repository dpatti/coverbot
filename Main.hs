{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

import           BasePrelude
import           Control.Lens hiding (Action)
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BS
import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Configured, Name)
import qualified Data.Map as Map
import           Data.Text (Text, unpack)
import qualified Network.Wreq as Wreq
import           System.Directory (doesFileExist)

type Token = Text
type Key = Text
data Auth = Auth { authToken :: Token, authKey :: Key }

type ObjectId = String
data Member = Member ObjectId
data Board = Board ObjectId
data Card = Card ObjectId
data ActionType = MiscType String
data Action = Action ObjectId ActionType
data Comment = Comment Action Text

class TrelloResource a where
  oid :: a -> ObjectId
instance TrelloResource Member where
  oid (Member idMember) = idMember
instance TrelloResource Board where
  oid (Board idBoard) = idBoard
instance TrelloResource Card where
  oid (Card idCard) = idCard
instance TrelloResource Action where
  oid (Action idAction _) = idAction

-- Map of board id -> last action id scanned
type TailMarker = ObjectId
type PollState = Map.Map ObjectId TailMarker

-- request stuff
type Path = String

authOptions :: Auth -> Wreq.Options -> Wreq.Options
authOptions auth = (Wreq.param "key" .~ [authKey auth])
                 . (Wreq.param "token" .~ [authToken auth])

apiRoot :: Path
apiRoot = "https://api.trello.com"

fullPath :: Path -> Path
fullPath = (apiRoot ++)

getTrello :: Auth -> Path -> IO (Wreq.Response BS.ByteString)
getTrello auth path = do
  let opts = Wreq.defaults & authOptions auth
  Wreq.getWith opts (fullPath path)

postTrello :: Auth -> Path -> IO (Wreq.Response BS.ByteString)
postTrello auth path = do
  let opts = Wreq.defaults & authOptions auth
  Wreq.postWith opts (fullPath path) BS.empty

getNewActions :: Auth -> Board -> TailMarker -> IO [Action]
getNewActions auth board tm = do
  r <- getTrello auth url
  let actions = r ^.. (Wreq.responseBody . values)
  return $ map (makeAction . toAction) actions
  where url = "/1/boards/" ++ (oid board) ++ "/actions" ++ qs
        qs = "?actions_since=" ++ tm
        toAction a = ((unpack . fromJust $ a ^? key "id" . _String), (unpack . fromJust $ a ^? key "type" . _String))
        makeAction (idAction, t) = Action idAction (MiscType t)

-- Get list of boards you are a member of
getBoardsList :: Auth -> IO [Board]
getBoardsList = do
  undefined
  -- getTrello auth "/1/members/me/boards?fields=id" :: IO [Board]

updateState :: Auth -> (Board, TailMarker) -> IO TailMarker
updateState auth (board, tm) = do
  actions <- getNewActions auth board tm
  case actions of
    []  -> return tm
    _   ->
      -- See if we take action
      -- Return the new last read
      return . oid . last $ actions

-- Turn a state object and board list into a list of tail markers
extractStates :: PollState -> [Board] -> [TailMarker]
extractStates ps boards = map getMarker boards where
  getMarker board = case Map.lookup (oid board) ps of
                  Just tm -> tm
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
conf name = do
  config <- Conf.load [Conf.Required "conf"]
  Conf.require config name

main :: IO ()
main = do
  auth <- Auth <$> (conf "token") <*> (conf "key")

  pollState <- restoreState
  pollBoards auth pollState

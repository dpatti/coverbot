{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

import           BasePrelude
import           Control.Lens hiding (Action)
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BS
import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Configured, Name)
import qualified Data.Map as Map
import           Data.Text (Text, pack, unpack)
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

type PartialQuery = Wreq.Options -> Wreq.Options

emptyQuery :: PartialQuery
emptyQuery = id

getTrello :: Auth -> Path -> PartialQuery -> IO (Wreq.Response BS.ByteString)
getTrello auth path query = do
  let opts = Wreq.defaults & authOptions auth & query
  -- putStrLn $ "GET " ++ path
  -- putStrLn $ show opts
  Wreq.getWith opts (fullPath path)

{-
postTrello :: Auth -> Path -> IO (Wreq.Response BS.ByteString)
postTrello auth path = do
  let opts = Wreq.defaults & authOptions auth
  Wreq.postWith opts (fullPath path) BS.empty
-}

grabValue :: AsValue a => a -> Text -> String
grabValue obj name = unpack . fromJust $ obj ^? key name . _String

-- Get new actions on a single board
getNewActions :: Auth -> Board -> TailMarker -> IO [Action]
getNewActions auth board tm = do
  r <- getTrello auth url query
  let actions = r ^.. (Wreq.responseBody . values)
  return $ map (makeAction . toAction) actions
  where url = "/1/boards/" ++ oid board ++ "/actions"
        -- TODO: if we get the max number back, we have to re-requst using a
        -- before as well, as we can't guarantee order and therefore cannot
        -- update the tail
        query = (Wreq.param "since" .~ [pack tm])
              . (Wreq.param "filter" .~ ["addMemberToCard,commentCard"])
              . (Wreq.param "limit" .~ ["100"])
        toAction a = (grabValue a "id", grabValue a "type")
        makeAction (idAction, t) = Action idAction (MiscType t)

-- Get list of boards you are a member of
getBoardsList :: Auth -> IO [Board]
getBoardsList auth = do
  r <- getTrello auth url emptyQuery
  let boards = r ^.. (Wreq.responseBody . values)
  -- debug: only process a single board
  return . {-(:[]) . head . -} map (Board . toBoard) $ boards
  where url = "/1/members/me/boards?fields=id"
        toBoard b = grabValue b "id"

{-
actionTrigger :: Action -> Maybe ()
actionTrigger (Action idAction actionType) = case actionType of
  MiscType "commentCard" -> Just ()
  MiscType "addMemberToCard" -> Just ()
  _ -> Nothing

processTrigger :: Maybe () -> IO ()
processTrigger _ = return ()
-}

updateState :: Auth -> (Board, TailMarker) -> IO TailMarker
updateState auth (board, tm) = do
  putStrLn $ "Updating board " ++ oid board
  actions <- getNewActions auth board tm
  let newActions = filter ((> tm) . oid) actions
  putStrLn $ "  Found " ++ show (length newActions) ++ " new actions"
  case newActions of
    [] -> return tm
    _ -> do
      let newTm = maximum . map oid $ newActions
      putStrLn $ "  Processed up to action " ++ newTm
      return newTm

-- Turn a state object and board list into a list of tail markers
extractStates :: PollState -> [Board] -> [TailMarker]
extractStates ps = map getMarker where
  getMarker board = fromMaybe (oid board) (Map.lookup (oid board) ps)

-- Merge states together. Note: the first argument is given precedence.
mergeState :: PollState -> PollState -> PollState
mergeState = Map.union

stateFile :: String
stateFile = ".state"

-- Save poll state to disk
commitState :: PollState -> IO ()
commitState ps = writeFile stateFile (show ps)

-- Load poll state from disk
restoreState :: IO PollState
restoreState = do
  exists <- doesFileExist stateFile
  if exists
    then do
      contents <- readFile stateFile
      return (read contents)
    else return Map.empty

-- Main loop
pollBoards :: Auth -> PollState -> IO ()
pollBoards auth state = do
  putStrLn "Polling..."
  -- Get the boards we are on
  boards <- getBoardsList auth
  putStrLn . ("Found boards: " ++) . show . map oid $ boards
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
  auth <- Auth <$> conf "token" <*> conf "key"

  pollState <- restoreState
  pollBoards auth pollState

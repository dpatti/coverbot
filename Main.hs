{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

import           BasePrelude
import           Control.Lens hiding (Action)
import           Data.Aeson.Lens
import           Data.Aeson.Types (Value)
import           Data.ByteString.Lens
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
type Username = String
type ActionData = Value

data Member = Member ObjectId Username
data Board = Board ObjectId
data Card = Card ObjectId
data ActionType = MiscType String deriving Show
data Action = Action ObjectId ActionType ActionData deriving Show

class TrelloResource a where
  oid :: a -> ObjectId
instance TrelloResource Member where
  oid (Member idMember _) = idMember
instance TrelloResource Board where
  oid (Board idBoard) = idBoard
instance TrelloResource Card where
  oid (Card idCard) = idCard
instance TrelloResource Action where
  oid (Action idAction _ _) = idAction

-- Map of board id -> last action id scanned
type TailMarker = ObjectId
type PollState = Map.Map ObjectId TailMarker

-- request stuff
type Path = String

{- So here's how this works.
 -
 - addMemberToCard: Check if data.idMember is me.id. If so, get the card title,
 -                  add a cover based on the title, and remove self.
 -
 - commentCard: If you are at mentioned (@coverbot, maybe use me.username), get
 -              the rest of the message, and if it is blank, default it to the
 -              card title. Use the message as the source for the cover image.
 -}

-- Actions

addMemberToCard :: Auth -> Action -> IO ()
addMemberToCard auth action = do
  putStrLn $ "addMemberToCard " ++ show action
  me <- getMe auth
  when (isMyId me (getDataIdMember action)) $ do
    putStrLn $ "Action with id " ++ oid action ++ " added me"

    r <- getCard auth $ getDataCardId action
    let cardTitle = grabString (r ^. Wreq.responseBody) "name"
    putStrLn $ "Card title: " ++ cardTitle

    img <- getJpgTo cardTitle
    putStrLn $ "Using image: " ++ img

    let cid = grabString (r ^. Wreq.responseBody) "id"
    addAttachment auth cid img

commentCard :: Auth -> Action -> IO ()
commentCard _ _ = putStrLn "commentCard"

-- Helpers

getJpgTo :: String -> IO String
getJpgTo name = do
  r <- Wreq.get $ "http://" ++ clean name ++ ".jpg.to/l+r+jpg"
  return . parseImageTag $ r ^. Wreq.responseBody . from packedChars

clean :: String -> String
clean = dotSpace . compactSpace . removeSpecial
  where removeSpecial = filter (\c -> isAlphaNum c || isSpace c)
        compactSpace (' ':' ':xs) = ' ' : compactSpace xs
        compactSpace (a:xs) = a : compactSpace xs
        compactSpace [] = []
        dotSpace = map (\c -> if isSpace c then '.' else c)

parseImageTag :: String -> String
parseImageTag = leftChunk . rightChunk
  where rightChunk = drop 75
        leftChunk = takeWhile (/= '"')

getDataIdMember :: Action -> ObjectId
getDataIdMember (Action _ _ adata) = grabString adata "idMember"

getDataCardId :: Action -> ObjectId
getDataCardId (Action _ _ adata) = grabString (fromJust $ adata ^? key "card") "id"

isMyId :: Member -> ObjectId -> Bool
isMyId (Member me _) other = me == other

{-
isMyName :: Member -> String -> Bool
isMyName (Member _ me) other = (me == other)

cardTitle :: Card -> String
cardTitle = undefined
-}

-- Trello client

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
  Wreq.getWith opts (fullPath path)

postTrello :: Auth -> Path -> PartialQuery -> IO (Wreq.Response BS.ByteString)
postTrello auth path query = do
  let opts = Wreq.defaults & authOptions auth & query
  Wreq.postWith opts (fullPath path) BS.empty

addAttachment :: Auth -> ObjectId -> String -> IO ()
addAttachment auth cid url = void $ postTrello auth ("/1/cards/" ++ cid ++ "/attachments") params
  where params = (Wreq.param "name" .~ ["image"])
               . (Wreq.param "url" .~ [pack url])

getMe :: Auth -> IO Member
getMe auth = do
  r <- getTrello auth "/1/members/me" emptyQuery
  let me = r ^. Wreq.responseBody
  return $ Member (grabString me "id") (grabString me "username")

getCard :: Auth -> ObjectId -> IO (Wreq.Response BS.ByteString)
getCard auth cid = getTrello auth ("/1/cards/" ++ cid) emptyQuery

grabString :: AsValue a => a -> Text -> String
grabString obj name = unpack . fromMaybe "undefined" $ obj ^? key name . _String

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
        toAction a = (grabString a "id", grabString a "type", fromJust $ a ^? key "data")
        makeAction (idAction, t, d) = Action idAction (MiscType t) d

-- Get list of boards you are a member of
getBoardsList :: Auth -> IO [Board]
getBoardsList auth = do
  r <- getTrello auth url emptyQuery
  let boards = r ^.. (Wreq.responseBody . values)
  -- debug: only process a single board
  return . {-(:[]) . head . -} map (Board . toBoard) $ boards
  where url = "/1/members/me/boards?fields=id"
        toBoard b = grabString b "id"

actionTrigger :: Auth -> Action -> IO ()
actionTrigger auth action@(Action _ atype _) = case atype of
  MiscType "addMemberToCard" -> addMemberToCard auth action
  MiscType "commentCard" -> commentCard auth action
  -- Noop
  _ -> return ()

updateState :: Auth -> (Board, TailMarker) -> IO TailMarker
updateState auth (board, tm) = do
  putStrLn $ "Updating board " ++ oid board
  actions <- getNewActions auth board tm
  let newActions = filter ((> tm) . oid) actions
  let actionTriggers = map (actionTrigger auth) newActions
  putStrLn $ "  Found " ++ show (length newActions) ++ " new actions"
  case newActions of
    [] -> return tm
    _ -> do
      -- do them all
      sequence_ actionTriggers
      let newTm = maximum . map oid $ newActions
      putStrLn $ "  Processed up to action " ++ newTm
      return tm -- newTm

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
  -- threadDelay 2000000
  -- pollBoards auth newState

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

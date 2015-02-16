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
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.Wreq as Wreq
import           Numeric (showHex)
import           System.Directory (doesFileExist)
import           System.IO

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

username :: Member -> Username
username (Member _ u) = u

getData :: Action -> ActionData
getData (Action _ _ d) = d

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
  let adata = getData action
  when (isMyId me (grabString adata "idMember")) $ do
    putStrLn $ "Action with id " ++ oid action ++ " added me"

    let card = fromJust $ adata ^? key "card"
    let cardTitle = strip $ grabString card "name"
    putStrLn $ "Card title: " ++ show cardTitle

    let cid = grabString card "id"
    coverMessage auth cid cardTitle
    removeSelf auth me cid

commentCard :: Auth -> Action -> IO ()
commentCard auth action = do
  putStrLn $ "commentCard " ++ show action
  me <- getMe auth
  let adata = getData action
  let comment = grabString adata "text"
  when (isMention me comment) $ do
    putStrLn $ "Action with id " ++ oid action ++ " commented at me"

    let message = strip . spliceList ('@':username me) $ comment
    putStrLn $ "Message: " ++ show message

    let card = fromJust $ adata ^? key "card"

    useMessage <- if message /= ""
      then return message
      else do
        let cardTitle = strip $ grabString card "name"
        putStrLn $ "Using card title: " ++ show cardTitle
        return cardTitle

    coverMessage auth (grabString card "id") useMessage

-- Helpers

coverMessage :: Auth -> ObjectId -> String -> IO ()
coverMessage auth cid message = do
  img <- getJpgTo message
  putStrLn $ "Using image: " ++ img
  addAttachment auth cid img

getJpgTo :: String -> IO String
getJpgTo name = do
  r <- Wreq.get $ "http://" ++ clean name ++ ".jpg.to/l+r+jpg"
  return . parseImageTag $ r ^. Wreq.responseBody . from packedChars

clean :: String -> String
clean = dotSpace . strip . compactSpace . removeSpecial
  where removeSpecial = filter (\c -> isAlphaNum c || isSpace c)
        compactSpace (' ':' ':xs) = ' ' : compactSpace xs
        compactSpace (a:xs) = a : compactSpace xs
        compactSpace [] = []
        dotSpace = map (\c -> if isSpace c then '.' else c)

strip :: String -> String
strip = rstrip . lstrip
  where lstrip (x:xs)
          | isSpace x = lstrip xs
          | otherwise = x:xs
        lstrip [] = []
        rstrip = reverse . lstrip . reverse

parseImageTag :: String -> String
parseImageTag = leftChunk . rightChunk
  where rightChunk = drop 75
        leftChunk = takeWhile (/= '"')

isMyId :: Member -> ObjectId -> Bool
isMyId (Member me _) other = me == other

isMention :: Member -> String -> Bool
isMention (Member _ me) comment = ('@':me) `isInfixOf` comment

spliceList :: Eq a => [a] -> [a] -> [a]
spliceList remove source@(x:xs)
  | remove `isPrefixOf` source = spliceList remove $ drop (length remove) source
  | otherwise = x : spliceList remove xs
spliceList _ [] = []

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

deleteTrello :: Auth -> Path -> PartialQuery -> IO (Wreq.Response BS.ByteString)
deleteTrello auth path query = do
  let opts = Wreq.defaults & authOptions auth & query
  Wreq.deleteWith opts (fullPath path)

removeSelf :: Auth -> Member -> ObjectId -> IO ()
removeSelf auth (Member me _) cid = void $
  deleteTrello auth ("/1/cards/" ++ cid ++ "/members/" ++ me) emptyQuery

addAttachment :: Auth -> ObjectId -> String -> IO ()
addAttachment auth cid url = void $ postTrello auth ("/1/cards/" ++ cid ++ "/attachments") params
  where params = (Wreq.param "name" .~ ["image"])
               . (Wreq.param "url" .~ [pack url])

getMe :: Auth -> IO Member
getMe auth = do
  r <- getTrello auth "/1/members/me" emptyQuery
  let me = r ^. Wreq.responseBody
  return $ Member (grabString me "id") (grabString me "username")

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
      return newTm

-- Turn a state object and board list into a list of tail markers
extractStates :: PollState -> ObjectId -> [Board] -> [TailMarker]
extractStates ps start = map getMarker where
  getMarker board = fromMaybe start (Map.lookup (oid board) ps)

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

-- Return an ObjectId whose date equivalent is equal to now
currentObjectId :: IO ObjectId
currentObjectId = do
  now <- getPOSIXTime
  return $ showHex (round now :: Integer) (replicate 16 '0')

-- Main loop
pollBoards :: Auth -> PollState -> IO ()
pollBoards auth state = do
  putStrLn "Polling..."
  -- Get the boards we are on
  boards <- getBoardsList auth
  putStrLn . ("Found boards: " ++) . show . map oid $ boards
  -- Set a default tailmarker based on the current timestamp
  now <- currentObjectId
  -- Pull out the states for those boards
  let states = extractStates state now boards
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
  hSetBuffering stdout LineBuffering
  auth <- Auth <$> conf "token" <*> conf "key"

  pollState <- restoreState
  pollBoards auth pollState

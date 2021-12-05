{-# LANGUAGE NamedFieldPuns #-}

-- Enumerations

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

data Colour
  = Black
  | Red
  | Green
  | Blue
  | Cyan
  | Yellow
  | Magenta
  | White
  | RGB Int Int Int -- constructor has arguments -> no enumeration

data MyBool = MyFalse | MyTrue
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- Named Fields (Record Syntax)

data Configuration
  = Configuration
      String -- User name
      String -- Local host
      String -- Remote host
      Bool -- Is guest?
      Bool -- Is superuser?
      String -- Current directory
      String -- Home directory
      Integer -- Time connected
  deriving (Eq, Show)

getUserName (Configuration un _ _ _ _ _ _ _) = un

getLocalHost (Configuration _ lh _ _ _ _ _ _) = lh

getRemoteHost (Configuration _ _ rh _ _ _ _ _) = rh

getIsGuest (Configuration _ _ _ ig _ _ _ _) = ig -- And so on...

data RecordConfiguration = RecordConfiguration
  { username :: String,
    localHost :: String,
    remoteHost :: String,
    isGuest :: Bool,
    isSuperuser :: Bool,
    currentDir :: String,
    homeDir :: String,
    timeConnected :: Integer
  }
  deriving (Eq, Show)

directoryExists _ = True -- dummy

changeDirAndIncreaseTime :: RecordConfiguration -> String -> Integer -> RecordConfiguration
changeDirAndIncreaseTime cfg newDir time =
  if directoryExists newDir
    then cfg {currentDir = newDir, timeConnected = time + timeConnected cfg}
    else error "Directory does not exist"

postWorkingDir :: RecordConfiguration -> String
postWorkingDir cfg = currentDir cfg

getHostData, getHostData', getHostData'' :: RecordConfiguration -> (String, String)
getHostData RecordConfiguration {localHost = lh, remoteHost = rh} = (lh, rh)
getHostData' RecordConfiguration {localHost, remoteHost} = (localHost, remoteHost) -- only with GHC's NamedFieldPuns extension
getHostData'' RecordConfiguration {localHost, remoteHost = rh} = (localHost, rh) -- only with GHC's NamedFieldPuns extension

initCFG = RecordConfiguration "nobody" "nowhere" "nowhere" False False "/" "/" 0

initCFG' =
  RecordConfiguration
    { username = "nobody",
      localHost = "nowhere",
      remoteHost = "nowhere",
      isGuest = False,
      isSuperuser = False,
      currentDir = "/",
      homeDir = "/",
      timeConnected = 0
    }

cfgFoo = RecordConfiguration {username = "Foo"} -- evaluating any field excpet username will fail

cfgBar = RecordConfiguration {localHost = "Bar", remoteHost = "Baz"} -- evaluating any field except localHost and remoteHost will fail

cfgUndef = Configuration {} -- evaluating any field will fail

-- Parameterized Types

data MyMaybe a = MyNothing | MyJust a

lookupName :: [(String, String)] -> String -> MyMaybe (String, String)
lookupName _ _ = error "dummy"

type Stack a = [a]

data MyEither a b = MyLeft a | MyRight b

pairOff :: Integral a => a -> MyEither String a
pairOff people
  | people < 0 = MyLeft "Can't pair off negative number of people."
  | people > 30 = MyLeft "Too many people for this activity."
  | even people = MyRight (people `div` 2)
  | otherwise = MyLeft "Can't pair off an odd number of people."

groupPeople :: (Integral a, Show a) => a -> String
groupPeople people = case pairOff people of
  MyRight groups -> "We have " ++ show groups ++ " group(s)."
  MyLeft problem -> "Problem! " ++ problem

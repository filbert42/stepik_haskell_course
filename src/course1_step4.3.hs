import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show
data LogEntry = LogEntry { timestamp::UTCTime, logLevel::LogLevel, message::String } deriving Show

logLevelToString :: LogLevel -> String
logLevelToString = show 

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry t l m) = timeToString t ++ ": " ++ logLevelToString l ++ ": " ++ m

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = lastName p1}

abbrFirstName :: Person -> Person
abbrFirstName prsn@(Person fn _ _) 
  | length fn < 2 = prsn
  | otherwise = prsn {firstName = take 1 fn ++ "."}
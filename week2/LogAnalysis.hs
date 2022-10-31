{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.Maybe
import Text.Read

main :: IO ()
main = print "hi"

-- The first char in the error should be I, W or E.
-- If E, this must be followed by an integer.
parseType :: [String] -> Maybe (MessageType, [String])
parseType ("I" : rest)         =  Just (Info, rest)
parseType ("W" : rest)         =  Just (Warning, rest)
parseType ("E" : level : rest) = case readMaybe level of
                               Just n -> Just (Error n, rest)
                               _ -> Nothing
parseType _                    = Nothing

-- After the message type is parsed, the next section must be an integer.
parseTime :: [String] -> Maybe (TimeStamp, [String])
parseTime (time: rest) = case readMaybe time of 
                           Just n -> Just(n, rest)
                           _ -> Nothing
parseTime _ = Nothing

-- The remainder of the line is an informational message.
-- We allow this to be empty.
parseInfo :: [String] -> String
parseInfo [] = ""
parseInfo msg = unwords msg

parseMessage :: String -> LogMessage
parseMessage s = fromMaybe (Unknown s) (f s) where
  f t =
    do
    (mType, r1) <- parseType . words $ t
    (mTime, r2) <- parseTime r1
    let info = unwords r2
    return (LogMessage mType mTime info)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Q2. Insert a message into a message tree.
insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert new@(LogMessage _ newTime _) (Node left old@(LogMessage _ oldTime _) right)
  | newTime <= oldTime = Node (insert new left) old right
  | otherwise          = Node left old (insert new right)
insert _ tree = tree

-- Q3. Build a message tree from a list of messages.
build :: [LogMessage] -> MessageTree
build xs = foldr insert Leaf xs

-- Q4. Traverse a tree in order.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = (inOrder l) ++ [msg] ++ (inOrder r)

-- Q5. 
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map getMsg (filter isSevere xs)
  where isSevere (LogMessage (Error n) _ _ ) = n >= 50
        isSevere _ = False
        getMsg (LogMessage _ _ msg) = msg
        getMsg _ = ""

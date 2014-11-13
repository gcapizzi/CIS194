{-# OPTIONS_GHC -Wall #-}
module LogAnalysis
( parseMessage,
  parse,
  insert,
  build,
  inOrder,
  whatWentWrong
) where

import Log
import Text.Regex.Posix

logRegex :: [Char]
logRegex = "(I|W) ([0-9]+) (.*)|E ([0-9]+) ([0-9]+) (.*)"

parseMessage :: String -> LogMessage
parseMessage message =
    case message =~ logRegex :: (String, String, String, [String]) of
        (_, _, _, ["I", timestamp, text, _, _, _]) -> LogMessage Info (read timestamp) text
        (_, _, _, ["W", timestamp, text, _, _, _]) -> LogMessage Warning (read timestamp) text
        (_, _, _, [_, _, _, level, timestamp, text]) -> LogMessage (Error (read level)) (read timestamp) text
        _ -> Unknown message

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert message@(LogMessage _ _ _) Leaf = Node Leaf message Leaf
insert message@(LogMessage _ timestamp _) (Node left nodeMessage@(LogMessage _ nodeTimestamp _) right)
    | timestamp > nodeTimestamp = Node left nodeMessage (insert message right)
    | otherwise                 = Node (insert message left) nodeMessage right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build messages = foldl (\t m -> insert m t) Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = inOrder . build

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = [m | LogMessage (Error l) _ m <- sortMessages messages, l >= 50]

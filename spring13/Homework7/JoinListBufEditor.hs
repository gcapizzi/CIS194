module Main where

import Buffer
import JoinList
import Scrabble
import Sized
import JoinListBuffer
import Editor

main = runEditor editor (fromString "This buffer is for notes you don't want to save, and for\nevaluation of steam valve coefficients.\nTo load a different file, type the character L followed\nby the name of the file." :: JoinList (Score, Size) String)

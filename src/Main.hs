{-# LANGUAGE TemplateHaskell #-}
module Main where

import Game.Utils
import TitleScene

main :: IO ()
main = play 640 480 "SnakeGameHaskell" titleScene

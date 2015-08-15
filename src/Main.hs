{-# LANGUAGE TemplateHaskell #-}
module Main where

import Game.Scene
import TitleScene
import GameScene
import ResultScene

import Control.Monad

main :: IO ()
main = play 640 480 "SnakeGameHaskell" $ do
    forever $ do
        titleScene
        score <- gameScene
        resultScene score

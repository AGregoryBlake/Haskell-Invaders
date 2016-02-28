{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Linear
import Linear.Affine
import Control.Monad (unless)
import World
import Data.Word
import Control.Concurrent
import System.Random

windowWidth = 500
windowHeight = 500
shipWidth = 25
shipHeight = 15
invaderSide = 20
bulletSide = 4            
       
main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
         { windowInitialSize = V2 windowWidth windowHeight }
  renderer <- createRenderer window (-1) defaultRenderer
  gen <- getStdGen
  appLoop renderer gen World.worldInit

appLoop :: Renderer -> StdGen -> World -> IO ()
appLoop renderer gen world = do
  (a,g) <- return $ randomR (0, (length (World.getInvaders world) - 1)) gen
  events <- pollEvents
  let eventIsKeyPress key event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == key
          _ -> False
      keyPressed key = not (null (filter (eventIsKeyPress key) events))
      qPressed = keyPressed KeycodeQ
      leftPressed = keyPressed KeycodeLeft
      rightPressed = keyPressed KeycodeRight
      spacePressed = keyPressed KeycodeSpace

  drawWorld renderer world
  present renderer
  threadDelay 16500
  unless (qPressed || World.gameOver world) $ appLoop renderer g $ do
         if rightPressed then worldUpdate a world DRight spacePressed
         else if leftPressed then worldUpdate a world DLeft spacePressed
         else worldUpdate a world (getShipDirection world) spacePressed

drawBackground :: Renderer -> IO ()
drawBackground renderer = do
               rendererDrawColor renderer $= V4 0 0 0 255
               clear renderer

drawWorld :: Renderer -> World -> IO ()
drawWorld renderer (World ship invaders shipBullets invaderBullets) = do
          drawBackground renderer 
          drawShip renderer ship
          drawInvaders renderer invaders
          drawBullets renderer shipBullets 255 255 255
          drawBullets renderer invaderBullets 255 0 0
               
drawShip :: Renderer -> Ship -> IO ()
drawShip renderer (Ship _ x) = do
         rendererDrawColor renderer $= V4 255 255 255 255
         fillRect renderer (Just (Rectangle x (V2 shipWidth shipHeight)))
         
         
drawInvaders :: Renderer -> [Invader] -> IO ()
drawInvaders renderer invaders = do
         rendererDrawColor renderer $= V4 255 0 0 255
         mapM_ (\(Invader pos) -> fillRect renderer (Just (Rectangle pos (V2 invaderSide invaderSide)))) invaders
         return ()

drawBullets :: Renderer -> [Bullet] -> Word8 -> Word8 -> Word8 -> IO ()
drawBullets renderer bullets x y z = do
         rendererDrawColor renderer $= V4 x y z 255
         mapM_ (\(Bullet _ pos) -> fillRect renderer (Just (Rectangle pos (V2 bulletSide bulletSide)))) bullets

{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Linear
import Linear.Affine
import Control.Monad (unless)
import World
import Data.Word
import Control.Concurrent       

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
  appLoop renderer World.worldInit

appLoop :: Renderer -> World -> IO ()
appLoop renderer world = do
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
  unless qPressed $ appLoop renderer $ do
         if rightPressed then worldUpdate world DRight spacePressed
         else if leftPressed then worldUpdate world DLeft spacePressed
         else worldUpdate world (getShipDirection world) spacePressed

drawBackground :: Renderer -> IO ()
drawBackground renderer = do
               rendererDrawColor renderer $= V4 0 0 0 255
               clear renderer

drawWorld :: Renderer -> World -> IO ()
drawWorld renderer (World ship invaders shipBullets invaderBullets) = do
          drawBackground renderer 
          drawShip renderer ship
          drawInvaders renderer invaders
          drawBullets renderer shipBullets 0
          drawBullets renderer invaderBullets 255
               
drawShip :: Renderer -> Ship -> IO ()
drawShip renderer (Ship _ x) = do
         rendererDrawColor renderer $= V4 255 255 255 255
         fillRect renderer (Just (Rectangle x (V2 shipWidth shipHeight)))
         
         
drawInvaders :: Renderer -> [Invader] -> IO ()
drawInvaders renderer invaders = do
         rendererDrawColor renderer $= V4 255 0 0 255
         mapM_ (\(Invader pos) -> fillRect renderer (Just (Rectangle pos (V2 invaderSide invaderSide)))) invaders
         return ()

drawBullets :: Renderer -> [Bullet] -> Word8 -> IO ()
drawBullets renderer bullets x = do
         rendererDrawColor renderer $= V4 x 0 0 255
         mapM_ (\(Bullet _ pos) -> fillRect renderer (Just (Rectangle pos (V2 bulletSide bulletSide)))) bullets

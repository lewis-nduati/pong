module Pong where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort (ViewPort)
-- import Graphics.Gloss.Data.ViewPort

playPong' :: IO ()
playPong' = animate window background frame
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds initialState

{-
playPong'' :: IO ()
playPong'' = simulate window background fps initialState render update'
-}

playPong :: IO ()
playPong = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball.
update' :: ViewPort -> Float -> PongGame -> PongGame
update' _ seconds = paddleBounce . wallBounce . moveBall seconds 

-- | Update the game by moving the ball.
update :: Float -> PongGame -> PongGame
update seconds = paddleBounce . wallBounce . moveBall seconds 

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
     -- Radius. Use the same thing as in `render`
    radius = 10 

    -- The old velocities.
    (vx, vy) = ballVel game

    vx' = if paddleCollision game radius
            then
                -- Update the velocity
                -vx
            else
                -- Do nothing. Return the old velocity
                vx


-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`
    radius = 10 

    -- The old velocities.
    (vx, vy) = ballVel game


    vy' = if wallCollision (ballLoc game) radius
            then
                -- Update the velocity
                -vy
            else
                -- Do nothing. Return the old velocity
                vy

type Radius = Float
type Position = (Float, Float)

-- | Given a position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >= fromIntegral width / 2


-- | Given a position and radius of the ball, return whether a collision occurred.
paddleCollision :: PongGame -> Radius -> Bool
paddleCollision game radius = leftPaddleCollision || rightPaddleCollision
  where
    leftPaddleCollision = isLeftPaddleOffset game && isLeftPaddleHeight game
    rightPaddleCollision = isRightPaddleOffset game && isRightPaddleHeight game

    isLeftPaddleOffset :: PongGame -> Bool
    isLeftPaddleOffset game = fst (ballLoc game) - radius <= (-120 + (fromIntegral paddleWidth / 2))

    isRightPaddleOffset :: PongGame -> Bool
    isRightPaddleOffset game = fst (ballLoc game) - radius >= (120 - fromIntegral paddleWidth)

    isLeftPaddleHeight :: PongGame -> Bool
    isLeftPaddleHeight game = (snd (ballLoc game) <= player2 game + (fromIntegral paddleHeight / 2)) && (snd (ballLoc game) >= player2 game - (fromIntegral paddleHeight / 2))

    isRightPaddleHeight :: PongGame -> Bool
    isRightPaddleHeight game = (snd (ballLoc game) <= player1 game + (fromIntegral paddleHeight / 2)) && (snd (ballLoc game) >= player1 game - (fromIntegral paddleHeight / 2))

    paddleWidth :: Int
    paddleWidth = 26

    paddleHeight :: Int
    paddleHeight = 86

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'r') _ _ _) game = 
    game {ballLoc = (0,0  ) }
handleKeys (EventKey (Char 's') _ _ _) game = 
    game {player1 = player1 game - 10 }
handleKeys (EventKey (Char 'w') _ _ _) game = 
    game {player1 = player1 game + 10 }
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = 
    game {player2 = player2 game + 10 }
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = 
    game {player2 = player2 game - 10 }
handleKeys _ game = game

fps :: Int
fps = 60

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | A data structure to hold the state of the Pong game.
-- | Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen. 
  , player2 :: Float           -- ^ Right player paddle height.
  } deriving Show 

-- | Initialize the game with this game state.
initialState :: PongGame
initialState = Game
  { ballLoc = (-80, -30)
  , ballVel = (-85, -53)
  , player1 = 40
  , player2 = -80
  }

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game = 
    pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset' =
      translate 0 offset' $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y')}
  where
    -- Old locations and velocities
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New Locations.
    x' = x + vx * seconds
    y' = y + vy * seconds
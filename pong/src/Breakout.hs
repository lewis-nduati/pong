module Breakout where
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss


windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)

type Model = (Float, Float)

playBreakout :: IO ()
playBreakout = simulate
  windowDisplay
  white
  simulationRate
  initialModel
  drawingFunc
  updateFunc
  where
    simulationRate :: Int
    simulationRate = 20

    initialModel :: Model
    initialModel = (0,0)

    drawingFunc :: Model -> Picture
    drawingFunc (theta, dtheta) = Line [(0, 0), (50 * cos theta, 50 * sin theta)]

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt (theta, dtheta) = (theta + dt * dtheta, dtheta - dt * (cos theta))


data BreakoutGame = Game
    { ballLoc :: (Float, Float)       -- ^ Ball (x, y) location
    , ballVel :: (Float, Float)         -- ^ Ball (x,y) velocity
    , paddleLoc :: (Float, Float)  -- ^ Paddle (x, y) location 
    , paddleVel :: (Float, Float)   -- ^ Paddle (x, y) location 
    , score         :: Int                       -- ^ The current game score
    , boxesLoc :: [(Float, Float)] -- ^ The (x, y) list of box locations
    } deriving Show


-- | Update the game by moving the ball.
update :: ViewPort -> Float -> BreakoutGame -> BreakoutGame
update _ = moveBall

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: BreakoutGame -> BreakoutGame
paddleBounce = undefined

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: BreakoutGame -> BreakoutGame
wallBounce = undefined

type Radius = Float
type Position = (Float, Float)

-- | Given a position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = undefined

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> BreakoutGame -- ^ The initial game state
         -> BreakoutGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballVel = (x', y')}
  where
    -- Old locations and velocities
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New Locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

drawing :: Picture
drawing = pictures 
    [ rotate 90 $ translate 30 50 $ color paddleColor $ rectangleSolid 20 100
    , color ballColor $ circleSolid 10
    , boxes
    ]

boxes :: Picture
boxes = pictures 
    [ rotate 90 $ translate (-100) (-100) $ color boxesColor1 $ rectangleSolid 10 50 
    , rotate 90 $ translate (-90) (-20) $ color boxesColor2 $ rectangleSolid 10 50 
    , rotate 90 $ translate (-80) 20 $ color boxesColor3 $ rectangleSolid 10 50 
    , rotate 90 $ translate (-70) 45 $ color boxesColor1 $ rectangleSolid 10 50 
    ]

boxesColor1, boxesColor2, boxesColor3, ballColor, paddleColor  :: Color 
boxesColor1 = blue
boxesColor2 = light blue
boxesColor3 = light $ light blue
ballColor   = white 
paddleColor = white

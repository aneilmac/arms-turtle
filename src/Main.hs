module Main where

import Control.Monad (forever)
import qualified Graphics.Gloss.Data.Picture as G
import qualified Graphics.Gloss.Data.Point.Arithmetic as G
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg)
import Graphics.WorldTurtle

armLength1 :: Float
armLength1 = 100

armLength2 :: Float
armLength2 = 100

arm1Speed :: Float
arm1Speed = 2

arm2Speed :: Float
arm2Speed = 5

locusPosition :: Float
locusPosition = 0.5

main :: IO ()
main = runWorld $ do
  -- Generate arms
  arm1 <- armTurtle arm1Speed armLength1
  arm2 <- armTurtle arm2Speed armLength2
  -- Generate elastic line
  elastic <- armTurtle 0 0
  -- Generate locus point
  locus <- locusTurtle
  
  -- Set up Locus's initial position
  -- based on starting radiuses of lines.
  locus >/> setPenUp 
  drawLocus arm1 arm2 locus
  locus >/> setPenDown
  
  forever $ do 
    -- Draw arms
    arm1 >/> rt arm1Speed
    arm2 >/> rt arm2Speed
    -- Draw elastic
    drawLocusLine arm1 arm2 elastic
    -- Update locus
    drawLocus arm1 arm2 locus

-- Draws the elastic line between two arms
drawLocusLine :: Turtle -- ^ Arm A
              -> Turtle -- ^ Arm B 
              -> Turtle -- ^ Elastic
              -> WorldCommand ()
drawLocusLine armA armB t = do
  ah <- armA >/> pointFromArm armLength1
  bh <- armB >/> pointFromArm armLength2
  t >/> do
    jump ah
    let v = bh G.- ah
    setRepresentation $ G.color red $ G.line [(0, 0), (magV v, 0)]
    setHeading $ radToDeg $ argV v

-- | Moves the locus at its new postion.
drawLocus :: Turtle -- ^ Arm A
          -> Turtle -- ^ Arm B
          -> Turtle -- ^ Locus
          -> WorldCommand ()
drawLocus armA armB t = do
   p <- armA >/> pointFromArm armLength1
   q <- armB >/> pointFromArm armLength2
   let l = lerp locusPosition p q
   t >/> goto l

-- | Turtle representing an arm/elastic line. Is only used to rotate. 
armTurtle :: Float -> Float -> WorldCommand Turtle
armTurtle rSpeed length = do
  t <- makeTurtle
  t >/> do
    setRepresentation $ G.color black $ G.line [(0, 0), (length, 0)]
    setSpeed 0
    setRotationSpeed 0
    setRotationSpeed rSpeed
  return t

-- | Generates the turtle used to draw the locus. Is a red dot, with a red 
-- | line which moves instantly.
locusTurtle :: WorldCommand Turtle
locusTurtle = do
  t <- makeTurtle
  t >/> do
    setSpeed 0
    setRotationSpeed 0
    setPenColor red
    setPenSize 2
    setRepresentation $ G.color red $ G.circleSolid 2
  return t

-- | Grabs the endpoint of a Turtle arm. This is calcualted
--   from a turtle's heading, turned into a unit vector, multiplied by 
--   the giving length.
pointFromArm :: Float -- ^ Length of line
             -> TurtleCommand Point 
pointFromArm length = heading >>= \h -> return $ length G.* unitVectorAtAngle (degToRad h)

-- | What it says on the tin. A lerp function. 
lerp :: Float -- Coefficient between 0 and 1.
     -> Point -- Point /a/.
     -> Point -- Point /b/.
     -> Point -- new point some percentage value between /a/ and /b/.
lerp l a b = let (ux, uy) = (1 - l) `mulSV` a
                 (vx, vy) = l `mulSV` b
                 n = (ux + vx, uy + vy)
              in n

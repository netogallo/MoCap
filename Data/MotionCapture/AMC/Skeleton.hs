module Data.MotionCapture.AMC.Skeleton where

type Coord2D = (Double,Double)
type Coord3D = (Double,Double,Double)

data Skeleton = Skeleton {position :: Coord3D
                          orientation :: Coord3D
                          children :: [Bone]} deriving Show
                
data DegreeOfFreedom = RX | RY | RZ                                                       

data Bone = Bone {id :: Int,
                  name :: String,
                  direction :: Coord3D
                  length :: Double,
                  limits :: [Coord2D]
                  axis :: Coord3D
                  degreesOfFreedom :: [DegreeOfFreedom],
                  children :: [Bone]}
                  
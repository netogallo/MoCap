module Data.MotionCapture.AMC.Types where

type Coord2D = (Double,Double)
type Coord3D = (Double,Double,Double)

data Skeleton = Skeleton {position :: Coord3D,
                          orientation :: Coord3D,
                          skeletonBones :: [Bone]} deriving Show
                
data DegreeOfFreedom = RX | RY | RZ | TX | TY | TZ deriving (Show,Read)

data Axis = AX | AY | AZ deriving (Show,Read)

data Bone = Bone {id :: Int,
                  name :: String,
                  direction :: Coord3D,
                  length :: Double,
                  limits :: [Coord2D],
                  axis :: Coord3D,
                  degreesOfFreedom :: [DegreeOfFreedom],
                  children :: [Bone]} deriving Show


data BoneDataSection = BoneDataSection{
  boneId :: Int,
  boneName :: String,
  boneDirection :: Coord3D,
  boneLength :: Double,
  boneAxis :: (Coord3D,(Axis,Axis,Axis)),
  boneDof :: Maybe [DegreeOfFreedom],
  boneLimits :: Maybe [Coord2D]
  } deriving Show

data AsfSection = Version String
                | Name String
--                | Units UnitsSection
                | Documentation String
                | Root RootSection
                | BoneData [BoneDataSection]
--                | Hierarchy HierarchySection
                deriving Show


data RootSection = RootSection{
  rootOrder :: [DegreeOfFreedom],
  rootAxis :: (Axis,Axis,Axis),
  rootPosition :: Coord3D,
  rootOrientation :: Coord3D
  } deriving Show
             
  
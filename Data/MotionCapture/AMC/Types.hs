module Data.MotionCapture.AMC.Types where

import Text.ParserCombinators.Parsec

type Coord2D = (Double,Double)
type Coord3D = (Double,Double,Double)

data Skeleton = Skeleton {position :: Coord3D,
                          orientation :: Coord3D,
                          skeletonBones :: [Bone]} deriving Show
                
data DegreeOfFreedom = RX | RY | RZ | TX | TY | TZ deriving (Show,Read,Eq,Ord)

data Axis = AX | AY | AZ deriving (Show,Read)

data AMCSpecData = Full

instance Show AMCSpecData where
  show Full = "FULLY-SPECIFIED"
  
data AMCAngleUnits = Degrees | Radians

instance Show AMCAngleUnits where
  show Degrees = "DEGREES"
  show Radians = "RADIANS"

rootSpecifier :: String
rootSpecifier = "root"

-- | Tries to match the 6 expected coordinates of a root bone
-- rom a list.
rootBoneMatcher :: [Double] -> GenParser a st (Coord3D,Coord3D)
rootBoneMatcher [rx,ry,rz,tx,ty,tz] = return ((rx,ry,rz),(tx,ty,tz))
rootBoneMatcher l = fail $ "The root bone requires 6 coordinates, " ++ (show l) ++ " were given."

data Bone = Bone {
  id :: Int,
  name :: String,
  direction :: Coord3D,
  length :: Double,
  limits :: [Coord2D],
  axis :: Coord3D,
  degreesOfFreedom :: [DegreeOfFreedom],
  children :: [Bone]
  } deriving Show

data AMCFile = AMCFile{
  specData :: Maybe AMCSpecData,
  angleUnits :: Maybe AMCAngleUnits,
  amcSectioins :: [AMCSection]
  } deriving Show

data AMCSection = AMCSection{
  -- | The position in which this section appears
  sectionNumber :: Int,
  -- | The Coordinates of the root bone
  rootBonePosition :: Coord3D,
  -- | The orientation of the root bone
  rootBoneOrientation :: Coord3D,
  -- | The bones contained in thi 
  boneList :: [AMCBone]
  } deriving (Show)

-- | Represents a bone with it's positional values as
-- it is shown in the AMC file
data AMCBone = AMCBone{
  -- | Name given to the bone
  boneSpecifier :: String,
  -- | Rotation of the DOFs of the bone
  boneRotations :: [Double]
  } deriving (Show,Eq)

data BoneDataSection = BoneDataSection{
  boneId :: Int,
  boneName :: String,
  boneDirection :: Coord3D,
  boneLength :: Double,
  boneAxis :: (Coord3D,(Axis,Axis,Axis)),
  boneDof :: Maybe [DegreeOfFreedom],
  boneLimits :: Maybe [Coord2D]
  } deriving Show


data AsfFile = AsfFile{
  
  rootSection :: RootSection,
  boneDataSections :: [BoneDataSection]
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
             
  
skipSpace :: GenParser Char st ()
skipSpace = skipMany1 $ (space >> return ())
            <|> (tab >> return ())
            <|> (char '#' >> (skipMany $ noneOf ['\n']) >> return ())

skipWhitespace :: GenParser Char st ()
skipWhitespace = skipMany $ skipSpace 
                 <|> (newline >> return ())
                 <|> (string "\r" >> return ())
                 
-- | Parses a bone identifier
parseIdentifier :: GenParser Char st String
parseIdentifier = do
  first <- letter
  rest <- many $ alphaNum
  return $ first:rest
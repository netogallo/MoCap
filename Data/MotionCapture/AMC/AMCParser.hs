module Data.MotionCapture.AMC.AMCParser where

-- MoCap
import Data.MotionCapture.AMC.Types

-- Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import Text.Parsec.Perm

-- Haskell
import Data.Char (toUpper,toLower)
import Data.List (sortBy,find,delete)
import Data.Maybe (fromMaybe)

parseAMCFile :: AsfFile -> GenParser Char st AMCFile
parseAMCFile asf = do
  skipWhitespace
  mSpec <- optionMaybe $ try $ string $ ":" ++ (show Full)
  skipWhitespace
  mDeg <- optionMaybe $ try $ string (":" ++ (show Degrees)) 
          <|> string (":" ++ (show Radians))
  sections <- many $ try $ parseCoordsSection asf
  return $ AMCFile{
    specData=Nothing,
    angleUnits=Nothing,
    amcSectioins=sections}
  

parseCoordsSection ::  AsfFile -> GenParser Char st AMCSection
parseCoordsSection asf = do
  skipWhitespace
  secId <- parseIntegral
  secBones <- many $ try $ do
    skipWhitespace
    parseBoneCoords asf
  let
    mRoot = find (\x -> boneSpecifier x == rootSpecifier) secBones
  case mRoot of 
    Nothing -> fail $ "The root bone was not found in the list."
    Just root -> do
      (rotCoords,posCoords) <- rootBoneMatcher $ boneRotations root
      return $ AMCSection{
        sectionNumber=secId,
        rootBonePosition=posCoords,
        rootBoneOrientation=rotCoords,
        boneList=delete root secBones
        }
  

-- | Parses a bone identifier plus the coordinates indicating the 
-- current position of the bone
parseBoneCoords :: AsfFile -> GenParser Char st AMCBone
parseBoneCoords asf = do
  skipWhitespace
  identifier <- parseIdentifier
  if identifier == rootSpecifier then rootParser
    else boneFinder identifier >>= boneParser
    
  where
    bones = boneDataSections asf
    rootParser = boneParser' rootSpecifier $ Just $ rootOrder $ rootSection asf
    boneFinder bId = case find (\x-> boneName x == bId) bones of
      Just b -> return b
      Nothing -> fail $ "The bone named " ++ bId ++ " is not in the given skeleton."
    boneParser bSection = boneParser' (boneName bSection) (boneDof bSection)
    boneParser' bName bDof = case bDof of
      Nothing -> fail $ "The bone named " ++ (bName) ++ " has no channels."
      Just dofs -> do
        coords <- coordsParser dofs
        return $ AMCBone{
          boneSpecifier=bName,
          boneRotations = coords
          }

-- | Given a list of degree of freedom, create a parser
-- that parses a list of numbers representing those degrees
-- of freedom in the list order and sorts them as TX TY TZ RX RY RZ
coordsParser :: [DegreeOfFreedom] -> GenParser Char st [Double]
coordsParser dofs = do
  coords' <- mapM parseNextCoord dofs
  let
    (_,coords) = unzip $ sortBy (\(dof1,_) (dof2,_) -> compare dof1 dof2) coords'
  return $ coords
  where
    parseNextCoord dof = do
      skipSpace
      num <- parseFloat
      return (dof,num)
module Data.MotionCapture.AMC.SkeletonParser (
  skeletonParser
  )where

-- MoCap
import Data.MotionCapture.AMC.Types

-- Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import Text.Parsec.Perm

-- Haskell
import Data.Char (toUpper,toLower)
import Data.Maybe (fromMaybe)
import Debug.Trace

skeletonParser :: GenParser Char st [Maybe AsfSection]
skeletonParser = many $ try (skipWhitespace >>  section)

section :: GenParser Char st (Maybe AsfSection)
section = do
  head <- header
  skipWhitespace
  t <- return $ trace head ()
  case head of
    "units"     -> Just `fmap` unitSectionParser
    "root"      -> Just `fmap` rootSectionParser
    "bonedata"  -> Just `fmap` bonesSection
    "hierarchy" -> Just `fmap` boneHierarchy
    _           -> skipMany bodyLine >> (return . Just . Name) head

bonesSection :: GenParser Char st AsfSection
bonesSection = do
  bSections <- many $ try $ do 
    skipWhitespace
    bSec <- boneSection
    return bSec
  return $ BoneData bSections

boneSection :: GenParser Char st BoneDataSection
boneSection = do
  string "begin"
  ((_,bId),(_,bName),(_,bDir),(_,bLen),(_,bAxis),mBDof,mBLims) <- permute $ (,,,,,,)
                                                                        <$$> descriptor (string "id") parseIntegral
                                                                        <||> descriptor (string "name") parseIdentifier
                                                                        <||> descriptor (string "direction") (parseCoord3D)
                                                                        <||> descriptor (string "length") (parseFloat)
                                                                        <||> descriptor (string "axis") (parseCoordAxis)
                                                                        <|?> (Nothing,Just `fmap` descriptor (string "dof") parseDOF)
                                                                        <|?> (Nothing,Just `fmap` descriptor (string "limits") manyCoord2D)
  string "end"
  let
    bDof = fromMaybeDesc mBDof
    bLims = fromMaybeDesc mBLims
  return $ BoneDataSection{boneId=bId,
                    boneName=bName, 
                    boneDirection=bDir, 
                    boneLength=bLen,
                    boneAxis=bAxis,
                    boneDof=bDof, 
                    boneLimits=bLims}
                
fromMaybeDesc :: Maybe (String,a) -> Maybe a
fromMaybeDesc (Just (_,val)) = Just val
fromMaybeDesc Nothing = Nothing

boneHierarchy :: GenParser Char st AsfSection
boneHierarchy = do
  string "begin"
  hNodes <- many $ try $ do
    skipWhitespace
    hNode <- hierarchyNode
    return hNode
  skipWhitespace
  string "end"
  return $ Hierarchy hNodes

hierarchyNode :: GenParser Char st HierarchyNode
hierarchyNode = do
  rootName <- parseIdentifier
  childNames <- many1 $ try $ do
    many1 (char ' ')
    parseIdentifier
  return $ HierarchyNode rootName childNames

unitSectionParser :: GenParser Char st AsfSection
unitSectionParser = do
  ((_,uAngle),(_,uMass),(_,uLength)) <- permute $ (,,)
                                        <$$> descriptor (string "angle") (parseAngle)
                                        <||> descriptor (string "mass") (parseFloat)
                                        <||> descriptor (string "length") (parseFloat)
  return $ Units $ UnitsSection{unitsAngle = uAngle,
                                unitsMass = uMass,
                                unitsLength = uLength}

rootSectionParser :: GenParser Char st AsfSection
rootSectionParser = do
  ((_,rOrder),(_,rAxis),(_,rPosition),(_,rOrientation)) <- permute $ (,,,)
                                        <$$> descriptor (string "order") parseDOF
                                        <||> descriptor (string "axis") parseAxis
                                        <||> descriptor (string "position") parseCoord3D
                                        <||> descriptor (string "orientation") parseCoord3D
  
  return $ Root $ RootSection{rootOrder = rOrder,
                              rootAxis = rAxis,
                              rootPosition = rPosition,
                              rootOrientation = rOrientation}
    
parseAngle :: GenParser Char st AMCAngleUnits
parseAngle =   (string "deg" >> return Degrees)
           <|> (string "rad" >> return Radians)
           <|> fail "Didn't recognize angle unit type"

parseCoordAxis :: GenParser Char st (Coord3D,(Axis,Axis,Axis))
parseCoordAxis = do
  c <- parseCoord3D
  spaces
  ax <- parseAxis
  return (c,ax)
    
parseAxis :: GenParser Char st (Axis,Axis,Axis)    
parseAxis = do 
  spaces
  elems <- many $ do
    pAxis <- choice [try $ char ax | ax <- ['X' .. 'Z']]
    return $ read $ ['A',pAxis]
  case elems of
    [ax,ay,az] -> return (ax,ay,az)
    _ -> pzero
  
manyCoord2D :: GenParser Char st [Coord2D]
manyCoord2D = many $ do
  skipWhitespace
  char '('
  res <- parseCoord2D
  char ')'
  skipWhitespace
  return res
  
parseCoord3D :: GenParser Char st Coord3D
parseCoord3D = do
  (a:b:c:_) <- parseCoord 3
  return (a,b,c)
  
parseCoord2D :: GenParser Char st Coord2D
parseCoord2D = do
  (a:b:_) <- parseCoord 2
  return (a,b)
  
parseCoord :: Int -> GenParser Char st [Double]
parseCoord dim = do
  coords <- many1 $ try $ do
    skipMany skipSpace
    res <- parseFloat
    return res
  if Prelude.length coords == dim 
    then return $ take dim coords 
    else fail $ "Only " ++ show dim ++ " values were expected, found " ++ show coords

parseDOF :: GenParser Char st [DegreeOfFreedom]
parseDOF = do
  dofs <- many readDofs
  return dofs
  
  where
    readDofs = do
      spaces
      dof <- choice $ [try $ string x | x <- [show x' | x' <- [TX,TY,TZ,RX,RY,RZ]] ++ [map toLower $ show x' | x' <- [TX,TY,TZ,RX,RY,RZ]]]
      spaces
      return $ read $ map toUpper dof
  
bodyLine :: GenParser Char st String
bodyLine = do
  f <- noneOf [':']
  rest <- manyTill anyChar newline
  skipWhitespace
  return $ f:rest
  
descriptor :: GenParser Char st String -> GenParser Char st a -> GenParser Char st (String,a)
descriptor attrParser valParser = do
  attr <- try $ do
    spaces
    attrParser
  spaces
  value <- valParser
  skipWhitespace
  return (attr,value)
  
header :: GenParser Char st String
header = do
  char ':'
  parseIdentifier
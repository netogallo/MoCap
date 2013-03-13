module Data.MotionCapture.AMC.Parser where

-- MoCap
import Data.MotionCapture.AMC.Types

-- Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import Text.Parsec.Perm

-- Haskell
import Data.Char (toUpper,toLower)
import Data.Maybe (fromMaybe)

skipWhitespace = skipMany $ skipMany1 space 
                 <|> skipMany1 newline
                 <|> (char '#' >> (skipMany $ noneOf ['\n']) >> newline >> return ())

skeletonParser :: GenParser Char st [Maybe AsfSection]
skeletonParser = many $ try (skipWhitespace >>  section)

section :: GenParser Char st (Maybe AsfSection)
section = do
  head <- header
  skipWhitespace
  case head of
    "root" -> rootSection >>= return.Just
    "bonedata" -> bonesSection >>= return.Just
    _ -> skipMany bodyLine >> return Nothing

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
  ((_,bId),(_,bName),(_,bDir),(_,bLen),(_,bAxis),mBDof,mBLims) <- permute $ (\x1 x2 x3 x4 x5 x6 x7->(x1,x2,x3,x4,x5,x6,x7)) <$$> descriptor (string "id") parseIntegral
                                                                        <||> descriptor (string "name") (many $ noneOf ['\n'])
                                                                        <||> descriptor (string "direction") (parseCoord3D)
                                                                        <||> descriptor (string "length") (parseFloat)
                                                                        <||> descriptor (string "axis") (parseCoordAxis)
                                                                        <|?> (Nothing,descriptor (string "dof") (parseDOF) >>= return.Just)
                                                                        <|?> (Nothing,descriptor (string "limits") (manyCoord2D) >>= return.Just)
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
    
rootSection :: GenParser Char st AsfSection
rootSection = do
  ((_,rOrder),(_,rAxis),(_,rPosition),(_,rOrientation)) <- permute $ (\x1 x2 x3 x4 -> (x1,x2,x3,x4)) <$$> descriptor (string "order") parseDOF
                                            <||> descriptor (string "axis") parseAxis
                                            <||> descriptor (string "position") parseCoord3D
                                            <||> descriptor (string "orientation") parseCoord3D
  
  return $ Root $ RootSection{rootOrder = rOrder,
                              rootAxis = rAxis,
                              rootPosition = rPosition,
                              rootOrientation = rOrientation}
    
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
    skipWhitespace
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
  rest <- many $ noneOf ['\n']
  skipMany $ char '\n'
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
  ident <- many $ noneOf ['\n']
  return ident
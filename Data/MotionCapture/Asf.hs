module Data.MotionCapture.Asf( 
  loadAsfFromFile,
  findAsfBone,
  
  )where

-- MoCap Modules
import Data.MotionCapture.AMC.SkeletonParser
import Data.MotionCapture.AMC.Types

-- Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos (newPos)

-- Haskell
import Data.List (find)

-- | Try to parse the given Asf file into it's representation
-- return the file on success or the resulting parse error
loadAsfFromFile :: FilePath -> IO (Either ParseError AsfFile)
loadAsfFromFile fp = do
  mSections <- parseFromFile skeletonParser fp
  case mSections of
    Left error -> return $ Left error
    Right sections -> return $ createAsfFromSections fp sections
    
-- | Generate an Asf file representation if the list of sections contains
-- all the necessary sections. Otherwise return an error informing about
-- the missing sections
createAsfFromSections :: String -> [(String,AsfSection)] -> Either ParseError AsfFile
createAsfFromSections srcName sections = do 
  fSections <- return $ do
    rSect <- lookup (rootName sectionsName) sections
    uSect <- lookup (unitsName sectionsName) sections
    bdSect <- lookup (bonedataName sectionsName) sections
    hSect <- lookup (hierarchyName sectionsName) sections
    return (rSect,uSect,bdSect,hSect)
    
  case fSections of
    Nothing -> fail $ "Missing Asf File Sections."
    Just (Root rSect,Units uSect,BoneData bdSect,Hierarchy hSect) -> return $ AsfFile{
      rootSection = rSect,
      boneDataSections = bdSect,
      unitsSection = uSect,
      hierarchySection = hSect}
    _ -> fail $ "Pattern match failure, the parser has a bug"
  
-- | Get the bone information for the bone with the given identifier
findAsfBone :: AsfFile -> String -> Maybe (BoneDataSection)
findAsfBone asf name = find (\x -> boneName x == name) $ boneDataSections asf
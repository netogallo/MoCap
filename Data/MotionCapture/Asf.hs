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
createAsfFromSections :: String -> [Maybe AsfSection] -> Either ParseError AsfFile
createAsfFromSections srcName sections = sectionsResult
  where
    mSections = foldl collectSections (Nothing,Nothing) sections
    collectSections (mRootSection,mBoneDataSection) asfSection = case asfSection of
      Just (Root rSect) -> (Just rSect,mBoneDataSection)
      Just (BoneData bDataSection) -> (mRootSection,Just bDataSection)
      _ -> (mRootSection,mBoneDataSection)
    sectionError msg = Left $ newErrorMessage (Message msg) (newPos srcName 0 0) 
    sectionsResult = case mSections of
      (Nothing,Nothing) -> sectionError "No root and bonedata section found."
      (Nothing,_) -> sectionError "No root section found."
      (_,Nothing) -> sectionError "No bonedata section found."
      (Just rSection,Just bDataSection) -> Right $ AsfFile{
        rootSection = rSection,
        boneDataSections = bDataSection}
  
-- | Get the bone information for the bone with the given identifier
findAsfBone :: AsfFile -> String -> Maybe (BoneDataSection)
findAsfBone asf name = find (\x -> boneName x == name) $ boneDataSections asf
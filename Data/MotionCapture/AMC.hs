module Data.MotionCapture.AMC(
  loadAMCFromFile,
  filterBones,
  filterSections,
  getAMCBone,
  exportAMC,
  setBone
  ) where

-- MoCap Modules
import Data.MotionCapture.AMC.Types
import Data.MotionCapture.AMC.AMCParser
import Data.MotionCapture.AMC.AMCExporter

-- Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos (newPos)
import Data.List (find)
import Data.Maybe (isJust)

-- | Parse the positions in the multiple frames present in an AMCFile
-- which corresponds to the skeleton encoded in the given AsfFile
loadAMCFromFile :: AsfFile -> FilePath -> IO (Either ParseError AMCFile)
loadAMCFromFile asf fp = parseFromFile (parseAMCFile asf) fp

filterSections :: [String] -> [AMCSection] -> [AMCSection]
filterSections wantedBones oldSections = map (sectionFilter) oldSections
  where
    nameInList name = isJust $ find (== name) wantedBones
    acceptBone accepted sBone
      | nameInList $ boneSpecifier sBone = sBone:accepted
      | otherwise = accepted
    sectionFilter sect = sect{boneList=foldl acceptBone [] $ boneList sect}


getAMCBone :: String -> AMCSection -> Maybe AMCBone
getAMCBone name amcSection = find (((==) name) . boneSpecifier) $ boneList amcSection

-- | Remove all bones except the ones in the list 
-- from all instances in the AMCFile.
filterBones :: [String] -> AMCFile -> AMCFile
filterBones wantedBones amc = amc{amcSections=filteredSections}
  where
    filteredSections = filterSections wantedBones $ amcSections amc
    
replaceIf _ _ [] = []
replaceIf matches obj (x:xs)
  | matches x = obj:(replaceIf matches obj xs)
  | otherwise = x:(replaceIf matches obj xs)
    
setBone :: AMCSection -> AMCBone -> AMCSection
setBone amcSect bone = amcSect{
  boneList = replaceIf nameEq bone $ boneList amcSect
  }
  where
    nameEq x = (boneSpecifier x) == (boneSpecifier bone)

module Data.MotionCapture.AMC(
  loadAMCFromFile
  ) where

-- MoCap Modules
import Data.MotionCapture.AMC.Types
import Data.MotionCapture.AMC.AMCParser

-- Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos (newPos)

-- | Parse the positions in the multiple frames present in an AMCFile
-- which corresponds to the skeleton encoded in the given AsfFile
loadAMCFromFile :: AsfFile -> FilePath -> IO (Either ParseError AMCFile)
loadAMCFromFile asf fp = parseFromFile (parseAMCFile asf) fp
module Data.MotionCapture.MoCap (
  -- Asf Files
  loadAsfFromFile,
  findAsfBone,
  -- Amc Files
  loadAMCFromFile,
  exportAMC,
  setBone
  ) where

import Data.MotionCapture.Asf
import Data.MotionCapture.AMC
module Data.MotionCapture.AMC.ASFExporter where
  
-- MoCap
import Data.MotionCapture.AMC.Types

-- Haskell
import System.IO
import Control.Exception as E
import Control.Monad (mapM_)
import Data.Char
import Data.List
import Text.Printf

exportASF :: AsfFile -> FilePath -> IO ()
exportASF asf outFile = do
  h <- openFile outFile WriteMode
  E.catch (writeASFFile asf h) ((\e -> return ()) :: IOError -> IO ())
  hClose h

writeASFFile :: AsfFile -> Handle -> IO ()
writeASFFile asf h = do
  writeRootSection      h asf
  writeUnitsSection     h asf
  writeBoneDataSections h asf
  writeHierarchySection h asf

writeRootSection :: Handle -> AsfFile -> IO ()
writeRootSection h asf = do
  hPutStrLn h ":root"
  hPutStr   h "order "
  hPutStrLn h $ concat . intersperse " " . map show $ rootOrder rootSec
  hPutStr   h "axis "
  hPutStrLn h $ showAxis $ rootAxis rootSec
  hPutStr   h "position "
  hPutStrLn h $ show3DCoord $ rootPosition rootSec
  hPutStr   h "orientation "
  hPutStrLn h $ show3DCoord $ rootOrientation rootSec
  where
    rootSec = rootSection asf

writeUnitsSection :: Handle -> AsfFile -> IO ()
writeUnitsSection h asf = do
  hPutStrLn h ":units"
  hPutStr   h "angle "
  hPutStrLn h $ angleText $ unitsAngle unitsSec
  hPutStr   h "mass "
  hPrintf   h "%f\n" $ unitsMass unitsSec
  hPutStr   h "length " 
  hPrintf   h "%f\n" $ unitsLength unitsSec
  where
    unitsSec    = unitsSection asf
    angleText x =
      case x of
        Degrees -> "deg"
        Radians -> "rad"

writeBoneDataSections :: Handle -> AsfFile -> IO ()
writeBoneDataSections h asf = do
  hPutStrLn h ":bonedata"
  mapM_ writeBoneDataSection $ boneDataSections asf
  where
    writeBoneDataSection x = do
      hPutStrLn h "begin"
      hPutStr   h "id "
      hPutStrLn h $ show $ boneId x
      hPutStr   h "name "
      hPutStrLn h $ boneName x
      hPutStr   h "direction "
      hPutStrLn h $ show3DCoord $ boneDirection x
      hPutStr   h "length "
      hPrintf   h "%f\n" $ boneLength x
      hPutStr   h "axis "
      hPutStrLn h $
        show3DCoord (fst $ boneAxis x) ++ " " ++ showAxis (snd $ boneAxis x)
      writeDof    $ boneDof x
      writeLimits $ boneLimits x
      hPutStrLn h "end"
      where
        writeDof (Just xs) = do
          hPutStr   h "dof "
          hPutStrLn h $ concat . intersperse " " . 
                          map ((map toLower) . show) $ xs
        writeDof Nothing = return ()
        writeLimits (Just xs) = do
          hPutStr   h "limits "
          hPutStrLn h $ concat . intersperse " " . 
                          map ((\x -> "(" ++ x ++ ")") . show2DCoord) $ xs
        writeLimits Nothing = return ()

writeHierarchySection :: Handle -> AsfFile -> IO ()
writeHierarchySection h asf = do
  hPutStrLn h "begin"
  mapM_ writeHierarchyNode hierarchySec
  hPutStrLn h "end"
  where
    hierarchySec = hierarchySection asf
    writeHierarchyNode x = do
      hPutStr   h $ rootBoneName x ++ " "
      hPutStrLn h $ concat . intersperse " " $ childBoneNames x

showAxis :: (Axis,Axis,Axis) -> String
showAxis (x,y,z) = map axisLetter [x,y,z]
  where
    axisLetter x =
      case x of
        AX -> 'X'
        AY -> 'Y'
        AZ -> 'Z'
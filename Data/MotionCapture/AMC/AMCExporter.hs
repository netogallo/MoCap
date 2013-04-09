module Data.MotionCapture.AMC.AMCExporter where
  
-- MoCap
import Data.MotionCapture.AMC.Types

-- Haskell
import System.IO
import Control.Exception as E
import Control.Monad (mapM_)

exportAMC amcData asf outFile = do
  h <- openFile outFile WriteMode
  E.catch (writeAMCFile amcData asf h) ((\e -> return ()) :: IOError -> IO ())
  hClose h
  
writeAMCFile amcData asf h = do
  maybeWrite $ specData amcData
  maybeWrite $ angleUnits amcData
  mapM_ (writeAMCSection h asf) $ amcSections amcData  
  
  where
    maybeWrite (Just a) = hPutStrLn h $ show a
    maybeWrite Nothing = return ()
    
orderCoords [] (x,y,z) (rx,ry,rz) = ""
orderCoords (dof:xs) (x,y,z) (rx,ry,rz) = case dof of
  TX -> contOrder x
  TY -> contOrder y
  TZ -> contOrder z
  RX -> contOrder rx
  RY -> contOrder ry
  RZ -> contOrder rz
  where
    contOrder v = (show x) ++ " " ++ (orderCoords xs (x,y,z) (rx,ry,rz))
    
writeBone h asf bone = do
  hPutStrLn h boneText
  where
    boneCoords = foldr (\v str -> (show v) ++ " " ++ str) [] $ boneRotations bone
    boneText = (boneSpecifier bone) ++ " " ++ boneCoords
    
writeAMCSection h asf amcSection = do
  hPutStrLn h $ show $ sectionNumber amcSection
  hPutStrLn h $ rootString
  mapM_ (writeBone h asf) $ boneList amcSection
    
  where
    coordOrder = rootOrder $ rootSection asf
    rootString = rootSpecifier ++ " " 
                 ++ orderCoords coordOrder (rootBonePosition amcSection) (rootBoneOrientation amcSection)

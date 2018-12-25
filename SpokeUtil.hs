module SpokeUtil where
import Data.Complex
import Control.Monad
import MySpinBox
import Graphics.UI.Gtk
rad d = ((2*pi)/360)*d
dig r = r / (yen/360)
yen = rad 360 :: Double
hosei = 2.4 / 2 :: Double
dist :: RealFloat a => Complex a -> Complex a -> a -> a
dist x y z = sqrt (l * l + z * z) where l = magnitude (x - y)
calLen a b d k h = dist x y d - hosei         
    where   x = mkPolar b (yen / (h / 2) * (k - 1) / 2)
            y = mkPolar a (yen / h * (-1))

--calLen a b d k h = sqrt ((a^2+b^2+d^2) - (2*a*b*(cos $ alfa h k))) - hosei
--    where alfa h k = rad (360 / h * k)
mkList :: Int -> IO ([HBox],[Button]) 
mkList i = do
    bs <- mapM (buttonNewWithLabel . show)  [0.. i-1]
    hs <- mapM (\_ -> hBoxNew False 0) [0.. i-1]
    zipWithM (\x y -> containerAdd x y) hs bs
    return (hs,bs)

mkShowButton h n v = do
    adj <- mkAdjustment (v,v,v,0,0)
    myAddSpinButton h n adj
--Rim
data Rim = Rim{erd :: Double ,rName :: String} deriving (Show,Read)
getRim :: IO Rim
getRim = do
    r <- readFile rimd
    return $ read r
getRims :: IO [Rim]
getRims = do
    r <- readFile rimsd
    return [read x|x <- lines r]
writeRim :: Double -> String -> IO()
writeRim d s = writeFile rimd $ show (Rim d s) ++ "\n"
appendRim :: Double -> String -> IO()
appendRim d s = appendFile rimsd $ show (Rim d s) ++ "\n"
getErd :: Rim -> IO Double
getErd = return . erd
getRimName :: Rim -> IO String
getRimName = return . rName
showRimAsButton :: Rim -> HBox -> IO ()
showRimAsButton r h = do
    v <- getErd r
    n <- getRimName r
    mkShowButton h "erd(mm)" v
    l <- labelNew $ Just n
    miscSetAlignment l 0.0 0.5
    l `set` [labelWidthChars := 50]
    containerAdd h l
--Hub   
data Hub = 
    Hub{lpcd::Double,lfcd::Double,rpcd::Double,rfcd::Double,hName::String} 
        deriving (Show,Read)
getHub :: IO Hub
getHub = do
    r <- readFile hubd
    return $ read r
getHubs :: IO [Hub]
getHubs = do
    r <- readFile hubsd
    return [read x|x <- lines r]
writeHub :: [Double] -> String -> IO ()
writeHub (a:b:c:d:_) s = writeFile hubd $ show (Hub a b c d s) ++ "\n"
appendHub :: [Double] -> String -> IO ()
appendHub (a:b:c:d:_) s = appendFile hubsd $ show (Hub a b c d s) ++ "\n" 
getLpcd, getRpcd, getLfcd, getRfcd :: Hub -> IO Double
getLpcd = return . lpcd
getRpcd = return . rpcd
getLfcd = return . lfcd
getRfcd = return . rfcd
getHubName :: Hub -> IO String
getHubName = return . hName
dispHub :: [Double] -> IO ()
dispHub ds = do
    let a:b:c:d:_ = calH ds
    print $ Hub a b c d ""
calH :: [Double] -> [Double]
calH (a:b:c:d:e:_) = let h = a / 2 in [b,(h - c), d, (h - e)]
showHubsAsButton :: [Hub] -> HBox -> IO ()
showHubsAsButton hubs box = do
    mapM_ (\x -> showHubAsButton x box) hubs
showHubAsButton :: Hub -> HBox -> IO ()
showHubAsButton hub box = do
    let Hub a b c d e = hub
    mkShowButton box "left pcd(mm)" a
    mkShowButton box "left f2c(mm)" b
    mkShowButton box "right pcd(mm)" c
    mkShowButton box "right lf2c(mm)" d
    l <- labelNew $ Just e
    miscSetAlignment l 0.0 0.5
    l `set` [labelWidthChars := 50]
    containerAdd box l
data Wheel = Wheel {rim :: Rim, hub :: Hub, holes :: Int}deriving Show
getHoles :: Wheel -> IO Double
getHoles = return . fromIntegral . holes
getLenList w = do
    let r = rim w
    let h = hub w
    e <- getErd r
    lp <- getLpcd h
    lf2c <- getLfcd h
    rp <- getRpcd h
    rf2c <- getRfcd h
    hs <- getHoles w
    let l0 = map (\x -> calLen (e / 2) (lp / 2) lf2c x hs) [0,2..8]
    let l1 = map (\x -> calLen (e / 2) (rp / 2) rf2c x hs) [0,2..8]
    return (l0,l1)
    
--files
rimsd,hubsd,rimd,hubd::FilePath 
rimsd = "rims.d"
rimd = "rim.d"
hubsd = "hubs.d"
hubd = "hub.d"

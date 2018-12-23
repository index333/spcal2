import Graphics.UI.Gtk
import System.IO
import SpokeUtil
import MySpinBox
import Round
import Control.Monad
disp (a,b) = do
    print "left side"
    disp' a
    print "right side"
    disp' b
    print () 
disp' l = zipWithM (\x y -> do  putStr (show y)
                                putStr "-cross "
                                putStr (show (round1 x)) 
                                putStrLn "mm") l [0..4]
main = do
    r <- readFile rimd
    let rim = read r
    erd <- getErd rim
    print erd 
    r <- readFile hubd
    let hub = read r
    lp <- getLpcd hub
    lf2c <- getLfcd hub
    rp <- getRpcd hub
    rf2c <- getRfcd hub
    let [a,b,c,d,e] = [erd,lp,lf2c,rp,rf2c]
    print lp
    print lf2c
    print rp
    print rf2c
    initGUI
    window  <- windowNew
    hbox <- hBoxNew False 0
    vbox <- vBoxNew False 0
    boxPackStart vbox hbox PackNatural 0
    let names = ["スポーク穴数",
                    "erd(mm)",
                    "pcd(mm)",
                    "flange2center(mm)",
                    "pcd(mm)",
                    "flange2center(mm)"]
    adjs <- mkAdjustments [(32, 20, 40 ,4,4),
                            (a,200, 700, 1,10),
                            (b,30, 100 ,1,1),
                            (c,10, 50, 0.1,1),
                            (d,30, 100 ,1,1),
                            (e,10, 50, 0.1,1)]
    spins@s0:s1:s2:s3:s4:s5 <- myAddSpinButtons hbox names adjs
    mapM_ ( `set` [spinButtonDigits := 0]) [s0,s1,s2,s4]

    update adjs
    mapM (\x-> onValueChanged x (update adjs)) adjs
    containerAdd window vbox
    widgetShowAll window
    window `on` unrealize $ end adjs
    mainGUI
end adjs = do
    l <- mapM (`get` adjustmentValue) adjs
    mapM_ (\x -> hPutStrLn stderr $ show x) $ tail l
    mainQuit
update adjs = do
    h:a:b:c:d:e:_ <- mapM (`get` adjustmentValue) adjs
    let rim = Rim a ""
    let hub = Hub b c d e ""
    let w = Wheel rim hub (round h)
    getLenList w >>= disp

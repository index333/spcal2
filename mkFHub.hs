import Graphics.UI.Gtk
import SpokeUtil
import System.Process
import MySpinBox
main = do
    initGUI
    window  <- windowNew
    hbox <- hBoxNew False 0
    vbox <- vBoxNew False 0
    boxPackStart vbox hbox PackNatural 0
    let names = ["pcd(mm)", "エンドフランジ距離"]
    adjs <- mkAdjustments [(40,30, 100, 1,10), (30,30,50,1,10)]
    spins <- myAddSpinButtons hbox names adjs
    mapM_ (`set` [spinButtonDigits := 0]) spins
    l <- labelNew $ Just "hub name"
    e <- entryNew
    e `on` entryActivate $ end adjs e
    containerAdd vbox l
    containerAdd vbox e

    containerAdd window vbox
    widgetShowAll window
    window `on` unrealize $ mainQuit
    mainGUI
end adjs e = do
    v0:v1:[] <- mapM (`get` adjustmentValue) adjs
    let h = 100 / 2
    e' <- e `get` entryText
    let hub = Hub v0 v1 v0 v1 e'
    appendFile hubsd $ show hub ++ "\n"
    writeFile hubd $ show hub ++ "\n"
    --createProcess (proc "runghc" ["selector.hs"])
    mainQuit
{-
end2 = do 
    createProcess (proc "runghc" ["selector.hs"])
    mainQuit
-}

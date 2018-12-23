import Graphics.UI.Gtk
import SpokeUtil
import MySpinBox
main = do
    initGUI
    window  <- windowNew
    hbox <- hBoxNew False 0
    vbox <- vBoxNew False 0
    boxPackStart vbox hbox PackNatural 0
    let names = ["エンド幅(mm)",
                    "pcd(mm)",
                    "エンドフランジ距離(mm)",
                    "pcd(mm)",
                    "エンドフランジ距離"]
    adjs <- mkAdjustments [(100,100,135,1,10), 
                            (40,30, 100, 1,10), 
                            (30,30,50,1,10),
                            (30,30, 100, 1,10), 
                            (30,30,50,1,10)]
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
    v0:v1:v2:v3:v4:[] <- mapM (`get` adjustmentValue) adjs
    let h = v0 / 2
    e' <- e `get` entryText
    let hub = Hub v1 (h - v2) v3 (h - v4) e'
    appendFile hubsd $ show hub ++ "\n"
    writeFile hubd $ show hub ++ "\n"
    mainQuit

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
    update adjs
    mapM (\x-> onValueChanged x (update adjs)) adjs
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
update adjs = do
    v0:v1:v2:v3:v4:[] <- mapM (`get` adjustmentValue) adjs
    dispHub [v0,v1,v2,v3,v4]
end adjs e = do
    v0:v1:v2:v3:v4:[] <- mapM (`get` adjustmentValue) adjs
    e' <- e `get` entryText
    let ds = calH (v0:v1:v2:v3:v4:[])
    writeHub ds e'
    appendHub ds e'
    mainQuit

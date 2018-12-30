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
                    "左エンドフランジ距離(mm)",
                    "pcd(mm)",
                    "右エンドフランジ距離(mm)"]
    adjs <- mkAdjustments [(100,100,135,1,10), 
                            (40,30, 100, 1,10), 
                            (30,30, 100, 1,10), 
                            (40,30, 100, 1,10), 
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
    vs <- mapM (`get` adjustmentValue) adjs
    (adjs !! 3) `set` [adjustmentValue := (vs !! 1)]
    vs <- mapM (`get` adjustmentValue) adjs
    dispHub vs --[v0,v1,v2,v1,v3]
end adjs e = do
    vs <- mapM (`get` adjustmentValue) adjs
    e' <- e `get` entryText
    let  ds = calH vs 
    writeHub ds e'
    appendHub ds e'
    mainQuit

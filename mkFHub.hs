import Graphics.UI.Gtk
import SpokeUtil
import MkFrame
import MySpinBox
main = do
    initGUI
    (window, hbox ,vbox) <- mkFrame
    let names = ["エンド幅(100mm固定)",
                    "pcd(mm)",
                    "左エンドフランジ距離(mm)",
                    "pcd(mm)",
                    "右エンドフランジ距離(mm)"]
    adjs <- mkAdjustments [(100,100,100,1,10), 
                            (40,30, 100, 1,10), 
                            (30,30, 100, 1,10), 
                            (40,30, 100, 1,10), 
                            (30,30,50,1,10)]

    update adjs
    mapM (\x-> onValueChanged x (update adjs)) adjs

    spins <- myAddSpinButtons hbox names adjs
    mapM_ (`set` [spinButtonDigits := 0]) spins
    e <- setEntry vbox
    e `on` entryActivate $ end adjs e
    widgetShowAll window
    mainGUI
update adjs = do
    vs <- mapM (`get` adjustmentValue) adjs
    (adjs !! 3) `set` [adjustmentValue := (vs !! 1)]
    (adjs !! 4) `set` [adjustmentValue := (vs !! 2)]
    vs <- mapM (`get` adjustmentValue) adjs
    dispHub vs

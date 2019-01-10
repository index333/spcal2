import Graphics.UI.Gtk
import SpokeUtil
import MySpinBox
import MkFrame
main = do
    initGUI
    (window, hbox ,vbox) <- mkFrame
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
    e <- setEntry vbox
    e `on` entryActivate $ end adjs e
    widgetShowAll window
    mainGUI
update adjs = do
    v0:v1:v2:v3:v4:[] <- mapM (`get` adjustmentValue) adjs
    dispHub [v0,v1,v2,v3,v4]

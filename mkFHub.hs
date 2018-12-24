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
    adjs <- mkAdjustments [(40,30, 100, 1,10), (30,20,50,1,10)]
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
    v0:v1:[] <- mapM (`get` adjustmentValue) adjs
    dispHub [100, v0, v1,v0,v1]
end adjs e = do
    v0:v1:[] <- mapM (`get` adjustmentValue) adjs
    e' <- e `get` entryText
    let a:b:c:d:_ = calH [100, v0, v1, v0, v1]
    let hub = Hub a b c d e'
    setHub hub
    appendHub hub
    mainQuit

import Graphics.UI.Gtk
import System.Process
import SpokeUtil
import MySpinBox
main = do
    initGUI
    window  <- windowNew
    hbox <- hBoxNew False 0
    vbox <- vBoxNew False 0
    boxPackStart vbox hbox PackNatural 0
    let names = ["リム外径(mm)", "デプス計の値(mm)"]
    adjs <- mkAdjustments [(600,200, 700, 1,10), (10,0,30,1,10)]
    s0:s1:_ <- myAddSpinButtons hbox names adjs
    update adjs
    mapM (\x-> onValueChanged x (update adjs)) adjs
    s0 `set` [spinButtonDigits := 0] 
    s1 `set` [spinButtonDigits := 0] 
    l <- labelNew $ Just "rim name"
    e <- entryNew
    (e `on` entryActivate) $ end adjs e
    containerAdd vbox l
    containerAdd vbox e

    containerAdd window vbox
    widgetShowAll window
    window `on` unrealize $ end2
    mainGUI
update adjs = do
    v0:v1:_ <- mapM (`get` adjustmentValue) adjs
    let r = v0 - v1 * 2
    print $ Rim r ""
end adjs e = do
    v0:v1:_ <- mapM (`get` adjustmentValue) adjs
    let r = v0 - v1 * 2
    s <- e `get` entryText
    let rim = Rim r s
    appendRim rim
    setRim rim
    mainQuit
end2 = do 
    mainQuit


import Graphics.UI.Gtk
import System.Process
import SpokeUtil
import MySpinBox
import MkFrame
main = do
    initGUI
    (window,hbox,vbox) <-mkFrame
    let names = ["リム外径(mm)", "デプス計の値(mm)"]
    adjs <- mkAdjustments [(600,200, 700, 1,10), (10,0,30,1,10)]
    ss <- myAddSpinButtons hbox names adjs
    update adjs
    mapM_ (\x-> onValueChanged x (update adjs)) adjs
    mapM_ (`set` [spinButtonDigits := 0]) ss
    e <- setEntry vbox
    (e `on` entryActivate) $ endf adjs e
    widgetShowAll window
    mainGUI
calcRim rimD depth = return $ (err - a) * 2 
    where   err = rimD / 2
            t = 0.5
            nh = 1
            a = depth - t + nh
update adjs = do
    v0:v1:_ <- mapM (`get` adjustmentValue) adjs
    r <- calcRim v0 v1 
    print $ Rim r ""
endf adjs e = do
    v0:v1:_ <- mapM (`get` adjustmentValue) adjs
    r <- calcRim v0 v1 
    s <- e `get` entryText
    writeRim r s
    appendRim r s
    mainQuit


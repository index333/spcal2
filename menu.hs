import Graphics.UI.Gtk
import System.Process
main = do
    initGUI
    window  <- windowNew
    vbox <- vBoxNew False 0
    b0 <- buttonNewWithLabel "リム測定"
    b1 <- buttonNewWithLabel "ハブ測定"
    b2 <- buttonNewWithLabel "リム選択"
    b3 <- buttonNewWithLabel "ハブ選択"
    b4 <- buttonNewWithLabel "計算"
    b5 <- buttonNewWithLabel "終了"
    mapM_ (containerAdd vbox) [b0,b1,b2,b3,b4,b5] 
    b0 `on` buttonActivated $ f0
    b1 `on` buttonActivated $ f1
    b2 `on` buttonActivated $ f2
    b3 `on` buttonActivated $ f3
    b4 `on` buttonActivated $ f4
    b5 `on` buttonActivated $ f5
    containerAdd window vbox
    widgetShowAll window
    window `on` unrealize $ mainQuit
    mainGUI
f0 = createProcess (proc "runghc" ["mkRim.hs"]) >> return ()
f1 = createProcess (proc "runghc" ["selector.hs"]) >> return ()
f2 = createProcess (proc "runghc" ["selectRim.hs"]) >> return ()
f3 = createProcess (proc "runghc" ["selectHub.hs"]) >> return ()
f4 = createProcess (proc "runghc" ["spcal.hs"]) >> return ()
f5 = mainQuit

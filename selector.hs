import System.IO
import System.Process
import Graphics.UI.Gtk
main = do
    initGUI
    window  <- windowNew
    hbox <- hBoxNew False 0
    vbox <- vBoxNew False 0
    boxPackStart vbox hbox PackNatural 0
    let l = ["front hub","rear hub","rear 異径hub"] 
    bs@(b0:b1:b2:[]) <- mapM buttonNewWithLabel l
    b0 `on` buttonActivated $ e0
    b1 `on` buttonActivated $ e1
    b2 `on` buttonActivated $ e2
    mapM_ (containerAdd hbox) bs
    containerAdd window vbox
    widgetShowAll window
    window `on` unrealize $ mainQuit
    mainGUI
e0 = do
    createProcess (proc "runghc" ["mkFHub.hs"])
    mainQuit
e1 = do
    createProcess (proc "runghc" ["mkHub0.hs"])
    mainQuit
e2 = do
    createProcess (proc "runghc" ["mkHub.hs"])
    mainQuit

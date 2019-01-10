module MkFrame where
import Graphics.UI.Gtk
import SpokeUtil
mkFrame = do
    window  <- windowNew
    hbox <- hBoxNew False 0
    vbox <- vBoxNew False 0
    boxPackStart vbox hbox PackNatural 0
    containerAdd window vbox
    window `on` unrealize $ mainQuit
    return (window,hbox,vbox)   
setEntry vbox = do
    l <- labelNew $ Just "name"
    e <- entryNew
    containerAdd vbox l
    containerAdd vbox e
    return e
end adjs e = do
    vs <- mapM (`get` adjustmentValue) adjs
    e' <- e `get` entryText
    let ds = calH vs
    writeHub ds e'
    appendHub ds e'
    mainQuit

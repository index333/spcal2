import SpokeUtil
import Control.Monad
import Graphics.UI.Gtk
main = do
    rims <- getRims
    initGUI
    window  <- windowNew
    window `set` [windowDefaultWidth := 500,
                    windowDefaultHeight := 500,
                    windowTitle := "To select an item, click number."]
    sw <- scrolledWindowNew Nothing Nothing
    vbox <- vBoxNew False 0
    (hs,bs) <- mkList (length rims)
    zipWithM showRimAsButton rims hs
    mapM_ (containerAdd vbox) hs
    mapM_ (\x -> (x `on` buttonActivated) (buttonOn x rims)) bs
    containerAdd window sw
    scrolledWindowAddWithViewport sw vbox
    widgetShowAll window
    window `on` unrealize $ mainQuit
    mainGUI
buttonOn b rims = do
    bl <- b `get` buttonLabel
    let i = read bl::Int
    setRim $ rims !! i
    mainQuit


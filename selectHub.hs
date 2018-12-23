import SpokeUtil
import Control.Monad
import Graphics.UI.Gtk
main = do
    hubs <- getHubs
    initGUI
    window  <- windowNew
    window `set` [windowDefaultWidth := 500,
                    windowDefaultHeight := 500,
                    windowTitle := "To select an item, click number."]
    sw <- scrolledWindowNew Nothing Nothing
    vbox <- vBoxNew False 0
    (hs,bs) <- mkList (length hubs)
    zipWithM showHubAsButton hubs hs
    mapM_ (containerAdd vbox) hs
    mapM_ (\x -> (x `on` buttonActivated) (buttonOn x hubs)) bs
    containerAdd window sw
    scrolledWindowAddWithViewport sw vbox
    widgetShowAll window
    window `on` unrealize $ mainQuit
    mainGUI

buttonOn b hubs = do
    bl <- b `get` buttonLabel
    let i = read bl::Int
    setHub $ hubs !! i
    mainQuit

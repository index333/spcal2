module SelectFrame where
import Graphics.UI.Gtk
import SpokeUtil
mkSelectFrame :: Int -> IO (Window,([HBox],[Button])) 
mkSelectFrame i = do
    (hs,bs) <- mkList i
    window  <- windowNew
    window `set` [windowDefaultWidth := 500,
                    windowDefaultHeight := 500,
                    windowTitle := "To select an item, click number."]
    window `on` unrealize $ mainQuit
    sw <- scrolledWindowNew Nothing Nothing
    containerAdd window sw
    vbox <- vBoxNew False 0
    scrolledWindowAddWithViewport sw vbox
    mapM_ (containerAdd vbox) hs
    return (window ,(hs,bs))

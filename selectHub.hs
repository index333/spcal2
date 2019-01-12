import SpokeUtil
import Control.Monad
import Graphics.UI.Gtk
import SelectFrame
main = do
    hubs <- getHubs
    initGUI
    (window ,(hs,bs)) <- mkSelectFrame (length hubs)
    zipWithM showHubAsButton hubs hs
    mapM_ (\x -> (x `on` buttonActivated) (buttonOn x hubs)) bs
    widgetShowAll window
    mainGUI

buttonOn b hubs = do
    bl <- b `get` buttonLabel
    let i = read bl::Int
    setHub $ hubs !! i
    mainQuit

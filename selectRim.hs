import SpokeUtil
import Control.Monad
import Graphics.UI.Gtk
import SelectFrame
main = do
    rims <- getRims
    initGUI
    (window ,(hs,bs)) <- mkSelectFrame (length rims)
    zipWithM showRimAsButton rims hs
    mapM_ (\x -> (x `on` buttonActivated) (buttonOn x rims)) bs
    widgetShowAll window
    mainGUI

buttonOn b rims = do
    bl <- b `get` buttonLabel
    let i = read bl::Int
    setRim $ rims !! i
    mainQuit


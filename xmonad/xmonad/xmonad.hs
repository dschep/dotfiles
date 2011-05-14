import Control.OldException(catchDyn,try)
import DBus
import DBus.Connection
import DBus.Message
import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog

main = withConnection Session $ \ dbus -> do
  getWellKnownName dbus
  xmonad $ gnomeConfig {
    focusedBorderColor = "#4b6e99"
--    focusedBorderColor = "#F07746"
  , borderWidth        = 2
  , logHook            = dynamicLogWithPP (myPrettyPrinter dbus)
  }

-- -----------------------------------------------------------------------------

myPrettyPrinter :: Connection -> PP
myPrettyPrinter dbus = defaultPP {
    ppOutput  = outputThroughDBus dbus
  , ppTitle   = pangoColor "#4b6e99" . shorten 50 . pangoSanitize
--  , ppTitle   = pangoColor "#F07746" . shorten 50 . pangoSanitize
  , ppCurrent = pangoColor "#4b6e99" . wrap "[" "]" . pangoSanitize
--  , ppCurrent = pangoColor "#a344a8" . wrap "[" "]" . pangoSanitize
  , ppVisible = pangoColor "#b667b9" . wrap "(" ")" . pangoSanitize
  , ppHidden  = wrap " " " "
  , ppUrgent  = pangoColor "red"
  }

-- -----------------------------------------------------------------------------

getWellKnownName :: Connection -> IO ()
getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) ->
                                                getWellKnownName dbus)
 where
  tryGetName = do
    namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
    addArgs namereq [String "org.xmonad.Log", Word32 5]
    sendWithReplyAndBlock dbus namereq 0
    return ()

outputThroughDBus :: Connection -> String -> IO ()
outputThroughDBus dbus str = do
  let str' = "<span font=\"Terminus 9 Bold\">" ++ str ++ "</span>"
  msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
  addArgs msg [String str']
  send dbus msg 0 `catchDyn` (\ (DBus.Error _ _ ) -> return 0)
  return ()

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left  = "<span foreground=\"" ++ fg ++ "\">"
  right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
 where
  sanitize '>'  acc = "&gt;" ++ acc
  sanitize '<'  acc = "&lt;" ++ acc
  sanitize '\"' acc = "&quot;" ++ acc
  sanitize '&'  acc = "&amp;" ++ acc
  sanitize x    acc = x:acc

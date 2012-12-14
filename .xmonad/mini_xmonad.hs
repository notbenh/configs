import XMonad

main = xmonad $ defaultConfig
  { borderWidth        = 1
  , terminal           = "urxvt"
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00" 
  }

-- vim: ts=2 sts=2 sw=2 et:

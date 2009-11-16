{- xmonad.hs
 - Author: ben.hengst (based on work from Øyvind 'Mr.Elendig' Heggstad )
 - Version: 0.0.1
 -}

-------------------------------------------------------------------------------
-- Imports --
-- stuff
{-
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import IO (Handle, hPutStrLn) 

-- utils
import XMonad.Util.Run

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

-}


import Data.Ratio
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Arrow hiding ((|||))
import System.Exit
import System.IO
import XMonad
import qualified XMonad.StackSet as W
import Data.IORef
import Graphics.X11.Xlib
import IO (Handle, hPutStrLn) 

----- xmonad-contrib -----
import qualified XMonad.Actions.FlexibleResize as FlexibleResize
import XMonad.Actions.CycleWS
import XMonad.Layout.DragPane (dragPane, DragType(..))
import XMonad.Layout.NoBorders
-- (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.LayoutHints (layoutHints)
 
-- xprop -f _NET_WM_STRUT 32c -set _NET_WM_STRUT '0, 0, 16, 0'
--import qualified XMonad.Hooks.ManageDocks as ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Layout.ResizableTile

-------------------------------------------------------------------------------
-- Main --
{-
		main = do
       h <- spawnPipe "xmobar"
       xmonad $ defaultConfig
-}
main = do
    --xmobar <- spawnPipe "xmobar"
    -- Dzen config
    h <- spawnPipe "dzen2 -ta l  "
    -- the next N new windows will be floated
    floatNextWindows <- newIORef 0
    xmonad $ withUrgencyHook dzenUrgencyHook $ defaultConfig
              { workspaces = workspaces'
              , modMask = modMask'
              , borderWidth = borderWidth'
              , normalBorderColor = normalBorderColor'
              , focusedBorderColor = focusedBorderColor'
              , terminal = terminal'
              , keys = keyB floatNextWindows
             -- , logHook = logHook' h 
              , layoutHook = layoutHook'
              , manageHook = manageH floatNextWindows
              , logHook = dynamicLogWithPP $ dzenPP{ ppOutput = hPutStrLn h }
              --, logHook = dynamicLogDzen $ dzenPP{ ppOutput = hPutStrLn h }
              }



-------------------------------------------------------------------------------


manageH :: IORef Integer -> ManageHook
manageH floatNextWindows = composeAll $ concat
                [[ className =? name        --> doFloat                 | name <- floats ]
                ,[ className =? name        --> doF (W.shift workspace) | (name, workspace) <- shifts ]
                ,[ resource  =? res         --> doIgnore                | res <- ignores ]
                ,[ (> 0) `liftM` io (readIORef floatNextWindows)
                                            --> do io (modifyIORef floatNextWindows pred) >> doCenterFloat ]
                ]
    where
        floats  = ["MPlayer", "Gimp", "Blender", "Xmessage", "Cinelerra", "foobillard", "xaos", "Ddd", "xine","gmplayer"]
        shifts  = ("Minefield",   "2-web")
                : ("Pidgin",        "5-ims")
                : zip ["Songbird", "Gmpc", "Sonata"] (repeat "music")
        ignores = ["desktop_window", "kdesktop"]



-- Hooks --
{-
manageHook' :: ManageHook
manageHook' = composeAll
                    [ className =? "MPlayer"        --> doFloat
                    , className =? "Gimp"           --> doFloat
		    , resource	=? "firefox-bin"    --> doF (W.shift "2-web")
                    , resource  =? "desktop_window" --> doIgnore
                    , resource  =? "kdesktop"       --> doIgnore ]

newManageHook' = manageHook defaultConfig <+> manageDocks
-}
{-
logHook' :: Handle ->  X ()
logHook' h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }
-}
layoutHook' = customLayout
{-
-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP :: PP
customPP = defaultPP { ppCurrent = xmobarColor "#D7AF87" ""
                     , ppTitle =  shorten 80
                     , ppSep = " | "
                     , ppHiddenNoWindows = xmobarColor "#C4C4C4" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                     }
-}
-- borders
borderWidth' :: Dimension
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#1C1C1C"
focusedBorderColor' = "#C4C4C4"

-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["1-web", "2-code", "3-code", "4-db", "5-comm", "6", "7", "8", "9"]

-- layouts
{-
	[
customLayout = avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled)  ||| noBorders Full
  where
    tiled = ResizableTall 1 (2/100) (1/2) []
-}

customLayout = avoidStruts . smartBorders
        $ onWorkspace "1-web"   (Mirror tiled2
                                ||| tiled2
                                ||| Full
                                )
        $ onWorkspace "5-comm"  (tiled3
                                ||| Mirror tiled3
                                ||| Full
                                )
        $ onWorkspace "2-code"  (Mirror tiled3
                                ||| tiled3
                                ||| Full
                                )
        $ Mirror tiled
            ||| tiled
						||| Full
--          ||| dragPane Horizontal 0.1 0.5
  where
     tiled  = Tall 1 (1 % 100) (1 % 2)
     tiled2 = Tall 1 (1 % 100) (3 % 5)
     tiled3 = Tall 1 (1 % 100) (4 % 5)
-- rules

-- myManageHook = composeAll . concat $
--                [[ className =? c --> doFloat | c <- floats]
--               , [resource  =? r --> doIgnore | r <- ignores]
--               , [className =? "opera"          --> doF (W.shift "2-web" )]
--               , [className =? "Firefox-bin"    --> doF (W.shift "2-web" )]
--               , [className =? "rxvt-unicode"          --> doF (W.shift "3-trm" )]
--               , [className =? "emesene"           --> doF (W.shift "5-ims")]
--               , [className =? "Pidgin"           --> doF (W.shift "5-ims")]
--               ]
--    where floats = ["MPlayer", "Pidgin", "Savebox", 
--                    "display", "Xmessage", "emesene"]
--          ignores = ["desktop_window", "kdesktop"]

-------------------------------------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "gnome-terminal --hide-menubar"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' :: KeyMask
--modMask' = mod4Mask
modMask' = mod1Mask

-- keys
{-
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
-}


keyB :: IORef Integer -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
keyB floatNextWindows conf@(XConfig {modMask = modMask}) = Map.fromList $

-- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) 
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u' -nb '#1c1c1c' -nf '#C4C4C4' -sf '#D7AF87'` && eval \"exec $exe\"") 
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask .|. shiftMask, xK_m     ), spawn "claws-mail")
		, ((modMask,            xK_Print    ), spawn "scrot -q10 'shot-%Y%m%d-%H.%M.%S.png'")

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- mpd controls
    , ((modMask .|. controlMask,  xK_h     ), spawn "mpc prev")
    , ((modMask .|. controlMask,  xK_t     ), spawn "mpc pause")
    , ((modMask .|. controlMask,  xK_n     ), spawn "mpc play")
    , ((modMask .|. controlMask,  xK_s     ), spawn "mpc next")
    , ((modMask .|. controlMask,  xK_g     ), spawn "mpc seek -2%")
    , ((modMask .|. controlMask,  xK_c     ), spawn "mpc volume -4")
    , ((modMask .|. controlMask,  xK_r     ), spawn "mpc volume +4")
    , ((modMask .|. controlMask,  xK_l     ), spawn "mpc seek +2%")

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    

-- multimedia keys
--
-- XF86AudioLowerVolume
		, ((0            , 0x1008ff11), spawn "aumix -v -2")
-- XF86AudioRaiseVolume
		, ((0            , 0x1008ff13), spawn "aumix -v +2")
-- XF86AudioMute
		, ((0            , 0x1008ff12), spawn "amixer -q set PCM toggle")
-- XF86AudioNext
		, ((0            , 0x1008ff17), spawn "mocp -f")
-- XF86AudioPrev
		, ((0            , 0x1008ff16), spawn "mocp -r")
-- XF86AudioPlay
		, ((0            , 0x1008ff14), spawn "mocp -G")

	 --                                           

		]
  
{-
		++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-}

				    ++ prevNextWorkspaceBindings modMask xK_Left xK_Right
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    -- mod-shift-control-[1..9] %! Move client and switch to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0)
                    ,(W.shift, shiftMask)
                    ,(\i -> W.greedyView i . W.shift i, shiftMask .|. controlMask)
                    ]]



prevNextWorkspaceBindings :: KeyMask -> a -> a -> [((KeyMask, a), X ())]
prevNextWorkspaceBindings modMask prev next =
    [ ((modMask,               next),   nextWS)
    , ((modMask,               prev),   prevWS)
    , ((modMask .|. shiftMask, next),   shiftToNext)
    , ((modMask .|. shiftMask, prev),   shiftToPrev)
    , ((modMask .|. shiftMask .|. controlMask
                             , next),   shiftToNext >> nextWS)
    , ((modMask .|. shiftMask .|. controlMask
                             , prev),   shiftToPrev >> prevWS)

    ]


-------------------------------------------------------------------------------

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
--import XMonad.Layout.Accordion
import XMonad.Layout.Dishes
import XMonad.Layout.Maximize
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

myTerminal = "urxvt"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    [ ((modm,               xK_Return), spawn "urxvt")
    --, ((modm,               xK_p     ), spawn "dmenu_run")
    --, ((modm .|. shiftMask, xK_p     ), spawn "dmenu_path_c > /tmp/dmenu_path_c.txt")
    --, ((modm,               xK_p     ), spawn "dmenu_path_c | dmenu")
    , ((modm,               xK_p     ), spawn "dmenu_path_c | dmenu_run")
    --, ((modm,               xK_p     ), spawn "dmenu_path | dmenu")
    , ((modm .|. shiftMask, xK_c     ), kill)                               -- close focused window
    , ((modm,               xK_space ), sendMessage NextLayout)             -- Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) --  Reset the layouts on the current workspace to default
    , ((modm,               xK_n     ), refresh)                            -- Resize viewed windows to the correct size
    , ((modm,               xK_Tab   ), windows W.focusDown)                -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)                -- Move focus to the next window
    , ((modm,               xK_k     ), windows W.focusUp  )                -- Move focus to the previous window
    , ((modm,               xK_m     ), windows W.focusMaster  )            -- Move focus to the master window
    --, ((modm,               xK_Return), windows W.swapMaster)               -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )               -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )               -- Swap the focused window with the previous window
    , ((modm,               xK_h     ), sendMessage Shrink)                 -- Shrink the master area
    , ((modm,               xK_l     ), sendMessage Expand)                 -- Expand the master area
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)     -- Push window back into tiling
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))         -- Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))      -- Deincrement the number of windows in the master area
    --, ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))          -- Quit xmonad
    --, ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart") 
    , ((modm              , xK_q     ),  restart "xmonad" True )            -- Restart xmonad
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myTabConfig = defaultTheme { inactiveBorderColor = "#000000"
                           , inactiveTextColor   = "#CCCCCC"
                           , activeBorderColor   = "#000000"
                           , activeTextColor     = "#FFFFFF"
                           , urgentBorderColor   = "#FF0000"
                           , urgentTextColor     = "#FF0000"
                           , fontName            = "-artwiz-nu-*-*-*-*-*-*-*-*-*-*-*-*"
                           , decoHeight          = 11
                           }

--myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| tabbed shrinkText myTabConfig 
--myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| tabbed shrinkText myTabConfig ||| Dishes 2 (1/6) ||| Accordion
--myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| tabbed shrinkText myTabConfig ||| Dishes 2 (1/6)
myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| tabbed shrinkText myTabConfig 
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    nmaster = 1        -- The default number of windows in the master pane
    ratio   = 1/2      -- Default proportion of screen occupied by master pane
    delta   = 1/100    -- Percent of screen to increment by when resizing panes

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        --, layoutHook = avoidStruts  $ smartBorders $ layoutHook defaultConfig
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , normalBorderColor  = "#222222"
        , focusedBorderColor = "#666666"
        , keys    = myKeys
        , modMask = mod1Mask 
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]

-- vim: ts=2 sts=2 sw=2 et:

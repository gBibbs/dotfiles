------------------------------
-- XMonad Config
-- Author: Gabriel Bibbs
------------------------------
import System.Exit
import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified Data.Map as M
import qualified XMonad.StackSet as W

------------------------------
-- Base Definitions
------------------------------
myTerminal = "urxvt"
myModMask = mod4Mask
myBaseConfig = defaultConfig
myBorderWidth = 2
myNormalBorder = "#2E3440"
myFocusedBorder = "#81A1C1"

------------------------------
-- Layouts Definitions
------------------------------
mySpacing = spacingRaw True (Border 0 0 0 0) True (Border 0 5 5 5) True

tall    = renamed [Replace "Tall"]
	$ mySpacing
	$ ResizableTall 1 (3/100) (1/2) []
floats	= renamed [Replace "Float"]
	$ simplestFloat
spirals = renamed [Replace "Spiral"]
	$ mySpacing
	$ spiral (6/7)

myLayout = gaps[(U,10), (R,10), (D,10), (L,10)] $ (tall ||| spiral (6/7) ||| Mirror tall ||| floats ||| noBorders Full)

------------------------------
-- Workspaces
------------------------------
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

xmobarEscape = concatMap doubleLts
    where 
	doubleLts '<' = "<<"
	doubleLts x   = [x]

myClickableWorkspaces = clickable . (map xmobarEscape) $ myWorkspaces
    where
	clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
		      (i,ws) <- zip [1..9] l,
		      let n = i ]

------------------------------
-- Key Bindings
------------------------------
myMouseKeys =
	[ ((mod1Mask, 1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
	, ((mod1Mask, 2), (\w -> focus w >> windows W.shiftMaster))
	, ((mod1Mask, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
	]

-- Keys overwrite base XMonad defaults
myKeys = 
	[ 
	-- Most used programs	
	  ("M-S-<Return>", spawn "thunar")
	, ("M-p", spawn "dmenu_run -nb '#2e3440' -sb '#5e81ac' -nf '#eceff4' -sf '#eceff4' -fn 'DeJaVu Sans:regular:pixelsize=12'")
	, ("M-f", spawn "firefox")
	, ("M-<Return>", spawn myTerminal)
	
	, ("C-S-t", withFocused $ windows . W.sink)
	-- Alt Tab between windows in current ws		
	, ("M1-<Tab>", windows W.focusDown)

	-- Resize the layout
	, ("M-S-h", sendMessage Shrink)
	, ("M-S-l", sendMessage Expand)
	, ("M-C-h", sendMessage MirrorShrink)
	, ("M-C-l", sendMessage MirrorExpand)
	
	-- Kill selected window
	, ("M-q", kill)

	-- Quit or Restart Xmonad
	, ("M-C-q", io (exitWith ExitSuccess))
	, ("M-C-r", spawn "xmonad --recompile && xmonad --restart")	
	]

------------------------------
-- Layout Exceptions
------------------------------
myManageHook = composeAll
	[ className =? "Gimp"		--> doFloat
	, resource  =? "desktop_window"	--> doIgnore
	, resource  =? "kdesktop"	--> doIgnore
	]

------------------------------
-- Startup Hook
------------------------------
myStartupHook = do
	spawnOnce "xsetroot -cursor_name left_ptr &"
	spawn "picom --config /home/g/.config/picom/picom.conf &"
	spawnOnce "nitrogen --restore"
	spawn "stalonetray &"
	spawn "volumeicon &"
	spawn "nm-applet &"
	spawn "urxvtd -q -o -f &"
	setWMName "LG3D"

------------------------------
-- Main
------------------------------
main = do
    xmproc <- spawnPipe "xmobar -x 0 /home/g/.config/xmobar/xmobarrc1"
    --xmproc1 <- spawnPipe "xmobar -x 1 /home/g/.config/xmobar/xmobarrc1"
    xmonad $ docks myBaseConfig
	{ startupHook = myStartupHook
	, manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook myBaseConfig
	, layoutHook = avoidStruts $ myLayout ||| layoutHook myBaseConfig
	, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = \x -> hPutStrLn xmproc x
			, ppTitle = xmobarColor "#a3be8c" "" . shorten 60
			, ppCurrent = xmobarColor "#ebcb8b" "" . wrap "[" "]"
			, ppHidden = xmobarColor "#88c0d0" ""
			, ppLayout = xmobarColor "#B48EAD" ""
			, ppWsSep = " | "
			}
	, workspaces = myClickableWorkspaces
	, modMask = myModMask
	, terminal = myTerminal
	, borderWidth = myBorderWidth
	, normalBorderColor = myNormalBorder
	, focusedBorderColor = myFocusedBorder
	} `additionalKeysP` myKeys 
	  `additionalMouseBindings` myMouseKeys
	  `removeKeysP` [("M-h"),("M-l"),("M-<Tab>"),("M-t")]

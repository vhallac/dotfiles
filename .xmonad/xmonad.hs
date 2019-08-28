import System.IO (hPutStrLn)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Monad (liftM2, join)
import Data.List ( isPrefixOf, nub )
import Data.Monoid (mappend)
import Data.Maybe (maybeToList)
import XMonad
import qualified XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat, isInProperty)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Util.Run(safeSpawn, spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Actions.NoBorders(toggleBorder)
import XMonad.Layout.TwoPane
import XMonad.Layout.Master (mastered)
import XMonad.Layout.Maximize (maximize, maximizeRestore)
import XMonad.Layout.Minimize (minimize)
import XMonad.Actions.Minimize (minimizeWindow, withFirstMinimized, maximizeWindowAndFocus)
import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import qualified XMonad.Actions.WindowGo as WG
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicHooks (oneShotHook)
import Graphics.X11.Xlib.Extras (changeProperty32
                                ,propModePrepend)
import Graphics.X11.Xlib.Atom (aTOM)

ezSpawn cmd = safeSpawn (head (words cmd)) (tail (words cmd))

smallScreenLayoutHook = let goldenMasteredTwoPane = mastered (1/100) (500/809) (Mirror (TwoPane (3/100) (1/2)))
                        in minimize $ maximize goldenMasteredTwoPane ||| layoutHook def

namedWorkspaces = ["1.control", "2.browse", "3.code", "4.chat", "5.media", "6.--", "7.db", "8.--", "9.temp"]

manageAppViews = composeAll . concat $
                 [
                  [ WG.className =? b --> setView "1.control"        | b <- controlViewClasses ],
                  [ WG.className =? b --> setView "2.browse"         | b <- browseViewClasses  ],
                  [ WG.className =? b --> setView "3.code"           | b <- codeViewClasses    ],
                  [ WG.className =? b --> setView "4.chat"           | b <- chatViewClasses    ],
                  [ WG.className =? b --> setView "9.temp"           | b <- tempViewClasses    ]
                 ]
    where
      setView = doF . liftM2 (.) W.greedyView W.shift
      officerViewClasses = [ "libreoffice-calc" ]
      browseViewClasses  = [ "Chromium", "firefox" ]
      controlViewClasses = [ "UXTerm", "Gnome-terminal", "Lxterminal", "Emacs" ]
      chatViewClasses    = [ "Icedove", "Google-Apps", "Skype", "irc" ]
      codeViewClasses    = [ "Eclipse", "Java", "Code", "jetbrains-rider" ]
      tempViewClasses    = [ "Git-gui" ]

manageFloats = composeAll . concat $
               [
--                [ WG.className =? b <&&> isFullscreen --> doFullFloat | b <- fullScreenClasses ],
                 [ isFullscreen --> doFullFloat ],
                 [ WG.className =? b --> doFloat | b <- floatClasses ],
                 [ WG.appName =? b --> doFloat | b <- floatAppNames ],
                 [ (isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH") <||>
                   WG.title =? "Eclipse " <&&> WG.className =? "Java" --> doFloat ]
               ]
    where
      floatAppNames = [ "Dialog" ]
      floatClasses = [ "grun", "java-lang-Thread" ]
      fullScreenClasses = [ "VirtualBox", "firefox" ]

baseConf =  docks {--$ ewmh--} def
                  { modMask            = mod4Mask,
                    borderWidth        = 2,
                    terminal           = "uxterm",
                    normalBorderColor  = "#888888",
                    focusedBorderColor = "#cd8d00",
                    manageHook         = (placeHook $ fixed (0.5, 0.5))
                                         <+> (manageHook defaultConfig)
                                         <+> manageAppViews
                                         <+> manageFloats,
                    layoutHook         = avoidStruts
                                         $ smartBorders
                                         $ smallScreenLayoutHook,
                    workspaces         = namedWorkspaces,
                    handleEventHook    = handleEventHook def
                                         <+> minimizeEventHook
                                         {-- <+> fullscreenEventHook --}
                  }
                 `removeKeysP`
                  [ "M-S-q", "M-S-c"]
                 `additionalKeysP`
                 [
                  ("M-d", ezSpawn "grun"),
                  ("M-S-d", ezSpawn "dmenu_run"),
                  ("M-p M-l", ezSpawn "loginctl lock-session"),
                  ("M-p M-s", ezSpawn "systemctl suspend"),
                  ("M-p M-h", ezSpawn "systemctl hibernate"),
                  ("M-p M-o", ezSpawn "systemctl poweroff"),
                  ("M-p M-r", ezSpawn "systemctl reboot"),
                  ("M-c M-S-p", ezSpawn "keepass2 --auto-type"),
                  ("M-c M-e", WG.runOrRaise "emacsclient -c" (className =? "Emacs")),
                  ("M-c M-c", WG.runOrRaise "st" (className =? "UXTerm")),
               --   ("M-c M-S-c", confirmPrompt defaultXPConfig "Exit" $ ezSpawn "uxterm -fs 16"),
                  ("M-c M-S-c", shellPrompt def),
                  ("M-c M-S-p", ezSpawn "keepass --auto-type"),
                  ("M-S-m", withFocused (sendMessage . maximizeRestore)),
                  ("M-S-n", withFocused minimizeWindow),
                  ("M-n", withFirstMinimized maximizeWindowAndFocus),
                  ("M-b", withFocused toggleBorder),
--                  ("M-S-e", confirmPrompt defaultXPConfig "Exit" $ io (exitWith ExitSuccess)),
                  ("M-S-C-e", io (exitWith ExitSuccess)),
                  ("M-S-q", kill),
--                  ("M-f", withFocused $ \w -> windows . W.float w (W.RationalRect 0 0 1 1)),
                  ("M-S-f", sendMessage ToggleStruts),
                  ("<XF86MonBrightnessUp>", ezSpawn "light -A 5"),
                  ("<XF86MonBrightnessDown>", ezSpawn "light -U 5"),
                  ("<XF86AudioMute>", ezSpawn "pactl set-sink-mute 1 toggle"),
                  ("<XF86AudioMicMute>", ezSpawn "pactl set-source-mute 1 toggle"),
                  ("<XF86AudioRaiseVolume>", ezSpawn "pactl set-sink-volume 1 +5%"),
                  ("<XF86AudioLowerVolume>", ezSpawn "pactl set-sink-volume 1 -5%")
                 ]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/vedat/.config/xmobar/xmobarrc"
  ezSpawn "/usr/bin/lxsession -s xmonad -e xmonad"
  xmonad $ baseConf
             { logHook            = logHook baseConf >> (DL.dynamicLogWithPP DL.xmobarPP
                                                         { DL.ppOutput = hPutStrLn xmproc
                                                         , DL.ppTitle  = DL.xmobarColor "green" "" . DL.shorten 50
                                                         }),
               startupHook        = startupHook baseConf >> setWMName "LG3D"
             }



setFullScreen :: Window -> X ()
setFullScreen window = do
    atom_NET_WM_STATE <- getAtom "_NET_WM_STATE"
    atom_NET_FULLSCREEN <- getAtom "_NET_WM_STATE_FULLSCREEN"
    atom_NET_MAX_VERT <- getAtom "_NET_WM_STATE_MAXIMIZED_VERT"
    atom_NET_MAX_HORZ <- getAtom "_NET_WM_STATE_MAXIMIZED_HORZ"

    dpy <- asks display
    io $ do
        supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy atom_NET_WM_STATE window
        changeProperty32 dpy window atom_NET_WM_STATE aTOM propModePrepend (nub $ fromIntegral atom_NET_FULLSCREEN
                                                              : fromIntegral atom_NET_MAX_HORZ
                                                              : fromIntegral atom_NET_MAX_VERT
                                                              : supportedList)

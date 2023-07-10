{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-tabs #-}

    -- Base
import XMonad
import XMonad (asks)
import XMonad.Config.Desktop
import Data.Maybe (isJust)
import Data.List (isPrefixOf)
import Control.Monad (when, unless)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)

    -- Utilities
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP, additionalMouseBindings)
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadManageHook, namedScratchpadAction, customFloating)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedWindows (getName)

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, pad, xmobarPP, xmobarColor, shorten, PP(..))
-- import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..)) -- error
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FloatNext (floatNextHook, toggleFloatNext, toggleFloatAllNew)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops   -- required for xcomposite in obs to work. Usage: "ewmh $ config"
import XMonad.Hooks.UrgencyHook (readUrgents)

    -- Actions
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), toggleWS)
import XMonad.Actions.GroupNavigation
import XMonad.Actions.GridSelect (GSConfig(..), goToSelected, bringSelected, colorRangeFromClassName, buildDefaultGSConfig)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.Warp (warpToWindow, banishScreen, Corner(LowerRight))
import XMonad.Actions.MouseResize
import XMonad.Actions.Minimize
import qualified XMonad.Actions.ConstrainedResize as Sqr

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.Hidden
import XMonad.Layout.ThreeColumns
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))

    -- Prompts
import XMonad.Prompt (XPConfig(..), XPPosition(Top), Direction1D(..))

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as SS

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Exit
import Data.Char (toLower)
import Data.List (intersperse, find)
import Data.Bool (bool)
import Data.Maybe
import Codec.Binary.UTF8.String (encodeString) -- For myDynamicLog
import qualified Data.Map as M

-----------------
-- Keybindings --
-----------------

myKeys :: [ (String, X ()) ]
myKeys =
	[ ("M-<F2>",          sendMessage ToggleStruts)                                    -- toggle panel

	, ("M-S-w",           kill1)                                                       -- kill current window
	, ("M-m",             withFocused minimizeWindow)                                  -- minimize window
	, ("M-S-m",           withLastMinimized maximizeWindowAndFocus)                    -- unminimize window

	, ("M-l",             moveTo Next groupedWSP)                                      -- go to next workspace
	, ("M-h",             moveTo Prev groupedWSP)                                      -- go to next workspace
	, ("M-j",             windows SS.focusDown)                                         -- %! Move focus to the next window
	, ("M-k",             windows SS.focusUp)                                           -- %! Move focus to the previous window
	, ("M-<Tab>",         nextMatch History (return True))                             -- previus window
	, ("M-S-<Tab>",       toggleWS)                                                    -- previus workspace
	, ("M-S-j",           windows SS.swapDown)                                          -- %! Swap the focused window with the next window
	, ("M-S-k",           windows SS.swapUp)                                            -- %! Swap the focused window with the previous window
	, ("M-[",             sendMessage Shrink)                                          -- %! Shrink the master area
	, ("M-]",             sendMessage Expand)                                          -- %! Expand the master area

	, ("M-S-l",           shiftTo Next groupedWSP >> moveTo Next groupedWSP)           -- shitf window to next workspace and go to next workspace
	, ("M-S-h",           shiftTo Prev groupedWSP >> moveTo Prev groupedWSP)           -- shitf window to next workspace and go to next workspace

	, ("M-a",             doWorkspaceAction False True temporaryDesktop) -- switch to temporary desktop
	, ("M-S-a",           doWorkspaceAction True True temporaryDesktop)                        -- shitf window to free temporary desktop

	, ("M-f",             withFocused float)                                           -- Make window float
	, ("M-S-f",           withFocused $ windows . SS.sink)                              -- Unfloat one window
	, ("M-<F10>",         sinkAll)                                                     -- Unfloat all windows

	, ("M-x",             sendMessage NextLayout)                                      -- Next layout

	-- , ("M-n",             openFree False)
	-- , ("M-S-n",           openFree True)

	, ("M-d",             windows copyToAll)                                           -- show on all desktops
	, ("M-S-d",           killAllOtherCopies)                                          -- stop showing on all desktops

	, ("M-S-<F12>",       io (exitWith ExitSuccess))                                   -- Quit xmonad
	]
	++
	[ ("M-C-" ++ [x],     doWorkspaceAction False False x) | x <- ['a' .. 'z'] ]          -- #additional-workspaces
	++
	[ ("M-C-S-" ++ [x],   doWorkspaceAction True True x) | x <- ['a' .. 'z'] ]            -- additional workspaces
	++
	[ ("M-" ++ show (x :: Int),    doWorkspaceAction False False x) | x <- [1 .. 9]]                     -- numbered sub-worspaces
	++
	[ ("M-S-" ++ show (x :: Int),  doWorkspaceAction True True x) | x <- [1 .. 9]]                     -- numbered sub-workspaces

dynamicKeysP :: [(String, X ())]
dynamicKeysP =
	map regural ['a' .. 'z']
		++ map reguralShift ['a' .. 'z']
	where
		regural :: Char -> (String, X())
		regural key = ("M-" ++ [key], spawn $ "awesomewm-key-" ++ [key])

		reguralShift :: Char -> (String, X())
		reguralShift key = ("M-S-"  ++ [key], spawn $ "awesomewm-key-shift-" ++ [key])

dynamicKeys :: [((KeyMask, KeySym), X ())]
dynamicKeys = map special specialKeys ++ map specialShift specialKeys
	where
		special, specialShift :: (String, KeySym) -> ((KeyMask, KeySym), X())
		special      (sym, key) = ((myModMask, key),               spawn $ "awesomewm-key-" ++ (map toLower sym))
		specialShift (sym, key) = ((myModMask .|. shiftMask, key), spawn $ "awesomewm-key-shift-" ++ (map toLower sym))

-- | A list of special key names and their corresponding KeySyms.
specialKeys :: [(String, KeySym)]
specialKeys =
	[ ("Backspace"    , xK_BackSpace)
	, ("Tab"          , xK_Tab)
	, ("Return"       , xK_Return)
	, ("Pause"        , xK_Pause)
	, ("Scroll_lock"  , xK_Scroll_Lock)
	, ("Sys_Req"      , xK_Sys_Req)
	, ("Print"        , xK_Print)
	, ("Escape"       , xK_Escape)
	, ("Esc"          , xK_Escape)
	, ("Delete"       , xK_Delete)
	, ("Home"         , xK_Home)
	, ("Left"         , xK_Left)
	, ("Up"           , xK_Up)
	, ("Right"        , xK_Right)
	, ("Down"         , xK_Down)
	, ("L"            , xK_Left)
	, ("U"            , xK_Up)
	, ("R"            , xK_Right)
	, ("D"            , xK_Down)
	, ("Page_Up"      , xK_Page_Up)
	, ("Page_Down"    , xK_Page_Down)
	, ("End"          , xK_End)
	, ("Insert"       , xK_Insert)
	, ("Break"        , xK_Break)
	, ("Space"        , xK_space)
	, ("KP_Space"     , xK_KP_Space)
	, ("KP_Tab"       , xK_KP_Tab)
	, ("KP_Enter"     , xK_KP_Enter)
	, ("KP_F1"        , xK_KP_F1)
	, ("KP_F2"        , xK_KP_F2)
	, ("KP_F3"        , xK_KP_F3)
	, ("KP_F4"        , xK_KP_F4)
	, ("KP_Home"      , xK_KP_Home)
	, ("KP_Left"      , xK_KP_Left)
	, ("KP_Up"        , xK_KP_Up)
	, ("KP_Right"     , xK_KP_Right)
	, ("KP_Down"      , xK_KP_Down)
	, ("KP_Prior"     , xK_KP_Prior)
	, ("KP_Page_Up"   , xK_KP_Page_Up)
	, ("KP_Next"      , xK_KP_Next)
	, ("KP_Page_Down" , xK_KP_Page_Down)
	, ("KP_End"       , xK_KP_End)
	, ("KP_Begin"     , xK_KP_Begin)
	, ("KP_Insert"    , xK_KP_Insert)
	, ("KP_Delete"    , xK_KP_Delete)
	, ("KP_Equal"     , xK_KP_Equal)
	, ("KP_Multiply"  , xK_KP_Multiply)
	, ("KP_Add"       , xK_KP_Add)
	, ("KP_Separator" , xK_KP_Separator)
	, ("KP_Subtract"  , xK_KP_Subtract)
	, ("KP_Decimal"   , xK_KP_Decimal)
	, ("KP_Divide"    , xK_KP_Divide)
	, ("KP_0"         , xK_KP_0)
	, ("KP_1"         , xK_KP_1)
	, ("KP_2"         , xK_KP_2)
	, ("KP_3"         , xK_KP_3)
	, ("KP_4"         , xK_KP_4)
	, ("KP_5"         , xK_KP_5)
	, ("KP_6"         , xK_KP_6)
	, ("KP_7"         , xK_KP_7)
	, ("KP_8"         , xK_KP_8)
	, ("KP_9"         , xK_KP_9)
	]

unsetKeys =
	[ "M-<Space>"
	]

-------------------
-- WorkspaceName --
-------------------

type WorkspaceNameMain = Int
type WorkspaceNameSecondary = Char
data WorkspaceName = WorkspaceName WorkspaceNameMain WorkspaceNameSecondary
	deriving (Eq, Read, Show, Typeable)

instance Ord WorkspaceName where
	compare a b =
		let x = compare (workspaceNameGetMain a) (workspaceNameGetMain b)
		in if x == EQ
			then compare (workspaceNameGetSecondary a) (workspaceNameGetSecondary b)
			else x

workspaceNameSeparatorChar :: Char
workspaceNameSeparatorChar = '/'

workspaceNameGetMain :: WorkspaceName -> WorkspaceNameMain
workspaceNameGetMain (WorkspaceName a b) = a

workspaceNameGetSecondary :: WorkspaceName -> WorkspaceNameSecondary
workspaceNameGetSecondary (WorkspaceName a b) = b

makeWorkspaceName :: WorkspaceNameMain -> WorkspaceNameSecondary -> WorkspaceName
makeWorkspaceName prefix suffix = WorkspaceName prefix suffix

makeDefaultWorkspaceName :: WorkspaceNameMain -> WorkspaceName
makeDefaultWorkspaceName prefix = WorkspaceName prefix defaultWorkspaceNamePart

workspaceNameGetCurrent :: X WorkspaceName
workspaceNameGetCurrent = do
	name <- withWindowSet (pure . SS.currentTag)
	return $ workspaceNameFromStringUnsafe name

workspaceNameOfWindowSpace :: WindowSpace -> WorkspaceName
workspaceNameOfWindowSpace ws = ret
	where
	name = SS.tag ws
	ret = workspaceNameFromStringUnsafe name

workspaceNameToString :: WorkspaceName -> String
workspaceNameToString w =
	decorate $
		[]
		++ (maybeSkip $ workspaceNameGetMain w)
		++ [workspaceNameSeparatorChar]
		++ (alwaysShow $ workspaceNameGetSecondary w)
	where
	decorate x
		| head x == workspaceNameSeparatorChar = tail x
		| last x == workspaceNameSeparatorChar = init x
		| otherwise = x
	maybeSkip x = if x == defaultWorkspaceNamePart then [] else showWorkspaceNamePart x
	alwaysShow = showWorkspaceNamePart

workspaceNameFromStringUnsafe :: String -> WorkspaceName
workspaceNameFromStringUnsafe name =
	if any (== workspaceNameSeparatorChar) name
	then makeWorkspaceName (readWorkspaceActable name) (readWorkspaceActable name)
	else WorkspaceName defaultWorkspaceNamePart (readWorkspaceActable name)

class WorkspaceActable a where
	doWorkspaceAction :: Bool -> Bool -> a -> X ()
	readWorkspaceActable :: String -> a

instance WorkspaceActable WorkspaceNameMain where
	doWorkspaceAction = doDesktopIndex
	readWorkspaceActable = readWorkspaceNamePart . takeWhile (/= workspaceNameSeparatorChar)

instance WorkspaceActable WorkspaceNameSecondary where
	doWorkspaceAction = doGroupedIndex
	readWorkspaceActable s =
		readWorkspaceNamePart $
			if all (/= workspaceNameSeparatorChar) s
			then s
			else tail $ dropWhile (/= workspaceNameSeparatorChar) s

class WorkspaceSerializable a where
	readWorkspaceNamePart :: String -> a
	tryReadWorkspaceNamePart :: String -> Maybe a
	showWorkspaceNamePart :: a -> String
	defaultWorkspaceNamePart :: a

instance WorkspaceSerializable Int where
	showWorkspaceNamePart = show
	readWorkspaceNamePart = read
	tryReadWorkspaceNamePart = maybeRead
	defaultWorkspaceNamePart = 1

instance WorkspaceSerializable Char where
	showWorkspaceNamePart x = [x]
	readWorkspaceNamePart = head
	tryReadWorkspaceNamePart s = if null s then Nothing else Just (head s)
	defaultWorkspaceNamePart = 't'

-------------------
-- Focus history --
-------------------

wsFocusHistoryHook :: X ()
wsFocusHistoryHook = XS.get >>= updateFocusHistory >>= XS.put

-- | Holds names of workspaces
data WSFocusDictionary = WSFocusDictionary [WorkspaceName]
	deriving (Eq, Read, Show, Typeable)

instance ExtensionClass WSFocusDictionary where
	initialValue  = WSFocusDictionary []
	extensionType = PersistentExtension

updateFocusHistory :: WSFocusDictionary -> X WSFocusDictionary
updateFocusHistory (WSFocusDictionary hist) = do
	curr <- workspaceNameGetCurrent
	return $ kek curr
	where
		kek curr = WSFocusDictionary $ replaceFirstOrAdd curr (repl curr) hist
		repl curr x =
			if workspaceNameGetMain x == workspaceNameGetMain curr
			then Just curr
			else Nothing

---------------
-- Constants --
---------------

myModMask = mod4Mask -- Windows key

temporaryDesktop :: WorkspaceNameSecondary
temporaryDesktop = defaultWorkspaceNamePart

---------------
-- Functions --
---------------

findLastDesktop :: WorkspaceNameMain -> WSFocusDictionary -> String
findLastDesktop wsId (WSFocusDictionary hist) =
	workspaceNameToString $
		maybe
			(makeDefaultWorkspaceName wsId)
			id
			(find findPredicate hist)
	where findPredicate w = workspaceNameGetMain w == wsId

getWsLastDesktop :: WorkspaceNameMain -> X String
getWsLastDesktop wsId = XS.get >>= return . findLastDesktop wsId

getWsNewDesktop :: WorkspaceNameMain -> X String
getWsNewDesktop = getWsLastDesktop

emptyWorkspacesOfWindowSet :: WindowSet -> [WindowSpace]
emptyWorkspacesOfWindowSet set = filter (isNothing . SS.stack) $ SS.workspaces set

getEmptyWorkspaces :: X [WorkspaceName]
getEmptyWorkspaces = do
	ws <- withWindowSet (return . emptyWorkspacesOfWindowSet)
	return $ map workspaceNameOfWindowSpace ws

getWsFreeDesktopWorkspaceName :: WorkspaceNameMain -> X (Maybe WorkspaceName)
getWsFreeDesktopWorkspaceName onWsMain = do
	mEmpties <- getEmptyWorkspaces
	return (func mEmpties)
	where
	func :: [WorkspaceName] -> Maybe WorkspaceName
	func mEmpties = ret
		where
		ret =
			if null currentEmptiesTags
			then Nothing
			else Just (minimum currentEmptiesTags)

		currentEmptiesTags = filter (\w -> workspaceNameGetMain w == onWsMain) mEmpties

getWsFreeDesktop :: WorkspaceNameMain -> X (Maybe String)
getWsFreeDesktop onWsMain = do
	w <- getWsFreeDesktopWorkspaceName onWsMain
	return $ maybe Nothing (Just . workspaceNameToString) w

getWsFreeDesktopOrCurrent :: WorkspaceNameMain -> X String
getWsFreeDesktopOrCurrent onWsMain = do
	mfree <- getWsFreeDesktop onWsMain
	return $ maybe (workspaceNameToString $ makeDefaultWorkspaceName onWsMain) id mfree

-- Open free desktop on current workspace
openFree :: Bool -> X ()
openFree shiftQ = do
	curr <- workspaceNameGetCurrent
	freeDesktop <- getWsFreeDesktop (workspaceNameGetMain curr)
	maybe (return ()) (action) freeDesktop
	where
	action target =
		if shiftQ
		then do
			windows $ SS.shift target
			windows $ SS.greedyView target
		else windows $ SS.greedyView target

doDesktopIndex :: Bool -> Bool -> WorkspaceNameMain -> X ()
doDesktopIndex shiftQ newQ desktopName = do
	x <- if newQ then getWsFreeDesktopOrCurrent desktopName else getWsLastDesktop desktopName
	when shiftQ $ (windows . SS.shift) x
	(windows . SS.greedyView) x

doGroupedIndex :: Bool -> Bool -> WorkspaceNameSecondary -> X ()
doGroupedIndex shiftQ newQ index = do
	curr <- workspaceNameGetCurrent
	act shiftQ (workspaceNameToString $ makeNew curr)

	where
		makeNew w = makeWorkspaceName (workspaceNameGetMain w) index

		act True = \x -> windows (SS.shift x) >> windows (SS.view x)
		act False = windows . SS.greedyView

groupedWS :: X (WindowSpace -> Bool)
groupedWS = do
	curr <- workspaceNameGetCurrent
	let main = workspaceNameGetMain curr
	return (\ws -> workspaceNameGetMain (workspaceNameOfWindowSpace ws) == main)

groupedWSP = WSIs groupedWS

-------------
-- Helpers --
-------------

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Like for dictionary
replaceFirstOrAdd :: a -> (a -> Maybe a) -> [a] -> [a]
replaceFirstOrAdd add f [] = [add]
replaceFirstOrAdd add f (x : xs) = maybe (x : replaceFirstOrAdd add f xs) (: xs) (f x)

------------
-- XMOBAR --
------------

myXmobarrc = "~/.xmonad/xmobar-single.hs"
myBar = "xmobar " ++ myXmobarrc
myXmobarPP xmproc = xmobarPP
	{  ppTitle   = xmobarColor "darkgreen"  "" . shorten 100
	,  ppOutput  = hPutStrLn xmproc
	}
addxmobar xmproc conf =
	conf {
		logHook = logHook conf >> (myDynamicLog $ myXmobarPP xmproc)
	}

myDynamicLog :: PP -> X ()
myDynamicLog pp = myDynamicLogString pp >>= io . ppOutput pp

myDynamicLogString :: PP -> X String
myDynamicLogString pp = do
	winset <- gets windowset
	urgents <- readUrgents
	sort' <- ppSort pp

	-- workspace list
	let ws = pprWindowSet sort' urgents pp winset

	-- window title
	wt <- maybe (return "") (fmap show . getName) . SS.peek $ winset

	-- run extra loggers, ignoring any that generate errors.
	extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

	return $ encodeString . sepBy " " . ppOrder pp $
						[ ws
						, ppTitle  pp $ ppTitleSanitize pp wt
						]
						++ catMaybes extras
	where
		sepBy :: String -> [String] -> String
		sepBy sep = concat . intersperse sep . filter (not . null)

----------
-- Main --
----------

main = do
	xmproc <- spawnPipe myBar
	xmonad $ (addxmobar xmproc) myConfig

myConfig = ewmh . docks $ def
	{
		  layoutHook    = myLayout
		, manageHook    = myManageHook <+> manageDocks <+> manageHook def
		, startupHook   = myStartupHook
		, modMask       = myModMask
		, workspaces    = myWorkspaces
		, borderWidth   = 0
		, logHook       = myLogHook
	}
	`additionalKeys`  dynamicKeys
	`additionalKeysP` dynamicKeysP
	`additionalKeysP` myKeys
	`removeKeysP` unsetKeys

myStartupHook = do
	-- xdir <- asks (cfgDir . directories)
	setWMName "LG3D"

debugFile = unsafePerformIO $ openFile "xmonadlog.txt" AppendMode
debugwrite str = io (hPutStrLn debugFile str >> hFlush debugFile)

myLogHook = do
	historyHook
	wsFocusHistoryHook

myWorkspaces =
	[getWSname x mod | mod <- [1 .. 9], x <- ['a' .. 'z']]
	where
	getWSname x y = workspaceNameToString $ makeWorkspaceName y x

myLayout = avoidStruts $ minimize $ boringWindows (
	Full |||
	smartBorders (withBorder 3 (Tall 1 (3/100) (1/2)))
	)

myManageHook = placeHook (withGaps (20,12,12,12) (smart (0.5,0.5))) <+> insertPosition End Newer <+> floatNextHook <+>
		(composeAll . concat $
		[
		  [ resource  =? r --> doF (SS.view "web" . SS.shift "web")   | r <- myWebApps	 ]
		, [ resource  =? r --> doF (SS.view "media" . SS.shift "media") | r <- myMediaApps   ]
		, [ resource  =? r --> doFloat							| r <- myFloatApps   ]
		, [ className =? c --> ask >>= doF . SS.sink			   | c <- myUnfloatApps ]
		])
		where
			myTermApps	= ["termite", "xterm", "htop", "irssi"]
			myWebApps	 = ["firefox", "thunderbird"]
			myMediaApps   = ["ncmpcpp", "weechat", "mplayer", "cmus"]
			mySystApps	= ["ranger", "thunar", "geany", "nitrogen"]

			myFloatApps   = ["file-roller", "nitrogen"] ++ myMediaApps
			myUnfloatApps = ["gimp"]

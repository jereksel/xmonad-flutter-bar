-----------------------------------------------------------------------------
-- |
-- Module      : FlutterBar.PagerHints
-- Copyright   : (c) Andrzej Ressel
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Andrzej Ressel <jereksel@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Complements the "XMonad.Hooks.EwmhDesktops" with additional hint
-- not contemplated by the EWMH standard:
--
-- [@_FLUTTERBAR_VISIBLE_WORKSPACES@] Contains a list of UTF-8 strings with the
-- names of all the workspaces that are currently showed on all displays
-- sorted using provided @sorter@
--
-- Hint should be considered read-only, and is set every time
-- XMonad calls its log hooks.
--
-----------------------------------------------------------------------------

module FlutterBar.PagerHints (
    pagerHints
) where

import           Data.Maybe
import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Util.WorkspaceCompare   ( getWsIndex , WorkspaceSort)

-- $usage
--
-- You can use this module with the following in your @xmonad.hs@ file:
--
-- > import FlutterBar.PagerHints (pagerHints)
-- >
-- > main = xmonad $ ewmh $ pagerHints getSortByIndex $ defaultConfig
-- > ...

-- | The \"Visible Workspaces\" custom hint.
xVisibleProp :: X Atom
xVisibleProp = getAtom "_FLUTTERBAR_VISIBLE_WORKSPACES"

-- | Add support for the \"Visible Workspaces\" custom hint to the given config.
pagerHints :: X WorkspaceSort -> XConfig a -> XConfig a
pagerHints sorter c = c { logHook = logHook c +++ pagerHintsLogHook sorter }
  where x +++ y = x `mappend` y

-- | Update the current values of custom hint.
pagerHintsLogHook :: X WorkspaceSort -> X ()
pagerHintsLogHook sorter = do
  wsIndex <- getWsIndex
  sort <- sorter
  withWindowSet
    (setVisibleWorkspaces . mapMaybe (wsIndex . W.tag) . sort . map W.workspace . getDisplayedWorkspaces)

getDisplayedWorkspaces :: W.StackSet i l a sid sd -> [W.Screen i l a sid sd]
getDisplayedWorkspaces a = W.visible a ++ [W.current a]

-- | Set the value of the \"Visible Workspaces\" hint to the one given.
setVisibleWorkspaces :: [Int] -> X ()
setVisibleWorkspaces vis = withDisplay $ \dpy -> do
  r  <- asks theRoot
  a  <- xVisibleProp
  c  <- getAtom "CARDINAL"
  let vis' = map fromIntegral vis
  io $ changeProperty32 dpy r a c propModeReplace vis'

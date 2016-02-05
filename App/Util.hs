module App.Util (
     State(..)
    ,uncurry5
    ,readHeadMaybe
    ,buttonGetLabelWidget
    ,setExpand
    ,reveal
    ,renderLabel
    ,adjacent
    ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums

import Control.Monad
import Data.IORef
import Data.Array.IO
import Data.Array.Unboxed

-- Game information
data State = State {
     isMine      :: UArray Int Bool
    ,adjacencies :: UArray Int Int
    ,visited     :: IOUArray Int Bool
    ,remainingIO :: IORef Int
}

-- General utility
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e
{-# INLINE uncurry5 #-}

-- General utility
readMaybe :: Read a => String -> Maybe a
readMaybe s = 
    case reads s of
        (x, ""):_ -> Just x
        _         -> Nothing

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x

readHeadMaybe :: Read a => String -> Maybe a
readHeadMaybe = join . liftM readMaybe . headMaybe . words

-- Game helper functions
buttonGetLabelWidget :: Button -> IO Label
buttonGetLabelWidget = liftM (castToLabel . head) . containerGetChildren

setExpand :: WidgetClass self => self -> IO()
setExpand w = do
    set w [ widgetExpand := True ]
    widgetSetHAlign w AlignFill
    widgetSetVAlign w AlignFill

reveal :: Button -> Int -> IORef Int -> IO()
reveal b adjs revealsLeft = do
    renderLabel adjs =<< buttonGetLabelWidget b
    buttonSetRelief b ReliefNone
    widgetSetSensitive b False
    modifyIORef' revealsLeft (subtract 1)

renderLabel :: Int -> Label -> IO()
renderLabel 0 l = labelSetText l " "
renderLabel 1 l = labelSetMarkup l "<span foreground=\"green\"><b>1</b></span>"
renderLabel 2 l = labelSetMarkup l "<span foreground=\"blue\"><b>2</b></span>"
renderLabel 3 l = labelSetMarkup l "<span foreground=\"purple\"><b>3</b></span>"
renderLabel 4 l = labelSetMarkup l "<span foreground=\"orange\"><b>4</b></span>"
renderLabel 5 l = labelSetMarkup l "<span foreground=\"red\"><b>5</b></span>"
renderLabel 6 l = labelSetMarkup l "<span foreground=\"cyan\"><b>6</b></span>"
renderLabel 7 l = labelSetMarkup l "<span foreground=\"magenta\"><b>7</b></span>"
renderLabel 8 l = labelSetMarkup l "<b>8</b>"
renderLabel _ _ = return ()

adjacent :: Int -> Int -> [Int]
adjacent ix c = 
    let lBound = ix `rem` c == 0
        uBound = (ix-1) `rem` c == 0
    in [ix-c, ix+c]
          ++ (if lBound then [] else [ix+1, ix-c+1, ix+c+1])
          ++ (if uBound then [] else [ix-1, ix-c-1, ix+c-1])
{-# INLINE adjacent #-}

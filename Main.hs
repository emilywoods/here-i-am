{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Util (fg, on)
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

sections :: [T.Text]
sections =
    [ "Home"
    , "About"
    , "Skills"
    , "Experience"
    , "Education"
    , "Projects"
    , "Objective"
    ]

borderDemos :: [Widget ()]
borderDemos = mkBorderDemo <$> sections

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr,         fg V.blue)
    ]

mkBorderDemo :: T.Text -> Widget ()
mkBorderDemo sectionName =
    updateAttrMap (A.applyAttrMappings borderMappings) $
    withBorderStyle BS.unicodeBold $
    B.border $
    hLimit 20 $
    vLimit 3 $
    C.center $
    txt $ "  " <> sectionName <> "  "

introduction :: Widget ()
introduction =
    C.center $
    txt $ "Hello, my name is Emily 👋"


ui :: Widget ()
ui =
    B.hBorderWithLabel (str "Sections")
    <=> hBox borderDemos
    <=> B.hBorder
    <=> introduction

main :: IO ()
main = M.simpleMain ui
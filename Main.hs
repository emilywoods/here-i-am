{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.Widgets.Dialog as D
import qualified Brick.Main as M
import Brick.Util (fg, on)
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , vLimit
  , hLimit
  , padAll
  , txt
  , str
  )

import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

data Choice = Home | About | Skills | Experience | Education | Projects | Objective 
          deriving (Show, Eq)

appEvent :: D.Dialog Choice -> BrickEvent () e -> T.EventM () (T.Next (D.Dialog Choice))
appEvent d (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt d
        V.EvKey (V.KChar 'q') [] -> M.halt d
        _ -> M.continue =<< D.handleDialogEvent ev d
appEvent d _ = M.continue d

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

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.buttonAttr, V.cyan `on` V.black)
    , (D.buttonSelectedAttr, V.magenta `on` V.black)
    ]

introduction :: Widget ()
introduction =
    C.center $
    txt $ "Emily's resume.\n\nTo explore the different sections, click ← or →. To exit, click q or esc"

contactMe :: Widget ()
contactMe =
    B.borderWithLabel (str "How to reach me") $
    hLimit 50 $
    vLimit 8 $
    C.hCenter $
    txt $ "\n\n  Website: https://emilywoods.me  \n\n  Email: hello@emilywoods.me  \n\n  Github: emilywoods  \n\n"

aboutSection :: Widget ()
aboutSection =
    C.vCenter (str "Hello, my name is Emily 👋.\n\nI am an engineer, mostly of software these days.\n\nI started out as a Process Engineer, took a detour into Bioengineering\nand ended up in Software. Most recently, I am a backend developer and\ninfrastructure engineer.\n\nI also do technical writing and help organise PyLadies Berlin.\n\nMy non-coding hobbies include bumbling around bookstores,\ntrying to keep plants alive, sewing, and generally learning new things.📚 🌱")
    <=> contactMe

skillsSection :: Widget ()
skillsSection =
    C.center $
    txt $ "Skills, Interests, Volunteering."

experienceSection :: Widget ()
experienceSection =
    C.center $
    txt $ "great experience"

educationSection :: Widget ()
educationSection =
    C.center $
    txt $ "2015-2016 :: MRes Bioengineering, Imperial College London\nGrade: Distinction\n\n2009 - 2013 :: BEng Process and Chemical Engineering, University College Cork\nGrade: 1.1\n\n "

projectsSection :: Widget ()
projectsSection =
    C.center $
    txt $ "look at what else i've made 👀"


objectiveSection :: Widget ()
objectiveSection =
    C.center $
    txt $ "this is what i want!"

selection :: D.Dialog Choice -> Widget ()  
selection d  
    | D.dialogSelection d == Just About = aboutSection
    | D.dialogSelection d == Just Skills = skillsSection
    | D.dialogSelection d == Just Experience = experienceSection
    | D.dialogSelection d == Just Education = educationSection
    | D.dialogSelection d == Just Projects = projectsSection
    | D.dialogSelection d == Just Objective = objectiveSection
    | otherwise   = introduction 

drawUI :: D.Dialog Choice -> [Widget ()]
drawUI d = [ui]
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str ""
            <=> selection d

initialState :: D.Dialog Choice
initialState = D.dialog (Just " ✨ Here I am ✨ ") (Just (0, choices)) 500
    where
        choices = [ ("Home", Home)
                  , ("About", About)
                  , ("Skills", Skills)
                  , ("Experience", Experience)
                  , ("Education", Education)
                  , ("Projects", Projects)
                  , ("Objective", Objective)
                  ]


-- App definition

theApp :: M.App (D.Dialog Choice) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
    d <- M.defaultMain theApp initialState
    putStrLn "Goodbye!"



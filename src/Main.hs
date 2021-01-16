{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text                  as T
import qualified Graphics.Vty               as V

import qualified Brick.AttrMap              as A
import qualified Brick.Main                 as M
import           Brick.Types                (BrickEvent (..), Widget)
import           Brick.Util                 (fg, on)
import           Brick.Widgets.Core         (hBox, hLimit, padAll, str, txt,
                                             updateAttrMap, vBox, vLimit,
                                             vLimitPercent, withAttr,
                                             withBorderStyle, (<+>), (<=>))
import qualified Brick.Widgets.Dialog       as D

import qualified Brick.Types                as T
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C

data Choice = Home | About | Skills | Experience | Education | Projects | LookingFor
          deriving (Show, Eq)

appEvent :: D.Dialog Choice -> BrickEvent () e -> T.EventM () (T.Next (D.Dialog Choice))
appEvent d (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc []        -> M.halt d
        V.EvKey (V.KChar 'q') [] -> M.halt d
        _                        -> M.continue =<< D.handleDialogEvent ev d
appEvent d _ = M.continue d

titleAttr :: A.AttrName
titleAttr = "title"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings = [
     (B.borderAttr,        fg  V.blue)
     , (titleAttr,     fg  V.cyan)
    ]

dialogMap :: A.AttrMap
dialogMap = A.attrMap V.defAttr
    [ (D.buttonAttr, fg V.magenta)
    , (D.buttonSelectedAttr, V.white `on` V.magenta)
    ]

-- Introduction

introduction :: Widget ()
introduction =
    C.center $
    txt $ "Welcome to Emily's resume!\n\nTo explore the different sections, click ← or →.\nTo exit, click q or esc."


contactInfo :: String
contactInfo = unlines [ ""
    , "Website: https://sometimesemily.codes\n"
    , "Email: hello@emilywoods.me\n"
    , "Github: emilywoods\n"
    ]

aboutMe :: String
aboutMe = unlines [ "Hello, I'm Emily. :)\n"
    , "I'm a senior software engineer, currently working"
    , "as a site reliability engineer. In previous roles, I've"
    , "been a backend engineer, sysadmin and even a chemical engineer.\n"
    , "I like building things (especially tools for developers),"
    , "automation and scalable infrastructure.\n"
    , "I help organise Pyladies Berlin and enjoy technical writing.\n"
    , "When I want a break from computers, I make or upcycle clothes,"
    , "drink tea or pet my dog. But I have many other short-lived hobbies -"
    , "there is so much to learn!"
    ]

contactMe :: Widget ()
contactMe =
    updateAttrMap (A.applyAttrMappings borderMappings) $
    B.borderWithLabel (withAttr titleAttr $ str "How to reach me") $
    hLimit 60 $
    vLimit 10 $
    C.hCenter $
    str $ contactInfo

aboutSection :: Widget ()
aboutSection =
    C.vCenter (str aboutMe)
    <=> contactMe


-- Skills

technologies :: String
technologies = unlines[ "Languages I've worked with"
    , "- Python"
    , "- Java"
    , "- Ruby"
    , " "
    , "Languages I would like to do more with"
    , "- Clojure"
    , "- Rust"
    , " "
    , "Databases"
    , "- CrateDB"
    , " "
    , "Message Processing"
    , "- Kafka "
    , "- Azure IoT Hub and Event Hubs"
    , " "
    , "Infrastructure"
    , "- Docker"
    , "- Terraform"
    , "- Helm/Helmfile"
    , "- SaltStack"
    , "- Kubernetes"
    , "- Prometheus"
    , "- AWS"
    , "- Azure"
    ]

interests :: String
interests = unlines[ "- Distributed systems\n"
    , "- Test driven development\n"
    , "- Scalable infrastructure\n"
    , "- Technical writing\n"
    , "- Creative coding\n"
    , "- Learning and research!\n"
    ]

community :: String
community = unlines [ "- Coach at Django Girls Berlin\n"
    , "- Organiser at PyLadies Berlin\n"
    , "- Blog posts and writing at\nhttps://sometimesemily.codes/posts"
    ]

skillsSections :: [(String, String)]
skillsSections =
    [ ("technologies", technologies)
    , ("interests", interests)
    , ("community involvement", community)
    ]

skillsBlocks :: [Widget ()]
skillsBlocks = skillsBlock <$> skillsSections

skillsBlock :: (String, String) -> Widget ()
skillsBlock (skillTitle, content) =
    updateAttrMap (A.applyAttrMappings borderMappings) $
    withBorderStyle BS.unicodeRounded $
    B.borderWithLabel (withAttr titleAttr $ str skillTitle) $
    C.center $
    str $ content

skillsSection :: Widget ()
skillsSection =
    hBox skillsBlocks

-- Work Experience

workExperiences :: [T.Text]
workExperiences =
    [ "May 2020 - Present Ecosia.org:: Site Reliability Engineer"
    , "Jan 2018 - April 2020 Crate.io (remote/Berlin):: Backend Engineer and SysAdmin"
    , "September 2016 - December 2017 The App Business (London):: Associate Backend Engineer"
    , "November 2013 - June 2015 PWF:: Junior Project and Operations Engineer."
    ]

experienceBlock :: T.Text -> Widget ()
experienceBlock experienceText =
    updateAttrMap (A.applyAttrMappings borderMappings) $
    B.border $
    C.center $ txt $ "  " <> experienceText <> "  "

experienceSection :: Widget ()
experienceSection =
    vBox $ experienceBlock <$> workExperiences

-- Education

education :: String
education = unlines [ "2015-2016 :: MRes Bioengineering, Imperial College London"
    , "Thesis: Acoustic Particle Palpation for Tissue Elasticity Imaging"
    , ""
    , ""
    , ""
    , ""
    , "2009 - 2013 :: BEng Process and Chemical Engineering, University College Cork"
    ]

educationSection :: Widget ()
educationSection =
    updateAttrMap (A.applyAttrMappings borderMappings) $
    B.border $
    C.center $
    str $ education

-- Projects I've worked on

projects :: [(String, String)]
projects =
    [ ("Leabharlann", "A Rust-based CLI tool to keep track of books I'm reading or want to read")
    , ("Nature of Corrode", "Creative code sketches written in Rust")
    , ("Tráta", "A tiny tool for pomodoros")
    , ("Look-Up", "A small Gleam project to tell you when to look up if you wanna see the space station")
    , ("A dhéanamh", "A Python-based tool for keeping organised")
    , ("Emerald", "A Ruby Language with Lisp Syntax")
    ]

projectBlock :: (String, String) -> Widget ()
projectBlock (projectTitle, content) =
    updateAttrMap (A.applyAttrMappings borderMappings) $
    withBorderStyle BS.unicodeRounded $
    B.borderWithLabel (withAttr titleAttr $ str projectTitle) $
    C.center $
    str $ content

projectsSection :: Widget ()
projectsSection =
    vBox $ projectBlock <$> projects

-- Looking for

lookingForList :: [T.Text]
lookingForList =
    [ "◦ An asynchronous, remote-first environment"
    , "◦ To build technology that has a positive impact"
    , "◦ Interesting infrastructure and backend challenges"
    , "◦ Opportunities for technical writing"
    , "◦ An environment which values transparency and feedback"
    ]

lookingForBlock :: T.Text -> Widget ()
lookingForBlock lookingForList =
    C.center $ txt $ "  " <> lookingForList <> "  "

lookingForSection :: Widget ()
lookingForSection = vBox [ updateAttrMap (A.applyAttrMappings borderMappings) $ C.center $ vBox [withAttr titleAttr $ str "What I look for:"]
          , vLimitPercent 80 $ vBox $ lookingForBlock <$> lookingForList
          ]

selection :: D.Dialog Choice -> Widget ()
selection d
    | D.dialogSelection d == Just About = aboutSection
    | D.dialogSelection d == Just Skills = skillsSection
    | D.dialogSelection d == Just Experience = experienceSection
    | D.dialogSelection d == Just Education = educationSection
    | D.dialogSelection d == Just Projects = projectsSection
    | D.dialogSelection d == Just LookingFor = lookingForSection
    | otherwise   = introduction


drawUI :: D.Dialog Choice -> [Widget ()]
drawUI d = [ui]
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str ""
            <=> selection d

initialState :: D.Dialog Choice
initialState = D.dialog (Just " ✨ 🍄 🌱 🍵 ✨ ") (Just (0, choices)) 500
    where
        choices = [ ("Home", Home)
                  , ("About", About)
                  , ("Skills", Skills)
                  , ("Experience", Experience)
                  , ("Education", Education)
                  , ("Projects", Projects)
                  , ("Looks For", LookingFor)
                  ]


-- App definition

theApp :: M.App (D.Dialog Choice) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const dialogMap
          }

main :: IO ()
main = do
    d <- M.defaultMain theApp initialState
    putStrLn "Goodbye!"

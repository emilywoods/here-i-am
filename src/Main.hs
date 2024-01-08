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
    , "Email: hello@sometimesemily.codes\n"
    , "Github: emilywoods\n"
    ]

aboutMe :: String
aboutMe = unlines [ "Hello, I'm Emily. :)\n"
    , "I'm a senior software engineer with an interest in backend and platform"
    , "engineering. I've worked as a platform engineer, a backend developer,"
    , "and even as a chemical engineer.\n"
    , "I like building products that can have a positive impact and tools for"
    , "developers. I appreciate good automation, thoughtful documentation, as"
    , "well as resilient, simplified infrastructure.\n"
    , "When I want a break from computers, I can be found making pottery,"
    , "drinking tea or hanging out with my dog."
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
    , "- Golang"
    , "- Clojure"
    , "- Python"
    , "- Java"
    , "- Ruby"
    , " "
    , "Languages I would like to do more with"
    , "- Rust"
    , "- Gleam"
    , " "
    , "Databases"
    , "- PostgreSQL"
    , "- CrateDB"
    , " "
    , "Infrastructure"
    , "- Docker"
    , "- Terraform"
    , "- Helm/Helmfile"
    , "- SaltStack"
    , "- Nomad"
    , "- Kubernetes"
    , "- AWS"
    , "- Azure"
    , " "
    , "Observability Tooling"
    , "- Honeycomb"
    , "- Grafana"
    , "- Prometheus"
    , "- Datadog"
    ]

interests :: String
interests = unlines[ "- Distributed systems\n"
    , "- Well-tested systems\n"
    , "- Thoughtful documentation\n"
    , "- Scalable infrastructure\n"
    , "- Technical writing\n"
    , "- Developer Tooling\n"
    , "- Creative coding\n"
    , "- Investigation and research\n"
    ]

community :: String
community = unlines [ "- Coached at Django Girls Berlin and Rails Girls London\n"
    , "- Previously an organiser at PyLadies Berlin\n"
    , "- Taught 'Intro to Computer Science' as a volunteer teacher at Redi School\n"
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
    [ "July 2021 - Present CircleCI :: Senior Software Engineer"
    , "May 2020 - June 2021 Ecosia.org :: Site Reliability Engineer"
    , "Jan 2018 - April 2020 Crate.io :: Backend Engineer and SysAdmin"
    , "September 2016 - December 2017 The App Business :: Associate Backend Engineer"
    , "November 2013 - June 2015 PWF :: Junior Project and Operations Engineer."
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
    , ("Glaze", "A small Gleam pottery diary")
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

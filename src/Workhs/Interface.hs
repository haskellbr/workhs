{-# LANGUAGE OverloadedStrings #-}

module Workhs.Interface (mainScreen) where

import           Engager.Exercise
import           Graphics.Vty.Input             (Key (..), Modifier (..))
import           Graphics.Vty.Widgets.Box       (ChildSizePolicy (..),
                                                 setBoxChildSizePolicy, vBox)
import           Graphics.Vty.Widgets.Core      (addToFocusGroup, newFocusGroup,
                                                 onKeyPressed)
import           Graphics.Vty.Widgets.EventLoop (Collection, addToCollection,
                                                 newCollection)
import           Graphics.Vty.Widgets.List      (ActivateItemEvent (..),
                                                 addToList, newList,
                                                 onItemActivated)
import           Graphics.Vty.Widgets.Text      (plainText)
import           System.Exit                    (exitSuccess)
import           Workhs.Exercise

mainScreen :: [Exercise] -> IO Collection
mainScreen exercises = do
                    title <- plainText "tutorial title"
                    list  <- newList 1
                    ui    <- vBox title list
                    _     <- setBoxChildSizePolicy ui $ Percentage 10
                    fg    <- newFocusGroup
                    exitOnCtrlQ fg
                    _     <- addToFocusGroup fg ui
                    coll  <- newCollection
                    main  <- addToCollection coll ui fg
                    _     <- exerciseList list coll main
                    _     <- list `onItemActivated` \(ActivateItemEvent _ switch _) -> switch
                    return coll
                where exercisePage e coll backToMain = do ui <- plainText (exerciseStatement e)
                                                          fg <- newFocusGroup
                                                          fg `onKeyPressed` \_ k _ -> if k == KBS
                                                                                         then backToMain >> return True
                                                                                         else return False
                                                          exitOnCtrlQ fg
                                                          _  <- addToFocusGroup fg ui
                                                          addToCollection coll ui fg
                      exerciseList l coll backToMain = forM_ exercises (\e -> do title <- plainText $ exerciseTitle e
                                                                                 page  <- exercisePage e coll backToMain
                                                                                 addToList l page title)
                      exitOnCtrlQ fg      = fg `onKeyPressed` \_ k modifiers -> if k == KChar 'q' && MCtrl `elem` modifiers
                                                                                   then exitSuccess
                                                                                   else return False



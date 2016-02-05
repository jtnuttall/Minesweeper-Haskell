module Main where
------------------------------------------------------------
-- Minesweeper clone -- Haskell
--
-- Jeremy Nuttall (jtnuttal@usc.edu)
--
-- TODO:
--     - Add difficulties and standardized sizes
--     - Improve UI a bit, maybe
-------------------------------------------------------------

import App.Game

import Graphics.UI.Gtk

-- Construct basic GUI
main :: IO()
main = do
    initGUI
    window <- windowNew

    set window [ windowTitle := "Haskell Minesweeper"
                ,containerBorderWidth := 10
                ,windowResizable := False
               ]

    grid <- gridNew
    gridSetRowSpacing    grid 5
    gridSetColumnSpacing grid 5
    containerAdd window grid

    info <- labelNew (Just "Haskell Minesweeper")

    rowI <- labelNew (Just "Rows:")
    rowE <- entryNew

    clear <- buttonNewWithLabel "Clear"

    colI <- labelNew (Just "Columns:")
    colE <- entryNew
    on colE entryActivated $ runGame window rowE colE clear info grid

    runB  <- buttonNewWithLabel "Run"

    on runB buttonActivated $ runGame window rowE colE clear info grid

    gridAttach grid info  10  0 20  2
    gridAttach grid rowI   0  0  5  1
    gridAttach grid rowE   5  0  5  1
    gridAttach grid colI   0  1  5  1
    gridAttach grid colE   5  1  5  1
    gridAttach grid runB   0  2  5  1
    gridAttach grid clear  5  2  5  1

    widgetShowAll window 
    on window objectDestroy mainQuit
    mainGUI

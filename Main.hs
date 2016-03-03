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

import App.Util (Difficulty(..))
import App.Game

import Graphics.UI.Gtk
import Data.IORef

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

    -- Create game information
    info      <- frameNew
    frameSetLabel info "Game updates:"
    infoBox   <- vBoxNew True 2
    infoLabel <- labelNew (Just "Haskell Minesweeper")
    boxPackStart infoBox infoLabel PackGrow 0
    containerSetBorderWidth infoBox 5
    containerAdd info infoBox

    -- Create row and column labels and entries
    rowI <- labelNew (Just "Rows:")
    rowE <- entryNew

    colI <- labelNew (Just "Columns:")
    colE <- entryNew

    -- Create clear and run buttons
    runB  <- buttonNewWithLabel "Run"
    clear <- buttonNewWithLabel "Clear"

    -- Create difficulty selection
    difficulty <- newIORef Medium
    diff <- frameNew
    frameSetLabel diff "Difficulty:"

    inBox <- vBoxNew True 2
    med   <- radioButtonNewWithLabel "Medium"
    easy  <- radioButtonNewWithLabelFromWidget med "Easy"
    hard  <- radioButtonNewWithLabelFromWidget med "Hard"

    on easy buttonActivated (writeIORef difficulty Easy)
    on med buttonActivated  (writeIORef difficulty Medium)
    on hard buttonActivated (writeIORef difficulty Hard)
    
    boxPackStart inBox easy PackGrow 0
    boxPackStart inBox med  PackGrow 0
    boxPackStart inBox hard PackGrow 0
    containerSetBorderWidth inBox 5

    containerAdd diff inBox

    -- Connect signals
    on rowE entryActivated  $ runGame window rowE colE clear difficulty infoLabel grid
    on colE entryActivated  $ runGame window rowE colE clear difficulty infoLabel grid
    on runB buttonActivated $ runGame window rowE colE clear difficulty infoLabel grid

    -- Construct grid
    gridAttach grid rowI   0  0  5  1
    gridAttach grid rowE   5  0  5  1
    gridAttach grid colI   0  1  5  1
    gridAttach grid colE   5  1  5  1
    gridAttach grid runB   0  2  5  1
    gridAttach grid clear  5  2  5  1
    gridAttach grid diff  10  0  5  3
    gridAttach grid info  15  0 20  3

    widgetShowAll window 
    on window objectDestroy mainQuit
    mainGUI

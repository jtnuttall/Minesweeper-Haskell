module App.Game (runGame) where

import App.Util
import Graphics.UI.Gtk

import Control.Monad
import Data.IORef
import Data.Array.IO
import Data.Array.Unboxed
import Data.Array.Unsafe
import qualified Data.IntSet as S
import qualified Data.Vector as V
import System.Random

-- Run game unless input has errors.
runGame :: Window -> Entry -> Entry -> Button -> Label -> Grid -> IO()
runGame window rowE colE clear info grid = do
    sweepMaybe <- gridGetChildAt grid 0 3
    case sweepMaybe of
        Just sweepNow -> widgetDestroy sweepNow
        Nothing       -> return ()
        
    rMaybe <- readHeadMaybe <$> entryGetText rowE
    cMaybe <- readHeadMaybe <$> entryGetText colE
    case (rMaybe,cMaybe) of
        (Just r, Just c)
            | r > 25    -> labelSetText info "Row value greater than 25.\nWill not continue."
            | c > 30    -> labelSetText info "Column value greater than 30.\nWill not continue."
            | otherwise -> do
                labelSetMarkup info "New game!"

                sweep <- runGame' r c info
                setExpand sweep
                gridAttach grid sweep 0 3 30 30

                widgetShowAll window
                void . on clear buttonActivated $ do
                    widgetDestroy sweep
                    labelSetMarkup info "New game!"

        _ -> labelSetText info "I couldn't read that. Try again."

-- Sets up the board and attaches the gameLogic function to each button
runGame' :: Int -> Int -> Label -> IO Grid
runGame' r c info = do
    sweep <- gridNew
    let (xs, ys) = V.unzip . V.fromList $ [(x,y) | x <- [1..c], y <- [1..r]]

    buttons <- V.replicateM (r*c) $ do
        b <- buttonNewWithLabel "   "
        setExpand b
        return b

    state <- newState r c
    forM_ (V.indexed buttons) $ \(i, b) -> 
        on b buttonActivated $ gameLogic (i+1) b buttons state

    let ones = V.replicate (r*c) 1
    mapM_ (uncurry5 $ gridAttach sweep) $ V.zip5 buttons xs ys ones ones

    return sweep
    
    where 
        {- Local binding is slightly better for gameLogic because it gives 
         - access to r, c, and info without passing them as arguments -}
        gameLogic :: Int -> Button -> V.Vector Button -> State -> IO()
        gameLogic i b buttons state
            | isMine state ! i = endGame False (isMine state) info buttons
            | otherwise        = do
                reveal b (neighbors state ! i) (totalMines state)

                let eliminate ix = do
                        writeArray (visited state) ix True
                        forM_ [x | x <- adjacent ix r, x > 0, x <= r*c] $ \i' -> do
                            wasVisited <- readArray (visited state) i'

                            unless (wasVisited || isMine state ! i') $ do
                                -- Mark current widget as visited
                                writeArray (visited state) i' True

                                reveal (buttons V.! (i'-1))
                                       (neighbors state ! i') 
                                       (totalMines state)

                                when (neighbors state ! i' == 0) (eliminate i')

                eliminate i

                won <- do
                    remaining <- readIORef (totalMines state) 
                    return (remaining == 0)

                if won 
                then endGame True (isMine state) info buttons
                else eliminate i

newState :: Int -> Int -> IO State
newState r c = do
    g <- newStdGen

    {- Ensure unique mine coordinates in O(n) time by putting random values 
     - into an IntSet -}
    let mineCoordinates = S.fromList . take (r*c `quot` 7) $ randomRs (1,r*c) g

    stateM <- newArray (1,r*c) False :: IO (IOUArray Int Bool)
    {- mapMintSet_ is a simple utility function that uses IntSet's built-in 
     - foldr' function specialized to monads and discards the result. -}
    mapMintSet_ (\ix -> writeArray stateM ix True) mineCoordinates

    -- Since we won't need the mutable array again, just do an unsafe freeze
    state <- unsafeFreeze stateM

    -- Count the number of mines adjacent to any given coordinate in the grid
    adjM <- newArray_ (1,r*c) :: IO (IOUArray Int Int)
    forM_ [1..r*c] $ \i -> do
        let count = length [x | x <- adjacent i r, x > 0, x <= r*c, state ! x]
        writeArray adjM i count

    adj   <- unsafeFreeze adjM

    {- We haven't clicked anything yet, so the array for tracking visited 
     - coordinates in elimination is empty -}
    visitedInit <- newArray (1,r*c) False :: IO (IOUArray Int Bool)

    {- Count the unique coordinates -}
    total <- newIORef (r*c - S.size mineCoordinates) :: IO (IORef Int)

    return State {
                  isMine     = state
                , neighbors  = adj
                , visited    = visitedInit
                , totalMines = total
            }

-- Mutate the window if player has won
endGame :: Bool -> UArray Int Bool -> Label -> V.Vector Button -> IO()
endGame hasWon mines info buttons = do
    let (infoText, mineText)
            | hasWon    = ( "<span foreground=\"blue\"><b>You've won! :)</b></span>"
                          , "<span foreground=\"blue\"><b>:)</b></span>"
                          )
            | otherwise = ( "<span foreground=\"red\"><b>You've been blown up! :(</b></span>"
                          , "<span foreground=\"black\"><b>*</b></span>"
                          )

    labelSetMarkup info infoText

    V.forM_ (V.indexed buttons) $ \(i',b') -> do
        when (mines ! (i'+1)) $ do
            lbl <- buttonGetLabelWidget b'
            labelSetMarkup lbl mineText
        widgetSetSensitive b' False

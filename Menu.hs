#!/usr/bin/env runhaskell

{-
This code was written by cheater__ and published here:
http://cheater.posterous.com/haskell-curses
I am still waiting to get permision to republish it under the GPL...
-}

module Menu where
import qualified Graphics.Vty as Vty

getName :: String -> String
getName item = item
-- returns the name of a item.
-- This will become more complicated some day.

itemImage :: String -> Bool -> Vty.Image
itemImage item cursor = do
-- prints out info on an item
    let wfc = Vty.with_fore_color
    let wbc = Vty.with_back_color
    let (indicator, useColor) = if cursor
        then (" > ", True)
        else ("   ", False)
    let attr = if useColor
        then Vty.current_attr `wfc` Vty.black `wbc` Vty.white
        else Vty.current_attr `wfc` Vty.white `wbc` Vty.black
    Vty.string attr $ indicator ++ (getName item)

allocate :: IO Vty.Vty
allocate = do
-- sets up Vty
    vt <- Vty.mkVty
    return vt

deallocate :: Vty.Vty -> IO ()
deallocate vt =
-- frees Vty resouces
    Vty.shutdown vt

handleKeyboard :: Vty.Key -> Int -> Int -> [String] -> Vty.Vty -> IO (Vty.Vty,Int)
handleKeyboard key position offset items vt = case key of
-- handles keyboard input
    Vty.KASCII 'q' -> return (vt,position)
    Vty.KEnter -> return (vt,position)
    Vty.KASCII 'j' -> work (position + 1) offset items vt
    Vty.KDown -> work (position + 1) offset items vt
    Vty.KASCII 'k' -> work (position - 1) offset items vt
    Vty.KUp -> work (position - 1) offset items vt
    _ -> work position offset items vt
	 

work :: Int -> Int -> [String] -> Vty.Vty -> IO (Vty.Vty,Int)
work requestedPosition offset items vt = do
-- displays items 
    let position = max 0 (min requestedPosition (length items - 1))
    Vty.DisplayRegion cols rows <- (Vty.terminal_handle >>= Vty.display_bounds)
    let (cols2, rows2) = (fromEnum cols, fromEnum rows)
    let screenPosition = position + offset
    let offset2 = if screenPosition >= rows2
        then offset - (screenPosition - rows2 + 1)
        else if screenPosition < 0
            then offset - screenPosition
            else offset
    let items2 = drop (0 - offset2) $ zip [0..] items
    let itemImages = map
            (\(line, item) -> itemImage item (line == position))
            items2
    let imagesUnified = Vty.vert_cat itemImages
    let pic = Vty.pic_for_image $ imagesUnified
    Vty.update vt pic
    eventLoop position offset2 items vt

eventLoop :: Int -> Int -> [String] -> Vty.Vty -> IO (Vty.Vty, Int)
eventLoop position offset items vt = do
    ev <- Vty.next_event vt
    case ev of
     Vty.EvKey key _ -> handleKeyboard key position offset items vt
     _ -> eventLoop position offset items vt

displayMenu :: [String] -> IO String
displayMenu items = do
 vty <- allocate
 (vty',pos) <- work 0 0 items vty
 deallocate vty'
 return $ items !! pos

--main = do
-- choice <- displayMenu ["Hi","Bye","The other thing."]
-- print choice
-- the main program

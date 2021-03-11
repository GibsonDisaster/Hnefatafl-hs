module Main where
    import UI.NCurses
    import qualified Data.Map as M
    import Types
    import Control.Monad.IO.Class
    import Data.Maybe (isNothing)

    main :: IO ()
    main = runCurses $ do
        setEcho False
        setCursorMode CursorInvisible
        wc <- newColorID ColorWhite ColorBlue 1
        bc <- newColorID ColorBlack ColorBlue 2
        dc <- newColorID ColorBlue ColorBlue 3
        w <- defaultWindow
        waitFor w (initGS wc bc dc)

    waitFor :: Window -> GameState -> Curses ()
    waitFor w = loop where
        loop gs' = do
            let current_tile = M.lookup (cursorPos gs') (board gs')
            updateWindow w (clear
                                >> mapM (drawBoard gs') (M.toList (board gs'))
                                >> uncurry moveCursor (cursorPos gs')
                                >> drawString "X"
                                >> moveCursor 8 20
                                >> drawString (show (selectedPiece gs'))
                                >> moveCursor 10 20
                                >> drawString (show (cursorPos gs'))
                                >> moveCursor 9 20
                                >> drawString (show current_tile))
            render
            ev <- getEvent w Nothing
            case ev of
                Nothing -> loop gs'
                Just ev' -> case ev' of
                                (EventCharacter 'w') -> loop (moveGameCursor gs' (-1, 0))
                                (EventCharacter 's') -> loop (moveGameCursor gs' (1, 0))
                                (EventCharacter 'a') -> loop (moveGameCursor gs' (0, -1))
                                (EventCharacter 'd') -> loop (moveGameCursor gs' (0, 1))
                                (EventCharacter 'p') -> loop gs'
                                (EventCharacter ' ') -> loop (changeSelectedPiece gs')
                                (EventCharacter 'q') -> return ()
                                _ -> loop gs'

    drawBoard :: GameState -> (Position, (Maybe Piece, PosType)) -> Update ()
    drawBoard gs' ((y, x), (Nothing, Normal)) = moveCursor y x >> setColor (defColorID gs') >> drawString "."
    drawBoard gs' ((y, x), (Nothing, Castle)) = moveCursor y x >> setColor (whiteColorID gs') >> drawString "C"
    drawBoard gs' ((y, x), (Just (Piece c k), _)) = moveCursor y x >> if k then setColor wc >> drawString "K" 
                                                                           else (if c == Black then setColor bc else setColor wc) >> drawString "o"
        where
            wc = whiteColorID gs'
            bc = blackColorID gs'

    saveBoard :: Board -> String -> IO()
    saveBoard b n = do
        let boardStr = concatMap createBoardStr (M.toList b)
        writeFile n boardStr

    createBoardStr :: (Position, (Maybe Piece , PosType)) -> String
    createBoardStr (_, (Nothing, Normal)) = "."
    createBoardStr (_, (Nothing, Castle)) = "C"
    createBoardStr (_, (Just (Piece c k), _))
      | c == Black = "b"
      | k = "K"
      | otherwise = "w"

    loadBoard :: String -> IO Board
    loadBoard fileName = do
        let b = blankBoard
        fileContents <- readFile fileName
        let pieces = getPieces fileContents
        return b

    getPieces :: String -> [(Position, Maybe Piece)]
    getPieces boardStr = undefined

    getLegalMoves :: Board -> Position -> [Position]
    getLegalMoves board (x, y) = validateMoves board $ [(x, p) | p <- [1..10]] ++ [(q, y) | q <- [1..10]]

    validateMoves :: Board -> [Position] -> [Position]
    validateMoves b [] = []
    validateMoves b (x:xs) = if isNothing (getPieceAtPos b x) then [x] ++ validateMoves b xs else validateMoves b xs
module Types where
    import qualified Data.Map as M
    import UI.NCurses (ColorID)

    blankBoard :: Board
    blankBoard = M.fromList [((x, y), (Nothing, Normal)) | x <- [0..10], y <- [0..10]]

    startingBoard :: Board
    startingBoard = finalBoard
        where finalBoard = insertDefenders $ insertAttackers $ insertCastles blankBoard

    insertCastles :: Board -> Board
    insertCastles b = M.insert (5, 5) (Nothing, Castle)
        $ M.insert (10, 10) (Nothing, Castle)
        $ M.insert (10, 0) (Nothing, Castle)
        $ M.insert (0, 10) (Nothing, Castle)
        $ M.insert (0, 0) (Nothing, Castle) b

    attacker :: Maybe Piece
    attacker = Just (Piece Black False)

    defender :: Maybe Piece
    defender = Just (Piece White False)

    king :: Maybe Piece
    king = Just (Piece White True)

    insertAttackers :: Board -> Board
    insertAttackers b =  M.insert (1, 5) (attacker, Normal) -- top-side attackers
        $ M.insert (0, 7) (attacker, Normal)
        $ M.insert (0, 6) (attacker, Normal)
        $ M.insert (0, 5) (attacker, Normal)
        $ M.insert (0, 4) (attacker, Normal)
        $ M.insert (0, 3) (attacker, Normal)
        $ M.insert (9, 5) (attacker, Normal) -- bottom-side attackers
        $ M.insert (10, 7) (attacker, Normal)
        $ M.insert (10, 6) (attacker, Normal)
        $ M.insert (10, 5) (attacker, Normal)
        $ M.insert (10, 4) (attacker, Normal)
        $ M.insert (10, 3) (attacker, Normal)
        $ M.insert (5, 9) (attacker, Normal) -- right-side attackers
        $ M.insert (7, 10) (attacker, Normal)
        $ M.insert (6, 10) (attacker, Normal)
        $ M.insert (5, 10) (attacker, Normal)
        $ M.insert (4, 10) (attacker, Normal)
        $ M.insert (3, 10) (attacker, Normal)
        $ M.insert (5, 1) (attacker, Normal) -- left-side attackers
        $ M.insert (7, 0) (attacker, Normal)
        $ M.insert (6, 0) (attacker, Normal)
        $ M.insert (5, 0) (attacker, Normal)
        $ M.insert (4, 0) (attacker, Normal)
        $ M.insert (3, 0) (attacker, Normal) b

    insertDefenders :: Board -> Board
    insertDefenders b = M.insert (6, 4) (defender, Normal)
        $ M.insert (6, 6) (defender, Normal)
        $ M.insert (4, 6) (defender, Normal)
        $ M.insert (4, 4) (defender, Normal)
        $ M.insert (5, 7) (defender, Normal)
        $ M.insert (5, 6) (defender, Normal)
        $ M.insert (4, 5) (defender, Normal)
        $ M.insert (3, 5) (defender, Normal)
        $ M.insert (5, 4) (defender, Normal)
        $ M.insert (5, 3) (defender, Normal)
        $ M.insert (7, 5) (defender, Normal)
        $ M.insert (6, 5) (defender, Normal) b

    initGS :: ColorID -> ColorID -> ColorID -> GameState
    initGS wc bc dc = GameState (5, 5) WhiteTurn wc bc dc startingBoard Nothing

    moveGameCursor :: GameState -> (Integer, Integer) -> GameState
    moveGameCursor old@(GameState (x1, y1) c wc bc dc b selPiece) (x2, y2) = if onScreen newPos then GameState newPos c wc bc dc b selPiece else old
        where
            newPos = (x1 + x2, y1 + y2)
            onScreen = \(x, y) -> (x >= 0 && x <= 10) && (y >= 0 && y <= 10)

    {- Start of Type Definitions -}

    type Position = (Integer, Integer)

    data PieceColor = Black | White deriving (Show, Eq, Ord)

    data PosType = Normal | Castle deriving (Show, Eq, Ord)

    -- (x, y) color isKing
    data Piece = Piece PieceColor Bool deriving (Show, Eq, Ord)

    type Board = M.Map Position (Maybe Piece, PosType)

    data GamePhase = StartMenu
                   | WhiteTurn
                   | BlackTurn
                   | Paused
                   | Victory PieceColor
                   deriving (Show, Eq, Ord)

    data GameState = GameState {
        cursorPos :: (Integer, Integer),
        gamePhase :: GamePhase,
        whiteColorID :: ColorID,
        blackColorID :: ColorID,
        defColorID :: ColorID,
        board :: Board,
        selectedPiece :: Maybe Piece
    } deriving (Show, Eq)
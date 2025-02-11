data Color = White | Black | No deriving (Eq)
data Piece = Normal | King | Empty deriving (Eq)

type Cell = (Color,Piece) 

type Board = [[Cell]]

type Pos = (Int, Int)

type Move = [Pos]

showCell :: Cell -> String
showCell (_,Empty) = "*"
showCell (White,Normal) = "w"
showCell (Black,Normal) = "b"
showCell (White,King) = "W"
showCell (Black,King) = "B"

showRow :: Int -> [Cell] -> String
showRow rowIdx row = show rowIdx ++ " " ++ unwords (map showCell row)

showBoard :: Board -> String
showBoard board = foldr (\row acc -> row ++ "\n" ++ acc)  "" ( colHeader : (zipWith showRow [0..] board))
    where 
        colHeader = "  " ++ unwords (map show [0 .. length (head board) - 1])

printBoard :: Board -> IO()
printBoard board = putStrLn (showBoard board)

changeRow :: Int -> Cell -> [Cell] -> [Cell]
changeRow pos c (x:xs)
    | pos == 0 = c : xs
    | otherwise = x : changeRow (pos-1) c xs

changeBoard :: Int -> Int -> Cell -> Board -> Board
changeBoard 0 dx c (x:xs)  = changeRow dx c x : xs
changeBoard dy dx c (x:xs) = x : changeBoard (dy - 1) dx c xs

inBorder :: Board -> Pos -> Bool
inBorder board (x,y) = x >= 0 && y >= 0 && x < boardWidth && y < boardHeight
    where
        boardHeight = length board 
        boardWidth = length (head board)

checkLegalMove :: Board -> Pos -> Pos -> Bool
checkLegalMove board (startX, startY) (x,y) 
    | not (inBorder board (x,y)) = False
    | endCell /= (No,Empty) = False
    | currentCell == (No,Empty) = False
    | posTake = checkLegalTake board (startX, startY) (x, y)
    | allTakesOnPos board (startX, startY) /= [] = False
    | otherwise = case currentCell of 
                    (Black, Normal) -> legalNormalBlackMove
                    (White, Normal) -> legalNormalWhiteMove
                    (_, King) -> False
    where 
        endCell = board !! y !! x
        currentCell = board !! startY !! startX
        posTake = abs (startX - x) > 1 && abs(startY - y) > 1
        legalNormalBlackMove = startX - x == -1 && startY - y == -1 || startX - x == 1 && startY - y == -1
        legalNormalWhiteMove = startX - x == -1 && startY - y == 1  || startX - x == 1 && startY - y == 1

checkLegalTake :: Board -> Pos -> Pos -> Bool
checkLegalTake board (sx, sy) (x, y) = isOpponent (enemyCell) (fst (board !! y !! x))
    where
      ex = x + (sx - x) `div` 2 
      ey = y + (sy - y) `div` 2 
      enemyCell = board !! ey !! ex 
      isOpponent (c, _) playerColor = c /= No && c /= playerColor 

executeTake :: Board -> Pos -> Pos -> Board
executeTake board (sx, sy) (ex, ey) = changeBoard sy sx (No, Empty) $ changeBoard ey ex (startColor, startPiece) $ changeBoard (sy + dy) (sx + dx) (No, Empty) board
    where
        (startColor, startPiece) = board !! sy !! sx
        dx = (ex - sx) `div` 2
        dy = (ey - sy) `div` 2

executeMove :: Board -> Pos -> Pos -> Board
executeMove board (startX, startY) (endX, endY) = changeBoard endY endX (board !! startY !! startX) $ changeBoard startY startX (No, Empty) board

allTakesOnPos :: Board -> Pos -> [Move]
allTakesOnPos board (x, y) = 
    [[(x, y) , (tx, ty)] | (dx, dy) <- directions,
    let ex = x + dx,
    let ey = y + dy,
    inBorder board (ex, ey),
    isOpponent (enemyCell ex ey) (fst (board !! y !! x)),
    let tx = ex + dx,
    let ty = ey + dy,
    inBorder board (tx,ty),
    board !! ty !! tx == (No, Empty)]
    where 
        directions = [(-1, -1), (1, -1), (-1, 1), (1, 1)]
        enemyCell nx ny = board !! ny !! nx
        isOpponent (c, _) playerColor = c /= No && c /= playerColor

---ENGING STUFF---

allMovesOnPos :: Board -> Pos -> [Move]
allMovesOnPos board (x, y) = [ [(x, y), (nx, ny)] | (dx, dy) <- direction c , let nx = x + dx , let ny = y + dy , checkLegalMove board (x, y) (nx, ny)]
    where 
        c = fst (board !! y !! x)
        direction Black = [(-1, 1), (1, 1)]
        direction White = [(-1, -1) , (1, -1)]

allNormalMoves :: Board -> Color -> [Move]
allNormalMoves board c = concatMap (allMovesOnPos board) playerPos
    where
        playerPos = [(x, y) | y <- [0..7] , x <- [0..7] , isPlayer (board !! y !! x) c]
        isPlayer (c, Normal) color = c == color
        isPlayer _ _ = False

findChains :: Board -> Pos -> Move -> [Move]
findChains board pos currentC = 
    case allTakesOnPos board pos of
        [] -> if length currentC == 1 then [] else [currentC]
        moves -> concatMap (\nextPos -> findChains (executeTake board pos nextPos) nextPos (addMove nextPos currentC)) (getLastMove moves) 
    where 
        getLastMove moves = map last moves
        addMove pos move = move ++ [pos] 

executeMoveChain :: Board -> Move -> Board
executeMoveChain board [pos] = board 
executeMoveChain board (start:next:rest)
  | isTakeMove start next = executeMoveChain (executeTake board start next) (next : rest)
  | otherwise             = executeMoveChain (executeMove board start next) (next : rest)
  where
    isTakeMove (sx, sy) (ex, ey) = abs (ex - sx) > 1 || abs (ey - sy) > 1

allPlays :: Board -> Color -> [Move]
allPlays board color = if null allTakes then allNormalMoves board color else allTakes
    where
        allTakes = concatMap (\pos -> findChains board pos [pos]) playerPos
        playerPos = [(x, y) | y <- [0..7], x <- [0..7], isPlayer (board !! y !! x) color]
        isPlayer (c, Normal) col = c == col
        isPlayer (c, King) col = c == col
        isPlayer _ _ = False

minimax :: Board -> Color -> Int -> Bool -> Int
minimax board playerColor depth maximizingPlayer
  | depth == 0 || null legalMoves = evalBoard board playerColor
  | maximizingPlayer = maximum [minimax (executeMoveChain board move) playerColor (depth - 1) False | move <- legalMoves]
  | otherwise = minimum [minimax (executeMoveChain board move) playerColor (depth - 1) True | move <- legalMoves]
  where
    legalMoves = allPlays board currentPlayer
    currentPlayer = if maximizingPlayer then playerColor else opponentColor
    opponentColor = if playerColor == White then Black else White

findBestMove :: Board -> Color -> Int -> (Int,Move)
findBestMove board playerColor depth =  (maximum moveScores)
  where
    moves = allPlays board playerColor
    moveScores = [(minimax (executeMoveChain board move) playerColor (depth - 1) False, move) | move <- moves]

--- DONT LIKE IT! ---
evalBoard :: Board -> Color -> Int
evalBoard board playerColor = pieceScore + positionScore + captureOpportunityScore - captureRiskScore - promotionRiskScore
  where
    pieceScore = sum [pieceValue color piece | row <- board, (color, piece) <- row]
    
    positionScore = sum [positionValue color (x, y) | (y, row) <- zip [0..] board, (x, (color, piece)) <- zip [0..] row, color /= No]
    
    captureOpportunityScore = sum [captureOpportunityValue (x, y) | (y, row) <- zip [0..] board, (x, (color, piece)) <- zip [0..] row, color == playerColor, piece /= Empty]

    captureRiskScore = sum [captureRiskValue (x, y) | (y, row) <- zip [0..] board, (x, (color, piece)) <- zip [0..] row, color == playerColor, piece /= Empty]

    promotionRiskScore = sum [promotionRiskValue color (x, y) | (y, row) <- zip [0..] board, (x, (color, piece)) <- zip [0..] row, color /= No, piece == Normal]

    pieceValue :: Color -> Piece -> Int
    pieceValue color Normal
      | color == playerColor = 10
      | otherwise = -10
    pieceValue color King
      | color == playerColor = 30
      | otherwise = -30
    pieceValue _ Empty = 0

    positionValue :: Color -> Pos -> Int
    positionValue color (x, y)
      | color == playerColor = rowWeight y  
      | otherwise = -rowWeight (7 - y)    

    rowWeight :: Int -> Int
    rowWeight row
      | row == 0 || row == 7 = 5  
      | row == 1 || row == 6 = 10
      | row == 2 || row == 5 = 15
      | row == 3 || row == 4 = 20

    captureOpportunityValue :: Pos -> Int
    captureOpportunityValue pos = if not (null (allTakesOnPos board pos)) then 20 else 0

    captureRiskValue :: Pos -> Int
    captureRiskValue pos = if pos `elem` concat opponentTakes then 15 else 0

    opponentTakes = concatMap (allTakesOnPos board) opponentPositions
    opponentPositions = [(x, y) | y <- [0..7], x <- [0..7], fst (board !! y !! x) == opponentColor]

    opponentColor = if playerColor == White then Black else White

    promotionRiskValue :: Color -> Pos -> Int
    promotionRiskValue White (_, 1) = 10  
    promotionRiskValue Black (_, 6) = 10  
    promotionRiskValue _ _ = 0

------------------------------------------------------------------------------------------------------------------------------------------------


initBoard :: Board
initBoard = [
           [(No, Empty)    , (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal)],
           [(Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal), (No, Empty)],
           [(No, Empty)    , (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty)],
           [(No, Empty)    , (White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal)],
           [(White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty)]
        ]
testBoard :: Board
testBoard = [
           [(Black, Normal), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (White, Normal), (No, Empty), (White, Normal), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (Black, Normal), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (White, Normal), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (White, Normal), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)]
        ]

test = [
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (White, Normal), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (Black, Normal), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)]
        ]

main = do 
    printBoard test
    print ("take")
    print (checkLegalMove test (2,2) (4,4)) -- true
    print (checkLegalMove test (2,2) (0,0)) -- true

    print ("normal move")
    print (checkLegalMove test (1, 1) (0, 2)) -- true
    print (checkLegalMove test (1, 1) (2, 2)) -- false
    print (checkLegalMove test (1, 1) (0, 0)) -- false
    print (checkLegalMove test (1, 1) (2, 0)) -- false

    print ("normal move")
    print (checkLegalMove test (3, 3) (2, 4)) -- true
    print (checkLegalMove test (3, 3) (4, 4)) -- true
    print (checkLegalMove test (3, 3) (2, 2)) -- false
    print (checkLegalMove test (3, 3) (4, 2)) -- false

    print ("test no move bc take")
    print (checkLegalMove test (2, 2) (3, 1)) -- false
    -- print (allPlays testBoard Black)
    -- print (evalBoard testBoard Black)
    -- print (findBestMove testBoard Black 1)
    -- print (findBestMove testBoard Black 2)
    -- print (findBestMove testBoard Black 3)
    -- print (findBestMove testBoard Black 4)
    -- print (findBestMove testBoard Black 5)
    -- print (findBestMove testBoard Black 6)
    -- print (findBestMove testBoard Black 7)
    -- print (findBestMove testBoard Black 8)
    -- print (findBestMove testBoard Black 9)
    -- print (findBestMove testBoard Black 10)

import System.IO
import System.Random
import Data.Array


--------------------------------------------------------
-- 
-- Data definitions
-- 
--------------------------------------------------------

-- Game state: includes info about
-- the current board
-- the score
-- whose turn it is
-- turns played so far
data GameState = Game Board Score Active Int

-- board: its dimensions, and the actual table
-- the table is implemented using Array instead of List to speed up access to elements
type CellMatrix = Array (Int,Int) Cell
-- data Board = Board Int Int [[Cell]]
data Board = Board Int Int CellMatrix
-- board helper methods
instance Show Board where show board = showBoard board
-- to string
showBoard :: Board -> String
showBoard (Board rows cols board) = topRow++boardRows
  where board' = [[getCell board x y | y<-[1..cols]] | x<-[1..rows]]
        topRow = boardHorizontalSeparator '+' "---+" cols
        midRow = boardHorizontalSeparator '|' "   |" cols
        boardRows = showBoardRows board' topRow midRow
--
showBoardRows :: [[Cell]] -> String -> String -> String
showBoardRows [] _ _ = ""
showBoardRows (bx:bxs) topRow midRow = midRow++row++midRow++topRow++moreRows
  where row = showBoardRowCells bx
        moreRows = showBoardRows bxs topRow midRow
--
showBoardRowCells :: [Cell] -> String
showBoardRowCells cells = '|':((concatMap showBoardRowCellsFunc cells)++"\n")
  where showBoardRowCellsFunc cell = ' ':((show cell)++" |")
--
boardHorizontalSeparator :: Char -> String -> Int -> String
boardHorizontalSeparator c s cols = c:((concat (take cols $ repeat s))++"\n")

-- a board cell: S, O, or empty
data Cell = S | O | Empty deriving Eq
-- cell helper methods
instance Show Cell where show cell = showCell cell
-- to string
showCell :: Cell -> String
showCell S = "S"
showCell O = "O"
showCell Empty = " "
-- parse Int
intToCell :: Int -> Cell
intToCell (-1) = Empty
intToCell 0 = O
intToCell 1 = S
-- parse String
stringToCell :: String -> Cell
stringToCell "S" = S
stringToCell "O" = O

-- score: points for player 1 and points for player 2
data Score = Score Int Int

-- whose turn is it?
data Active = Player1 | Player2 deriving (Eq,Show)


-- actual player: has an ID, and a strategy
data Player = Player PlayerID Strategy
-- player to string: print its ID
instance Show Player where show (Player id _) = show id

-- Player ID
data PlayerID = User | EasyAI | NormalAI | HardAI deriving Show

-- strategy: a function that returns a move, give the game state
type Strategy = (GameState -> IO Move)

-- alias for a player pair
type Players = (Player,Player)


-- move: the cell, and where in the table it goes
data Move = Move Cell Position

-- position: alias for int pair (used for moves)
type Position = (Int,Int)

-- alias for int pair (used in prompts)
type Range = (Int,Int)



--------------------------------------------------------
-- 
-- Strategies
-- 
--------------------------------------------------------

-- execute the player's strategy to get a move
evaluateStrat :: GameState -> Player -> IO Move
evaluateStrat game (Player _ strat) = strat game


-- Human strat is to just read move from system in
humanStrat :: Strategy
humanStrat (Game board _ _ _) = do 
  putStrLn "" >> movePrompt board


-- Easy AI: just plays random moves
easyAIStrat :: Strategy
easyAIStrat (Game board _ _ _) = do
  putStrLn "IA pensant moviment..."
  let free = emptyCells board
      freeLen = length free
  num <- randomRIO (0,1)
  pos <- randomRIO (0,freeLen-1)
  return $ Move (intToCell num) (free!!pos)


-- Normal AI: plays 30% random moves, and 70% smart moves
normalAIStrat :: Strategy
normalAIStrat game = do
  num <- randomRIO (1,100)
  if (num::Int)<=30 then easyAIStrat game
                    else hardAIStrat game


-- Hard AI: plays 'smart' moves using a short minimax:
-- makes an 'SOS' if possible, 
-- otherwise moves somewhere for the enemy to get the least possible number of 'SOS' in the next move
hardAIStrat :: Strategy
hardAIStrat (Game _ _ _ 0) = return $ Move S (1,1)  -- hardcoded opening move
hardAIStrat (Game board _ _ _) = do
  putStrLn "IA pensant moviment..."
  stdGen <- getStdGen
  let moves = allMoves board
      moveLen = length moves
      movesSublist = randomTake (min 100 moveLen) stdGen moves (moveLen-1)  -- evaluate 100 moves tops, otherwise too slow!
      moveEvals = map (evalMove board) movesSublist
      bestMoves = improveBestFilter board $ keepBestMoves moveEvals
  getRandomMove bestMoves -- choose a random move amongst those with best value

-- take n random moves from l
randomTake :: RandomGen g => Int -> g -> [Move] -> Int -> [Move]
randomTake 0 _ _ _ = []
randomTake n g l len = x:randomTake (n-1) g' (a++b) (len-1)
  where (i,g') = randomR (0,len) g
        (a,x:b) = splitAt i l

-- following methods are supposed to refine original strategy:
-- if player can't make SOS, but can stop enemy from making SOS in next move,
-- then try to get following pattern on the board: S-Empty-Empty-S
-- it is easy to see that when no other empty positions are left, enemy will give player a SOS no matter what move he makes
rockOutMove :: Board -> Move -> Int -> Int -> Int
rockOutMove (Board rows cols b) (Move S (x,y)) xMov yMov
  | lastX>rows||lastX<1||lastY>cols||lastY<1 = 0
  | cell2==Empty&&cell3==Empty&&cell4==S = 1
  | cell2==Empty&&cell3==Empty&&cell4==Empty = 0
  | otherwise = -1
  where (lastX,lastY) = (x+3*xMov,y+3*yMov)
        (cell2,cell3,cell4) = (getCell b (x+xMov) (y+yMov),getCell b (x+2*xMov) (y+2*yMov),getCell b lastX lastY)
--
weedOut :: Board -> [(Int,Move)] -> [(Int,Move)]
weedOut board moveEvals
  | (length altMoves)>0 = keepBestMoves altMoves
  | otherwise = moveEvals 
  where moves = map snd moveEvals
        altMoves = weedOutFilter board moves
--
weedOutGood :: Board -> Move -> (Bool,Int)
weedOutGood board move = (a>0&&b==0,a)
  where (a,b) = foldr (foldrGoodFunc board move) (0,0) xyMoves
--
foldrGoodFunc :: Board -> Move -> (Int,Int) -> (Int,Int) -> (Int,Int)
foldrGoodFunc board move (xMov,yMov) (a,b)
  | val==(-1) = (a,b+1)
  | val==1 = (a+1,b)
  | otherwise = (a,b)
  where val = rockOutMove board move xMov yMov
--
weedOutFilter :: Board -> [Move] -> [(Int,Move)]
weedOutFilter _ [] = []
weedOutFilter board (move:mxs)
  | good = (val,move):moveEvals
  | otherwise = moveEvals
  where (good,val) = weedOutGood board move
        moveEvals = weedOutFilter board mxs
--
improveBestFilter :: Board -> [(Int,Move)] -> [(Int,Move)]
improveBestFilter board moveEvals@((1,_):_) = weedOut board moveEvals
improveBestFilter _ moveEvals = moveEvals


-- pick a random move from the list
getRandomMove :: [(Int,Move)] -> IO Move
getRandomMove bestMoves = do
  let len = length bestMoves
  pos <- randomRIO (0,len-1)
  return $ snd (bestMoves!!pos)

-- rate the move depending on how many SOS the player and the enemy can make
evalMove :: Board -> Move -> (Int,Move)
evalMove board move@(Move cell _) = (2*(valSOS+valNoSOS)+valCell,move)
  where board' = applyMoveToBoard board move
        valSOS = pointsCount board' move
        valNoSOS = if valSOS==0 then enemyEvalMove board' else 0
        valCell = getCellValue cell valSOS
--

getCellValue :: Cell -> Int -> Int
getCellValue cell points
  | points>0 = if cell==S then 0 else 1
  | otherwise = if cell==S then 1 else 0

-- how many SOS can the enemy make with the given board?
enemyEvalMove :: Board -> Int
enemyEvalMove board = val
  where val = if (length moves)>0 then -(maximum moveEvals) else 0
        moves = allMoves board
        moveEvals = map enemyEval moves
        enemyEval m = pointsCount (applyMoveToBoard board m) m

-- keep only the moves with the highest rating
keepBestMoves :: [(Int,Move)] -> [(Int,Move)]
keepBestMoves (moveEval:[]) = [moveEval]
keepBestMoves (moveEval@(val1,move1):xs)
  | val1>val2 = [moveEval]
  | val1<val2 = bestMoves
  | otherwise = moveEval:bestMoves
  where bestMoves@((val2,move2):_) = keepBestMoves xs

-- generate all possible moves
allMoves :: Board -> [Move]
allMoves board = [Move cell pos | cell<-[S,O],pos<-emptyCells board]

-- find all empty cells in the board
--emptyCells' :: Board -> [Position]
--emptyCells' (Board _ _ board) = checkRow 1 board
--  where checkRow _ [] = []
--        checkRow x (rx:rxs) = (checkCells x 1 rx)++(checkRow (x+1) rxs)
--        checkCells _ _ [] = []
--        checkCells x y (cx:cxs)
--          | cx==Empty = (x,y):xs
--          | otherwise = xs
--          where xs = checkCells x (y+1) cxs
--
emptyCells :: Board -> [Position]
emptyCells (Board _ _ board) = [(x,y) | ((x,y),Empty)<-assocs board]


--------------------------------------------------------
-- 
-- Game messages
-- 
--------------------------------------------------------

-- get move message: who played what move, and how many points did they make
moveMessage :: Players -> Active -> Move -> Int -> Bool -> String
moveMessage (player1,player2) player (Move cell (x,y)) points over = s1++s2++s3
  where s1 = "\nTorn del "++(show player)++" "++(show (if player==Player1 then player1 else player2))++"\n"
        s2 = "Posa una '"++(show cell)++"' a la casella ("++(show x)++", "++(show y)++")\n"
        s3 = if points>0 then "Ha fet "++(show points)++" SOS"++s4++"\n"
                         else "No ha fet cap SOS"++s4++"\n"
        s4 = if over then ""
                     else if points>0 then ", i torna a tirar"
                                      else ", i ara li toca al "++(show (nextPlayer player)) 

-- get score message: how many SOS they made, and who won the game
scoreMessage :: Players -> Score -> String
scoreMessage (player1,player2) (Score score1 score2) = player1Score++player2Score++winner
  where player1Str = (show Player1)++" "++(show player1)
        player2Str = (show Player2)++" "++(show player2)
        player1Score = "Puntuació "++player1Str++": "++(show score1)++"\n"
        player2Score = "Puntuació "++player2Str++": "++(show score2)++"\n"
        winner
          | score1>score2 = player1Str++" guanya!\n"
          | score1<score2 = player2Str++" guanya!\n"
          | otherwise = "Empat!\n"



--------------------------------------------------------
-- 
-- Game logic
-- 
--------------------------------------------------------

-- ask the corresponding player for his move
getMove :: GameState -> Players -> IO Move
getMove game@(Game _ _ Player1 _) (player1,_) = evaluateStrat game player1
getMove game@(Game _ _ Player2 _) (_,player2) = evaluateStrat game player2


-- make move and return new board
--applyMoveToBoard :: Board -> Move -> Board
--applyMoveToBoard (Board rows cols board) (Move cell (x,y)) = Board rows cols board'
--  where (up,mid:down) = splitAt (x-1) board
--        (left,cen:right) = splitAt (y-1) mid
--        board' = up++[left++[cell]++right]++down
--
applyMoveToBoard :: Board -> Move -> Board
applyMoveToBoard (Board rows cols board) (Move cell (x,y)) = Board rows cols board'
  where board' = updateBoard x y cell board

-- get cell at (x,y)
getCell :: CellMatrix -> Int -> Int -> Cell
--getCell board x y = board!!(x-1)!!(y-1)
getCell board x y = board!(x,y)

-- update a cell in array
updateBoard :: Int -> Int -> Cell -> CellMatrix -> CellMatrix
updateBoard x y v a = a//[((x,y),v)]

-- check if a SOS point was made with this move, in a particular direction (xMov,yMov)
sosPoints :: Board -> Move -> Int -> Int -> Int
sosPoints (Board rows cols b) (Move S (x,y)) xMov yMov
  | lastX>rows||lastX<1||lastY>cols||lastY<1 = 0
  | cell1==S&&cell2==O&&cell3==S = 1
  | otherwise = 0
  where (lastX,lastY) = (x+2*xMov,y+2*yMov)
        (cell1,cell2,cell3) = (getCell b x y,getCell b (x+xMov) (y+yMov),getCell b lastX lastY)
--
sosPoints (Board rows cols b) (Move O (x,y)) xMov yMov
  | lastX>rows||lastX<1||lastY>cols||lastY<1||frstX>rows||frstX<1||frstY>cols||frstY<1 = 0
  | cell1==S&&cell2==O&&cell3==S = 1
  | otherwise = 0
  where (frstX,frstY,lastX,lastY) = (x-xMov,y-yMov,x+xMov,y+yMov)
        (cell1,cell2,cell3) = (getCell b frstX frstY,getCell b x y,getCell b lastX lastY)


-- all 8 directions (4 diagonals, 2 horizontal, 2 vertical)
xyMoves :: [(Int,Int)]
xyMoves = [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]

-- calculate how many SOS points were made with this move
-- an S-cell move can get 8 SOS from all 8 directions, whereas O-cell move can get just 4
pointsCount :: Board -> Move -> Int
pointsCount board move@(Move cell _) = foldr doCount 0 moves
  where moves = if cell==S then xyMoves else take 4 xyMoves
        doCount (x,y) points = points+(sosPoints board move x y)


-- add points to the player that made the move
updateScore :: Score -> Active -> Int -> Score
updateScore (Score score1 score2) Player1 points = Score (score1+points) score2
updateScore (Score score1 score2) Player2 points = Score score1 (score2+points)


-- who plays after who
nextPlayer :: Active -> Active
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1


-- get the new game state after applying the given move
-- don't forget to print current game state!
updateGameState :: GameState -> Players -> Move -> IO GameState
updateGameState (Game board score player turn) players move = do
  let board' = applyMoveToBoard board move
      points = pointsCount board' move
      score' = updateScore score player points
      player' = if points>0 then player else nextPlayer player
      game' = Game board' score' player' (turn+1)
  putStrLn $ moveMessage players player move points (gameOver game')
  putStrLn $ show board'
  return game'


-- is this game finished?
gameOver :: GameState -> Bool
gameOver (Game (Board rows cols _) _ _ turn) = rows*cols==turn


-- have the current player play a move, and continue with game loop
play :: GameState -> Players -> IO()
play game@(Game board score player turn) players = do
  move <- getMove game players
  game' <- updateGameState game players move
  gameLoop game' players

-- print result if the game is finished, or keep playing
gameLoop :: GameState -> Players -> IO()
gameLoop game@(Game _ score _ _) players = do
  if gameOver game then putStrLn $ '\n':(scoreMessage players score)
                   else play game players



--------------------------------------------------------
-- 
-- Game initialization and input prompts
-- 
--------------------------------------------------------

-- show message, then read Int value from input
prompt :: String -> IO Int
prompt s = putStrLn s >> getLine >>= return . read

-- show message and read input int -then check if it's within range
-- if it isn't, ask again for input int
intPrompt :: String -> Range -> IO Int
intPrompt s r@(lo,hi) = do
  x <- prompt s
  let errorMessage = "\nEl valor ha d'estar entre "++(show lo)++" i "++(show hi)++"!"
  if x>=lo&&x<=hi then return x
                  else putStrLn errorMessage >> intPrompt s r


-- move prompt
movePrompt :: Board -> IO Move
movePrompt board@(Board _ _ b) = do 
  c <- letterPrompt
  pos@(x,y) <- positionPrompt board
  let errorMessage = "\nLa posició ("++(show x)++", "++(show y)++") està ocupada!"
  if (getCell b x y)==Empty then return $ Move (stringToCell c) pos
                            else putStrLn errorMessage >> movePrompt board
  
-- ask for move letter, allow only 'S' and 'O'
letterPrompt :: IO String
letterPrompt = do
  putStrLn "Escriu la lletra"
  c <- getLine
  let errorMessage = "\nLa lletra ha de ser \"S\" o \"O\"!"
  if c=="S"||c=="O" then return c
                    else putStrLn errorMessage >> letterPrompt

-- ask for move position, and make sure pos is not already occupied!
positionPrompt :: Board -> IO Position
positionPrompt board@(Board rows cols b) = do
  x <- intPrompt "Escriu la fila" (1,rows)
  y <- intPrompt "Escriu la columna" (1,cols)
  return (x,y)


-- select mode: User vs AI, or AI vs AI, then other player settings (AI level, 1st player)
playersPrompt :: IO Players
playersPrompt = do 
  mode <- intPrompt "Selecciona mode: 1 - User VS IA; 2 - IA VS IA" (1,2)
  levelPrompt mode

-- get AI Player given difficulty level
getAIPlayer :: Int -> Player
getAIPlayer 1 = Player EasyAI easyAIStrat
getAIPlayer 2 = Player NormalAI normalAIStrat
getAIPlayer 3 = Player HardAI hardAIStrat

-- prompt user for AI difficulty level: user vs AI, or AI vs AI
levelPrompt :: Int -> IO Players
levelPrompt 1 = do
  diff <- intPrompt "Selecciona nivell IA: 1 - Easy; 2 - Normal; 3 - Hard" (1,3)
  firstMovePrompt (Player User humanStrat,getAIPlayer diff)
-- 
levelPrompt 2 = do
  diff1 <- intPrompt "Selecciona nivell primera IA: 1 - Easy; 2 - Normal; 3 - Hard" (1,3)
  diff2 <- intPrompt "Selecciona nivell segona IA: 1 - Easy; 2 - Normal; 3 - Hard" (1,3)
  firstMovePrompt (getAIPlayer diff1,getAIPlayer diff2)

-- ask the user who plays the first move
firstMovePrompt :: Players -> IO Players 
firstMovePrompt players@(player1,player2) = do
  frst <- intPrompt ("Selecciona qui juga primer: 1 - "++(show player1)++"; 2 - "++(show player2)) (1,2)
  return $ if frst==1 then players else (player2,player1)


-- get initial empty board
getBoard :: Int -> Int -> CellMatrix
getBoard rows cols = array ((1,1),(rows,cols)) [((x,y),Empty)| x<-[1..rows],y<-[1..cols]]

-- initial game state: get input for board size
-- max board size = 100x100
gamePrompt :: IO GameState
gamePrompt = do
  rows <- intPrompt "Escriu el numero de files del tauler" (1,100)
  cols <- intPrompt "Escriu el numero de columnes del tauler" (1,100)
  return $ Game (Board rows cols (getBoard rows cols)) (Score 0 0) Player1 0


-- app entry point
main :: IO()
main = do
  setStdGen $ mkStdGen 1981  -- always same seed
  players <- playersPrompt
  game <- gamePrompt
  gameLoop game players
--



import Data.List
import Data.Char
import Data.Tuple

type Board = [[Char]]
type Move = (Int, Int)
emptyBoard = ["   ", "   ", "   "]

main = do
    jumpLines
    putStrLn("Jogo da Velha")
    putStrLn("\nPara realizar um movimento, digite 'CN', onde C é o numero da coluna (A - C), e N é o número da linha (1 - 3)")
    putStrLn("Digite 'sair' a qualquer momento para sair")
    putStrLn("Pressione qualquer tecla para começar") 
    input <- getLine
    gameLoop emptyBoard 'x'

jumpLines :: IO()
jumpLines = 
    putStrLn(take 50 (repeat '\n'))

gameLoop :: Board -> Char -> IO()
gameLoop board playerChar = do
    -- Show current board
    putStrLn( "\n" ++ (showBoard board) ++ "\n")
    putStrLn $ "Jogador " ++ [playerChar] ++ ", faça seu movimento: "
    input <- getLine

    jumpLines

    if input == "exit" then 
        return ()
    else do
        -- try parsing the input to a valid movement
        let move = parseMove input
        -- Check if the move is valid
        if (snd move) then do
            -- try to transforming the move in a new board
            let newBoardAttemp = doMove board (fst move) playerChar
            -- Check if the new attemp is possible
            if (snd newBoardAttemp) then do
                --Create new board with move made
                let newBoard = fst newBoardAttemp
                -- Check for winner, ties or game on
                if (isWinner newBoard (fst move)) then do
                    putStrLn("Jogador " ++ [playerChar] ++ " é o vencedor!")
                    putStrLn $ showBoard newBoard
                    restart
                  -- Check for tie
                else if tiedGame newBoard then do
                    putStrLn $ showBoard newBoard
                    putStrLn("O jogo empatou!")
                    restart
                else
                    -- No win, no tie: continue game
                    gameLoop newBoard (returnNextChar playerChar)

            -- movement invalid, able to parse but unable to apply in the board
            else do
                putStrLn("Fora do limite! / Jogador ja marcou ai!")
                putStrLn("Tente novamente.")
                gameLoop board playerChar
        -- Movement invalid, unable to parse         
        else do 
            putStrLn("Movimento Inválido.\nTente novamente.")
            gameLoop board playerChar 
            
showBoard :: Board -> String
showBoard board = letterHeader ++ "\n\n" ++ (intercalate rowSep $ rowNumber $ map rowStr board)
    where
      width = length (head board)
      rowStr = intercalate " | " . map (\x -> [x])
      rowNumber s = [(show n) ++ "  " ++ x | n <- [0..(length s)-1], x <- [s!!n]]
      letterHeader = "   " ++ (intercalate "   " (convertToSingleStrings ( take width ['A'..])))
      rowSep = "\n" ++ "  ---+---+---" ++ "\n"

restart :: IO()
restart = do
    putStrLn "Você gostaria de jogar denovo? (s/n)"
    playAgain <- getLine
    if playAgain == "s" || playAgain == "S" then do
        jumpLines
        main
    else if playAgain == "n" || playAgain == "N" then
        return ()
    else do
        putStrLn "Input invalido. Por favor informe 'S-s' ou 'N-n'"
        restart

parseMove :: String -> (Move, Bool)
parseMove str
    | length str /= 2 = ((0,0), False)
    | (elem l ['A'..'C'])  && (elem n ['0'..'3'])  = ( ((ord l)-65, (ord n)-48), True )
    | otherwise = ((0,0), False)
  where
    l = str!!0 -- letter
    n = str!!1 -- number

doMove :: Board -> Move -> Char -> (Board, Bool)
doMove board move player
    | x < 0 || y < 0 || x >= width || y >= height  = (board, False) -- Out of bounds
    | getElemFrom2dArray x y board /= ' '                  = (board, False) -- Location already taken
    | otherwise                           = (insertItemIn2dArray x y player board, True)
    where
    x = fst move 
    y = snd move
    width = length $ head board 
    height = length board 

isWinner :: Board -> Move -> Bool
isWinner b m = verticalMatch || horizontalMatch || diagonalUpperLeft || diagonalUpperRight
  where
    dUL             = diagonalUL b 
    dUR             = diagonalUR b 
    verticalMatch            = checkAllSame $ b !! (snd m)
    horizontalMatch = checkAllSame $ map (!! (fst m)) b
    diagonalUpperLeft   = (not $ all (== ' ') dUL) && (checkAllSame dUL)
    diagonalUpperRight  = (not $ all (== ' ') dUR) && (checkAllSame dUR)

tiedGame :: Board -> Bool
tiedGame = not . foldr1 (||) . map (any(==' '))

insertItem :: Int -> a -> [a] -> [a]
insertItem position newValue list = take position list ++ newValue : drop (position+1) list

insertItemIn2dArray :: Int -> Int -> a -> [[a]] -> [[a]]
insertItemIn2dArray x y newValue matrix = insertItem y (insertItem x newValue (matrix!!y)) matrix

getElemFrom2dArray :: Int -> Int -> [[a]]  -> a
getElemFrom2dArray x y matrix = (matrix!!y)!!x

convertToSingleStrings :: String -> [String]
convertToSingleStrings = map (\x -> [x])

returnNextChar :: Char -> Char
returnNextChar current = if current == 'x' then 'o' else 'x'

checkAllSame :: Eq a => [a] -> Bool
checkAllSame (x:xs) = all (==x) xs

diagonalUR :: [[a]] -> [a]
diagonalUR xs = [(xs!!n)!!n | n <- [0..(length xs) -1]]

diagonalUL :: [[a]] -> [a]
diagonalUL xs = [(xs!!n)!!(len - n -1) | n <- [0..len-1]]
  where len = length xs
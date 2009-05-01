----------------------------------------------------------------------------------
--                                                                              --
--                        metodos de programacao I                              --
--                  anexo projecto practico nr.1 - monads                       --
--              universidade do minho, departamento de informatica              --
--                                                                              --
--                         outubro/novembro de 2005                             --
--                                                                              --
--   jorge amilcar pereira,       nr. 43102                                     --
--   bruno renato duarte,         nr. 43119                                     --
--   nuno andre pinto,            nr. 43190                                     --
--                                                                              --
------------------------------------------------------------------------(dscape)--

-- (01) iMports ---------------------------------------------------------------~--
import Control.Monad.State
import List ( (\\), nub, intersperse, sortBy, transpose )
import Char ( isSpace )

-- (02) dEclaracoes de tipos --------------------------------------------------~--
type Linha      = [Int]
type Grelha     = [Linha]
type Posicao    = (Int, Int)
type Posicoes   = [Posicao]
type Movimento  = (Posicao, Int)
type Movimentos = [Movimento]
type Candidato  = (Posicao, Linha)
type Candidatos = [Candidato]

type StateSudoku a = StateT Grelha IO a

-- (03) cOnstantes ------------------------------------------------------------~--
grelhaInicial :: Grelha
grelhaInicial = replicate 9 (replicate 9 0)

exemplo :: Grelha
exemplo = [[6,0,0,0,9,0,0,0,8]
          ,[0,0,0,0,0,4,1,0,0]
          ,[0,2,1,0,0,8,3,0,0]
          ,[0,4,5,9,0,7,0,0,0]
          ,[9,0,0,0,0,0,0,0,4]
          ,[0,0,0,6,0,5,9,3,0]
          ,[0,0,6,4,0,0,5,7,0]
          ,[0,0,4,1,0,0,0,0,0]
          ,[3,0,0,0,7,0,0,0,6]]

prompt :: String
prompt = "sudoku # "

-- (04) cOnversoes -------------------------------------------------------------~--
toChar :: Int -> String
toChar a = [toEnum (a + (fromEnum '0'))]

toInt :: Char -> Int
toInt c = fromEnum (c) - 48

spacetoZero :: String -> String
spacetoZero ""     = ""
spacetoZero (a:as) | isSpace(a) = '0' : spacetoZero(as)
                   | otherwise = a : spacetoZero (as)

zerotoSpace :: String -> String
zerotoSpace ""     = ""
zerotoSpace (a:as) | (a == '0') = ' ' : zerotoSpace(as)
                   | otherwise = a : zerotoSpace(as)
                   
-- (05) fUncoes auxiliares -----------------------------------------------------~--
leColAux :: Linha -> Grelha -> Grelha
leColAux alcance grelha = map (\n -> map (!! n) grelha) alcance

obtemCaixa :: Posicao -> Int
obtemCaixa (x,y) = (x `div` 3) + (3 * (y `div` 3))

existentes :: Linha -> Linha -> Linha -> Linha
existentes linha coluna caixa = nub $ filter (>0) $ linha ++ coluna ++ caixa

cortar :: Int -> Int -> Linha -> Linha -> Grelha
cortar _ _ y [] = if null y then [] else [y]
cortar n i y (x:xs) = if (i==n) then ((y++[x]):(cortar n 1 [] xs)) else (cortar n (i+1) (y++[x]) xs)

insereNaGrelha :: Posicao -> Int -> Grelha -> Linha
insereNaGrelha (x,y) valor grelha = let indexada = zip [0..] $ concat grelha in
                                         map (\(pos, v) -> if pos == (9*y + x) then valor else v) indexada

movimentos :: Posicao -> Grelha -> Movimentos
movimentos _ []                                   = []
movimentos (acx,acy) ([]:resto)                   = movimentos (0,acy+1) resto
movimentos (acx,acy) ((valor:restodalinha):resto) = ((acx, acy), valor) : movimentos (acx+1,acy) (restodalinha:resto)

linhaValida :: Linha -> Bool
linhaValida linha = if (nub (filter (>0) linha) == (filter (>0) linha))
                        then True
                        else False

optimizaJogadas :: Candidatos -> Movimentos
optimizaJogadas candidatos =  let melhor = head $ sortBy (\(_, vals1) (_, vals2) -> compare (length vals1) (length vals2)) candidatos in
                                  (\(pos, valores) -> map (\valor -> (pos, valor)) valores) melhor

-- (06) fUncoes monadicas ------------------------------------------------------~--
leLinhas :: StateSudoku Grelha
leLinhas = gets id

leColunas :: StateSudoku Grelha
leColunas = gets $ \grelha -> transpose grelha

leCaixas :: StateSudoku Grelha
leCaixas = mapM leCaixa [0..8]

leLinha :: Int -> StateSudoku Linha
leLinha n = leLinhas >>= return . flip (!!) n

leColuna :: Int -> StateSudoku Linha
leColuna n = leColunas >>= return . flip (!!) n

leCaixa :: Int -> StateSudoku Linha
leCaixa n = gets $ \grelha -> case n of 
                                   0 -> concat $ map (take 3) (leColAux [0..2] grelha)
                                   1 -> concat $ map (take 3) (leColAux [3..5] grelha)
                                   2 -> concat $ map (take 3) (leColAux [6..8] grelha)
                                   3 -> concat $ map (take 3 . drop 3) (leColAux [0..2] grelha)
                                   4 -> concat $ map (take 3 . drop 3) (leColAux [3..5] grelha)
                                   5 -> concat $ map (take 3 . drop 3) (leColAux [6..8] grelha)
                                   6 -> concat $ map (drop 6) (leColAux [0..2] grelha)
                                   7 -> concat $ map (drop 6) (leColAux [3..5] grelha)
                                   8 -> concat $ map (drop 6) (leColAux [6..8] grelha)

leCelulasVazias :: StateSudoku Posicoes
leCelulasVazias = do  moves <- leMovimentos (0,0)
                      return $ map (\((x,y),v) -> (x,y)) $ filter (\((x,y), v) -> v == 0) moves

leMovimentos :: Posicao -> StateSudoku Movimentos
leMovimentos (x,y) = gets $ \grelha -> movimentos (x,y) grelha

leJogadasPossiveis :: StateSudoku Candidatos
leJogadasPossiveis = do  vazios <- leCelulasVazias
                         mapM (leCandidato) vazios 

leCandidato :: Posicao -> StateSudoku Candidato
leCandidato posicao = do  cands <- candidatos posicao
                          return $ (posicao, cands)

candidatos :: Posicao -> StateSudoku Linha
candidatos (x,y) = do  linha  <- leLinha y
                       coluna <- leColuna x
                       caixa  <- leCaixa $ obtemCaixa (x,y)
                       return $ [1..9] \\ existentes linha coluna caixa

modificar :: Movimento -> StateSudoku ()
modificar ((x,y),valor) = modify $ \grelha -> cortar 9 1 [] $ insereNaGrelha (x,y) valor grelha

efectuaMovimentos :: Grelha -> StateSudoku ()
efectuaMovimentos grelha = mapM_ (\m -> modificar m) $ movimentos (0,0) grelha

simplificaTudo :: StateSudoku ()
simplificaTudo = do  g1 <- get
                     sudokuSimplify
                     g2 <- get
                     if (g1 /= g2)
                        then do  sudokuSimplify
                                 simplificaTudo
                        else do {return ()}
 
grelhaValida :: StateSudoku Bool
grelhaValida = do  linhas  <- leLinhas
                   colunas <- leColunas
                   caixas  <- leCaixas
                   return $ all (linhaValida) caixas && all (linhaValida) linhas && all (linhaValida) colunas

{--
leProximaJogada :: StateSudoku Movimentos
leProximaJogada = do  possiveis <- leJogadasPossiveis
                      let preenchido = map (\(_,valores) -> null valores) possiveis
                      if (length possiveis == 0)
                         then return []
                         else if (length preenchido == 0)
                                 then return (optimizaJogadas possiveis)
                                 else mzero--}

-- (07) fUncoes do jogo --------------------------------------------------------~--

{-- read: le ficheiro com a informacao de um tabuleiro Su-Doku. Este ficheiro devera
    conter 9 linhas, cada uma das quais com 9 caracteres (digitos de 1 a 9 ou o
    espaco que marca uma casa vazia).
--}

sudokuRead :: FilePath -> IO (Grelha)
sudokuRead ficheiro = do  r <- readFile ficheiro
                          case ((\l -> if (False `elem` l) then False else True) $ concat $ map (\l -> map (\a -> if (a `elem` [0..9]) then True else False) l) (map (\(a:_) -> map (\x -> (fromEnum x) - (fromEnum '0')) a) (map words (map (spacetoZero) (lines r))))) of
                               True  -> return (take 9 $ (map (\(a:_) -> map (\x -> (fromEnum x) - (fromEnum '0')) a) (map words (map (spacetoZero) (lines r)))))
                               False -> return (grelhaInicial)

{-- show: visualiza o estado actual do tabuleiro.
--}

sudokuShow :: StateSudoku ()
sudokuShow = do  grelha <- get
                 liftIO $ putStr (zerotoSpace $ concat $ intersperse "\n" (map (\linha -> concat $ (map (toChar) linha)) grelha))
                 liftIO $ putStr "\n"

{-- quit: sai do interpretador.
--}

sudokuQuit :: StateSudoku ()
sudokuQuit = do  liftIO $ putStr "tem a certeza que deseja sair (s/n): "
                 resposta <- liftIO $ getLine
                 if (resposta == "s" || resposta == "S")
                    then return ()
                    else do  liftIO $ putStr "\n"
                             interpretador

{-- set: permite realizar uma jogada no tabuleiro actual. Deve ler as coordenadas
    de uma posicao e o valor a inserir. Deve falhar se a jogada conduzir a um
    tabuleiro invalido (e.g. com digitos repetidos numa linha, etc.)
--}

sudokuSet :: Movimento -> StateSudoku ()
sudokuSet (pos,valor) = do  vazios <- leCelulasVazias
                            cands  <- candidatos pos
                            if (valor `elem` cands || pos `elem` vazios)
                               then modificar (pos,valor)
                               else liftIO $ putStr "jogada invalida.\n\n"

{-- cands: mostra conjunto de alternativas de jogadas admissiveis para uma dada
    posicao (jogadas que nao conduzem a um tabuleiro invalido). Estes sao
    designados por conjuntos candidatos de uma dada posicao.
--}

sudokuCands :: Posicao -> StateSudoku Linha
sudokuCands (x,y) = do  vazios <- leCelulasVazias
                        if ((x,y) `elem` vazios)
                           then do  coluna <- leColuna x
                                    linha  <- leLinha y
                                    caixa  <- leCaixa $ obtemCaixa (x,y)
                                    return $ [1..9] \\ (filter (>0) (coluna ++ linha ++ caixa))
                           else return []

{-- showCands: mostra todos os conjuntos candidatos das posicoes livres.
--}

sudokuShowCands :: StateSudoku ()
sudokuShowCands = do  possiveis  <- leJogadasPossiveis
                      liftIO $ putStr $ concat $ map (\((x,y),l) -> "(" ++ toChar (x+1) ++ "," ++ toChar (y+1) ++ "), " ++ (concat $ intersperse " " $ map (toChar) l) ++ "\t\n") possiveis

{-- simplify: realiza todas as jogadas que nao pressupoe escolhas.
--}

sudokuSimplify :: StateSudoku ()
sudokuSimplify = do  possiveis <- leJogadasPossiveis
                     mapM_ (\(pos,valores) -> if ((length valores) == 1) then (modificar (pos, head(valores))) else (liftIO $ putStr "")) possiveis

{-- autosolve: resolve o puzzle de uma forma automatica.
--}

sudokuAutosolve :: StateSudoku ()
sudokuAutosolve = do  simplificaTudo

{--                      valida <- grelhaValida
                      if (valida)
                         then do  proximo <- leProximaJogada 
                                  if (length proximo /= 0)                                 
                                     then msum . map (\jogada -> modificar jogada >> sudokuAutosolve) $ proximo
                                     else do  return ()
                                              interpretador
                         else mzero  --}

-- (08) mAin -----------------------------------------------------------------~--
main :: IO ()
main = evalStateT interpretador grelhaInicial

-- (09) iNterpretador de comandos --------------------------------------------~--
interpretador :: StateSudoku ()
interpretador = do  g <- get
                    if (all (\x -> x `elem` [1..9]) (concat g))
                       then do  sudokuShow
                                liftIO $ putStr "\nparabens!! acabou o jogo :)\ndeseja comecar um novo jogo (s/n): "
                                resposta <- liftIO $ getLine
                                if (resposta == "N" || resposta == "n")
                                    then do  liftIO $ putStr $ "\n"
                                             return ()
                                    else do  put grelhaInicial
                                             liftIO $ putStr "\n"
                                             interpretador
                       else do  liftIO $ putStr prompt
                                comando <- liftIO $ getLine
                                case (words comando !! 0) of
                                     "read"      -> do  grelha <- liftIO $ sudokuRead $ comando \\ ((words comando !! 0) ++ " ")
                                                        efectuaMovimentos grelha
                                                        interpretador
                                     "show"      -> do  sudokuShow
                                                        interpretador
                                     "quit"      -> do  sudokuQuit
                                     "set"       -> do  if ( (length $ words comando !! 1) /= 1 || (length $ words comando !! 1) /= 1 || (length $ words comando !! 1) /= 1 )
                                                           then do  liftIO $ putStr $ "erro de sintaxe: '" ++ comando ++ "'.\nuse help para obter a lista de comandos.\n\n"
                                                                    interpretador
                                                           else do  let x = toInt $ head $ words comando !! 1
                                                                    let y = toInt $ head $ words comando !! 2
                                                                    let v = toInt $ head $ words comando !! 3
                                                                    if ( not (x < 1 || x > 9 || y < 1 || y > 9 || v < 1 || v > 9) )
                                                                       then do  sudokuSet ((x-1,y-1),v)
                                                                                interpretador
                                                                       else do  liftIO $ putStr $ "erro de sintaxe: '" ++ comando ++ "'.\nuse help para obter a lista de comandos.\n\n"
                                                                                interpretador
                                     "cands"     -> do  if ( (length $ words comando !! 1) /= 1 || (length $ words comando !! 1) /= 1 )
                                                           then do  liftIO $ putStr $ "erro de sintaxe: '" ++ comando ++ "'.\nuse help para obter a lista de comandos.\n\n"
                                                                    interpretador
                                                           else do  let x = toInt $ head $ words comando !! 1
                                                                    let y = toInt $ head $ words comando !! 2
                                                                    if ( not (x < 1 || x > 9 || y < 1 || y > 9) )
                                                                       then do  candidatos <- sudokuCands (x-1,y-1)
                                                                                liftIO $ putStr $ concat $ intersperse " " $ map (toChar) candidatos
                                                                                liftIO $ putStr "\n"
                                                                                interpretador
                                                                       else do  liftIO $ putStr $ "erro de sintaxe: '" ++ comando ++ "'.\nuse help para obter a lista de comandos.\n\n"
                                                                                interpretador 
                                     "showCands" -> do  sudokuShowCands
                                                        interpretador
                                     "simplify"  -> do  sudokuSimplify
                                                        interpretador
                                     "autosolve" -> do  sudokuAutosolve
                                                        interpretador
                                     "help"      -> do  liftIO $ putStr "\nlista de comandos:\n-----------------\nread caminho \tle o ficheiro localizado em <caminho>.\nshow         \tmostra o tabuleiro actual.\nquit         \tsai do interpretador.\nset x y valor\tinsere valor em (x,y).\ncands x y    \tmostra os candidatos para a posicao (x,y).\nshowCands    \tmostra todos os candidatos para as posicoes vazias.\nsimplify     \tconsecuta todas as jogadas que nao pressupoe escolha.\nautosolve    \tresolve automaticamente o puzzle.\n\n"
                                                        interpretador
                                     _           -> do  liftIO $ putStr $ "'" ++ (words comando) !! 0 ++ "'" ++ " nao e' um comando reconhecido.\nuse help para obter a lista de comandos.\n\n"
                                                        interpretador
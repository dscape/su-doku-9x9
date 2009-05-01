----------------------------------------------------------------------------------
--                                                                              --
--                        metodos de programacao I                              --
--                     projecto practico nr.1 - sudoku                          --
--              universidade do minho, departamento de informatica              --
--                                                                              --
--                         outubro/novembro de 2005                             --
--                                                                              --
--   jorge amilcar pereira,       nr. 43102                                     --
--   bruno renato duarte,         nr. 43119                                     --
--   nuno andre pinto,            nr. 43190                                     --
--                                                                              --
------------------------------------------------------------------------(dscape)--
-- era fixe por as simplificacoes a trocar automaticamente com o keyboard..
-- ex: shift + s trocava os paineis que só tem um candidato possivel pra esse candidato
-- ate que a tecla fosse largada
--
-- importar tabelas de sudoku e arranjar um algoritmo qq de criacao de tabelas
-- e integrar no jogo.
--
-- integrar as monad de estado aperfeicoada para melhorar o tempo de resolucao automatica.

module Sudoku where

-- (01) iMports ---------------------------------------------------------------~--
import List
import Char ( isSpace )
import Control.Monad.Trans ( liftIO )
import Graphics.UI.WX
import Graphics.UI.WXCore
import Tags

-- (02) dEclaracoes de tipos --------------------------------------------------~--
type Linha      = [Int]
type Grelha     = [Linha]
type Posicao    = (Int, Int)
type Posicoes   = [Posicao]
type Movimento  = (Posicao, Int)
type Movimentos = [Movimento]
type Candidato  = (Posicao, Linha)
type Candidatos = [Candidato]

type Amb = (Frame (), Var Grelha, Var Grelha, Var Posicoes, Var Int, [Panel ()], Panel (), MenuItem (), MenuItem (), MenuItem (), MenuItem (), MenuItem (), MenuItem ())

-- (03) cOnstantes ------------------------------------------------------------~--
grelhaInicial :: Grelha
grelhaInicial = replicate 9 (replicate 9 0)

-- (04) lEitura do ficheiro ---------------------------------------------------~--
readFx :: FilePath -> IO (Grelha)
readFx fx = do  r <- readFile fx
                case ((\l -> if (False `elem` l) then False else True) $ concat $ map (\l -> map (\a -> if (a `elem` [0..9]) then True else False) l) (map (\(a:_) -> map (\x -> (fromEnum x) - (fromEnum '0')) a) (map words (map (spacetoZero) (lines r))))) of
                     True  -> return (take 9 $ (map (\(a:_) -> map (\x -> (fromEnum x) - (fromEnum '0')) a) (map words (map (spacetoZero) (lines r)))))
                     False -> return (grelhaInicial)

-- (05) tRabalhar a grelha -----------------------------------------------------~--
fazColunas :: Grelha -> Linha -> Grelha
fazColunas g l = map (fazColuna g) l

fazColuna :: Grelha -> Int -> Linha
fazColuna g x = map (!! x) g

fazLinhas :: Grelha -> Linha -> Grelha
fazLinhas g l = map (fazLinha g) l 

fazLinha :: Grelha -> Int -> Linha
fazLinha g x = g !! x

fazCaixas :: Grelha -> Linha -> Grelha
fazCaixas g l = map (caixas g) l

fazCaixa :: Grelha -> Int -> Int -> Linha
fazCaixa [] _ _ = []
fazCaixa g x y  = caixas g (obtemCaixa x y)

caixas :: Grelha -> Int -> Linha
caixas g i =  case i of
                0 -> concat $ map (take 3) (fazColunas g [0..2])
                1 -> concat $ map (take 3) (fazColunas g [3..5])
                2 -> concat $ map (take 3) (fazColunas g [6..8])
                3 -> concat $ map (take 3 . drop 3) (fazColunas g [0..2])
                4 -> concat $ map (take 3 . drop 3) (fazColunas g [3..5])
                5 -> concat $ map (take 3 . drop 3) (fazColunas g [6..8])
                6 -> concat $ map (drop 6) (fazColunas g [0..2])
                7 -> concat $ map (drop 6) (fazColunas g [3..5])
                8 -> concat $ map (drop 6) (fazColunas g [6..8])

obtemCaixa :: Int -> Int -> Int
obtemCaixa x y = (x `div` 3) + (3 * (y `div` 3))

validar :: Grelha -> Posicao -> Linha
validar g (x, y) = filter (>0) (fazLinha g y ++ fazColuna g x ++ fazCaixa g x y)

movimentos :: Int -> Int -> Grelha -> Movimentos
movimentos _ _ []            = []
movimentos acx acy ([]:z)    = movimentos 0 (acy+1) z
movimentos acx acy ((x:y):z) = ((acx,acy), x) : movimentos (acx+1) acy (y:z)

insereNaLinha x val l = let ordenada = zip [0..] l in
                            map (\(pos, v) -> if pos == x then val else v) ordenada

milky :: Linha -> Int
milky [x] = x
milky _   = 0

-- (06) cOnversoes ------------------------------------------------------------~--
toChar :: Int -> String
toChar a = [toEnum (a + (fromEnum '0'))]

toFile :: Grelha -> String
toFile g = zerotoSpace $ concat $ intersperse "\n" (map (\l -> concat $ (map (toChar) l)) g)

toStr :: Linha -> String
toStr [] = "\t"
toStr (a:as) = " " ++ (toEnum (a + (fromEnum '0'))) : toStr as

spacetoZero :: String -> String
spacetoZero ""     = "" 
spacetoZero (a:as) | isSpace(a) = '0' : spacetoZero(as)
                   | otherwise = a : spacetoZero (as)

zerotoSpace :: String -> String
zerotoSpace ""     = "" 
zerotoSpace (a:as) | (a == '0') = ' ' : zerotoSpace(as)
                   | otherwise = a : zerotoSpace(as)

-- (07) fUnções auxiliares ao wx ----------------------------------------------~--
acabou :: Grelha -> Bool
acabou []        = True
acabou([]:z)     = acabou z
acabou ((x:y):z) = if (x /= 0) then acabou(y:z) else False

validaKeyCode :: Int -> Int
validaKeyCode k = if ((elem) k [1..9]) then k else 0

getCand :: Grelha -> Int -> Int -> String
getCand [] _ _    = []
getCand g x y = toStr $ [1..9] \\ (validar g (x,y))

slice :: Int -> Int -> Linha -> Linha -> Grelha
slice _ _ y [] = if null y then [] else [y]                    
slice n i y (x:xs) = if (i==n) then ((y++[x]):(slice n 1 [] xs)) else (slice n (i+1) (y++[x]) xs)

-- (08) rEsolver o puzzle -----------------------------------------------------~--
resolver :: Grelha -> Grelha
resolver grelha = if (grelhaValida (simplify grelha))
                     then if (acabou (simplify grelha))
                             then simplify grelha
                             else if (null (leProximaJogada (getMovPossiveis (simplify grelha))))
                                     then []
                                     else take 9 $ concat $ map (\m -> resolver (insereNaGrelha m (simplify grelha))) $ leProximaJogada (getMovPossiveis (simplify grelha))
                     else []

insereNaGrelha :: Movimento -> Grelha -> Grelha
insereNaGrelha ((x,y),valor) g = insereNaLinha y (insereNaLinha x valor (g !! y)) g

leProximaJogada :: Candidatos -> Movimentos
leProximaJogada possiveis = do  let blocked = filter (\(_, cs) -> null cs) possiveis
                                if null possiveis
                                   then []
                                   else if not (null blocked)
                                        then []
                                        else (opcoesMenores possiveis)

opcoesMenores :: Candidatos -> Movimentos
opcoesMenores cands = let (f:_) = sortBy (\(_, f1) (_, f2) -> compare (length f1) (length f2)) cands
                          (pos, f3) = f in
                          [(pos, c) | c <- f3]

grelhaValida :: Grelha -> Bool
grelhaValida grelha = all (linhaValida) (fazColunas grelha [0..8]) && all (linhaValida) (fazLinhas grelha [0..8]) && all (linhaValida) (fazCaixas grelha [0..8])

linhaValida :: Linha -> Bool
linhaValida linha = if (nub (filter (>0) linha) == (filter (>0) linha))
                        then True
                        else False

getMovPossiveis :: Grelha -> Candidatos
getMovPossiveis g = map (\pos -> (pos, [1..9] \\ (validar g pos))) (getVazios(movimentos 0 0 g))

getVazios :: Movimentos -> Posicoes
getVazios m = map (\(p, _) -> p) . filter (\(_,v) -> v == 0) $ m

simplify :: Grelha -> Grelha
simplify g = if ((slice 9 1 [] (retSimply g (movimentos 0 0 g))) /= g)
                then simplify (slice 9 1 [] (retSimply g (movimentos 0 0 g)))
                else g

retSimply :: Grelha -> Movimentos -> Linha
retSimply _ []                 = []
retSimply g (((x,y), v):resto) = if (v == 0)
                                     then if (milky([1..9] \\ (validar g (x,y))) /= 0)
                                             then milky([1..9] \\ (validar g (x,y))) : retSimply g resto
                                          else 0 : retSimply g resto
                                     else v : retSimply g resto

-- (09) sIstema de menus ------------------------------------------------------~--
main :: IO()
main = start gui
 
gui :: IO()
gui
 = do
      f           <- frameFixed [text := tagTitulo]
      p1          <- panel f []
      p1_1        <- panel p1 [clientSize := sz 40 40]
      p1_2        <- panel p1 [clientSize := sz 40 40]
      p1_3        <- panel p1 [clientSize := sz 40 40]
      p1_4        <- panel p1 [clientSize := sz 40 40]
      p1_5        <- panel p1 [clientSize := sz 40 40]
      p1_6        <- panel p1 [clientSize := sz 40 40]
      p1_7        <- panel p1 [clientSize := sz 40 40]
      p1_8        <- panel p1 [clientSize := sz 40 40]
      p1_9        <- panel p1 [clientSize := sz 40 40]
      p2_1        <- panel p1 [clientSize := sz 40 40]
      p2_2        <- panel p1 [clientSize := sz 40 40]
      p2_3        <- panel p1 [clientSize := sz 40 40]
      p2_4        <- panel p1 [clientSize := sz 40 40]
      p2_5        <- panel p1 [clientSize := sz 40 40]
      p2_6        <- panel p1 [clientSize := sz 40 40]
      p2_7        <- panel p1 [clientSize := sz 40 40]
      p2_8        <- panel p1 [clientSize := sz 40 40]
      p2_9        <- panel p1 [clientSize := sz 40 40]
      p3_1        <- panel p1 [clientSize := sz 40 40]
      p3_2        <- panel p1 [clientSize := sz 40 40]
      p3_3        <- panel p1 [clientSize := sz 40 40]
      p3_4        <- panel p1 [clientSize := sz 40 40]
      p3_5        <- panel p1 [clientSize := sz 40 40]
      p3_6        <- panel p1 [clientSize := sz 40 40]
      p3_7        <- panel p1 [clientSize := sz 40 40]
      p3_8        <- panel p1 [clientSize := sz 40 40]
      p3_9        <- panel p1 [clientSize := sz 40 40]
      p4_1        <- panel p1 [clientSize := sz 40 40]
      p4_2        <- panel p1 [clientSize := sz 40 40]
      p4_3        <- panel p1 [clientSize := sz 40 40]
      p4_4        <- panel p1 [clientSize := sz 40 40]
      p4_5        <- panel p1 [clientSize := sz 40 40]
      p4_6        <- panel p1 [clientSize := sz 40 40]
      p4_7        <- panel p1 [clientSize := sz 40 40]
      p4_8        <- panel p1 [clientSize := sz 40 40]
      p4_9        <- panel p1 [clientSize := sz 40 40]
      p5_1        <- panel p1 [clientSize := sz 40 40]
      p5_2        <- panel p1 [clientSize := sz 40 40]
      p5_3        <- panel p1 [clientSize := sz 40 40]
      p5_4        <- panel p1 [clientSize := sz 40 40]
      p5_5        <- panel p1 [clientSize := sz 40 40]
      p5_6        <- panel p1 [clientSize := sz 40 40]
      p5_7        <- panel p1 [clientSize := sz 40 40]
      p5_8        <- panel p1 [clientSize := sz 40 40]
      p5_9        <- panel p1 [clientSize := sz 40 40]
      p6_1        <- panel p1 [clientSize := sz 40 40]
      p6_2        <- panel p1 [clientSize := sz 40 40]
      p6_3        <- panel p1 [clientSize := sz 40 40]
      p6_4        <- panel p1 [clientSize := sz 40 40]
      p6_5        <- panel p1 [clientSize := sz 40 40]
      p6_6        <- panel p1 [clientSize := sz 40 40]
      p6_7        <- panel p1 [clientSize := sz 40 40]
      p6_8        <- panel p1 [clientSize := sz 40 40]
      p6_9        <- panel p1 [clientSize := sz 40 40]
      p7_1        <- panel p1 [clientSize := sz 40 40]
      p7_2        <- panel p1 [clientSize := sz 40 40]
      p7_3        <- panel p1 [clientSize := sz 40 40]
      p7_4        <- panel p1 [clientSize := sz 40 40]
      p7_5        <- panel p1 [clientSize := sz 40 40]
      p7_6        <- panel p1 [clientSize := sz 40 40]
      p7_7        <- panel p1 [clientSize := sz 40 40]
      p7_8        <- panel p1 [clientSize := sz 40 40]
      p7_9        <- panel p1 [clientSize := sz 40 40]
      p8_1        <- panel p1 [clientSize := sz 40 40]
      p8_2        <- panel p1 [clientSize := sz 40 40]
      p8_3        <- panel p1 [clientSize := sz 40 40]
      p8_4        <- panel p1 [clientSize := sz 40 40]
      p8_5        <- panel p1 [clientSize := sz 40 40]
      p8_6        <- panel p1 [clientSize := sz 40 40]
      p8_7        <- panel p1 [clientSize := sz 40 40]
      p8_8        <- panel p1 [clientSize := sz 40 40]
      p8_9        <- panel p1 [clientSize := sz 40 40]
      p9_1        <- panel p1 [clientSize := sz 40 40]
      p9_2        <- panel p1 [clientSize := sz 40 40]
      p9_3        <- panel p1 [clientSize := sz 40 40]
      p9_4        <- panel p1 [clientSize := sz 40 40]
      p9_5        <- panel p1 [clientSize := sz 40 40]
      p9_6        <- panel p1 [clientSize := sz 40 40]
      p9_7        <- panel p1 [clientSize := sz 40 40]
      p9_8        <- panel p1 [clientSize := sz 40 40]
      p9_9        <- panel p1 [clientSize := sz 40 40]
      pver        <- panel p1 [visible := False, clientSize := sz 5 40]
      phor        <- panel p1 [visible := False, clientSize := sz 360 5]
-- (a) menu ficheiro ----------------------------------------------------------~--
      ficheiro    <- menuPane [text := tagFx]
      terminar    <- menuItem ficheiro [text := tagTerminar, help := tagTerminarhlp, enabled := False]
      guardar     <- menuItem ficheiro [text := tagGuardar, help := tagGuardarhlp, enabled := False]
      novojogo    <- menuItem ficheiro [text := tagNovoJogo, help := tagNovoJogohlp]
      l_fx        <- menuLine ficheiro
      sair        <- menuQuit ficheiro [text := tagSair, help := tagSairhlp]
-- (b) menu jogo --------------------------------------------------------------~--
      jogo        <- menuPane	[text := tagJogo]
      undo        <- menuItem jogo [text := tagUndo, help := tagUndohlp, enabled := False]
      l1_jg       <- menuLine jogo
      cands       <- menuItem jogo [text := tagCands, help := tagCandshlp, enabled := False]
      simplify    <- menuItem jogo [text := tagSimply, help := tagSimplyhlp, enabled := False]
      l2_jg       <- menuLine jogo
      asolve      <- menuItem jogo [text := tagAsolve, help := tagAsolvehlp, enabled := False]
-- (c) menu opcoes -------------------------------------------------------------~--
      skins        <- menuPane [text := tagOpt]
      m_skins      <- menuPane     [text := tagSkins]
      m_skins_sub  <- menuSub  skins  m_skins []
      skin1	       <- menuRadioItem m_skins [text := tagSkin1, help := tagSkin1hlp]
      skin2	       <- menuRadioItem m_skins [text := tagSkin2, help := tagSkin2hlp]
      skin3	       <- menuRadioItem m_skins [text := tagSkin3, help := tagSkin3hlp]
-- (d) menu ajuda -------------------------------------------------------------~--
      hlp          <- menuHelp      [text := tagHlp]
      regras       <- menuItem hlp [text := tagRegras, help := tagRegrashlp]
      about        <- menuAbout hlp [text := tagAbout, help := tagAbouthlp]
-- (e) barra de estado --------------------------------------------------------~--
      status       <- statusField   [text := tagStatus]
-- (f) variaveis --------------------------------------------------------------~--
      tabuleiro    <- variable [value := grelhaInicial]
      tabinit      <- variable [value := grelhaInicial]
      listaundo    <- variable [value := [(10,10)]]
      mascara      <- variable [value := 1]
      posicoes     <- return ([p1_1, p1_2, p1_3, p1_4, p1_5, p1_6, p1_7, p1_8, p1_9,
                               p2_1, p2_2, p2_3, p2_4, p2_5, p2_6, p2_7, p2_8, p2_9,
                               p3_1, p3_2, p3_3, p3_4, p3_5, p3_6, p3_7, p3_8, p3_9,
                               p4_1, p4_2, p4_3, p4_4, p4_5, p4_6, p4_7, p4_8, p4_9,
                               p5_1, p5_2, p5_3, p5_4, p5_5, p5_6, p5_7, p5_8, p5_9,
                               p6_1, p6_2, p6_3, p6_4, p6_5, p6_6, p6_7, p6_8, p6_9,
                               p7_1, p7_2, p7_3, p7_4, p7_5, p7_6, p7_7, p7_8, p7_9,
                               p8_1, p8_2, p8_3, p8_4, p8_5, p8_6, p8_7, p8_8, p8_9,
                               p9_1, p9_2, p9_3, p9_4, p9_5, p9_6, p9_7, p9_8, p9_9])
      ambiente     <- return (f, tabuleiro, tabinit, listaundo, mascara, posicoes, p1, terminar, undo, cands, simplify, asolve, guardar)
-- (g) sets -------------------------------------------------------------------~--
      set f [layout              := column 0 [row 0 [widget p1]]
             ,statusBar          := [status]
             ,menuBar            := [ficheiro, jogo, skins, hlp]
             ,on (menu terminar) := terminarJogo ambiente
             ,on (menu guardar)  := guardarFx ambiente
             ,on (menu novojogo) := novoJogo ambiente
             ,on (menu sair)     := close f
             ,on (menu undo)     := undoMove ambiente
             ,on (menu cands)    := showCands ambiente
             ,on (menu simplify) := simplyJogo ambiente
             ,on (menu asolve)   := asolveJogo ambiente
             ,on (menu skin1)    := mudaSkin ambiente 1
             ,on (menu skin2)    := mudaSkin ambiente 2
             ,on (menu skin3)    := mudaSkin ambiente 3
             ,on (menu regras)   := infoDialog f tagRegrasT tagRegrasI
             ,on (menu about)    := infoDialog f tagAboutT tagAboutI
             ,clientSize         := sz 450 450]
      set p1 [layout         := floatCentre $ column 0 [
              row 1  [row 0 [widget phor]],
              row 2  [row 0 [widget p1_1, widget p1_2, widget p1_3, widget pver, widget p1_4,
                            widget p1_5, widget p1_6, widget pver, widget p1_7, widget p1_8, widget p1_9]],
              row 3  [row 0 [widget p2_1, widget p2_2, widget p2_3, widget pver, widget p2_4,
                            widget p2_5, widget p2_6, widget pver, widget p2_7, widget p2_8, widget p2_9]],
              row 4  [row 0 [widget p3_1, widget p3_2, widget p3_3, widget pver, widget p3_4,
                            widget p3_5, widget p3_6, widget pver, widget p3_7, widget p3_8, widget p3_9]],
              row 5  [row 0 [widget phor]],
              row 6  [row 0 [widget p4_1, widget p4_2, widget p4_3, widget pver, widget p4_4,
                            widget p4_5, widget p4_6, widget pver, widget p4_7, widget p4_8, widget p4_9]],
              row 7  [row 0 [widget p5_1, widget p5_2, widget p5_3, widget pver, widget p5_4,
                            widget p5_5, widget p5_6, widget pver, widget p5_7, widget p5_8, widget p5_9]],
              row 8  [row 0 [widget p6_1, widget p6_2, widget p6_3, widget pver, widget p6_4,
                            widget p6_5, widget p6_6, widget pver, widget p6_7, widget p6_8, widget p6_9]],
              row 9  [row 0 [widget phor]],
              row 10 [row 0 [widget p7_1, widget p7_2, widget p7_3, widget pver, widget p7_4,
                            widget p7_5, widget p7_6, widget pver, widget p7_7, widget p7_8, widget p7_9]],
              row 11 [row 0 [widget p8_1, widget p8_2, widget p8_3, widget pver, widget p8_4,
                            widget p8_5, widget p8_6, widget pver, widget p8_7, widget p8_8, widget p8_9]],
              row 12 [row 0 [widget p9_1, widget p9_2, widget p9_3, widget pver, widget p9_4,
                            widget p9_5, widget p9_6, widget pver, widget p9_7, widget p9_8, widget p9_9]]]
              ,clientSize     := sz 450 450]
      frameSetIconFromFile f tagIco
      set skin1 [checked := True]
      mudaSkin ambiente (1 :: Int)

-- (10) fUncoes do wx ---------------------------------------------------------~--
ambFrm :: Amb -> Frame()
ambFrm (f,_,_,_,_,_,_,_,_,_,_,_,_) = f

ambGrelha :: Amb -> Var Grelha
ambGrelha (_,g,_,_,_,_,_,_,_,_,_,_,_) = g

ambGrelhaInit :: Amb -> Var Grelha
ambGrelhaInit (_,_,g,_,_,_,_,_,_,_,_,_,_) = g

ambUndo :: Amb -> Var Posicoes
ambUndo (_,_,_,g,_,_,_,_,_,_,_,_,_) = g

ambSkn :: Amb -> Var Int
ambSkn (_,_,_,_,s,_,_,_,_,_,_,_,_) = s

ambPos :: Amb -> [Panel ()]
ambPos (_,_,_,_,_,p,_,_,_,_,_,_,_) = p

ambP1 :: Amb -> Panel ()
ambP1 (_,_,_,_,_,_,p,_,_,_,_,_,_) = p

ambFch :: Amb -> MenuItem ()
ambFch (_,_,_,_,_,_,_,b,_,_,_,_,_) = b

ambUnd :: Amb -> MenuItem ()
ambUnd (_,_,_,_,_,_,_,_,u,_,_,_,_) = u

ambCands :: Amb -> MenuItem ()
ambCands (_,_,_,_,_,_,_,_,_,c,_,_,_) = c

ambSimplify :: Amb -> MenuItem ()
ambSimplify (_,_,_,_,_,_,_,_,_,_,s,_,_) = s

ambAsolve :: Amb -> MenuItem ()
ambAsolve (_,_,_,_,_,_,_,_,_,_,_,a,_) = a

ambSave :: Amb -> MenuItem ()
ambSave (_,_,_,_,_,_,_,_,_,_,_,_,s) = s

mudaSkin :: Amb -> Int -> IO()
mudaSkin a v = do  set (ambSkn a) [value := v]
                   t <- get (ambGrelha a) value
                   updateTab a t (ambPos a)
                   updateP1 a
                   
updateTab :: Amb -> Grelha -> [Panel ()] -> IO ()
updateTab _ [] []              = do { return() }
updateTab _ _ []               = do { return() }
updateTab _ [] _               = do { return() }
updateTab a ([]:bs) p          = updateTab a bs p
updateTab a ((b:bs):xs) (p:ps) = do updatePos a p b
                                    updateTab a (bs:xs) ps

updateP1 :: Amb -> IO ()
updateP1 a = do  set (ambP1 a) [on paint := aux a]
                 repaint (ambP1 a)
                 where
                   aux a dc _ = do  s <- get (ambSkn a) value
                                    bm <- bitmapCreateFromFile ("gfx/skins/" ++ (toChar s) ++ "/fundo.bmp")
                                    drawBitmap dc bm (pt 0 0) False []

updatePos :: Amb -> Panel () -> Int -> IO ()
updatePos a p e = do set p [on paint := aux a e]
                     repaint p
                        where
                           aux a e dc _ = do s <- get (ambSkn a) value
                                             bmp <- bitmapCreateFromFile ("gfx/skins/" ++ (toChar s) ++ "/" ++ (toChar e) ++ ".bmp")
                                             drawBitmap dc bmp (pt 0 0) True []

novoJogo :: Amb -> IO ()
novoJogo a = do  mbfname <- fileOpenDialog (ambFrm a) False True "Abrir" sudokuFiles "" ""
                 case mbfname of
                      Nothing      -> return ()
                      (Just fname) -> do grelha <- liftIO(readFx fname)
                                         set (ambGrelha a) [value := grelha]
                                         set (ambGrelhaInit a) [value := grelha]
                 b <- get (ambGrelha a) value
                 if ((b /= grelhaInicial) && (grelhaValida b)) then (inicioJogo a b) else (tabFalso a)

tabFalso :: Amb -> IO ()
tabFalso a = do  warningDialog (ambFrm a) tagInvalido tagInvalidohlp
                 terminarJogo a

inicioJogo :: Amb -> Grelha -> IO ()
inicioJogo a b = do  set (ambFch a) [enabled := True]
                     set (ambAsolve a) [enabled := True]
                     set (ambSimplify a) [enabled := True]
                     set (ambCands a) [enabled := True]
                     set (ambSave a) [enabled := True]
                     updateTab a b (ambPos a)
                     activarTabuleiro a (movimentos 0 0 b) (ambPos a)
                     activarRc a (movimentos 0 0 b) (ambPos a)
      
terminarJogo :: Amb -> IO ()
terminarJogo a = do  set (ambGrelha a) [value := grelhaInicial]
                     desactivarTabuleiro (ambPos a)
                     desactivarRc (ambPos a)
                     b <- get (ambGrelha a) value
                     updateTab a b (ambPos a)
                     set (ambFch a) [enabled := False]
                     set (ambAsolve a) [enabled := False]
                     set (ambSimplify a) [enabled := False]
                     set (ambCands a) [enabled := False]
                     set (ambSave a) [enabled := False]
                     set (ambUnd a) [enabled := False]

desactivarTabuleiro :: [Panel ()] -> IO ()
desactivarTabuleiro []     = do {return ()}
desactivarTabuleiro (p:ps) = do  set p [on anyKey := nada]
                                 desactivarTabuleiro ps
                                 where
                                   nada _ = do {return ()}

desactivarPosicao :: Int -> Int -> [Panel ()] -> IO ()
desactivarPosicao _ _ []      = do {return ()}
desactivarPosicao x y paineis = do  set (paineis !! (9*y + x)) [on anyKey := nada]
                                    where
                                       nada _ = do {return ()}

activarPosicao :: Amb -> Int -> Int -> [Panel ()] -> IO ()
activarPosicao _ _ _ []      = do  {return ()}
activarPosicao a x y paineis = do  set (paineis !! (9*y + x)) [on anyKey := setMove a (x, y)]

activarUmRc :: Amb -> Int -> Int -> [Panel ()] -> IO ()
activarUmRc _ _ _ []      = do { return () }
activarUmRc a x y paineis = do  g <- get (ambGrelha a) value
                                set (paineis !! (9*y + x)) [on clickRight := (\_ -> do {infoDialog (ambFrm a) tagCandis (getCand g x y)})]

activarTabuleiro :: Amb -> Movimentos -> [Panel()] -> IO ()
activarTabuleiro _ [] []                 = do  {return ()}
activarTabuleiro a (((x,y), v):r) (p:ps) = do  if (v == 0) then poe else activarTabuleiro a r ps
                                               where
                                                  poe = do  set p [on anyKey := setMove a (x, y)]
                                                            activarTabuleiro a r ps

activarCands :: Amb -> Movimentos ->[Panel()] -> IO()
activarCands _ [] []                 = do {return()}
activarCands a (((x,y), v):r) (p:ps) = do  if (v == 0) then poe else activarCands a r ps
                                           where
                                              poe = do  g <- get (ambGrelha a) value
                                                        set p [on click := (\_ -> do {infoDialog (ambFrm a) tagCandis (getCand g x y); desactivarCands (ambPos a)})]
                                                        activarCands a r ps

activarRc :: Amb -> Movimentos ->[Panel()] -> IO()
activarRc _ [] []                 = do {return()}
activarRc a (((x,y), v):r) (p:ps) = do  if (v == 0) then poe else activarRc a r ps
                                           where
                                              poe = do  g <- get (ambGrelha a) value
                                                        set p [on clickRight := (\_ -> do {infoDialog (ambFrm a) tagCandis (getCand g x y)})]
                                                        activarRc a r ps

desactivarRc :: [Panel ()] -> IO ()
desactivarRc []     = do {return ()}
desactivarRc (p:ps) = do  set p [on clickRight := nada]
                          desactivarRc ps
                             where
                               nada _ = do {return ()}

desactivarCands :: [Panel ()] -> IO ()
desactivarCands []     = do {return ()}
desactivarCands (p:ps) = do  set p [on click := nada]
                             desactivarCands ps
                             where
                               nada _ = do {return ()}

setMove :: Amb -> Posicao -> Key -> IO ()
setMove a (x,y) k = do  g <- get (ambGrelha a) value
                        if ((elem) (validaKeyCode ((keyToKeyCode k) - 48)) (validar g (x, y)))
                          then do {infoDialog (ambFrm a) tagViolacao tagViolacaohlp; return ()}
                          else do set (ambGrelha a) [value := (insereNaLinha y (insereNaLinha x (validaKeyCode ((keyToKeyCode k) - 48)) (g !! y)) g)]
                                  u <- get (ambUndo a) value
                                  set (ambUndo a) [value := (take 16 $ (x,y):u) ]
                                  set (ambUnd a) [enabled := True]
                                  g2 <- get (ambGrelha a) value
                                  desactivarPosicao x y (ambPos a)
                                  desactivarRc (ambPos a)
                                  activarRc a (movimentos 0 0 g2) (ambPos a)
                                  updateTab a g2 (ambPos a)
                                  updateP1 a
                                  if (acabou g2) then do {(infoDialog (ambFrm a) tagAcabou tagAcabouhlp); terminarJogo a} else do {return ()}

showCands :: Amb -> IO ()
showCands a = do  g <- get (ambGrelha a) value
                  infoDialog (ambFrm a) tagCandis tagCandishlp
                  desactivarTabuleiro (ambPos a)
                  activarCands a (movimentos 0 0 g) (ambPos a)
                  activarTabuleiro a (movimentos 0 0 g) (ambPos a)

simplyJogo :: Amb -> IO()
simplyJogo a = do  g   <- get (ambGrelha a) value
                   set (ambGrelha a) [value := (slice 9 1 [] (retSimply g (movimentos 0 0 g)))]
                   g2 <- get (ambGrelha a) value
                   set (ambUnd a) [enabled := False]
                   set (ambUndo a) [value := [(10,10)]]
                   activarTabuleiro a (movimentos 0 0 g2) (ambPos a)
                   desactivarRc (ambPos a)
                   activarRc a (movimentos 0 0 g2) (ambPos a)
                   updateTab a g2 (ambPos a)
                   updateP1 a
                   if (acabou g2) then do {(infoDialog (ambFrm a) tagAcabou tagAcabouhlp); terminarJogo a} else do {return ()}

undoMove :: Amb -> IO ()
undoMove a = do  g  <- get (ambGrelha a) value
                 u  <- get (ambUndo a) value
                 if (((\(x,y) -> x) $ u !! 0) == 10)
                    then do  set (ambUnd a) [enabled := False]
                             return()
                    else do  set (ambGrelha a) [value := (insereNaLinha ((\(x,y) -> y) $ head u) (insereNaLinha ((\(x,y) -> x) $ head u) 0 (g !! ((\(x,y) -> y) $ head u))) g)]
                             g2 <- get (ambGrelha a) value
                             set (ambUndo a) [value := (tail u)]
                             activarPosicao a ((\(x,y) -> x) $ head u) ((\(x,y) -> y) $ head u) (ambPos a)
                             activarUmRc a ((\(x,y) -> x) $ head u) ((\(x,y) -> y) $ head u) (ambPos a)
                             updateTab a g2 (ambPos a)
                             updateP1 a

guardarFx :: Amb -> IO ()
guardarFx a = do  g  <- get (ambGrelha a) value
                  fx <- fileSaveDialog (ambFrm a) False True "Guardar" sudokuFiles "" "save.mp1"
                  case fx of
                       Nothing    -> return ()
                       (Just res) -> do  writeFile res (toFile g)
                                         infoDialog (ambFrm a) tagSucess tagSucesshlp

asolveJogo :: Amb -> IO ()
asolveJogo a = do  set (ambUnd a) [enabled := False]
                   set (ambUndo a) [value := [(10,10)]]
                   g <- get (ambGrelhaInit a) value
                   set (ambGrelha a) [value := resolver g ]
                   g3 <- get (ambGrelha a) value
                   if (g3 == [])
                      then do  infoDialog (ambFrm a) tagTabInv tagTabInvhlp
                               terminarJogo a
                      else do  activarTabuleiro a (movimentos 0 0 g3) (ambPos a)
                               updateTab a g3 (ambPos a)
                               updateP1 a
                               if (acabou g3) then do {(infoDialog (ambFrm a) tagAcabou tagAcabouhlp); terminarJogo a} else do {return ()}
module Tags where

sudokuFiles
   = [("MP1 files (*.mp1)",["*.mp1"])
    ,("TXT files (*.txt)",["*.txt"])
    ,("Todos os ficheiros",["*.*"])]

tagVersao :: String
tagVersao = "v1.0"

tagTitulo :: String
tagTitulo = "Su-DoKu 9x9"

tagStatus :: String
tagStatus = "Bem-vindo ao jogo " ++ tagTitulo ++ " " ++ tagVersao ++ "."

tagInvalido :: String
tagInvalido = "Tabuleiro nao carregado"

tagInvalidohlp :: String
tagInvalidohlp = "Ficheiro danificado?\nUtilizador cancelou a accao?"

tagFx :: String
tagFx = "&Ficheiro"

tagTerminar :: String
tagTerminar = "Terminar"

tagTerminarhlp :: String
tagTerminarhlp = "Terminar o jogo."

tagGuardar :: String
tagGuardar = "&Guardar\tCtrl+G"

tagGuardarhlp :: String
tagGuardarhlp = "Permite continuar o sudoku noutra altura."

tagNovoJogo :: String
tagNovoJogo = "&Novo Jogo\tCtrl+N"

tagNovoJogohlp :: String
tagNovoJogohlp = "Comecar um novo jogo."

tagSair :: String
tagSair = "Sair"

tagSairhlp :: String
tagSairhlp = "Abandonar o programa."

tagJogo :: String
tagJogo = "Jogo"

tagUndo :: String
tagUndo = "&Anular Jogada\tCtrl+A"

tagUndohlp :: String
tagUndohlp = "Anular a ultima jogada."

tagCands :: String
tagCands = "&Mostrar Candidatos\tCtrl+M"

tagCandshlp :: String
tagCandshlp = "Mostrar os candidatos de uma celula."

tagSimply :: String
tagSimply = "&Simplificar\tCtrl+S"

tagSimplyhlp :: String
tagSimplyhlp = "Realiza todos os movimentos possiveis sem recorrer a palpites."

tagAsolve :: String
tagAsolve = "Resolver Automaticamente"

tagAsolvehlp :: String
tagAsolvehlp = "Resolve automaticamente o puzzle."

tagOpt :: String
tagOpt = "Opcoes"

tagSkins :: String
tagSkins = "Mascaras"

tagAvisar :: String
tagAvisar = "Avisos"

tagAvisarhlp :: String
tagAvisarhlp = "Liga/desliga os avisos."

tagSkin1 :: String
tagSkin1 = "01. basic"

tagSkin1hlp :: String
tagSkin1hlp = "Seleccionar a primeira mascara."

tagSkin2 :: String
tagSkin2 = "02. dscape"

tagSkin2hlp :: String
tagSkin2hlp = "Seleccionar a segunda mascara." 

tagSkin3 :: String
tagSkin3 = "03. mhack"

tagSkin3hlp :: String
tagSkin3hlp = "Seleccionar a segunda mascara." 

tagHlp :: String
tagHlp = "Ajuda"

tagRegras :: String
tagRegras = "&Regras\tCtrl+R"

tagRegrashlp :: String
tagRegrashlp = "Ver as regras de um jogo de sudoku."

tagRegrasT :: String
tagRegrasT = "Regras do sudoku"

tagRegrasI :: String
tagRegrasI = "A grelha é composta por uma tabela 9x9.\nA cada sub-grelha 3x3 contida na grelha chamamos caixa.\nExistem 9 caixas.\nCada caixa deve conter todos os números entre um e nove.\nCada linha deve conter todos os números entre um e nove.\nCada coluna deve conter todos os números entre um e nove.\n"

tagAbout :: String
tagAbout = "Sobre..."

tagAbouthlp :: String
tagAbouthlp = "Quem fez o programa."

tagAboutT :: String
tagAboutT = "SuDoKu 9x9 "++ tagVersao ++"." 

tagAboutI :: String
tagAboutI = "Por:\n43102\tJorge Amilcar\t(j_amilcar@iol.pt)\t\n43109\tBruno Duarte\t(brunoduarte@gmail.com)\t\n43190\tNuno Pinto\t(nuno@hotmail.co.uk)\t\n"

tagOpen :: String
tagOpen = "Abrir"

tagOpenFx :: String
tagOpenFx = "gfx/fileopen16.png"

tagIco :: String
tagIco = "gfx/sudoku.ico"

tagAcabou :: String
tagAcabou = "Parabens!!!"

tagAcabouhlp :: String
tagAcabouhlp = "Conseguiu decifrar o puzzle!!!!\n"

tagViolacao :: String
tagViolacao = "Jogada nao aceitada"

tagViolacaohlp :: String
tagViolacaohlp = "Essa jogada quebra as regras do sudoku.\nConsulte Ajuda -> Regras ou (Control+R) para mais informacoes!"

tagNum :: String
tagNum = " valor"

tagNum2 :: String
tagNum2 = "Inserir numero"

tagX :: String
tagX = "x"

tagY :: String
tagY = "y"

tagValor :: String
tagValor = "Valor"

tagCandis :: String
tagCandis = "Candidatos"

tagCandishlp :: String
tagCandishlp = "Selecione a posicao."

tagCandi :: String
tagCandi = "Todos os Candidatos"

tagCandihlp :: String
tagCandihlp = "Selecione as posicoes e veja os candidatos.\nPara terminar carregue no logotipo LESI.\t\n"

tagSucess :: String
tagSucess = "Sucesso"

tagSucesshlp :: String
tagSucesshlp = "O ficheiro foi guardado com sucesso."

tagTabInv :: String
tagTabInv = "Tabuleiro invalido"

tagTabInvhlp :: String
tagTabInvhlp = "A solucao deste problema conduziu a um tabuleiro invalido."
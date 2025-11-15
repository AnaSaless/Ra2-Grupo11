-- Sistema de Inventário em Haskell

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time (UTCTime, getCurrentTime)
import Control.Exception (catch, IOException)
import System.IO (hFlush, stdout)
import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)


-- SEÇÃO 1: TIPOS DE DADOS

-- Item: Representa um item no inventário
data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  } deriving (Show, Read, Eq)

-- Inventario: Mapa de itemID para Item
type Inventario = Map String Item

-- AcaoLog: Tipo algébrico para as ações possíveis
data AcaoLog
  = Add
  | Remove
  | Update
  | Query
  | QueryFail
  | List
  | Report
  deriving (Show, Read, Eq)

-- StatusLog: Tipo algébrico para o resultado da operação
data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

-- LogEntry: Entrada do log de auditoria
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao :: AcaoLog
  , detalhes :: String
  , status :: StatusLog
  } deriving (Show, Read, Eq)

-- Tipo para o resultado de operações
type ResultadoOperacao = (Inventario, LogEntry)



-- SEÇÃO 2: FUNÇÕES PURAS DE LÓGICA DE NEGÓCIO


-- Adiciona um novo item ao inventário
addItem :: UTCTime -> String -> String -> Int -> String -> Inventario 
        -> Either String ResultadoOperacao
addItem time iid nome' qtd cat inv
  | Map.member iid inv = Left $ "Erro: Item com ID '" ++ iid ++ "' já existe no inventário."
  | qtd < 0 = Left "Erro: Quantidade não pode ser negativa."
  | null iid || null nome' = Left "Erro: ID e nome do item não podem estar vazios."
  | otherwise = Right (novoInv, logEntry)
  where
    novoItem = Item iid nome' qtd cat
    novoInv = Map.insert iid novoItem inv
    detalhesMsg = "Adicionado: " ++ iid ++ " - " ++ nome' ++ " (Qtd: " ++ show qtd ++ ", Cat: " ++ cat ++ ")"
    logEntry = LogEntry time Add detalhesMsg Sucesso

-- Remove unidades de um item do inventário
removeItem :: UTCTime -> String -> Int -> Inventario 
           -> Either String ResultadoOperacao
removeItem time iid qtdRemover inv
  | not (Map.member iid inv) = Left $ "Erro: Item '" ++ iid ++ "' não encontrado no inventário."
  | qtdRemover < 0 = Left "Erro: Quantidade a remover não pode ser negativa."
  | qtdRemover > quantidade item = Left $ "Erro: Estoque insuficiente. Disponível: " 
                                         ++ show (quantidade item) ++ ", solicitado: " ++ show qtdRemover
  | otherwise = Right (novoInv, logEntry)
  where
    Just item = Map.lookup iid inv
    novaQtd = quantidade item - qtdRemover
    itemAtualizado = item { quantidade = novaQtd }
    novoInv = if novaQtd == 0
              then Map.delete iid inv
              else Map.insert iid itemAtualizado inv
    detalhesMsg = "Removido: " ++ iid ++ " - " ++ nome item ++ " (Qtd removida: " 
                  ++ show qtdRemover ++ ", Qtd restante: " ++ show novaQtd ++ ")"
    logEntry = LogEntry time Remove detalhesMsg Sucesso

-- Atualiza a quantidade de um item
updateQty :: UTCTime -> String -> Int -> Inventario 
          -> Either String ResultadoOperacao
updateQty time iid novaQtd inv
  | not (Map.member iid inv) = Left $ "Erro: Item '" ++ iid ++ "' não encontrado no inventário."
  | novaQtd < 0 = Left "Erro: Quantidade não pode ser negativa."
  | otherwise = Right (novoInv, logEntry)
  where
    Just item = Map.lookup iid inv
    qtdAnterior = quantidade item
    itemAtualizado = item { quantidade = novaQtd }
    novoInv = if novaQtd == 0
              then Map.delete iid inv
              else Map.insert iid itemAtualizado inv
    detalhesMsg = "Atualizado: " ++ iid ++ " - " ++ nome item ++ " (Qtd anterior: " 
                  ++ show qtdAnterior ++ ", Nova qtd: " ++ show novaQtd ++ ")"
    logEntry = LogEntry time Update detalhesMsg Sucesso

-- Cria uma entrada de log para operação falhada
logFalha :: UTCTime -> AcaoLog -> String -> LogEntry
logFalha time acao' msg = LogEntry time acao' msg (Falha msg)


-- SEÇÃO 3: FUNÇÕES DE I/O E PERSISTÊNCIA 


-- Carrega o inventário do arquivo, retorna inventário vazio se arquivo não existe
carregarInventario :: IO Inventario
carregarInventario = do
  conteudo <- catch (readFile "Inventario.dat")
                    (\(_ :: IOException) -> return "")
  if null conteudo
    then return Map.empty
    else return (read conteudo :: Inventario)

-- Salva o inventário no arquivo
salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile "Inventario.dat" (show inv)

-- Carrega o log de auditoria do arquivo
carregarAuditoria :: IO [LogEntry]
carregarAuditoria = do
  conteudo <- catch (readFile "Auditoria.log")
                    (\(_ :: IOException) -> return "")
  if null conteudo
    then return []
    else do
      let linhas = lines conteudo
          logs = [read linha :: LogEntry | linha <- linhas, not (null linha)]
      return logs

-- Adiciona uma entrada ao log de auditoria
adicionarAuditoria :: LogEntry -> IO ()
adicionarAuditoria entry = appendFile "Auditoria.log" (show entry ++ "\n")

-- Parse seguro de inteiros
parseIntSafe :: String -> Maybe Int
parseIntSafe str = case reads str of
                     [(n, "")] -> Just n
                     _ -> Nothing

-- Parser de comandos do usuário
parseComando :: String -> IO (Maybe (Inventario -> IO Inventario))
parseComando linha = do
  let palavras = words linha
  case palavras of
    ("add":iid:nome':qtdStr:cat:_) -> 
      case parseIntSafe qtdStr of
        Nothing -> do
          putStrLn $ "✗ Erro: Quantidade inválida '" ++ qtdStr ++ "'. Deve ser um número inteiro."
          return $ Just return
        Just qtd -> return $ Just $ \inv -> do
          time <- getCurrentTime
          case addItem time iid nome' qtd cat inv of
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              adicionarAuditoria logEntry
              putStrLn $ "✓ Item adicionado com sucesso: " ++ iid
              return novoInv
            Left erro -> do
              time' <- getCurrentTime
              let logEntry = logFalha time' Add erro
              adicionarAuditoria logEntry
              putStrLn $ "✗ " ++ erro
              return inv
    
    ("remove":iid:qtdStr:_) -> 
      case parseIntSafe qtdStr of
        Nothing -> do
          putStrLn $ "✗ Erro: Quantidade inválida '" ++ qtdStr ++ "'. Deve ser um número inteiro."
          return $ Just return
        Just qtd -> return $ Just $ \inv -> do
          time <- getCurrentTime
          case removeItem time iid qtd inv of
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              adicionarAuditoria logEntry
              putStrLn $ "✓ Item removido com sucesso."
              return novoInv
            Left erro -> do
              time' <- getCurrentTime
              let logEntry = logFalha time' Remove erro
              adicionarAuditoria logEntry
              putStrLn $ "✗ " ++ erro
              return inv
    
    ("update":iid:qtdStr:_) -> 
      case parseIntSafe qtdStr of
        Nothing -> do
          putStrLn $ "✗ Erro: Quantidade inválida '" ++ qtdStr ++ "'. Deve ser um número inteiro."
          return $ Just return
        Just qtd -> return $ Just $ \inv -> do
          time <- getCurrentTime
          case updateQty time iid qtd inv of
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              adicionarAuditoria logEntry
              putStrLn $ "✓ Quantidade atualizada com sucesso."
              return novoInv
            Left erro -> do
              time' <- getCurrentTime
              let logEntry = logFalha time' Update erro
              adicionarAuditoria logEntry
              putStrLn $ "✗ " ++ erro
              return inv
    
    ("list":_) -> return $ Just $ \inv -> do
      time <- getCurrentTime
      let logEntry = LogEntry time List "Listagem do inventário" Sucesso
      adicionarAuditoria logEntry
      mostrarInventario inv
      return inv
    
    ("report":_) -> return $ Just $ \inv -> do
      time <- getCurrentTime
      let logEntry = LogEntry time Report "Geração de relatório" Sucesso
      adicionarAuditoria logEntry
      gerarRelatorio
      return inv
    
    ("help":_) -> return $ Just $ \inv -> do
      mostrarAjuda
      return inv
    
    ("exit":_) -> return Nothing
    
    _ -> do
      putStrLn "Comando não reconhecido. Digite 'help' para ver os comandos disponíveis."
      return $ Just return

-- Mostra o inventário atual
mostrarInventario :: Inventario -> IO ()
mostrarInventario inv = do
  putStrLn "\n========================================="
  putStrLn "           INVENTÁRIO ATUAL"
  putStrLn "========================================="
  if Map.null inv
    then putStrLn "Inventário vazio."
    else do
      putStrLn $ printf' "%-10s %-20s %-10s %-15s" "ID" "Nome" "Quantidade" "Categoria"
      putStrLn "========================================="
      mapM_ exibirItem (Map.elems inv)
  putStrLn "=========================================\n"
  where
    exibirItem item' = putStrLn $ printf' "%-10s %-20s %-10s %-15s"
                                          (itemID item')
                                          (nome item')
                                          (show $ quantidade item')
                                          (categoria item')
    printf' fmt a b c d = take 10 (a ++ repeat ' ') ++
                          take 20 (b ++ repeat ' ') ++
                          take 10 (c ++ repeat ' ') ++
                          d


-- SEÇÃO 4: ANÁLISE DE LOGS E RELATÓRIOS 


-- Retorna o histórico de um item específico
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem iid logs = filter (\log -> iid `elem` words (detalhes log)) logs

-- Retorna apenas os logs de erro
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro logs = filter ehErro logs
  where
    ehErro log = case status log of
                   Falha _ -> True
                   _ -> False

-- Encontra o item mais movimentado
itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado logs = 
  case movimentacoes of
    [] -> Nothing
    _ -> Just $ fst $ maximumBy (comparing snd) movimentacoes
  where
    logsOperacionais = filter ehOperacional logs
    ehOperacional log = acao log `elem` [Add, Remove, Update]
    extrairItemID log = case words (detalhes log) of
                          (_:iid:_) -> Just iid
                          _ -> Nothing
    itens = [iid | log <- logsOperacionais, Just iid <- [extrairItemID log]]
    contarOcorrencias item' = (item', length $ filter (== item') itens)
    movimentacoes = map contarOcorrencias (removerDuplicatas itens)
    removerDuplicatas [] = []
    removerDuplicatas (x:xs) = x : removerDuplicatas (filter (/= x) xs)

-- Gera relatório completo baseado nos logs
gerarRelatorio :: IO ()
gerarRelatorio = do
  logs <- carregarAuditoria
  putStrLn "\n========================================="
  putStrLn "         RELATÓRIO DE AUDITORIA"
  putStrLn "========================================="
  
  putStrLn $ "\nTotal de operações registradas: " ++ show (length logs)
  
  let erros = logsDeErro logs
  putStrLn $ "Total de erros: " ++ show (length erros)
  
  putStrLn "\n--- ERROS REGISTRADOS ---"
  if null erros
    then putStrLn "Nenhum erro registrado."
    else mapM_ exibirLogErro erros
  
  case itemMaisMovimentado logs of
    Nothing -> putStrLn "\n--- ITEM MAIS MOVIMENTADO ---\nNenhuma movimentação registrada."
    Just item' -> putStrLn $ "\n--- ITEM MAIS MOVIMENTADO ---\nItem: " ++ item'
  
  let totalAdd = length $ filter (\l -> acao l == Add && status l == Sucesso) logs
      totalRemove = length $ filter (\l -> acao l == Remove && status l == Sucesso) logs
      totalUpdate = length $ filter (\l -> acao l == Update && status l == Sucesso) logs
  
  putStrLn "\n--- ESTATÍSTICAS POR TIPO DE OPERAÇÃO ---"
  putStrLn $ "Adições bem-sucedidas: " ++ show totalAdd
  putStrLn $ "Remoções bem-sucedidas: " ++ show totalRemove
  putStrLn $ "Atualizações bem-sucedidas: " ++ show totalUpdate
  
  putStrLn "=========================================\n"
  where
    exibirLogErro log = do
      putStrLn $ "  • Ação: " ++ show (acao log)
      putStrLn $ "    Data/Hora: " ++ show (timestamp log)
      case status log of
        Falha msg -> putStrLn $ "    Erro: " ++ msg
        _ -> return ()
      putStrLn ""

-- Mostra ajuda com os comandos disponíveis
mostrarAjuda :: IO ()
mostrarAjuda = do
  putStrLn "\n========================================="
  putStrLn "        COMANDOS DISPONÍVEIS"
  putStrLn "========================================="
  putStrLn "add <ID> <Nome> <Qtd> <Categoria>"
  putStrLn "  - Adiciona um novo item ao inventário"
  putStrLn "  - Exemplo: add tec001 Mouse 50 Perifericos"
  putStrLn ""
  putStrLn "remove <ID> <Qtd>"
  putStrLn "  - Remove unidades de um item"
  putStrLn "  - Exemplo: remove tec001 10"
  putStrLn ""
  putStrLn "update <ID> <NovaQtd>"
  putStrLn "  - Atualiza a quantidade total de um item"
  putStrLn "  - Exemplo: update tec001 100"
  putStrLn ""
  putStrLn "list"
  putStrLn "  - Lista todos os itens do inventário"
  putStrLn ""
  putStrLn "report"
  putStrLn "  - Gera relatório de auditoria completo"
  putStrLn ""
  putStrLn "help"
  putStrLn "  - Mostra esta ajuda"
  putStrLn ""
  putStrLn "exit"
  putStrLn "  - Sai do programa"
  putStrLn "=========================================\n"

-- Loop principal de execução
loop :: Inventario -> IO ()
loop inv = do
  putStr "> "
  hFlush stdout
  linha <- getLine
  comando <- parseComando linha
  case comando of
    Nothing -> putStrLn "Encerrando o sistema..."
    Just acao' -> do
      novoInv <- acao' inv
      loop novoInv

-- Inicializa o sistema com dados de exemplo (mínimo 10 itens)
inicializarDados :: Inventario -> IO Inventario
inicializarDados inv = do
  if Map.size inv >= 10
    then return inv
    else do
      putStrLn "Inicializando inventário com dados de exemplo..."
      time <- getCurrentTime
      let dadosExemplo = 
            [ ("tec001", "Mouse", 50, "Perifericos")
            , ("tec002", "Teclado", 30, "Perifericos")
            , ("tec003", "Monitor", 20, "Monitores")
            , ("tec004", "Webcam", 15, "Perifericos")
            , ("tec005", "HeadsetUSB", 25, "Audio")
            , ("mob001", "Cadeira", 10, "Moveis")
            , ("mob002", "Mesa", 8, "Moveis")
            , ("ele001", "Notebook", 12, "Computadores")
            , ("ele002", "Tablet", 18, "Computadores")
            , ("mat001", "CanetaAzul", 100, "Material")
            ]
      let adicionarTodos inv' [] = return inv'
          adicionarTodos inv' ((iid, nome', qtd, cat):resto) = do
            case addItem time iid nome' qtd cat inv' of
              Right (novoInv, logEntry) -> do
                adicionarAuditoria logEntry
                adicionarTodos novoInv resto
              Left _ -> adicionarTodos inv' resto
      novoInv <- adicionarTodos inv dadosExemplo
      salvarInventario novoInv
      putStrLn $ "✓ Inventário inicializado com " ++ show (Map.size novoInv) ++ " itens.\n"
      return novoInv


-- FUNÇÃO PRINCIPAL


main :: IO ()
main = do
  putStrLn "========================================="
  putStrLn "    SISTEMA DE INVENTÁRIO - HASKELL"
  putStrLn "========================================="
  putStrLn "Carregando dados...\n"
  
  inv <- carregarInventario
  putStrLn $ "✓ Inventário carregado: " ++ show (Map.size inv) ++ " itens"
  
  logs <- carregarAuditoria
  putStrLn $ "✓ Log de auditoria carregado: " ++ show (length logs) ++ " entradas\n"
  
  invInicializado <- inicializarDados inv
  
  putStrLn "Digite 'help' para ver os comandos disponíveis.\n"
  
  loop invInicializado

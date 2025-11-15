# Sistema de Inventário em Haskell

## Informações Acadêmicas

**Instituição:** PUCPR

**Disciplina:** Programação Funcional 

**Professor:** Frank Coelho de Alcantara  

### Alunos do Grupo (em ordem alfabética)
- Ana Carolina Afonso Meiado - GitHub: @nacalorias
- Ana Carolina Curi de Sales - GitHub: @AnaSaless

## Descrição do Projeto

Sistema de gerenciamento de inventário desenvolvido em Haskell com foco em:
- Programação funcional pura
- Separação rigorosa entre lógica de negócio e operações de I/O
- Persistência de dados em arquivo
- Sistema de auditoria completo
- Tratamento robusto de erros

## Estrutura do Código

O código está organizado em 4 seções principais, seguindo a divisão de tarefas sugerida:

### 1. Tipos de Dados 
- `Item`: Registro com itemID, nome, quantidade e categoria
- `Inventario`: Map String Item para armazenamento eficiente
- `AcaoLog`: ADT para tipos de ação (Add, Remove, Update, etc.)
- `StatusLog`: ADT para resultado (Sucesso | Falha String)
- `LogEntry`: Registro de log com timestamp, ação, detalhes e status

Todos os tipos derivam `Show` e `Read` para serialização/desserialização.

### 2. Lógica de Negócio Pura 
Funções puras que implementam a lógica do sistema:
- `addItem`: Adiciona novo item ao inventário
- `removeItem`: Remove unidades de um item
- `updateQty`: Atualiza quantidade de um item
- `logFalha`: Cria entrada de log para operações falhadas

Todas usam `Either String ResultadoOperacao` para tratamento de erros.

### 3. I/O e Persistência 
Funções de entrada/saída e gerenciamento de estado:
- `carregarInventario`: Carrega estado do arquivo Inventario.dat
- `salvarInventario`: Persiste inventário em disco
- `carregarAuditoria`: Lê log de auditoria
- `adicionarAuditoria`: Adiciona entrada ao log (append-only)
- `parseComando`: Parser de comandos do usuário
- `loop`: Loop principal de execução

### 4. Análise de Logs e Relatórios 
Funções de análise e geração de relatórios:
- `historicoPorItem`: Filtra logs por item específico
- `logsDeErro`: Retorna apenas logs de erro
- `itemMaisMovimentado`: Identifica item com mais movimentações
- `gerarRelatorio`: Gera relatório completo de auditoria

## Como Compilar e Executar

### No Online GDB
1. Acesse https://www.onlinegdb.com/online_haskell_compiler
2. Cole o código do arquivo `Main.hs`
3. Clique em "Run"

### Compilação Local
```bash
ghc Main.hs -o inventario
./inventario

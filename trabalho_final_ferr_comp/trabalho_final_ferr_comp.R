# ==========================================================================
# trabalho_final_ferr_comp.R
# Trabalho final de Ferramentas Computacionais
# Base de dados relacional (PostgreSQL) com os dados abertos da CAPES
# (Plataforma Sucupira, 2021 a 2024): Astronomia/Física x Química
#
# Grupo: Fernando Casellato, Felipe Piancatelli, Ana Carolina Soares,
#        Marcos Antônio Garcia
#
# ESTRUTURA DO ARQUIVO
#   PARTE 1  monta a base de dados (ou restaura, ou só conecta; ver MODO_BASE)
#   PARTE 2  deixa prontos a conexão, as pastas de saída e algumas funções
#            usadas por todas as análises
#   PARTE 3  análises estatísticas: cada integrante cola o seu bloco
#            (P1, P2, P3 ou P4) ABAIXO DA ÚLTIMA LINHA deste arquivo
#
# Pacotes: tidyverse, DBI, RPostgres
#   install.packages(c("tidyverse", "DBI", "RPostgres"))
# ==========================================================================

library(tidyverse)
library(DBI)


#? Configurações ----------------------------------------------------------

# O que a PARTE 1 deve fazer:
#   "construir": lê os 12 CSVs da CAPES e monta a base do zero (apaga e
#                recria o esquema "sucupira"); no fim salva uma cópia em base/
#   "restaurar": recria a base a partir da cópia mais recente em base/
#                (não precisa dos CSVs; é o modo para quem clonou o repositório)
#   "usar":      a base já existe no PostgreSQL; só conecta
MODO_BASE <- "construir"

# Pasta do trabalho (onde ficam base/, figuras/ e tabelas/)
pasta_trabalho <- "~/Downloads/dados_abertos_sucupira/trabalho_final_ferr_comp"

# Pasta com os 12 CSVs originais da CAPES (só usada no modo "construir")
pasta_dados <- "~/Downloads/dados_abertos_sucupira"

# Conexão com o PostgreSQL local
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sucupira", host = "localhost", port = 5432,
                 user = "postgres", password = "1234")

pasta_base <- file.path(path.expand(pasta_trabalho), "base")
dir.create(pasta_base, showWarnings = FALSE, recursive = TRUE)


# ==========================================================================
# PARTE 1. BASE DE DADOS
# ==========================================================================

if (MODO_BASE == "construir") {

  #! 1.1 Criação das tabelas ----------------------------------------------
  # Mesma estrutura da aula de SQL:
  #   tabelas de referência (ref_): guardam cada área, instituição e
  #     programa UMA vez só;
  #   tabelas de dados (tab_): guardam os registros de docentes e discentes
  #     e apontam para as de referência por chave estrangeira.
  # DROP SCHEMA apaga e recria só o esquema do trabalho.

  comandos_sql <- c(
    "DROP SCHEMA IF EXISTS sucupira CASCADE;",
    "CREATE SCHEMA sucupira;",

    # Áreas de avaliação da CAPES (aqui, Astronomia/Física e Química)
    "CREATE TABLE sucupira.ref_area (
       id_area INT GENERATED ALWAYS AS IDENTITY,
       cd_area INT NOT NULL,              -- código da área na CAPES
       nm_area VARCHAR NOT NULL,          -- nome da área de avaliação
       UNIQUE (cd_area),
       CONSTRAINT pk_area PRIMARY KEY (id_area));",

    # Instituições de ensino superior (IES)
    "CREATE TABLE sucupira.ref_ies (
       id_ies INT GENERATED ALWAYS AS IDENTITY,
       cd_ies VARCHAR NOT NULL,           -- código da IES na CAPES
       sg_ies VARCHAR,                    -- sigla
       nm_ies VARCHAR NOT NULL,           -- nome
       dependencia VARCHAR,               -- dependência administrativa
       uf CHAR(2),                        -- unidade da federação
       regiao VARCHAR,                    -- grande região
       UNIQUE (cd_ies),
       CONSTRAINT pk_ies PRIMARY KEY (id_ies));",

    # Programas de pós-graduação
    "CREATE TABLE sucupira.ref_programa (
       id_prog INT GENERATED ALWAYS AS IDENTITY,
       cd_programa VARCHAR(13) NOT NULL,  -- código do programa na CAPES
       nm_programa VARCHAR NOT NULL,      -- nome do programa
       grau VARCHAR,                      -- MESTRADO, DOUTORADO ou MESTRADO/DOUTORADO
       nota VARCHAR(1),                   -- nota da Quadrienal 2017-2020 (2 a 7, ou A = programa novo)
       area_id INT NOT NULL,              -- aponta para ref_area
       ies_id INT NOT NULL,               -- aponta para ref_ies
       UNIQUE (cd_programa),
       CONSTRAINT pk_prog PRIMARY KEY (id_prog));",
    "ALTER TABLE sucupira.ref_programa ADD FOREIGN KEY(area_id) REFERENCES sucupira.ref_area (id_area);",
    "ALTER TABLE sucupira.ref_programa ADD FOREIGN KEY(ies_id) REFERENCES sucupira.ref_ies (id_ies);",

    # Docentes: uma linha por vínculo docente-programa em cada ano
    "CREATE TABLE sucupira.tab_docente (
       id_docente INT GENERATED ALWAYS AS IDENTITY,
       ano INT NOT NULL,                  -- ano-base da coleta (2021 a 2024)
       programa_id INT NOT NULL,          -- aponta para ref_programa
       id_pessoa INT NOT NULL,            -- identificador da pessoa na CAPES
       categoria VARCHAR,                 -- PERMANENTE ou COLABORADOR
       bolsa_pq VARCHAR,                  -- bolsa PQ do CNPq (2, 1D, 1C, 1B, 1A, SR); vazio = sem bolsa
       ano_titulacao INT,                 -- ano do último grau
       pais_titulacao VARCHAR,            -- país da IES do último grau
       UNIQUE (ano, programa_id, id_pessoa),
       CONSTRAINT pk_docente PRIMARY KEY (id_docente));",
    "ALTER TABLE sucupira.tab_docente ADD FOREIGN KEY(programa_id) REFERENCES sucupira.ref_programa (id_prog);",

    # Discentes: uma linha por discente em cada programa, grau e ano
    "CREATE TABLE sucupira.tab_discente (
       id_discente INT GENERATED ALWAYS AS IDENTITY,
       ano INT NOT NULL,                  -- ano-base da coleta (2021 a 2024)
       programa_id INT NOT NULL,          -- aponta para ref_programa
       id_pessoa INT NOT NULL,            -- identificador da pessoa na CAPES
       grau VARCHAR,                      -- MESTRADO ou DOUTORADO
       situacao VARCHAR,                  -- MATRICULADO, TITULADO, DESLIGADO...
       meses_titulacao INT,               -- meses até a titulação (0 para quem não titulou)
       dt_matricula DATE,                 -- data da matrícula
       dt_situacao DATE,                  -- data da situação (para titulados, data da titulação)
       UNIQUE (ano, programa_id, id_pessoa, grau),
       CONSTRAINT pk_discente PRIMARY KEY (id_discente));",
    "ALTER TABLE sucupira.tab_discente ADD FOREIGN KEY(programa_id) REFERENCES sucupira.ref_programa (id_prog);"
  )

  walk(comandos_sql, ~ dbExecute(con, .x))
  dbListObjects(con, Id(schema = "sucupira"))   # devem aparecer as 5 tabelas


  #! 1.2 Funções da carga -------------------------------------------------

  AREAS <- c("ASTRONOMIA / FÍSICA", "QUÍMICA")

  # Lê as planilhas de um tema (prog, docente ou discentes), uma por ano.
  # Cada arquivo é filtrado logo após a leitura (áreas do trabalho e só
  # programas acadêmicos), para não acumular na memória o Brasil inteiro.
  #   fileEncoding = "latin1": os arquivos da CAPES não estão em UTF-8
  #   colClasses = "character": tudo como texto, para os códigos não perderem dígitos
  ler_planilhas <- function(tema, colunas) {
    arquivos <- list.files(pasta_dados,
                           pattern = paste0("^br-capes-colsucup-", tema, "-.*\\.csv$"),
                           full.names = TRUE)
    map(arquivos, function(arq) {
      message("Lendo ", basename(arq))
      read.csv2(arq, fileEncoding = "latin1", colClasses = "character",
                na.strings = c("", "NA")) %>%
        filter(NM_AREA_AVALIACAO %in% AREAS,
               NM_MODALIDADE_PROGRAMA == "ACADÊMICO") %>%
        select(all_of(colunas))
    }) %>%
      bind_rows()
  }

  # Mesma função da aula: troca a coluna-código X pelo identificador Z da
  # tabela de referência tabY (via left_join com a tabela que está na base).
  trocar_id <- function(con, df, X, Z, tabY){
    tab <- dbGetQuery(con, paste0("SELECT ", X, ",", Z, " FROM ", tabY, ";"))
    df %>%
      left_join(tab, by = X) %>%
      select(-all_of(X))
  }

  # Datas no formato "11MAR2024:00:00:00", com o mês em inglês. A conversão
  # é feita "na mão" para funcionar com o R em qualquer idioma.
  converter_data <- function(x) {
    meses <- c(JAN = "01", FEB = "02", MAR = "03", APR = "04", MAY = "05", JUN = "06",
               JUL = "07", AUG = "08", SEP = "09", OCT = "10", NOV = "11", DEC = "12")
    as.Date(paste(substr(x, 6, 9), meses[substr(x, 3, 5)], substr(x, 1, 2), sep = "-"))
  }

  gravar <- function(df, tabela) {
    dbWriteTable(con, name = Id(schema = "sucupira", table = tabela), value = df,
                 row.names = FALSE, append = TRUE)
  }


  #! 1.3 Programas -> ref_area, ref_ies, ref_programa ---------------------

  prog <- ler_planilhas("prog", c(
    "AN_BASE", "CD_AREA_AVALIACAO", "NM_AREA_AVALIACAO",
    "CD_ENTIDADE_CAPES", "SG_ENTIDADE_ENSINO", "NM_ENTIDADE_ENSINO",
    "DS_DEPENDENCIA_ADMINISTRATIVA", "SG_UF_PROGRAMA", "NM_REGIAO",
    "CD_PROGRAMA_IES", "NM_PROGRAMA_IES", "NM_GRAU_PROGRAMA", "CD_CONCEITO_PROGRAMA"))

  count(prog, AN_BASE, NM_AREA_AVALIACAO)   # esperado: 61-64 de física e 71-73 de química por ano

  # Alguns programas e instituições mudam de nome, sigla ou IES entre os anos:
  # do ano mais recente para o mais antigo, o distinct() guarda a versão mais recente.
  prog <- arrange(prog, desc(AN_BASE))

  prog %>%
    distinct(cd_area = as.integer(CD_AREA_AVALIACAO), nm_area = NM_AREA_AVALIACAO) %>%
    gravar("ref_area")

  prog %>%
    distinct(CD_ENTIDADE_CAPES, .keep_all = TRUE) %>%
    select(cd_ies = CD_ENTIDADE_CAPES, sg_ies = SG_ENTIDADE_ENSINO,
           nm_ies = NM_ENTIDADE_ENSINO, dependencia = DS_DEPENDENCIA_ADMINISTRATIVA,
           uf = SG_UF_PROGRAMA, regiao = NM_REGIAO) %>%
    gravar("ref_ies")

  prog %>%
    distinct(CD_PROGRAMA_IES, .keep_all = TRUE) %>%
    transmute(cd_programa = CD_PROGRAMA_IES, nm_programa = NM_PROGRAMA_IES,
              grau = NM_GRAU_PROGRAMA, nota = CD_CONCEITO_PROGRAMA,
              cd_area = as.integer(CD_AREA_AVALIACAO), cd_ies = CD_ENTIDADE_CAPES) %>%
    trocar_id(con, ., "cd_area", "id_area", "sucupira.ref_area") %>%
    trocar_id(con, ., "cd_ies", "id_ies", "sucupira.ref_ies") %>%
    rename(area_id = id_area, ies_id = id_ies) %>%
    gravar("ref_programa")


  #! 1.4 Docentes -> tab_docente ------------------------------------------

  doc <- ler_planilhas("docente", c(
    "AN_BASE", "CD_PROGRAMA_IES", "ID_PESSOA", "DS_CATEGORIA_DOCENTE",
    "CD_CAT_BOLSA_PRODUTIVIDADE", "AN_TITULACAO", "NM_PAIS_IES_TITULACAO"))

  doc %>%
    filter(DS_CATEGORIA_DOCENTE %in% c("PERMANENTE", "COLABORADOR")) %>%   # visitantes ficam de fora
    transmute(ano = as.integer(AN_BASE), cd_programa = CD_PROGRAMA_IES,
              id_pessoa = as.integer(ID_PESSOA), categoria = DS_CATEGORIA_DOCENTE,
              bolsa_pq = CD_CAT_BOLSA_PRODUTIVIDADE,
              ano_titulacao = as.integer(AN_TITULACAO),
              pais_titulacao = NM_PAIS_IES_TITULACAO) %>%
    trocar_id(con, ., "cd_programa", "id_prog", "sucupira.ref_programa") %>%
    rename(programa_id = id_prog) %>%
    gravar("tab_docente")


  #! 1.5 Discentes -> tab_discente ----------------------------------------
  # São as planilhas maiores: a leitura demora mais.

  disc <- ler_planilhas("discentes", c(
    "AN_BASE", "CD_PROGRAMA_IES", "ID_PESSOA", "DS_GRAU_ACADEMICO_DISCENTE",
    "NM_SITUACAO_DISCENTE", "QT_MES_TITULACAO",
    "DT_MATRICULA_DISCENTE", "DT_SITUACAO_DISCENTE"))

  disc %>%
    transmute(ano = as.integer(AN_BASE), cd_programa = CD_PROGRAMA_IES,
              id_pessoa = as.integer(ID_PESSOA), grau = DS_GRAU_ACADEMICO_DISCENTE,
              situacao = NM_SITUACAO_DISCENTE,
              meses_titulacao = as.integer(QT_MES_TITULACAO),
              dt_matricula = converter_data(DT_MATRICULA_DISCENTE),
              dt_situacao = converter_data(DT_SITUACAO_DISCENTE)) %>%
    trocar_id(con, ., "cd_programa", "id_prog", "sucupira.ref_programa") %>%
    rename(programa_id = id_prog) %>%
    gravar("tab_discente")

  rm(prog, doc, disc)


  #! 1.6 Salvar a base ----------------------------------------------------
  # Cópia completa do esquema (tabelas, chaves e dados) num único arquivo .sql,
  # feito com o pg_dump. É esse arquivo que vai para o repositório: com ele,
  # quem não tem os CSVs recria a base no modo "restaurar".
  # Nunca sobrescreve uma cópia anterior: se o nome já existir, ganha _v2, _v3...

  arquivo_base <- file.path(pasta_base, paste0("sucupira_base_", Sys.Date(), ".sql"))
  v <- 2
  while (file.exists(arquivo_base)) {
    arquivo_base <- file.path(pasta_base, paste0("sucupira_base_", Sys.Date(), "_v", v, ".sql"))
    v <- v + 1
  }
  Sys.setenv(PGPASSWORD = "1234")
  status <- system2("pg_dump", c("-h", "localhost", "-U", "postgres", "-d", "sucupira",
                                 "-n", "sucupira", "--clean", "--if-exists", "--no-owner",
                                 "-f", shQuote(arquivo_base)))
  if (status == 0) message("Base salva em ", arquivo_base) else warning("pg_dump falhou")

} else if (MODO_BASE == "restaurar") {

  #! Restaura a base a partir da cópia .sql mais recente da pasta base/
  copias <- list.files(pasta_base, pattern = "^sucupira_base_.*\\.sql$", full.names = TRUE)
  if (length(copias) == 0) stop("Nenhuma cópia da base em ", pasta_base)
  copia <- copias[which.max(file.mtime(copias))]
  message("Restaurando ", basename(copia))
  Sys.setenv(PGPASSWORD = "1234")
  status <- system2("psql", c("-q", "-h", "localhost", "-U", "postgres", "-d", "sucupira",
                              "-f", shQuote(copia)), stdout = FALSE)
  if (status != 0) stop("psql falhou ao restaurar a base")
}


#! Conferência da base (roda em qualquer modo) ----------------------------

dbGetQuery(con, "
  SELECT 'ref_area' AS tabela, COUNT(*) AS linhas FROM sucupira.ref_area
  UNION ALL SELECT 'ref_ies', COUNT(*) FROM sucupira.ref_ies
  UNION ALL SELECT 'ref_programa', COUNT(*) FROM sucupira.ref_programa
  UNION ALL SELECT 'tab_docente', COUNT(*) FROM sucupira.tab_docente
  UNION ALL SELECT 'tab_discente', COUNT(*) FROM sucupira.tab_discente;")
# esperado: 2, 84, 139, 16628, 50078


# ==========================================================================
# PARTE 2. OBJETOS COMUNS ÀS ANÁLISES
# ==========================================================================
# Os blocos de análise da PARTE 3 usam o que está definido aqui.

# Pastas de saída
pasta_figuras <- file.path(path.expand(pasta_trabalho), "figuras")
pasta_tabelas <- file.path(path.expand(pasta_trabalho), "tabelas")
dir.create(pasta_figuras, showWarnings = FALSE)
dir.create(pasta_tabelas, showWarnings = FALSE)

# Nome de arquivo livre: se "nome.ext" já existe, devolve nome_v2.ext,
# nome_v3.ext... Assim nenhuma figura ou tabela anterior é sobrescrita.
nome_livre <- function(pasta, nome, ext) {
  arq <- file.path(pasta, paste0(nome, ".", ext))
  v <- 2
  while (file.exists(arq)) {
    arq <- file.path(pasta, paste0(nome, "_v", v, ".", ext))
    v <- v + 1
  }
  arq
}

# Números no padrão brasileiro (vírgula decimal) para os textos dos gráficos
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")
fmt_p <- function(p) if (p < 0.001) "< 0,001" else paste("=", fmt(p, 3))

# Nomes das áreas para os gráficos
rotulo_area <- function(x) recode(x, "ASTRONOMIA / FÍSICA" = "Astronomia/Física",
                                  "QUÍMICA" = "Química")

# Nota do programa em categorias. Só UM programa tem nota 2 (em
# Astronomia/Física); sozinho ele não sustenta uma categoria, por isso
# entra com os de nota 3 em "3 ou menos". Programas novos (conceito "A")
# ficam de fora das análises por nota (filtrar antes com nota != "A").
nota_categoria <- function(nota) {
  factor(if_else(nota %in% c("2", "3"), "3 ou menos", nota),
         levels = c("3 ou menos", "4", "5", "6", "7"))
}

# Uma cor fixa para cada área em todos os gráficos (paleta testada para
# daltonismo e para impressão em tons de cinza)
cores_area <- c("Astronomia/Física" = "#2a78d6", "Química" = "#eb6834")


# ==========================================================================
# PARTE 3. ANÁLISES ESTATÍSTICAS
# Cole o seu bloco (P1, P2, P3 ou P4) abaixo da última linha.
# ==========================================================================

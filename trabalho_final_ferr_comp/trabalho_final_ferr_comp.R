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
MODO_BASE <- "usar"

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


# ==========================================================================
# P1. Região e nota dos programas
# Ano de referência: 2024
# Testes feitos por programa (região e nota são do programa, não do docente)
# ==========================================================================

#* P1.1 Programas em 2024 --------------------------------------------------
# uma linha por programa, com região, nota e nº de docentes

p1_programas <- dbGetQuery(con, "
  SELECT p.id_prog, a.nm_area, i.regiao, p.nota,
         COUNT(DISTINCT d.id_pessoa) AS docentes
  FROM sucupira.tab_docente d
  JOIN sucupira.ref_programa p ON d.programa_id = p.id_prog
  JOIN sucupira.ref_area a     ON p.area_id = a.id_area
  JOIN sucupira.ref_ies i      ON p.ies_id = i.id_ies
  WHERE d.ano = 2024
  GROUP BY p.id_prog, a.nm_area, i.regiao, p.nota;") %>%
  mutate(area = rotulo_area(nm_area),
         docentes = as.integer(docentes),
         regiao = factor(str_to_title(regiao),
                         levels = c("Norte", "Nordeste", "Centro-Oeste",
                                    "Sudeste", "Sul")))

count(p1_programas, area)


#* P1.2 Programas por região -----------------------------------------------
# Fisher em vez de qui-quadrado: tem célula com 2 a 4 programas

p1_tab_regiao <- table(p1_programas$area, p1_programas$regiao)
p1_tab_regiao
p1_fisher_regiao <- fisher.test(p1_tab_regiao)
p1_fisher_regiao

# % de programas e de docentes por região (docentes só como descrição;
# quem está em 2 programas conta nos 2)
p1_regiao <- p1_programas %>%
  group_by(area, regiao) %>%
  summarise(Programas = n(), Docentes = sum(docentes), .groups = "drop") %>%
  pivot_longer(c(Programas, Docentes),
               names_to = "unidade", values_to = "n") %>%
  group_by(area, unidade) %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  ungroup() %>%
  mutate(unidade = factor(unidade, levels = c("Programas", "Docentes")))

p1_regiao %>% select(-total) %>%
  pivot_wider(names_from = regiao, values_from = c(n, pct))

write_csv(p1_regiao,
          nome_livre(pasta_tabelas, "P1_programas_docentes_por_regiao_2024", "csv"))

# Gráfico 1: programas e docentes por região
p1_tema <- theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 12, hjust = 0),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(color = "grey40", hjust = 0, size = 9),
        plot.title.position = "plot", plot.caption.position = "plot")

p1_n <- p1_programas %>%
  group_by(area) %>% summarise(prog = n(), doc = sum(docentes))
p1_rodape_regiao <- paste0(
  "Astronomia/Física: ", p1_n$prog[1], " programas e ", p1_n$doc[1],
  " docentes; Química: ", p1_n$prog[2], " programas e ", p1_n$doc[2],
  " docentes (inclui programas novos, conceito A).\n",
  "Teste exato de Fisher sobre os programas (área x região): p ",
  fmt_p(p1_fisher_regiao$p.value),
  ". Fonte: CAPES, Dados Abertos (Sucupira).")

p1_g1 <- ggplot(p1_regiao,
                aes(x = pct, y = fct_rev(regiao), fill = area,
                    group = fct_rev(area))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72,
           color = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(fmt(pct, 0), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.15, size = 3.2, color = "grey25") +
  facet_wrap(~ unidade) +
  scale_fill_manual(values = cores_area, name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 70), expand = expansion(mult = c(0, 0))) +
  labs(title = "Programas e docentes por região",
       subtitle = "Percentual dentro de cada área (2024)",
       x = NULL, y = NULL, caption = p1_rodape_regiao) +
  p1_tema +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"))

p1_g1

ggsave(nome_livre(pasta_figuras, "P1_programas_docentes_por_regiao", "pdf"),
       p1_g1, width = 8.5, height = 4.8, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P1_programas_docentes_por_regiao", "png"),
       p1_g1, width = 8.5, height = 4.8, dpi = 300, bg = "white")


#* P1.3 Nota por área ------------------------------------------------------
# sem conceito A; Mann-Whitney porque a nota é ordinal

p1_com_nota <- p1_programas %>%
  filter(nota != "A") %>%
  mutate(nota_num = as.integer(nota), nota = nota_categoria(nota))

p1_com_nota %>%
  group_by(area) %>%
  summarise(programas = n(), mediana = median(nota_num),
            media = mean(nota_num), pct_6_ou_7 = 100 * mean(nota_num >= 6))

p1_mw_nota <- wilcox.test(nota_num ~ area, data = p1_com_nota,
                          exact = FALSE)
p1_mw_nota

p1_notas <- p1_com_nota %>%
  count(area, nota, .drop = FALSE) %>%
  group_by(area) %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  ungroup()

write_csv(p1_notas, nome_livre(pasta_tabelas, "P1_programas_por_nota_2024", "csv"))

# Gráfico 2: programas por nota
p1_n_nota <- distinct(p1_notas, area, total)

p1_g2 <- ggplot(p1_notas, aes(x = nota, y = n, fill = area)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72,
           color = "white", linewidth = 0.6) +
  geom_text(aes(label = n), position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.4, color = "grey25") +
  scale_fill_manual(values = cores_area, name = NULL) +
  scale_y_continuous(limits = c(0, max(p1_notas$n) * 1.12),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Programas por nota",
       subtitle = "Número de programas em cada nota da Avaliação Quadrienal 2017 a 2020",
       x = "Nota do programa", y = "Programas",
       caption = paste0(
         "Programas com docentes em 2024 e com nota: Astronomia/Física n = ",
         p1_n_nota$total[1], "; Química n = ", p1_n_nota$total[2],
         ".\nEm 2024, \"3 ou menos\" contém só programas de nota 3. ",
         "Teste de Mann-Whitney entre as áreas: p ", fmt_p(p1_mw_nota$p.value),
         ".\nFonte: CAPES, Dados Abertos (Sucupira).")) +
  p1_tema +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"))

p1_g2

ggsave(nome_livre(pasta_figuras, "P1_programas_por_nota", "pdf"), p1_g2,
       width = 7.5, height = 4.8, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P1_programas_por_nota", "png"), p1_g2,
       width = 7.5, height = 4.8, dpi = 300, bg = "white")


#* P1.4 Nota por região ----------------------------------------------------
# Kruskal-Wallis com as duas áreas juntas e depois separadas

p1_kw_todas <- kruskal.test(nota_num ~ regiao, data = p1_com_nota)
p1_kw_fisica <- kruskal.test(
  nota_num ~ regiao,
  data = filter(p1_com_nota, area == "Astronomia/Física"))
p1_kw_quimica <- kruskal.test(
  nota_num ~ regiao, data = filter(p1_com_nota, area == "Química"))
p1_kw_todas; p1_kw_fisica; p1_kw_quimica

# mediana e % de programas com nota 6 ou 7 em cada região
p1_nota_regiao <- p1_com_nota %>%
  group_by(regiao) %>%
  summarise(programas = n(), mediana = median(nota_num),
            nota_6_ou_7 = sum(nota_num >= 6),
            pct_6_ou_7 = 100 * mean(nota_num >= 6))
p1_nota_regiao

p1_com_nota %>%
  group_by(area, regiao) %>%
  summarise(programas = n(), mediana = median(nota_num),
            nota_6_ou_7 = sum(nota_num >= 6), .groups = "drop")

write_csv(p1_nota_regiao, nome_livre(pasta_tabelas, "P1_nota_por_regiao_2024", "csv"))
write_csv(tibble(
  teste = c("Fisher: area x regiao (programas)",
            "Mann-Whitney: nota entre areas",
            "Kruskal-Wallis: nota entre regioes (duas areas)",
            "Kruskal-Wallis: nota entre regioes (Astronomia/Fisica)",
            "Kruskal-Wallis: nota entre regioes (Quimica)"),
  estatistica = c(NA, unname(p1_mw_nota$statistic),
                  unname(p1_kw_todas$statistic),
                  unname(p1_kw_fisica$statistic),
                  unname(p1_kw_quimica$statistic)),
  p_valor = c(p1_fisher_regiao$p.value, p1_mw_nota$p.value,
              p1_kw_todas$p.value, p1_kw_fisica$p.value,
              p1_kw_quimica$p.value)),
  nome_livre(pasta_tabelas, "P1_testes", "csv"))

# Gráfico 3: nota por região (cinza mais escuro = nota maior)
p1_composicao <- count(p1_com_nota, area, regiao, nota)

p1_cinzas <- c("3 ou menos" = "#e4e3dd", "4" = "#bdbcb5",
               "5" = "#8f8e87", "6" = "#5f5e59", "7" = "#2f2f2c")
p1_rodape_nota <- paste0(
  "Programas com nota (conceito A excluído). Kruskal-Wallis da nota ",
  "entre regiões: duas áreas juntas p ", fmt_p(p1_kw_todas$p.value),
  ";\nAstronomia/Física p ", fmt_p(p1_kw_fisica$p.value),
  "; Química p ", fmt_p(p1_kw_quimica$p.value),
  ". Fonte: CAPES, Dados Abertos (Sucupira).")

p1_g3 <- ggplot(p1_composicao,
                aes(x = n, y = fct_rev(regiao), fill = fct_rev(nota))) +
  geom_col(width = 0.72, color = "white", linewidth = 0.6) +
  geom_text(aes(label = n, color = nota %in% c("6", "7"),
                group = fct_rev(nota)),
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ area) +
  scale_fill_manual(values = p1_cinzas, name = "Nota",
                    breaks = levels(p1_composicao$nota)) +
  scale_color_manual(values = c("TRUE" = "white",
                                "FALSE" = "grey15"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
  labs(title = "Nota dos programas por região",
       subtitle = "Programas em cada região, por nota (2024)",
       x = "Programas", y = NULL, caption = p1_rodape_nota) +
  p1_tema +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"))

p1_g3

ggsave(nome_livre(pasta_figuras, "P1_nota_por_regiao", "pdf"), p1_g3,
       width = 8.5, height = 4.8, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P1_nota_por_regiao", "png"), p1_g3,
       width = 8.5, height = 4.8, dpi = 300, bg = "white")


# ==========================================================================
# P2. Bolsa de produtividade (PQ) do CNPq
# Ano de referência: 2024 (2021 a 2023 só no gráfico de evolução)
# Modelo ajustado por programa, com quasibinomial
# ==========================================================================

#* P2.1 Docentes por área e ano --------------------------------------------
# sem conceito A; quem está em 2 programas da mesma área conta 1 vez
# (no de nota mais alta)

p2_docentes <- dbGetQuery(con, "
  SELECT DISTINCT ON (d.ano, a.nm_area, d.id_pessoa)
         d.ano, a.nm_area, d.id_pessoa, p.id_prog, p.nota, d.bolsa_pq
  FROM sucupira.tab_docente d
  JOIN sucupira.ref_programa p ON d.programa_id = p.id_prog
  JOIN sucupira.ref_area a     ON p.area_id = a.id_area
  WHERE p.nota <> 'A'
  ORDER BY d.ano, a.nm_area, d.id_pessoa, p.nota DESC;") %>%
  mutate(area = rotulo_area(nm_area),
         nota = nota_categoria(nota),
         pq = !is.na(bolsa_pq),
         # nível 1 = 1A, 1B, 1C, 1D e SR (sênior)
         nivel_pq = factor(case_when(is.na(bolsa_pq) ~ "Sem bolsa",
                                     bolsa_pq == "2" ~ "Nível 2",
                                     TRUE ~ "Nível 1"),
                           levels = c("Nível 1", "Nível 2", "Sem bolsa")))

p2_doc24 <- filter(p2_docentes, ano == 2024)
count(p2_doc24, area, nivel_pq)


#* P2.2 Bolsistas por programa e por nota ----------------------------------

# por programa
p2_prog <- p2_doc24 %>%
  group_by(id_prog, area, nota) %>%
  summarise(docentes = n(), bolsistas = sum(pq),
            .groups = "drop") %>%
  mutate(pct = 100 * bolsistas / docentes)

# variação entre programas
p2_prog %>%
  group_by(area) %>%
  summarise(programas = n(), minimo = min(pct), q1 = quantile(pct, 0.25),
            mediana = median(pct), q3 = quantile(pct, 0.75), maximo = max(pct))

# % do conjunto de docentes por área e nota
p2_conj <- p2_prog %>%
  group_by(area, nota) %>%
  summarise(programas = n(), docentes = sum(docentes),
            bolsistas = sum(bolsistas), .groups = "drop") %>%
  mutate(pct = 100 * bolsistas / docentes)
p2_conj

write_csv(p2_conj, nome_livre(pasta_tabelas, "P2_pq_por_area_e_nota_2024", "csv"))


#* P2.3 Regressão logística ------------------------------------------------
# resposta = bolsistas e não bolsistas de cada programa
# referência: Astronomia/Física, nota 4
# quasibinomial por causa da sobredispersão entre programas

p2_dados_mod <- p2_prog %>%
  mutate(nota_num = as.integer(substr(as.character(nota), 1, 1)),
         nota = relevel(nota, ref = "4"), area = factor(area))

p2_modelo <- glm(
  cbind(bolsistas, docentes - bolsistas) ~ area + nota,
  family = quasibinomial, data = p2_dados_mod)
summary(p2_modelo)

p2_razoes <- tibble(termo = names(coef(p2_modelo)),
                    razao_chances = exp(coef(p2_modelo)),
                    ic95_inf = exp(confint.default(p2_modelo)[, 1]),
                    ic95_sup = exp(confint.default(p2_modelo)[, 2]),
                    p_valor = summary(p2_modelo)$coefficients[, 4])
p2_razoes

# nota como número (efeito por ponto de nota)
p2_tendencia <- glm(
  cbind(bolsistas, docentes - bolsistas) ~ area + nota_num,
  family = quasibinomial, data = p2_dados_mod)
exp(cbind(coef(p2_tendencia), confint.default(p2_tendencia)))

# interação área x nota, com a nota em categorias e como número
p2_inter_cat <- glm(
  cbind(bolsistas, docentes - bolsistas) ~ area * nota,
  family = quasibinomial, data = p2_dados_mod)
p2_teste_cat <- anova(p2_modelo, p2_inter_cat, test = "F")
p2_inter_lin <- glm(
  cbind(bolsistas, docentes - bolsistas) ~ area * nota_num,
  family = quasibinomial, data = p2_dados_mod)
p2_teste_lin <- anova(p2_tendencia, p2_inter_lin, test = "F")
p2_teste_cat; p2_teste_lin

# efeito por ponto de nota em cada área
p2_b <- coef(p2_inter_lin)
# último coeficiente = interação área x nota_num
p2_por_ponto <- c("Astronomia/Física" = exp(unname(p2_b["nota_num"])),
                  "Química" = exp(unname(p2_b["nota_num"] + tail(p2_b, 1))))
p2_por_ponto

write_csv(p2_razoes,
          nome_livre(pasta_tabelas, "P2_razoes_de_chance_quasibinomial", "csv"))
write_csv(tibble(
  item = c("dispersao (modelo area + nota)",
           "razao de chances por ponto de nota",
           "interacao area x nota (categorias): p",
           "interacao area x nota (tendencia): p",
           "razao por ponto, Astronomia/Fisica",
           "razao por ponto, Quimica"),
  valor = c(summary(p2_modelo)$dispersion,
            exp(coef(p2_tendencia)["nota_num"]),
            p2_teste_cat$`Pr(>F)`[2], p2_teste_lin$`Pr(>F)`[2],
            p2_por_ponto)),
  nome_livre(pasta_tabelas, "P2_tendencia_e_interacao", "csv"))


#* P2.4 Gráficos -----------------------------------------------------------

p2_tema <- theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(color = "grey40", hjust = 0, size = 9),
        plot.title.position = "plot", plot.caption.position = "plot")

p2_n <- p2_prog %>%
  group_by(area) %>% summarise(prog = n(), doc = sum(docentes))
p2_rodape <- paste0(
  "Astronomia/Física: ", p2_n$prog[1], " programas e ", p2_n$doc[1],
  " docentes; Química: ", p2_n$prog[2], " programas e ", p2_n$doc[2],
  " docentes (2024, conceito A excluído).")

# Gráfico 1: um ponto por programa, traço = % do conjunto
p2_subtitulo <- paste("Ponto: um programa. Traço: percentual do",
                      "conjunto da área e da nota (2024)")
p2_rodape_g1 <- paste0(
  p2_rodape, "\nEm 2024, \"3 ou menos\" contém só programas de nota 3. ",
  "Fonte: CAPES, Dados Abertos (Sucupira).")
p2_espalha <- position_jitterdodge(jitter.width = 0.18,
                                   dodge.width = 0.7, seed = 1)

p2_g1 <- ggplot(p2_prog, aes(x = nota, y = pct, color = area)) +
  geom_point(position = p2_espalha, size = 1.9, alpha = 0.55) +
  geom_errorbar(data = p2_conj, aes(ymin = pct, ymax = pct),
                position = position_dodge(width = 0.7),
                width = 0.55, linewidth = 1.1) +
  scale_color_manual(values = cores_area, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Docentes com bolsa PQ por nota do programa",
       subtitle = p2_subtitulo,
       x = "Nota do programa (Quadrienal 2017 a 2020)", y = NULL,
       caption = p2_rodape_g1) +
  p2_tema

p2_g1

ggsave(nome_livre(pasta_figuras, "P2_pq_por_nota_programas", "pdf"), p2_g1,
       width = 7.5, height = 4.8, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P2_pq_por_nota_programas", "png"), p2_g1,
       width = 7.5, height = 4.8, dpi = 300, bg = "white")

# Gráfico 2: nível da bolsa por nota (barras de 100%)
p2_comp <- p2_doc24 %>%
  count(area, nota, nivel_pq) %>%
  group_by(area, nota) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

write_csv(p2_comp, nome_livre(pasta_tabelas, "P2_composicao_nivel_pq_2024", "csv"))

p2_g2 <- ggplot(p2_comp, aes(x = nota, y = pct, fill = fct_rev(nivel_pq))) +
  geom_col(width = 0.72, color = "white", linewidth = 0.6) +
  geom_text(aes(label = if_else(pct >= 6, paste0(fmt(pct, 0), "%"), ""),
                color = nivel_pq == "Nível 1", group = fct_rev(nivel_pq)),
            position = position_stack(vjust = 0.5), size = 3.1,
            show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey15"),
                     guide = "none") +
  scale_fill_manual(values = c("Nível 1" = "#3d3d3a", "Nível 2" = "#8f8e87",
                               "Sem bolsa" = "#dcdbd5"),
                    breaks = c("Nível 1", "Nível 2", "Sem bolsa"), name = NULL) +
  facet_wrap(~ area) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Nível da bolsa de produtividade por nota do programa",
       subtitle = "Composição dos docentes de cada nota (2024)",
       x = "Nota do programa", y = NULL,
       caption = paste0(p2_rodape, "\nNível 1 inclui 1A a 1D e sênior (SR). ",
                        "Fonte: CAPES, Dados Abertos (Sucupira).")) +
  p2_tema +
  theme(strip.text = element_text(face = "bold", size = 12, hjust = 0))

p2_g2

ggsave(nome_livre(pasta_figuras, "P2_nivel_pq_por_nota", "pdf"), p2_g2,
       width = 8.5, height = 4.8, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P2_nivel_pq_por_nota", "png"), p2_g2,
       width = 8.5, height = 4.8, dpi = 300, bg = "white")

# Gráfico 3: evolução 2021-2024
p2_evolucao <- p2_docentes %>%
  group_by(ano, area) %>%
  summarise(n = n(), bolsistas = sum(pq), .groups = "drop") %>%
  mutate(pct = 100 * bolsistas / n)

write_csv(p2_evolucao, nome_livre(pasta_tabelas, "P2_evolucao_pq_2021_2024", "csv"))

p2_g3 <- ggplot(p2_evolucao, aes(x = ano, y = pct, color = area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(data = filter(p2_evolucao, ano == max(ano)),
            aes(label = area), hjust = 0, nudge_x = 0.08, size = 3.6,
            show.legend = FALSE) +
  scale_color_manual(values = cores_area, guide = "none") +
  scale_x_continuous(breaks = 2021:2024,
                     expand = expansion(add = c(0.2, 1.1))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(p2_evolucao$pct) * 1.25),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Docentes com bolsa de produtividade, 2021 a 2024",
       subtitle = "Percentual dos docentes de cada área, todas as notas",
       x = NULL, y = NULL,
       caption = "Docentes distintos por ano e área; conceito A excluído.\nFonte: CAPES, Dados Abertos (Sucupira).") +
  p2_tema

p2_g3

ggsave(nome_livre(pasta_figuras, "P2_evolucao_pq", "pdf"), p2_g3,
       width = 7.5, height = 4.5, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P2_evolucao_pq", "png"), p2_g3,
       width = 7.5, height = 4.5, dpi = 300, bg = "white")


# ==========================================================================
# P3. Formação no exterior
# Ano de referência: 2024
# ==========================================================================

# P3.1 Docentes por área e ano
# Exclui programas com conceito "A" e evita duplicidade dentro da mesma área.

p3_docentes <- dbGetQuery(con, "
  SELECT DISTINCT ON (d.ano, a.nm_area, d.id_pessoa)
         d.ano, a.nm_area, d.id_pessoa, p.id_prog, p.nota,
         d.pais_titulacao
  FROM sucupira.tab_docente d
  JOIN sucupira.ref_programa p ON d.programa_id = p.id_prog
  JOIN sucupira.ref_area a     ON p.area_id = a.id_area
  WHERE p.nota <> 'A'
  ORDER BY d.ano, a.nm_area, d.id_pessoa, p.nota DESC;") %>%
  mutate(area = rotulo_area(nm_area),
         nota = nota_categoria(nota),
         exterior = pais_titulacao != "BRASIL")

p3_doc24 <- filter(p3_docentes, ano == 2024)
count(p3_doc24, area, exterior)


# P3.2 Titulação no exterior por área e nota

p3_prog <- p3_doc24 %>%
  group_by(id_prog, area, nota) %>%
  summarise(docentes = n(), exterior = sum(exterior),
            .groups = "drop") %>%
  mutate(pct = 100 * exterior / docentes)

p3_prog %>%
  group_by(area) %>%
  summarise(programas = n(), minimo = min(pct), q1 = quantile(pct, 0.25),
            mediana = median(pct), q3 = quantile(pct, 0.75), maximo = max(pct))

p3_conj <- p3_prog %>%
  group_by(area, nota) %>%
  summarise(programas = n(), docentes = sum(docentes),
            exterior = sum(exterior), .groups = "drop") %>%
  mutate(pct = 100 * exterior / docentes)
p3_conj

write_csv(p3_conj,
          nome_livre(pasta_tabelas, "P3_exterior_por_area_e_nota_2024", "csv"))


# P3.3 Regressão logística
# Nota 4 como categoria de referência; ajuste por programa (quasibinomial)

p3_dados_mod <- p3_prog %>%
  mutate(nota_num = as.integer(substr(as.character(nota), 1, 1)),
         nota = relevel(nota, ref = "4"), area = factor(area))

p3_modelo <- glm(
  cbind(exterior, docentes - exterior) ~ area + nota,
  family = quasibinomial, data = p3_dados_mod)
summary(p3_modelo)

p3_razoes <- tibble(termo = names(coef(p3_modelo)),
                    razao_chances = exp(coef(p3_modelo)),
                    ic95_inf = exp(confint.default(p3_modelo)[, 1]),
                    ic95_sup = exp(confint.default(p3_modelo)[, 2]),
                    p_valor = summary(p3_modelo)$coefficients[, 4])
p3_razoes

# Nota como tendência linear
p3_tendencia <- glm(
  cbind(exterior, docentes - exterior) ~ area + nota_num,
  family = quasibinomial, data = p3_dados_mod)
exp(cbind(coef(p3_tendencia), confint.default(p3_tendencia)))

# Interação área x nota
p3_inter_cat <- glm(
  cbind(exterior, docentes - exterior) ~ area * nota,
  family = quasibinomial, data = p3_dados_mod)
p3_teste_cat <- anova(p3_modelo, p3_inter_cat, test = "F")
p3_inter_lin <- glm(
  cbind(exterior, docentes - exterior) ~ area * nota_num,
  family = quasibinomial, data = p3_dados_mod)
p3_teste_lin <- anova(p3_tendencia, p3_inter_lin, test = "F")
p3_teste_cat; p3_teste_lin

write_csv(p3_razoes,
          nome_livre(pasta_tabelas, "P3_razoes_de_chance_quasibinomial", "csv"))
write_csv(tibble(
  item = c("dispersao (modelo area + nota)",
           "razao de chances por ponto de nota",
           "interacao area x nota (categorias): p",
           "interacao area x nota (tendencia): p"),
  valor = c(summary(p3_modelo)$dispersion,
            exp(coef(p3_tendencia)["nota_num"]),
            p3_teste_cat$`Pr(>F)`[2], p3_teste_lin$`Pr(>F)`[2])),
  nome_livre(pasta_tabelas, "P3_tendencia_e_interacao", "csv"))


#* P3.4 Gráficos -----------------------------------------------------------

p3_tema <- theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(color = "grey40", hjust = 0, size = 9),
        plot.title.position = "plot", plot.caption.position = "plot")

p3_n <- p3_prog %>%
  group_by(area) %>% summarise(prog = n(), doc = sum(docentes))
p3_rodape <- paste0(
  "Astronomia/Física: ", p3_n$prog[1], " programas e ", p3_n$doc[1],
  " docentes; Química: ", p3_n$prog[2], " programas e ", p3_n$doc[2],
  " docentes (2024, conceito A excluído).")

# Gráfico 1: Formação no exterior por nota
p3_espalha <- position_jitterdodge(jitter.width = 0.18,
                                   dodge.width = 0.7, seed = 1)

p3_g1 <- ggplot(p3_prog, aes(x = nota, y = pct, color = area)) +
  geom_point(position = p3_espalha, size = 1.9, alpha = 0.55) +
  geom_errorbar(data = p3_conj, aes(ymin = pct, ymax = pct),
                position = position_dodge(width = 0.7),
                width = 0.55, linewidth = 1.1) +
  scale_color_manual(values = cores_area, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 50), breaks = seq(0, 50, 10)) +
  labs(title = "Docentes com o último grau no exterior, por nota do programa",
       subtitle = "Ponto: um programa. Traço: percentual do conjunto da área e da nota (2024)",
       x = "Nota do programa (Avaliação Quadrienal 2017 a 2020)", y = NULL,
       caption = paste0(p3_rodape,
                        "\nEm 2024, \"3 ou menos\" contém só programas de nota 3. ",
                        "Fonte: CAPES, Dados Abertos (Sucupira).")) +
  p3_tema

p3_g1

ggsave(nome_livre(pasta_figuras, "P3_exterior_por_nota_programas", "pdf"), p3_g1,
       width = 7.5, height = 4.8, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P3_exterior_por_nota_programas", "png"), p3_g1,
       width = 7.5, height = 4.8, dpi = 300, bg = "white")

# Gráfico 2: Países de titulação
# Oito mais frequentes; demais agrupados em "Outros".
p3_paises <- p3_doc24 %>%
  filter(exterior) %>%
  mutate(pais = fct_lump_n(str_to_title(pais_titulacao), n = 8,
                           other_level = "Outros")) %>%
  count(area, pais) %>%
  left_join(select(p3_n, area, total_area = doc), by = "area") %>%
  mutate(pct = 100 * n / total_area)

p3_ordem <- p3_paises %>%
  filter(pais != "Outros") %>%
  group_by(pais) %>% summarise(t = sum(n)) %>%
  arrange(t) %>% pull(pais) %>% as.character()
p3_paises <- mutate(p3_paises,
                    pais = factor(pais, levels = c("Outros", p3_ordem)))

write_csv(p3_paises,
          nome_livre(pasta_tabelas, "P3_paises_titulacao_exterior_2024", "csv"))

p3_rodape_paises <- paste0(
  p3_rodape, "\nFonte: CAPES, Dados Abertos (Sucupira).")

p3_g2 <- ggplot(p3_paises, aes(x = pct, y = pais, fill = area,
                               group = fct_rev(area))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72,
           color = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(fmt(pct, 1), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.15, size = 3.2, color = "grey25") +
  scale_fill_manual(values = cores_area, name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(p3_paises$pct) * 1.15),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "País do último grau dos titulados no exterior",
       subtitle = "Percentual sobre os docentes de cada área (2024)",
       x = NULL, y = NULL, caption = p3_rodape_paises) +
  p3_tema +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"))

p3_g2

ggsave(nome_livre(pasta_figuras, "P3_paises_titulacao", "pdf"), p3_g2,
       width = 7.5, height = 5.2, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P3_paises_titulacao", "png"), p3_g2,
       width = 7.5, height = 5.2, dpi = 300, bg = "white")

# Gráfico 3: Evolução 2021–2024
p3_evolucao <- p3_docentes %>%
  group_by(ano, area) %>%
  summarise(n = n(), exterior = sum(exterior), .groups = "drop") %>%
  mutate(pct = 100 * exterior / n)

write_csv(p3_evolucao,
          nome_livre(pasta_tabelas, "P3_evolucao_exterior_2021_2024", "csv"))

p3_g3 <- ggplot(p3_evolucao, aes(x = ano, y = pct, color = area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(data = filter(p3_evolucao, ano == max(ano)),
            aes(label = area), hjust = 0, nudge_x = 0.08, size = 3.6,
            show.legend = FALSE) +
  scale_color_manual(values = cores_area, guide = "none") +
  scale_x_continuous(breaks = 2021:2024,
                     expand = expansion(add = c(0.2, 1.1))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(p3_evolucao$pct) * 1.4),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Docentes com o último grau obtido no exterior, 2021 a 2024",
       subtitle = "Percentual dos docentes de cada área, todas as notas",
       x = NULL, y = NULL,
       caption = "Docentes distintos por ano e área; conceito A excluído.\nFonte: CAPES, Dados Abertos (Sucupira).") +
  p3_tema

p3_g3

ggsave(nome_livre(pasta_figuras, "P3_evolucao_exterior", "pdf"), p3_g3,
       width = 7.5, height = 4.5, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P3_evolucao_exterior", "png"), p3_g3,
       width = 7.5, height = 4.5, dpi = 300, bg = "white")


# ==========================================================================
# P4. Mestres e doutores titulados no quadriênio
# ==========================================================================

#* P4.1 Titulados por ano, área e grau -------------------------------------
# Todos os programas acadêmicos das duas áreas (inclui conceito "A").

p4_por_ano <- dbGetQuery(con, "
  SELECT t.ano, a.nm_area, t.grau, COUNT(*) AS titulados
  FROM sucupira.tab_discente t
  JOIN sucupira.ref_programa p ON t.programa_id = p.id_prog
  JOIN sucupira.ref_area a     ON p.area_id = a.id_area
  WHERE t.situacao = 'TITULADO'
  GROUP BY t.ano, a.nm_area, t.grau
  ORDER BY a.nm_area, t.grau, t.ano;") %>%
  mutate(area = rotulo_area(nm_area),
         titulados = as.integer(titulados),
         grau = factor(str_to_title(grau), levels = c("Mestrado", "Doutorado")))

p4_por_ano %>%
  select(area, grau, ano, titulados) %>%
  pivot_wider(names_from = ano, values_from = titulados) %>%
  mutate(total = `2021` + `2022` + `2023` + `2024`)

write_csv(p4_por_ano, nome_livre(pasta_tabelas, "P4_titulados_por_ano", "csv"))


#* P4.2 Consulta: titulados e docentes-ano de cada programa ----------------

p4_prog <- dbGetQuery(con, "
  WITH doc AS (
    SELECT programa_id, COUNT(*) AS docente_anos
    FROM sucupira.tab_docente GROUP BY programa_id),
  tit AS (
    SELECT programa_id,
           COUNT(*) FILTER (WHERE grau = 'MESTRADO')  AS mestres,
           COUNT(*) FILTER (WHERE grau = 'DOUTORADO') AS doutores
    FROM sucupira.tab_discente
    WHERE situacao = 'TITULADO' GROUP BY programa_id)
  SELECT p.id_prog, a.nm_area, p.grau, p.nota, doc.docente_anos,
         COALESCE(tit.mestres, 0)  AS mestres,
         COALESCE(tit.doutores, 0) AS doutores
  FROM sucupira.ref_programa p
  JOIN sucupira.ref_area a ON p.area_id = a.id_area
  JOIN doc                 ON doc.programa_id = p.id_prog
  LEFT JOIN tit            ON tit.programa_id = p.id_prog;") %>%
  mutate(area = rotulo_area(nm_area),
         across(c(docente_anos, mestres, doutores), as.integer),
         tem_mestrado  = str_detect(grau, "MESTRADO"),
         tem_doutorado = str_detect(grau, "DOUTORADO"))

# Totais do quadriênio por área e titulados por docente por ano
p4_totais <- p4_prog %>%
  group_by(area) %>%
  summarise(programas = n(), docente_anos = sum(docente_anos),
            mestres = sum(mestres), doutores = sum(doutores)) %>%
  mutate(mestres_por_docente_ano  = mestres / docente_anos,
         doutores_por_docente_ano = doutores / docente_anos)
p4_totais

write_csv(p4_totais, nome_livre(pasta_tabelas, "P4_totais_quadrienio", "csv"))


#* P4.3 Taxa por programa, por área e nota ---------------------------------

p4_taxas <- p4_prog %>%
  filter(nota != "A") %>%
  mutate(nota = nota_categoria(nota)) %>%
  pivot_longer(c(mestres, doutores),
               names_to = "titulo", values_to = "titulados") %>%
  filter((titulo == "mestres" & tem_mestrado) |
           (titulo == "doutores" & tem_doutorado)) %>%
  mutate(taxa = titulados / docente_anos,
         titulo = factor(titulo, levels = c("mestres", "doutores"),
                         labels = c("Mestres", "Doutores")))

# Taxa do conjunto de cada área, só com os programas que oferecem o grau
p4_por_area <- p4_taxas %>%
  group_by(titulo, area) %>%
  summarise(programas = n(), sem_titulados = sum(titulados == 0),
            titulados = sum(titulados),
            docente_anos = sum(docente_anos), .groups = "drop") %>%
  mutate(taxa = titulados / docente_anos)
p4_por_area

# Taxa do conjunto de cada área e nota (soma dos titulados / soma dos
# docentes-ano) e mediana das taxas dos programas
p4_conj <- p4_taxas %>%
  group_by(titulo, area, nota) %>%
  summarise(programas = n(), titulados = sum(titulados),
            docente_anos = sum(docente_anos),
            mediana_programas = median(taxa), .groups = "drop") %>%
  mutate(taxa = titulados / docente_anos)
p4_conj

write_csv(p4_por_area, nome_livre(pasta_tabelas, "P4_taxa_por_area", "csv"))
write_csv(p4_conj, nome_livre(pasta_tabelas, "P4_taxa_por_area_e_nota", "csv"))


#* P4.4 Regressão de Poisson com offset ------------------------------------

p4_dados_dout <- p4_taxas %>%
  filter(titulo == "Doutores", nota != "3 ou menos") %>%
  mutate(nota = relevel(droplevels(nota), ref = "4"), area = factor(area))

p4_mod_dout <- glm(
  titulados ~ area + nota + offset(log(docente_anos)),
  family = quasipoisson, data = p4_dados_dout)
summary(p4_mod_dout)

# A diferença entre as áreas é a mesma em todas as notas? (teste F)
p4_inter_dout <- glm(
  titulados ~ area * nota + offset(log(docente_anos)),
  family = quasipoisson, data = p4_dados_dout)
p4_teste_dout <- anova(p4_mod_dout, p4_inter_dout, test = "F")
p4_teste_dout

# Mestres: programas com mestrado
p4_dados_mest <- p4_taxas %>%
  filter(titulo == "Mestres") %>%
  mutate(nota = relevel(nota, ref = "4"), area = factor(area))

p4_mod_mest <- glm(
  titulados ~ area + nota + offset(log(docente_anos)),
  family = quasipoisson, data = p4_dados_mest)
summary(p4_mod_mest)

p4_inter_mest <- glm(
  titulados ~ area * nota + offset(log(docente_anos)),
  family = quasipoisson, data = p4_dados_mest)
p4_teste_mest <- anova(p4_mod_mest, p4_inter_mest, test = "F")
p4_teste_mest

p4_razoes <- bind_rows(
  tibble(modelo = "doutores", termo = names(coef(p4_mod_dout)),
         razao_taxas = exp(coef(p4_mod_dout)),
         ic95_inf = exp(confint.default(p4_mod_dout)[, 1]),
         ic95_sup = exp(confint.default(p4_mod_dout)[, 2]),
         p_valor = summary(p4_mod_dout)$coefficients[, 4]),
  tibble(modelo = "mestres", termo = names(coef(p4_mod_mest)),
         razao_taxas = exp(coef(p4_mod_mest)),
         ic95_inf = exp(confint.default(p4_mod_mest)[, 1]),
         ic95_sup = exp(confint.default(p4_mod_mest)[, 2]),
         p_valor = summary(p4_mod_mest)$coefficients[, 4]))
p4_razoes

write_csv(p4_razoes, nome_livre(pasta_tabelas, "P4_razoes_de_taxas_quasipoisson", "csv"))
write_csv(tibble(
  item = c("dispersao (doutores)", "interacao area x nota (doutores): p",
           "dispersao (mestres)", "interacao area x nota (mestres): p"),
  valor = c(summary(p4_mod_dout)$dispersion, p4_teste_dout$`Pr(>F)`[2],
            summary(p4_mod_mest)$dispersion, p4_teste_mest$`Pr(>F)`[2])),
  nome_livre(pasta_tabelas, "P4_dispersao_e_interacao", "csv"))


#* P4.5 Gráficos -----------------------------------------------------------

p4_tema <- theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        strip.text = element_text(face = "bold", size = 12, hjust = 0),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(color = "grey40", hjust = 0, size = 9),
        plot.title.position = "plot", plot.caption.position = "plot")

# Gráfico 1: titulados por ano, um painel por grau.
p4_rodape_ano <- paste0(
  "Total do quadriênio: Astronomia/Física ", p4_totais$mestres[1],
  " mestres e ", p4_totais$doutores[1], " doutores; Química ",
  p4_totais$mestres[2], " mestres e ", p4_totais$doutores[2],
  " doutores.\nFonte: CAPES, Dados Abertos (Sucupira).")

p4_g1 <- ggplot(p4_por_ano,
                aes(x = ano, y = titulados, color = area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = titulados), vjust = -1.1, size = 3.1,
            color = "grey25") +
  geom_text(data = filter(p4_por_ano, ano == max(ano)),
            aes(label = area), hjust = 0, nudge_x = 0.12,
            size = 3.4) +
  facet_wrap(~ grau) +
  scale_color_manual(values = cores_area, guide = "none") +
  scale_x_continuous(breaks = 2021:2024,
                     expand = expansion(add = c(0.25, 1.5))) +
  scale_y_continuous(limits = c(0, 1000),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Mestres e doutores titulados por ano",
       subtitle = "Programas acadêmicos, 2021 a 2024",
       x = NULL, y = "Titulados", caption = p4_rodape_ano) +
  p4_tema

p4_g1

ggsave(nome_livre(pasta_figuras, "P4_titulados_por_ano", "pdf"), p4_g1,
       width = 8.5, height = 4.6, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P4_titulados_por_ano", "png"), p4_g1,
       width = 8.5, height = 4.6, dpi = 300, bg = "white")

# Gráfico 2: titulados por docente por ano. Cada ponto é um programa; o
# traço horizontal é a taxa do conjunto daquela área e nota.
p4_g2 <- ggplot(p4_taxas, aes(x = nota, y = taxa, color = area)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.18,
                                             dodge.width = 0.7, seed = 1),
             size = 1.9, alpha = 0.55) +
  geom_errorbar(data = p4_conj, aes(ymin = taxa, ymax = taxa),
                position = position_dodge(width = 0.7),
                width = 0.55, linewidth = 1.1) +
  facet_wrap(~ titulo) +
  scale_color_manual(values = cores_area, name = NULL) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) fmt(x, 1),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Titulados por docente por ano, por nota do programa",
       subtitle = "Ponto: um programa. Traço: taxa do conjunto da área e da nota (2021 a 2024)",
       x = "Nota do programa (Avaliação Quadrienal 2017 a 2020)",
       y = "Titulados por docente por ano",
       caption = paste0(
         "Taxa = titulados em 2021-2024 / docentes-ano do programa (permanentes e colaboradores). ",
         "Cada grau só para os programas que o oferecem;\nconceito A excluído. ",
         "Fonte: CAPES, Dados Abertos (Sucupira).")) +
  p4_tema

p4_g2

ggsave(nome_livre(pasta_figuras, "P4_titulados_por_docente", "pdf"), p4_g2,
       width = 9, height = 5, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P4_titulados_por_docente", "png"), p4_g2,
       width = 9, height = 5, dpi = 300, bg = "white")




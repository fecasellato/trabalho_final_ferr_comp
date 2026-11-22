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
MODO_BASE <- "restaurar"

# Pasta do trabalho (onde ficam base/, figuras/ e tabelas/)
pasta_trabalho <- "C:/Users/Marcos/Downloads/dados_abertos_sucupira/trabalho_final_ferr_comp"

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
# P3. Formação no exterior
# Ano de referência: 2024
# ==========================================================================

# P3.1 Docentes por área e ano
# Exclui programas com conceito "A" e evita duplicidade dentro da mesma área.

p3_docentes <- dbGetQuery(con, "
  SELECT DISTINCT ON (d.ano, a.nm_area, d.id_pessoa)
         d.ano, a.nm_area, d.id_pessoa, p.nota, d.pais_titulacao
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

p3_prop <- p3_doc24 %>%
  group_by(area, nota) %>%
  summarise(n = n(), exterior = sum(exterior), .groups = "drop") %>%
  rowwise() %>%
  mutate(pct = 100 * exterior / n,
         ic_inf = 100 * prop.test(exterior, n)$conf.int[1],
         ic_sup = 100 * prop.test(exterior, n)$conf.int[2]) %>%
  ungroup()
p3_prop

write_csv(p3_prop, nome_livre(pasta_tabelas, "P3_proporcao_exterior_por_nota_2024", "csv"))

# Comparação entre as áreas
p3_tab_area <- table(p3_doc24$area, p3_doc24$exterior)
p3_tab_area
p3_teste_area <- prop.test(p3_tab_area[, "TRUE"], rowSums(p3_tab_area))
p3_teste_area


# P3.3 Regressão logística
# Nota 4 como categoria de referência
p3_dados_mod <- mutate(p3_doc24, nota = relevel(nota, ref = "4"), area = factor(area))

p3_modelo <- glm(exterior ~ area + nota, data = p3_dados_mod, family = binomial)
summary(p3_modelo)

# Interação área x nota
p3_modelo_inter <- glm(exterior ~ area * nota, data = p3_dados_mod, family = binomial)
p3_teste_inter <- anova(p3_modelo, p3_modelo_inter, test = "LRT")
p3_teste_inter

p3_razoes <- tibble(termo = names(coef(p3_modelo)),
                    razao_chances = exp(coef(p3_modelo)),
                    ic95_inf = exp(confint.default(p3_modelo)[, 1]),
                    ic95_sup = exp(confint.default(p3_modelo)[, 2]),
                    p_valor = summary(p3_modelo)$coefficients[, 4])
p3_razoes

write_csv(p3_razoes, nome_livre(pasta_tabelas, "P3_regressao_logistica_razoes_de_chance", "csv"))
write_csv(tibble(teste = c("prop.test: exterior Fisica x Quimica",
                           "LRT: modelo com interacao area x nota"),
                 estatistica = c(unname(p3_teste_area$statistic), p3_teste_inter$Deviance[2]),
                 gl = c(unname(p3_teste_area$parameter), p3_teste_inter$Df[2]),
                 p_valor = c(p3_teste_area$p.value, p3_teste_inter$`Pr(>Chi)`[2])),
          nome_livre(pasta_tabelas, "P3_testes", "csv"))


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

p3_n24 <- count(p3_doc24, area)
p3_rodape <- paste0("Docentes distintos de programas acadêmicos em 2024: Astronomia/Física n = ",
                    p3_n24$n[p3_n24$area == "Astronomia/Física"], "; Química n = ",
                    p3_n24$n[p3_n24$area == "Química"], ".")

# Gráfico 1: Formação no exterior por nota
p3_g1 <- ggplot(p3_prop, aes(x = nota, y = pct, color = area)) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.18, linewidth = 0.7,
                position = position_dodge(width = 0.45)) +
  geom_point(size = 3.2, position = position_dodge(width = 0.45)) +
  scale_color_manual(values = cores_area, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 50), breaks = seq(0, 50, 10),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Docentes com o último grau obtido no exterior",
       subtitle = "Percentual por nota do programa e intervalo de 95% (2024)",
       x = "Nota do programa (Avaliação Quadrienal 2017 a 2020)", y = NULL,
       caption = paste0(p3_rodape,
                        "\nIntervalos de Wilson. Programas novos (conceito A) excluídos; o único programa de nota 2 entra em \"3 ou menos\".",
                        "\nFonte: CAPES, Dados Abertos (Sucupira).")) +
  p3_tema

p3_g1

ggsave(nome_livre(pasta_figuras, "P3_exterior_por_nota", "pdf"), p3_g1,
       width = 7.5, height = 4.8, device = cairo_pdf)
ggsave(nome_livre(pasta_figuras, "P3_exterior_por_nota", "png"), p3_g1,
       width = 7.5, height = 4.8, dpi = 300, bg = "white")

# Gráfico 2: Países de titulação
# Oito mais frequentes; demais agrupados em "Outros".
p3_paises <- p3_doc24 %>%
  filter(exterior) %>%
  mutate(pais = fct_lump_n(str_to_title(pais_titulacao), n = 8, other_level = "Outros")) %>%
  count(area, pais) %>%
  left_join(rename(p3_n24, total_area = n), by = "area") %>%
  mutate(pct = 100 * n / total_area)

p3_ordem <- p3_paises %>%
  filter(pais != "Outros") %>%
  group_by(pais) %>% summarise(t = sum(n)) %>%
  arrange(t) %>% pull(pais) %>% as.character()
p3_paises <- mutate(p3_paises, pais = factor(pais, levels = c("Outros", p3_ordem)))

write_csv(p3_paises, nome_livre(pasta_tabelas, "P3_paises_titulacao_exterior_2024", "csv"))

p3_g2 <- ggplot(p3_paises, aes(x = pct, y = pais, fill = area, group = fct_rev(area))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72,
           color = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(fmt(pct, 1), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.15, size = 3.2, color = "grey25") +
  scale_fill_manual(values = cores_area, name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(p3_paises$pct) * 1.15),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Onde os docentes titulados no exterior se formaram",
       subtitle = "Percentual dos docentes de cada área, por país do último grau (2024)",
       x = NULL, y = NULL,
       caption = paste0(p3_rodape, "\nFonte: CAPES, Dados Abertos (Sucupira).")) +
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

write_csv(p3_evolucao, nome_livre(pasta_tabelas, "P3_evolucao_exterior_2021_2024", "csv"))

p3_g3 <- ggplot(p3_evolucao, aes(x = ano, y = pct, color = area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(data = filter(p3_evolucao, ano == max(ano)),
            aes(label = area), hjust = 0, nudge_x = 0.08, size = 3.6, show.legend = FALSE) +
  scale_color_manual(values = cores_area, guide = "none") +
  scale_x_continuous(breaks = 2021:2024, expand = expansion(add = c(0.2, 1.1))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(p3_evolucao$pct) * 1.4),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Formação no exterior ao longo do quadriênio",
       subtitle = "Percentual de docentes com o último grau obtido no exterior (2021 a 2024)",
       x = NULL, y = NULL,
       caption = "Docentes distintos por ano e área; programas novos (conceito A) excluídos.\nFonte: CAPES, Dados Abertos (Sucupira).") +
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




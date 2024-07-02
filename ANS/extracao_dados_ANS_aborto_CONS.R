library(tidyverse)
library(readr)


# URL base dos dados
base_url <- "https://dadosabertos.ans.gov.br/FTP/PDA/TISS/HOSPITALAR/2020/"

# Lista de anos 
anos <- 2015:2022

# Lista de unidades federativas do Brasil
ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")

# Criar uma pasta para armazenar os arquivos baixados
dir.create("arquivos_TISS", showWarnings = FALSE)

# Loop para baixar e descompactar os arquivos para cada ano
for (ano in anos) {
  base_url <- paste0("https://dadosabertos.ans.gov.br/FTP/PDA/TISS/HOSPITALAR/", ano, "/")
  for (uf in ufs) {
    for (mes in 1:12) {
      # if (!(uf == "AC" && mes == 1)) {
        mes_str <- sprintf("%02d", mes)
        url <- paste0(base_url, uf, "/", uf, "_", ano, mes_str, "_HOSP_CONS.zip")
        filename <- paste0("arquivos_TISS/", uf, "_", ano, mes_str, "_HOSP_CONS.zip")
        download.file(url, filename)
        unzip(filename, exdir = "arquivos_TISS")
      # }
    }
  }
}

# Lista de todos os arquivos baixados
arquivos <- list.files("arquivos_TISS", full.names = TRUE, pattern = "\\.csv", recursive = TRUE)

# Loop para ler e processar os arquivos
dados <- list()
for (arquivo in arquivos) {
  # Ler o arquivo CSV
  df <- read_delim(arquivo, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # Extrair a unidade federativa a partir do nome do arquivo
  uf <- gsub("^.+/([A-Z]+)_\\d{6}_HOSP_CONS\\.csv", "\\1", arquivo)
  
  # Extrair o ano e mês a partir do nome do arquivo
  ano <- gsub(".+_(\\d{4})(\\d{2})_HOSP_CONS\\.csv", "\\1", arquivo)
  mes <- gsub(".+_(\\d{4})(\\d{2})_HOSP_CONS\\.csv", "\\2", arquivo)
  
  # Adicionar colunas de unidade federativa, ano e mês
  df1 <- df %>%
    mutate(Unidade_Federativa = uf,
           ano = ano,
           mes = mes)
  
  # Adicionar o dataframe processado à lista
  dados[[arquivo]] <- df1
}

# Concatenar os dados em um único dataframe
dados_concatenados_hosp <- bind_rows(dados)
# write.csv(dados_concatenados_hosp, "dados_ANS_hosp_2015_21-11-23.csv")

###UM NOVO DIA, COMECE AQUI - FILTRAGEM TODOS CASOS ANS#######################
# dados_concatenados_hosp <- read.csv("dados_ANS_hosp_2015_21-11-23.csv")

dados_concatenados_hosp <- dados_concatenados_hosp %>% 
  mutate(fet = ifelse(FAIXA_ETARIA %in% c("50 a 59", "60 a 69",
                                          "70 a 79", "80 ou mais"), ">=50", FAIXA_ETARIA))

# Filtrar as faixas etárias desejadas
dados_filtrados <- dados_concatenados_hosp %>%
  filter(fet %in% c("10 a 14", "15 a 19", "20 a 29", "30 a 39", "40 a 49", ">=50"))

# Definir as CIDs de aborto
cids_aborto <- c("O03", "O030", "O031", "O032", "O033", "O034", "O035", "O036", "O037", "O038", "O039",
                 "O04", "O040", "O041", "O042", "O043", "O044", "O045", "O046", "O047", "O048", "O049",
                 "O05", "O050", "O051", "O052", "O053", "O054", "O055", "O056", "O057", "O058", "O059",
                 "O06", "O060", "O061", "O062", "O063", "O064", "O065", "O066", "O067", "O068", "O069",
                 "O07", "O070", "O071", "O072", "O073", "O074", "O075", "O076", "O077", "O078", "O079",
                 "O08", "O080", "O081", "O082", "O083", "O084", "O085", "O086", "O087", "O088", "O089")

# Criar indicadora com CID de aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
dados_filtrados <- dados_filtrados %>%
  mutate(cid_aborto = ifelse(if_any(starts_with("CID_"), ~ . %in% cids_aborto),1,0))


# Definir as CIDs de não aborto
cids_nao_aborto <- c("O00", "O000", "O001", "O002", "O008", "O009",
                     "O01", "O010", "O011", "O019",
                     "O02", "O020", "O021", "O028", "O029")

# Criar indicadora com CID de não aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
dados_filtrados <- dados_filtrados %>%
  mutate(cid_nao_aborto = ifelse(if_any(starts_with("CID_"), ~ . %in% cids_nao_aborto), 1, 0))


dados_filtrados_cid <- dados_filtrados %>% 
  filter(cid_aborto == 1 | cid_nao_aborto == 1)

write.csv(dados_filtrados_cid, "dados_filtrados_CIDs_ANS_2015_2023_hosp.csv")
##vamos usar essa base para concatenar com os casos de aborto por DET
 

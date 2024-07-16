library(tidyverse)
library(readr)


# URL base dos dados
base_url <- "https://dadosabertos.ans.gov.br/FTP/PDA/TISS/HOSPITALAR/"

# Lista de anos  
anos <- 2016:2022

# Lista de unidades federativas do Brasil
ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ",  "RN", "RO", "RR", "RS", "SC", "SE", 
         "SP", "TO")

# Criar uma pasta para armazenar os arquivos baixados
dir.create("ANS/arquivos_TISS_DET", showWarnings = FALSE)

# Loop para baixar e descompactar os arquivos para cada ano
for (ano in anos) {
  base_url <- paste0("https://dadosabertos.ans.gov.br/FTP/PDA/TISS/HOSPITALAR/", ano, "/")
  for (uf in ufs) {
    for (mes in 1:12) {
      # if (!(uf == "AC" && mes == 1)) {
      mes_str <- sprintf("%02d", mes)
      url <- paste0(base_url, uf, "/", uf, "_", ano, mes_str, "_HOSP_DET.zip")
      filename <- paste0("ANS/arquivos_TISS_DET/", uf, "_", ano, mes_str, "_HOSP_DET.zip")
      download.file(url, filename)
      unzip(filename, exdir = "ANS/arquivos_TISS_DET")
      # }
    }
  }
}

for (a in anos){
  
  for(j in ufs){
    
    # Lista de todos os arquivos baixados
    arquivos <- list.files("ANS/arquivos_TISS_DET", full.names = TRUE, pattern = paste0("^", j, ".*", a, ".*\\.csv$"), recursive = TRUE)
    
    dados <- data.frame()
    
    for (arquivo in arquivos) {
      # Ler o arquivo CSV
      df <- read_delim(arquivo, delim = ";", escape_double = FALSE, trim_ws = TRUE)
      # Extrair a unidade federativa a partir do nome do arquivo
      uf <- gsub("^.+/([A-Z]+)_\\d{6}_HOSP_DET\\.csv", "\\1", arquivo)
      
      # Extrair o ano e mês a partir do nome do arquivo
      ano <- gsub(".+_(\\d{4})(\\d{2})_HOSP_DET\\.csv", "\\1", arquivo)
      mes <- gsub(".+_(\\d{4})(\\d{2})_HOSP_DET\\.csv", "\\2", arquivo)
      
      # Adicionar colunas de unidade federativa, ano e mês
      df1 <- df %>%
        mutate(Unidade_Federativa = uf,
               ano = ano,
               mes = mes, 
               QT_ITEM_EVENTO_INFORMADO = as.numeric(QT_ITEM_EVENTO_INFORMADO),
               VL_ITEM_EVENTO_INFORMADO = as.numeric(VL_ITEM_EVENTO_INFORMADO),
               VL_ITEM_PAGO_FORNECEDOR = as.numeric(VL_ITEM_PAGO_FORNECEDOR),
               IND_PACOTE = as.numeric(IND_PACOTE),
               IND_TABELA_PROPRIA = as.numeric(IND_TABELA_PROPRIA)
        )
      
      df1$CD_TABELA_REFERENCIA <-as.numeric(df1$CD_TABELA_REFERENCIA)
      
      # Adicionar o dataframe processado à lista
      dados <- rbind(dados, df1)
    }
    
    write.csv(dados, paste0("ANS/arquivos_DET_ano/dados_ANS_hosp_det", a, j, ".csv"))
    
  }
  
  dados_concatenados_hosp_det <- data.frame()
  

  for(i in ufs){
      
      arquivo2 <- list.files("ANS/arquivos_DET_ano", full.names = TRUE, pattern = paste0(a, i, "\\.csv$"), recursive = TRUE)
      df_estado <- read_csv(arquivo2)
      print(paste0('ESTADO ', i, " ANO ", a, " LIDO"))
      
      dados_concatenados_hosp_det <- rbind(dados_concatenados_hosp_det, df_estado)
      
  }
  
  write.csv(dados_concatenados_hosp_det, paste0("ANS/data/dados_ANS_hosp_det_", a, ".csv"))
  
  
  ##fitrando casos 31309020 ou 31309062
  dados_det_aborto <- dados_concatenados_hosp_det %>% 
    filter(CD_PROCEDIMENTO == '31309020' | CD_PROCEDIMENTO == '31309062')
  # 31309020 - Aspiração manual intra-uterina (AMIU) pós-abortamento
  # 31309062 - Curetagem pós-abortamento
  
  
  ##vamos filtrar os casos de HOSP total para ver se recuperamos faixa etária e sexo:
  
  dados_concatenados_hosp <- read.csv(paste0("ANS/data/dados_ANS_hosp_", a, ".csv")) |>
    filter(ano == a)
   # não tem casos duplicados por ID_EVENTO_ATENCAO_SAUDE 
  # teste <- dados_concatenados_hosp %>% 
  #   janitor::get_dupes(ID_EVENTO_ATENCAO_SAUDE)
  dados_concatenados_hosp <- dados_concatenados_hosp %>% 
    mutate(fet = ifelse(FAIXA_ETARIA %in% c("50 a 59", "60 a 69",
                                            "70 a 79", "80 ou mais"), ">=50", FAIXA_ETARIA))
  
  dados_det <- left_join(dados_det_aborto, dados_concatenados_hosp, by = c('ID_EVENTO_ATENCAO_SAUDE'))
  
  # Definir as CIDs de aborto
  cids_aborto <- c("O03", "O030", "O031", "O032", "O033", "O034", "O035", "O036", "O037", "O038", "O039",
                   "O04", "O040", "O041", "O042", "O043", "O044", "O045", "O046", "O047", "O048", "O049",
                   "O05", "O050", "O051", "O052", "O053", "O054", "O055", "O056", "O057", "O058", "O059",
                   "O06", "O060", "O061", "O062", "O063", "O064", "O065", "O066", "O067", "O068", "O069",
                   "O07", "O070", "O071", "O072", "O073", "O074", "O075", "O076", "O077", "O078", "O079",
                   "O08", "O080", "O081", "O082", "O083", "O084", "O085", "O086", "O087", "O088", "O089")
  
  # Criar indicadora com CID de aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_det <- dados_det %>%
    mutate(cid_aborto = ifelse(if_any(starts_with("CID_"), ~ . %in% cids_aborto),1,0))
  
  
  # Definir as CIDs de não aborto
  cids_nao_aborto <- c("O00", "O000", "O001", "O002", "O008", "O009",
                       "O01", "O010", "O011", "O019",
                       "O02", "O020", "O021", "O028", "O029")
  
  # Criar indicadora com CID de não aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_det <- dados_det %>%
    mutate(cid_nao_aborto = ifelse(if_any(starts_with("CID_"), ~ . %in% cids_nao_aborto), 1, 0))
  
  #vamos excluir os casos com cid_nao_aborto == 1 (sim)
  dados_det1 <- dados_det %>% 
    filter(cid_nao_aborto == 0 | is.na(cid_nao_aborto))
  
  ## ver se tem casos duplicados por ID_EVENTO_ATENCAO_SAUDE, CD_PROCEDIMENTO
  teste1 <- dados_det1 %>% 
    janitor::get_dupes(ID_EVENTO_ATENCAO_SAUDE, mes.x, CD_PROCEDIMENTO)
  length(unique(teste1$ID_EVENTO_ATENCAO_SAUDE))
  # tem 125 casos com repetição de ID_EVENTO_ATENCAO_SAUDE
  
  #vamos considerar só um caso por ID_EVENTO_ATENCAO_SAUDE
  dados_det2 <- dados_det1 %>%
    distinct(ID_EVENTO_ATENCAO_SAUDE, .keep_all = TRUE) %>% 
    select(ID_EVENTO_ATENCAO_SAUDE, CD_PROCEDIMENTO, Unidade_Federativa = Unidade_Federativa.x,
           mes = mes.x, SEXO, CD_MUNICIPIO_BENEFICIARIO, fet, cid_aborto, cid_nao_aborto)
  
  # Agora vamos carregar os dados de CID (base CONS)
  dados_filtrados_cid <- read.csv("ANS/dados_filtrados_CIDs_ANS_2015_2022_hosp.csv") |>
    filter(ano == a)
  
  
  ##vamos filtrar os casos que não tenham 1 (sim) para os casos não aborto
  dados_filtrados_cid <- dados_filtrados_cid %>% 
    filter(cid_nao_aborto == 0 | is.na(cid_nao_aborto)) %>% 
    select(ID_EVENTO_ATENCAO_SAUDE, Unidade_Federativa,
           mes, SEXO, CD_MUNICIPIO_BENEFICIARIO, fet, cid_aborto_cid = cid_aborto, 
           cid_nao_aborto_cid = cid_nao_aborto)
  
  #tem casos duplicados por ID_EVENTO_ATENCAO_SAUDE 
  teste <- dados_filtrados_cid %>% 
    janitor::get_dupes(ID_EVENTO_ATENCAO_SAUDE)
  #não tem casos duplicados de dados_filtrados_cid
  
  dados_det2$mes <- as.numeric(dados_det2$mes)
  
  ##vamos agora juntar as duas bases:
  dados <- full_join(dados_det2, dados_filtrados_cid, by = c('ID_EVENTO_ATENCAO_SAUDE', 'mes', 'Unidade_Federativa', 'CD_MUNICIPIO_BENEFICIARIO' ,'SEXO', 'fet'))
  
  dados <- dados %>% 
    mutate(CD_PROCEDIMENTO = ifelse(is.na(CD_PROCEDIMENTO),0, CD_PROCEDIMENTO),
           SEXO = ifelse(is.na(SEXO), "ignorado", SEXO),
           fet = ifelse(is.na(fet), "ignorado", fet))
  
  ## vamos criar a variável que indica se o caso tem CID ou só PROCEDIMENTO ou os dois
  dados <- dados %>% 
    mutate(origem = ifelse((CD_PROCEDIMENTO == 0 & !is.na(cid_aborto_cid)), "cid", 
                           ifelse((CD_PROCEDIMENTO != 0 & is.na(cid_aborto_cid)),
                                  "procedimento", "ambos")))
  
  
  ##filtrando as idades:
  dados1 <- dados %>%
    filter(fet %in% c("10 a 14", "15 a 19", "20 a 29", "30 a 39", "40 a 49", ">=50", "Não identificado"))
  
  # #tem casos do sexo masculino
  # table(dados1$SEXO)
  # Feminino  ignorado Masculino 
  # 15029       223       737 
  
  #Filtrando os casos do sexo feminino
  dados1 <- dados1 %>% 
    filter(SEXO == "Feminino")
  
  write.csv(dados1, paste0("ANS/data/dados_ANS_", a, '_abortos.csv'))
  
  
  ###UM NOVO DIA, COMECE AQUI - FILTRAGEM SÓ ABORTO ANS #######################
  dados1 <- read.csv(paste0("ANS/data/dados_ANS_", a, '_abortos.csv'))
  
  dados <- dados1  %>%  
    mutate(uf_beneficiaria = substr(CD_MUNICIPIO_BENEFICIARIO, 1, 2))
  
  
  dados <- dados %>%
    mutate(uf_beneficiaria_sigla = case_when(
      uf_beneficiaria == 11 ~ "RO",
      uf_beneficiaria == 12 ~ "AC",
      uf_beneficiaria == 13 ~ "AM",
      uf_beneficiaria == 14 ~ "RR",
      uf_beneficiaria == 15 ~ "PA",
      uf_beneficiaria == 16 ~ "AP",
      uf_beneficiaria == 17 ~ "TO",
      uf_beneficiaria == 21 ~ "MA",
      uf_beneficiaria == 22 ~ "PI",
      uf_beneficiaria == 23 ~ "CE",
      uf_beneficiaria == 24 ~ "RN",
      uf_beneficiaria == 25 ~ "PB",
      uf_beneficiaria == 26 ~ "PE",
      uf_beneficiaria == 27 ~ "AL",
      uf_beneficiaria == 28 ~ "SE",
      uf_beneficiaria == 29 ~ "BA",
      uf_beneficiaria == 31 ~ "MG",
      uf_beneficiaria == 32 ~ "ES",
      uf_beneficiaria == 33 ~ "RJ",
      uf_beneficiaria == 35 ~ "SP",
      uf_beneficiaria == 41 ~ "PR",
      uf_beneficiaria == 42 ~ "SC",
      uf_beneficiaria == 43 ~ "RS",
      uf_beneficiaria == 50 ~ "MS",
      uf_beneficiaria == 51 ~ "MT",
      uf_beneficiaria == 52 ~ "GO",
      uf_beneficiaria == 53 ~ "DF",
      TRUE ~ Unidade_Federativa  # Para casos que não correspondem a nenhum código
    ))
  
  
  
  table(dados$Unidade_Federativa)
  
  table(dados$uf_beneficiaria_sigla)
  
  table(dados$origem)
  
  # dados <- dados %>% 
  #   filter(origem == "procedimento")
  
  dados_uf_fet <- dados %>%  
    group_by(uf_beneficiaria_sigla, fet) %>% 
    summarise(cont = n())
  
  dados_uf_fet_pivotado <- dados_uf_fet %>%
    pivot_wider(names_from = fet, values_from = cont)
  
  write.csv(dados_uf_fet_pivotado, file = paste0("ANS/dados_ANS_aborto_", a, "_tabela.csv"), row.names = FALSE)
  
  # write.table(dados_uf_fet_pivotado, "dados_ANS_total.txt")
  
  
  
}

######## Juntando tudo

rm(list = ls(), envir = .GlobalEnv)
gc()

library(tidyverse)
library(readr)

anos <- 2015:2022

dados_totais <- data.frame()

for(b in anos){
  
  df_ano <- read_csv(paste0("ANS/dados_ANS_aborto_", b, "_tabela.csv")) |>
    select(uf_beneficiaria_sigla, `20 a 29`, `30 a 39`, `40 a 49`, `15 a 19`, `>=50`, `10 a 14`) |>
    mutate(ano = b)
  
  print(paste0("TABELA DO ANO", b, " LIDA"))
  dados_totais <- rbind(dados_totais, df_ano)
  
  
}

dados_totais[is.na(dados_totais)] <- 0

write.csv(dados_totais, "ANS/dados_ANS_aborto_2015_2022_tabela.csv")

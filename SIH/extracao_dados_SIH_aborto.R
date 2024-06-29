library(readr)
# install.packages("remotes")
# devtools::install_github("danicat/read.dbc")
# remotes::install_github("rfsaldanha/microdatasus")
require(microdatasus)
require(tidyverse)
library(summarytools)
library(writexl)


# dados1 <- fetch_datasus(year_start = 2015, month_start = 1,
#                        year_end = 2015, month_end = 2,
#                        uf = "AC", information_system = "SIH-RD"
#                        )



# dir.create("dados_SIH_csv", showWarnings = FALSE)

# Definir as CIDs de aborto
cids_aborto <- c("O03", "O030", "O031", "O032", "O033", "O034", "O035", "O036", "O037", "O038", "O039",
                 "O04", "O040", "O041", "O042", "O043", "O044", "O045", "O046", "O047", "O048", "O049",
                 "O05", "O050", "O051", "O052", "O053", "O054", "O055", "O056", "O057", "O058", "O059",
                 "O06", "O060", "O061", "O062", "O063", "O064", "O065", "O066", "O067", "O068", "O069",
                 "O07", "O070", "O071", "O072", "O073", "O074", "O075", "O076", "O077", "O078", "O079",
                 "O08", "O080", "O081", "O082", "O083", "O084", "O085", "O086", "O087", "O088", "O089")

# Definir as CIDs de não aborto
cids_nao_aborto <- c("O00", "O000", "O001", "O002", "O008", "O009",
                     "O01", "O010", "O011", "O019",
                     "O02", "O020", "O021", "O028", "O029")

for (ano in 2014:2023) {
  dados <- fetch_datasus(year_start = ano, month_start = 1,
                         year_end = ano, month_end = 12,
                         uf = "all", information_system = "SIH-RD",
                         vars = c("UF_ZI", "ANO_CMPT", "MES_CMPT",
                                  "MUNIC_RES", "DIAG_PRINC", "DIAG_SECUN",
                                  "CID_ASSO", "CID_NOTIF", "CID_MORTE", 
                                  "DIAGSEC1", "DIAGSEC2", "DIAGSEC3", 
                                  "DIAGSEC4", "DIAGSEC5", 
                                  "DIAGSEC6", "DIAGSEC7", 
                                  "DIAGSEC8", "DIAGSEC9", "SEXO",
                                  "COD_IDADE", "IDADE", "DT_INTER", "DT_SAIDA",
                                  "IDENT")
  )
  # Filtrar mulheres com CID de aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados1 <- dados %>%
    filter(if_any(starts_with("CID_"), ~ . %in% cids_aborto) |
             if_any(starts_with("DIAG_"), ~ . %in% cids_aborto) |
             if_any(starts_with("DIAG"), ~ . %in% cids_aborto)  )
  
  # Excluir mulheres com CID de não aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados2 <- dados_filtrados1 %>%
    filter(!if_any(starts_with("CID_"), ~ . %in% cids_nao_aborto))
  
  # Excluir mulheres com CID de não aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados2 <- dados_filtrados2 %>%
    filter(!if_any(starts_with("DIAG_"), ~ . %in% cids_nao_aborto))
  
  # Excluir mulheres com CID de não aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados2 <- dados_filtrados2 %>%
    filter(!if_any(starts_with("DIAG"), ~ . %in% cids_nao_aborto))
  
  # Salvar o arquivo CSV na pasta "dados_csv"
  write.csv(dados_filtrados2, file.path("dados_SIH_csv", paste0("dados_completos_SIH_abortos_", ano, ".csv")), row.names = FALSE)
}


# Criar uma lista para armazenar os dados de cada ano
lista_dados <- list()

# Loop de 2014 a 2024
for (ano in 2014:2024) {
  # Construir o caminho do arquivo
  caminho_arquivo <- file.path("dados_SIH_csv", paste0("dados_completos_SIH_abortos_", ano, ".csv"))
  
  # Verificar se o arquivo existe antes de tentar lê-lo
  if (file.exists(caminho_arquivo)) {
    # Ler o arquivo CSV
    dados_ano <- read.csv(caminho_arquivo)
    
    # Adicionar uma coluna 'ano' aos dados
    dados_ano$ano <- substr(dados_ano$DT_INTER, 1, 4)
    
    # Adicionar os dados do ano atual à lista
    lista_dados[[as.character(ano)]] <- dados_ano
  } else {
    warning(paste("Arquivo para o ano", ano, "não encontrado. Pulando este ano."))
  }
}

# Concatenar todos os dados em um único dataframe
dados_completos <- do.call(rbind, lista_dados)

dados_completos <- dados_completos %>%
  mutate(fet = case_when(
    IDADE >= 10 & IDADE <= 29 ~ "sih_menor_30",
    IDADE >= 30 & IDADE <= 39 ~ "sih_30_a_39",
    IDADE >= 40 & IDADE <= 49 ~ "sih_40_a_49", 
    TRUE ~ "não" # Caso não caia em nenhuma das faixas
  ))

dados_filtrados <- dados_completos %>% 
  filter(SEXO == 3 | SEXO == 2) %>% #filtragem sexo feminino 
  filter(fet != "não") #filtragem fet

 
#filtrar IDENT diferente de 5
dados_filtrados <- dados_filtrados %>% 
  filter(IDENT != 5 | is.na(IDENT))
# > table(dados$IDENT) #só tem IDENT 1
 

#extraindo a UF de residência
dados_filtrados <- dados_filtrados  %>%  
  mutate(UF = substr(MUNIC_RES, 1, 2))

dados_filtrados <- dados_filtrados  %>%
  mutate(UF_sigla = case_when(
    UF == 11 ~ "RO",
    UF == 12 ~ "AC",
    UF == 13 ~ "AM",
    UF == 14 ~ "RR",
    UF == 15 ~ "PA",
    UF == 16 ~ "AP",
    UF == 17 ~ "TO",
    UF == 21 ~ "MA",
    UF == 22 ~ "PI",
    UF == 23 ~ "CE",
    UF == 24 ~ "RN",
    UF == 25 ~ "PB",
    UF == 26 ~ "PE",
    UF == 27 ~ "AL",
    UF == 28 ~ "SE",
    UF == 29 ~ "BA",
    UF == 31 ~ "MG",
    UF == 32 ~ "ES",
    UF == 33 ~ "RJ",
    UF == 35 ~ "SP",
    UF == 41 ~ "PR",
    UF == 42 ~ "SC",
    UF == 43 ~ "RS",
    UF == 50 ~ "MS",
    UF == 51 ~ "MT",
    UF == 52 ~ "GO",
    UF == 53 ~ "DF",
    TRUE ~ NA_character_  # Para casos que não correspondem a nenhum código
  ))

dados_muni_fet <- dados_filtrados %>% 
  group_by(MUNIC_RES, ano, fet) %>% 
  summarise(cont = n())

dados_muni_fet_pivotado <- dados_muni_fet %>%
  pivot_wider(names_from = fet, values_from = cont)

dados_muni_fet_pivotado <- dados_muni_fet_pivotado %>% 
  mutate(sih_menor_30 = ifelse(is.na(sih_menor_30), 0, sih_menor_30),
         sih_30_a_39 = ifelse(is.na(sih_30_a_39), 0, sih_30_a_39),
         sih_40_a_49 = ifelse(is.na(sih_40_a_49), 0, sih_40_a_49)) %>% 
  mutate(sih_total = sih_menor_30 + sih_30_a_39 + sih_40_a_49)

dados_muni_fet_pivotado <- dados_muni_fet_pivotado %>% 
  filter(ano >= 2015 & ano <= 2023)
 
write.csv(dados_muni_fet_pivotado, file = "dados_SIH_aborto_2015_2023_tabela.csv", row.names = FALSE)
 


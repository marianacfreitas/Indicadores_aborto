library(readr)
library(openxlsx)

referencia_2015 <- read_csv("ANS/data/dados_ANS_aborto_2015_tabela_referencia.csv")

referencia_2015[is.na(referencia_2015)] <- 0

referencia_2015 <- referencia_2015 |>
  group_by(uf_beneficiaria_sigla) |>
  mutate(`< 20` = sum(`10 a 14`, `15 a 19`)) |>
  select(`< 20`, `20 a 29`, `30 a 39`, `40 a 49`)

write.xlsx(referencia_2015, "ANS/tabela_referencia_ANS_2015.xlsx")

ANS_2015 <- read_csv("ANS/dados_ANS_aborto_2015_tabela.csv")

ANS_2015[is.na(ANS_2015)] <- 0

ANS_2015 <- ANS_2015 |>
  group_by(uf_beneficiaria_sigla) |>
  mutate(`< 20` = sum(`10 a 14`, `15 a 19`)) |>
  select(`< 20`, `20 a 29`, `30 a 39`, `40 a 49`)

write.xlsx(ANS_2015, "ANS/tabela_calculada_ANS_2015.xlsx")

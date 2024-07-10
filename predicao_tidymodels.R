## PACOTES ##

library(tidyverse)	# manipulacao de dados
library(tidymodels) # ferramentas de ML
#library(extrasteps) # complemento para pre-processamento dos dados
library(plsmod)     # necessario para usar modelo pls
library(tictoc)     # registrar o tempo de execução de comandos
library(janitor)    # limpeza de dados
library("plsmod")
library(readxl)

# install.packages("BioManager")
# BiocManager::install("mixOmics")

##### CARREGANDO/LIMPANDO OS DADOS #####

df<- read.csv("~/Analise_de_regressao_mariana/Trabalho_predicao/dados_trab3.csv") |> 
  data.frame()

df$charges  <- as.numeric(df$charges)

df<- df |> 
  clean_names()

glimpse(df)




##### SPLIT TRAIN/TEST/VALIDATION #####

set.seed(2402)
split<- initial_split(df, prop=0.8, strata=charges)

df_train<- training(split)    # usado para cross-validation
df_test<- testing(split)      # usado para verificar desempenho

folds<- vfold_cv(df_train, v=3, strata=charges)




##### PRÉ-PROCESSAMENTO #####

receita<- recipe(charges ~ . , data = df_train) |>
  #step_rm(...) |>                                           # variaveis removidas
  step_filter_missing(all_predictors(), threshold = 0.3) |>  # variaveis +30% de faltantes
  step_zv(all_predictors()) |>                               # variaveis sem variabilidade
  #step_YeoJohnson(all_numeric_predictors()) |>              # normalizar variaveis
  step_impute_knn(all_predictors()) |>                       # imputando faltantes
  #step_naomit() |>                                          # deletando faltantes
  #step_unknown(all_nominal_predictors()) |>                 # transformar NA em unknown
  step_normalize(all_numeric_predictors()) |>                # padronizar variaveis
  #step_robust(all_numeric_predictors()) |>                  # padronizacao robusta
  step_other(all_nominal_predictors(),threshold=0.05) |>    # cria a categoria "outros"
  step_dummy(all_nominal_predictors())                        # variaveis dummy




##### MODELOS #####


model_pls<- parsnip::pls(num_comp = tune(),
                         predictor_prop = tune()) |>
  set_engine("mixOmics") |>
  set_mode("regression")


model_las<- linear_reg(penalty = tune(),
                       mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("regression")


model_rid<- linear_reg(penalty = tune(),
                       mixture = 0) |>
  set_engine("glmnet") |>
  set_mode("regression")


model_net<- linear_reg(penalty = tune(),
                       mixture = tune()) |>
  set_engine("glmnet") |>
  set_mode("regression")




##### WORKFLOW #####

wf_pls<- workflow() |>
  add_recipe(receita) |>
  add_model(model_pls)

wf_las<- workflow() |>
  add_recipe(receita) |>
  add_model(model_las)

wf_rid<- workflow() |>
  add_recipe(receita) |>
  add_model(model_rid)

wf_net<- workflow() |>
  add_recipe(receita) |>
  add_model(model_net)



##### TUNAGEM DE HIPERPARAMETROS - BAYESIAN SEARCH #####

## PLS - PARTIAL LEAST SQUARES

tic()
tune_pls<- tune_bayes(wf_pls,
                      resamples = folds,
                      initial = 10,
                      metrics = metric_set(mae),
                      param_info = parameters(num_comp(range=c(1,8)),
                                              predictor_prop(range=c(0,1)))
)
toc()
# 16.61 sec elapsed




## LAS - LASSO

tic()
tune_las<- tune_bayes(wf_las,
                      resamples = folds,
                      initial = 10,
                      metrics = metric_set(mae),
                      param_info = parameters(penalty(range=c(-10,5)))
)
toc()
# 14.03 sec elapsed



## RID - RIDGE

tic()
tune_rid<- tune_bayes(wf_rid,
                      resamples = folds,
                      initial = 10,
                      metrics = metric_set(mae),
                      param_info = parameters(penalty(range=c(-10,5)))
)
toc()
# 13.81 sec elapsed




## NET - ELASTIC NET

tic()
tune_net<- tune_bayes(wf_net,
                      resamples = folds,
                      initial = 10,
                      metrics = metric_set(mae),
                      param_info = parameters(penalty(range=c(-10,5)),
                                              mixture(range=c(0,1)))
)
toc()
# 20.77 sec elapsed




## VISUALIZANDO OS MELHORES MODELOS (BEST ROC AUC)

show_best(tune_pls, metric="mae", n=3)
show_best(tune_las, metric="mae", n=3)
show_best(tune_rid, metric="mae", n=3)
show_best(tune_net, metric="mae", n=3)





##### AJUSTANDO OS MODELOS TUNADOS AOS DADOS TREINO #####

wf_pls_trained<- wf_pls |> finalize_workflow(select_best(tune_pls,metric="mae")) |> fit(df_train)
wf_las_trained<- wf_las |> finalize_workflow(select_best(tune_las,metric="mae")) |> fit(df_train)
wf_rid_trained<- wf_rid |> finalize_workflow(select_best(tune_rid,metric="mae")) |> fit(df_train)
wf_net_trained<- wf_net |> finalize_workflow(select_best(tune_net,metric="mae")) |> fit(df_train)




### ESCOLHENDO O MELHOR MODELO ###

# PREDIZENDO DADOS TESTE

pred_pls<- wf_pls_trained |> predict(df_test)
pred_las<- wf_las_trained |> predict(df_test)
pred_rid<- wf_rid_trained |> predict(df_test)
pred_net<- wf_net_trained |> predict(df_test)

df_pred<- cbind.data.frame(df_test$charges,
                           pred_pls,
                           pred_las,
                           pred_rid,
                           pred_net)

colnames(df_pred)<- c("charges",
                      "pls",
                      "las",
                      "rid",
                      "net")

df_pred |> head()    # VISUALIZANDO PROBABILIDADES




#####  VERIFICANDO MEDIDAS DE PREDICAO  #####

# MEDIDAS

medidas<- rbind(
  df_pred |> metrics(charges,pls) |> dplyr::select(.estimate) |> t(),
  df_pred |> metrics(charges,las) |> dplyr::select(.estimate) |> t(),
  df_pred |> metrics(charges,rid) |> dplyr::select(.estimate) |> t(),
  df_pred |> metrics(charges,net) |> dplyr::select(.estimate) |> t()
)

colnames(medidas)<- c("rmse","rsq","mae")
rownames(medidas)<- c("pls",
                      "las",
                      "rid",
                      "net")
medidas



##### FINALIZATION #####

final_wf<- fit(wf_net_trained, df)    # escolha o melhor modelo


# SAVE THE MODEL/WORKFLOW
saveRDS(final_wf,"modelo_final.rds")


# rm(list = ls())

library(dplyr)
library(sf)
library(broom)
library(randomForest)
library(caret)

# dbs -------------------------------------------------------------------

train_df = train_df_valor_referencia %>% 
  left_join(df_train_arreglado %>% 
              rename(property_id = ID)) %>% 
  mutate(m2 = ifelse(is.na(surface_covered),m2,surface_covered),
         n_cuartos = ifelse(is.na(rooms),n_cuartos,rooms),
         categoria = ifelse(property_type == 'Casa','Casa',categoria),
         parqueadero = ifelse(is.na(parqueadero),F,parqueadero),
         asensor = ifelse(is.na(asensor),F,asensor),
         ESTRATO = as.character(ESTRATO)) %>% 
  mutate(m2 = ifelse(m2 > 2000,m2/1000,m2)) %>% 
  as.data.frame() %>% 
  select(price,m2,n_cuartos,bedrooms,categoria,parqueadero,asensor,ESTRATO,V_REF,m2_manzana,n_predios_manzana) %>% 
  group_by(categoria) %>% 
  mutate(m2 = ifelse(is.na(m2),median(m2,na.rm = T),m2),
         n_cuartos = ifelse(is.na(n_cuartos),median(n_cuartos,na.rm = T),n_cuartos)) %>% 
  ungroup() %>% 
  mutate(m2_manzana = ifelse(is.na(m2_manzana),median(m2_manzana),m2_manzana),
         n_predios_manzana = ifelse(is.na(n_predios_manzana),median(n_predios_manzana),n_predios_manzana)) %>% 
  tidyr::drop_na()

test_df = test_df_valor_referencia %>% 
  left_join(df_test_arreglado %>% 
              rename(property_id = ID)) %>% 
  mutate(m2 = ifelse(is.na(surface_covered),m2,surface_covered),
         n_cuartos = ifelse(is.na(rooms),n_cuartos,rooms),
         categoria = ifelse(property_type == 'Casa','Casa',categoria),
         parqueadero = ifelse(is.na(parqueadero),F,parqueadero),
         asensor = ifelse(is.na(asensor),F,asensor),
         ESTRATO = as.character(ESTRATO)) %>% 
  as.data.frame() %>% 
  select(property_id,m2,n_cuartos,categoria,parqueadero,asensor,ESTRATO,V_REF,m2_manzana,n_predios_manzana) %>% 
  group_by(categoria) %>% 
  mutate(m2 = ifelse(is.na(m2),median(m2,na.rm = T),m2),
         n_cuartos = ifelse(is.na(n_cuartos),median(n_cuartos,na.rm = T),n_cuartos)) %>% 
  ungroup() %>% 
  mutate(m2_manzana = ifelse(is.na(m2_manzana),median(m2_manzana,na.rm = T),m2_manzana),
         n_predios_manzana = ifelse(is.na(n_predios_manzana),median(n_predios_manzana,na.rm = T),n_predios_manzana),
         m2 = ifelse(is.na(m2),median(m2,na.rm = T),m2),
         n_cuartos = ifelse(is.na(n_cuartos),median(n_cuartos,na.rm = T),n_cuartos), 
         categoria = ifelse(is.na(categoria),'otro',categoria))


saveRDS(train_df,'train_df.rds')
saveRDS(test_df,'test_df.rds')


## XG boost ----------------------------------------------------------------

test_df <- readRDS("D:/Problem_Set_3/test_df.rds") %>% 
  mutate(categoria = ifelse(categoria == 'casa','Casa',categoria)) %>% 
  select(!bedrooms)

train_df <- readRDS("D:/Problem_Set_3/train_df.rds") %>% 
  mutate(categoria = ifelse(categoria == 'casa','Casa',categoria)) %>% 
  select(!bedrooms) %>% 
  mutate(m2 = ifelse(30 > m2,m2*1000,m2))



# Xgboost -----------------------------------------------------------------


fitControl<-trainControl(method ="cv",
                         number=5)

grid_xbgoost <- expand.grid(nrounds = c(100,250),
                            max_depth = c(2,4), 
                            eta = c(0.01,0.05), 
                            gamma = c(0,1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4,0.8))


set.seed(1234)
Xgboost_tree <- train(price ~ .,
                      data=train_df,
                      method = "xgbTree", 
                      trControl = fitControl,
                      tuneGrid=grid_xbgoost
)



df_hogares_test_variables1 <- test_df  %>% 
  mutate(price = predict(Xgboost_tree, newdata = test_df,
                         type = "raw"), 
         price = round(price/5000000)*5000000) %>% 
  dplyr::select(property_id, price)


write.table(df_hogares_test_variables1, file = "xgboost_10_variables_v3.csv", row.names = FALSE, sep = ",")



# Adaboost ----------------------------------------------------------------



grid_gbm<-expand.grid(n.trees=c(200,300),
                      interaction.depth=c(4,6),
                      shrinkage=c(0.001,0.01),
                      n.minobsinnode = c(12,30))


model_gradient_boost <- train(price ~., 
                              trControl = fitControl,
                              method = "gbm",
                              tuneGrid=grid_gbm,
                              data = train_df, 
                              verbose = FALSE
)  



df_hogares_test_variables1 <- df_testing_variables  %>% 
  mutate(price = predict(model_gradient_boost, newdata = df_testing_variables,
                         type = "raw"), 
         price = round(price, -5)) %>% 
  dplyr::select(property_id, price)




write.table(df_hogares_test_variables1, file = "adaboost_8_variables.csv", row.names = FALSE, sep = ",")




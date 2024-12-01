rm(list = ls())

library(dplyr)
library(glmnet)

test_df <- readRDS("D:/Problem_Set_3/test_df.rds")
train_df <- readRDS("D:/Problem_Set_3/train_df.rds") %>% 
  mutate(categoria = ifelse(categoria == 'casa','Casa',categoria))


precios = train_df$price

predictores = model.matrix( ~ m2 + n_cuartos + bedrooms + categoria + parqueadero + asensor + ESTRATO + V_REF + m2_manzana + n_predios_manzana, train_df)

set.seed(123)

ridge = glmnet(x = predictores,y = precios,alpha = 1)

betas = as.data.frame(as.matrix(ridge$beta))

plot(ridge,xvar = 'lambda')

cross_validation = cv.glmnet(x = predictores,y = precios,alpha = 0.70)

plot(cross_validation)


nombres_columnas <- c('alpha','best_lambda', 'CV_MSE')

df_cv_en <- data.frame(matrix(ncol = length(nombres_columnas), nrow = 0))

colnames(df_cv_en) <- nombres_columnas


for (alpha_input in seq(0,1, 0.05)) {
  
  cross_validation = cv.glmnet(x = predictores,y = precios,alpha = alpha_input)
  filas_loop = nrow(df_cv_en)
  
  df_cv_en[filas_loop + 1,'alpha'] = alpha_input
  df_cv_en[filas_loop + 1,'best_lambda'] = last(cross_validation$lambda)
  df_cv_en[filas_loop + 1,'CV_MSE'] = last(cross_validation$cvm)
  
}

best_hiperparametros = df_cv_en %>% 
  filter(CV_MSE == min(CV_MSE)) %>% 
  as.vector()

best_elastic_net = glmnet(x = predictores,y = precios,alpha = best_hiperparametros$alpha,lambda = best_hiperparametros$best_lambda)


prediccion_en = predict(best_elastic_net,newx = as.matrix(model.matrix( ~ m2 + n_cuartos + bedrooms + categoria + parqueadero + asensor + ESTRATO + V_REF + m2_manzana + n_predios_manzana, test_df))[,-1])

predicciones = test_df %>% 
  select(property_id) %>% 
  bind_cols(prediccion_en) %>% 
  rename(price = s0) %>% 
  mutate(price = ifelse(0 > price,min(train_df$price),price),
         price = round(price, -5))


write.table(predicciones, file = paste0("elastic_net_",best_hiperparametros$alpha,
                                        'alpha',round(best_hiperparametros$best_lambda),
                                        'Lambda',"_10_variables.csv"), row.names = FALSE, sep = ",")

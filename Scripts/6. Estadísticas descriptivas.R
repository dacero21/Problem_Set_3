library(dplyr)
library(scales)

# Código de estadísticas descriptivas -------------------------------------


# Tabla estadísiticas descriptivas ----------------------------------------

train_df <- readRDS("D:/Problem_Set_3/train_df.rds") %>% 
  mutate(categoria = ifelse(categoria == 'casa','Casa',categoria)) %>% 
  select(!bedrooms) %>% 
  mutate(m2 = ifelse(30 > m2,m2*1000,m2))


nombres_columnas <- c('Variable','Promedio', 'Desviación estándar','Mínimo','Máximo')

est_descriptivas <- data.frame(matrix(ncol = length(nombres_columnas), nrow = 0))

colnames(est_descriptivas) <- nombres_columnas

for (i in colnames(train_df)[-c(1,4,5,6,7,8)] ) {
  print(i)
  fila_actual = nrow(est_descriptivas)
  est_descriptivas[fila_actual + 1,'Variable'] = i
  est_descriptivas[fila_actual + 1,'Promedio'] = mean(train_df[[i]],na.rm = T) %>% comma()
  est_descriptivas[fila_actual + 1,'Desviación estándar'] = sd(train_df[[i]],na.rm = T) %>% comma()
  est_descriptivas[fila_actual + 1,'Mínimo'] = min(train_df[[i]],na.rm = T) %>% comma()
  est_descriptivas[fila_actual + 1,'Máximo'] = max(train_df[[i]],na.rm = T) %>% comma()
}


saveRDS(est_descriptivas,'est_descriptivas.rds')



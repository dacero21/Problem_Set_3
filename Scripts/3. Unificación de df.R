library(dplyr)

# rm(list = ls())

#ordenar resultados

lista_train = readRDS('train.rds')
lista_test = readRDS('test.rds')




nombres_columnas <- c('ID','m2', 'n_cuartos', 'vetustez', 'categoria', 'parqueadero', 'asensor')


# Arreglar base de train --------------------------------------------------

df_train <- data.frame(matrix(ncol = length(nombres_columnas), nrow = 0))

colnames(df_train) <- nombres_columnas

ids_errados = c()

for (i in names(lista_train)) {
  if (is.list(lista_train[[i]])) {
    valores = unlist(strsplit(lista_train[[i]][[1]],split = ';'))
  } else {
    valores = unlist(strsplit(lista_train[[i]],split = ';'))
  }
  
  
  if (6 != length(valores)) {
    print(paste0(i,' está mal'))
    ids_errados = c(ids_errados,i)
    
  } else {
    filas_loop = nrow(df_train)
    df_train[filas_loop + 1,'ID'] = i
    df_train[filas_loop + 1,'m2'] = valores[1]
    df_train[filas_loop + 1,'n_cuartos'] = valores[2]
    df_train[filas_loop + 1,'vetustez'] = valores[3]
    df_train[filas_loop + 1,'categoria'] = valores[4]
    df_train[filas_loop + 1,'parqueadero'] = valores[5]
    df_train[filas_loop + 1,'asensor'] = valores[6]
  }
  
}

df_train_arreglado = df_train %>% 
  mutate(m2 = str_to_lower(m2),
         m2 = str_replace_all(m2,'metros cuadrados',''),
         m2 = str_replace_all(m2,':',''),
         m2 = str_replace_all(m2,'m2',''),
         m2 = str_replace_all(m2,'mt2',''),
         m2 = str_replace_all(m2,'mts',''),
         m2 = str_replace_all(m2,' ',''),
         m2 = str_replace_all(m2,'mp',''),
         m2 = str_replace_all(m2,',[0-9]{3}','000'),
         m2 = str_replace_all(m2,',','.'),
         m2 = as.numeric(m2),
         n_cuartos = str_to_lower(n_cuartos),
         n_cuartos = str_replace_all(n_cuartos,' ',''),
         n_cuartos = str_replace_all(n_cuartos,'númerodecuartos',''),
         n_cuartos = str_replace_all(n_cuartos,':',''),
         n_cuartos = str_replace_all(n_cuartos,'cuartos',''),
         n_cuartos = str_replace_all(n_cuartos,'numerodecuartos',''),
         n_cuartos = str_replace_all(n_cuartos,'una','1'),
         n_cuartos = str_replace_all(n_cuartos,'dos','2'),
         n_cuartos = str_replace_all(n_cuartos,'tres','3'),
         n_cuartos = str_replace_all(n_cuartos,'cuatro','4'),
         n_cuartos = str_replace_all(n_cuartos,'cinco','5'),
         n_cuartos = as.numeric(n_cuartos),
         categoria = str_to_lower(categoria),
         categoria = str_replace_all(categoria,' ',''),
         categoria = str_replace_all(categoria,'\\n',''),
         categoria = str_replace_all(categoria,'categoríadevivienda:',''),
         categoria = str_replace_all(categoria,'categoria:',''),
         categoria = str_replace_all(categoria,'aparatamento','apartamento'),
         categoria = str_replace_all(categoria,'apto','apartamento'),
         categoria = str_replace_all(categoria,'apatamento','apartamento'),
         categoria = str_replace_all(categoria,'paraestrenar','apartamento'),
         categoria = str_replace_all(categoria,'duplex','apartamento'),
         categoria = str_replace_all(categoria,'loft','apartaestudio'),
         categoria = str_replace_all(categoria,'aptaestudio','apartaestudio'),
         categoria = str_replace_all(categoria,'10','otro'),
         categoria = str_replace_all(categoria,'^a$','otro'),
         categoria = str_replace_all(categoria,'^ana$','otro'),
         categoria = str_replace_all(categoria,'^10$','otro'),
         categoria = str_replace_all(categoria,'edificio','otro'),
         categoria = str_replace_all(categoria,'^na$','otro'),
         categoria = str_replace_all(categoria,'^si$','otro'),
         categoria = str_replace_all(categoria,'^finca$','otro'),
         categoria = str_replace_all(categoria,'4.casa','casa'),
         categoria = str_replace_all(categoria,'casacomercial','casa'),
         categoria = str_replace_all(categoria,'penhouse','penthouse'),
         parqueadero = str_to_lower(parqueadero),
         parqueadero = str_replace_all(parqueadero,'í','i'),
         parqueadero = case_when(grepl('si',parqueadero) ~ T,
                                 grepl('no',parqueadero) ~ F,
                                 T ~ NA),
         asensor = str_to_lower(asensor),
         asensor = str_replace_all(asensor,'í','i'),
         asensor = case_when(grepl('si',asensor) ~ T,
                                 grepl('no',asensor) ~ F,
                                 T ~ NA))
  

# Arreglar base de test ---------------------------------------------------


df_test <- data.frame(matrix(ncol = length(nombres_columnas), nrow = 0))

colnames(df_test) <- nombres_columnas

ids_errados = c()

for (i in names(lista_test)) {
  if (is.list(lista_test[[i]])) {
    valores = unlist(strsplit(lista_test[[i]][[1]],split = ';'))
  } else {
    valores = unlist(strsplit(lista_test[[i]],split = ';'))
  }
  
  
  if (6 != length(valores)) {
    print(paste0(i,' está mal'))
    ids_errados = c(ids_errados,i)
    
  } else {
    filas_loop = nrow(df_test)
    df_test[filas_loop + 1,'ID'] = i
    df_test[filas_loop + 1,'m2'] = valores[1]
    df_test[filas_loop + 1,'n_cuartos'] = valores[2]
    df_test[filas_loop + 1,'vetustez'] = valores[3]
    df_test[filas_loop + 1,'categoria'] = valores[4]
    df_test[filas_loop + 1,'parqueadero'] = valores[5]
    df_test[filas_loop + 1,'asensor'] = valores[6]
  }
  
}


lista_test_volver_a_Corres = lista_test[setdiff(names(lista_test),ids_errados)]
# saveRDS(lista_test_volver_a_Corres,'test.rds')


df_test_arreglado = df_test %>% 
  mutate(m2 = str_to_lower(m2),
         m2 = str_replace_all(m2,'metros cuadrados',''),
         m2 = str_replace_all(m2,':',''),
         m2 = str_replace_all(m2,'m2',''),
         m2 = str_replace_all(m2,'mt2',''),
         m2 = str_replace_all(m2,'mts',''),
         m2 = str_replace_all(m2,' ',''),
         m2 = str_replace_all(m2,'mp',''),
         m2 = str_replace_all(m2,',[0-9]{3}','000'),
         m2 = str_replace_all(m2,',','.'),
         m2 = as.numeric(m2),
         n_cuartos = str_to_lower(n_cuartos),
         n_cuartos = str_replace_all(n_cuartos,' ',''),
         n_cuartos = str_replace_all(n_cuartos,'númerodecuartos',''),
         n_cuartos = str_replace_all(n_cuartos,':',''),
         n_cuartos = str_replace_all(n_cuartos,'cuartos',''),
         n_cuartos = str_replace_all(n_cuartos,'numerodecuartos',''),
         n_cuartos = str_replace_all(n_cuartos,'una','1'),
         n_cuartos = str_replace_all(n_cuartos,'dos','2'),
         n_cuartos = str_replace_all(n_cuartos,'tres','3'),
         n_cuartos = str_replace_all(n_cuartos,'cuatro','4'),
         n_cuartos = str_replace_all(n_cuartos,'cinco','5'),
         n_cuartos = as.numeric(n_cuartos),
         categoria = str_to_lower(categoria),
         categoria = str_replace_all(categoria,' ',''),
         categoria = str_replace_all(categoria,'\\n',''),
         categoria = str_replace_all(categoria,'categoríadevivienda:',''),
         categoria = str_replace_all(categoria,'categoria:',''),
         categoria = str_replace_all(categoria,'aparatamento','apartamento'),
         categoria = str_replace_all(categoria,'apto','apartamento'),
         categoria = str_replace_all(categoria,'apatamento','apartamento'),
         categoria = str_replace_all(categoria,'paraestrenar','apartamento'),
         categoria = str_replace_all(categoria,'duplex','apartamento'),
         categoria = str_replace_all(categoria,'loft','apartaestudio'),
         categoria = str_replace_all(categoria,'apartaestudioapartamento','apartaestudio'),
         categoria = str_replace_all(categoria,'aptaestudio','apartaestudio'),
         categoria = str_replace_all(categoria,'10','otro'),
         categoria = str_replace_all(categoria,'^a$','otro'),
         categoria = str_replace_all(categoria,'^ana$','otro'),
         categoria = str_replace_all(categoria,'^10$','otro'),
         categoria = str_replace_all(categoria,'edificio','otro'),
         categoria = str_replace_all(categoria,'^na$','otro'),
         categoria = str_replace_all(categoria,'^si$','otro'),
         categoria = str_replace_all(categoria,'oficina','otro'),
         categoria = str_replace_all(categoria,'^finca$','otro'),
         categoria = str_replace_all(categoria,'4.casa','casa'),
         categoria = str_replace_all(categoria,'casacomercial','casa'),
         categoria = str_replace_all(categoria,'penhouse','penthouse'),
         parqueadero = str_to_lower(parqueadero),
         parqueadero = str_replace_all(parqueadero,'í','i'),
         parqueadero = case_when(grepl('si',parqueadero) ~ T,
                                 grepl('no',parqueadero) ~ F,
                                 T ~ NA),
         asensor = str_to_lower(asensor),
         asensor = str_replace_all(asensor,'í','i'),
         asensor = case_when(grepl('si',asensor) ~ T,
                             grepl('no',asensor) ~ F,
                             T ~ NA))





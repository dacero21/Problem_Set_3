library(httr)
library(jsonlite)


api_key <- "acá se almacenó la llave de OpenAI"

# Extraer las otras variables  --------------------------------------------

caracteristicas_vivienda <- function(texto_completo) {
  
  # Define la URL de la API de ChatGPT (puedes ajustar si cambia)
  api_url <- "https://api.openai.com/v1/chat/completions"
  
  # Agrega tu clave de API aquí
  api_key <- "acá se almacenó la llave de OpenAI"
  
  # Realiza la solicitud POST a la API de OpenAI
  response <- POST(api_url, 
                   add_headers(Authorization = paste("Bearer", api_key)),
                   content_type_json(),
                   encode = 'json',
                   body = list(
                     model = "gpt-3.5-turbo",
                     messages = list(list(role = "user",
                                          content = paste0("Dame las variables metros cuadrados (Solo número);
                                                            número de cuartos (solo número);anitguedad en años (Solo número); 
                                                            Cateogría de vivienda (Apartaestudio, Apartamento, Casa, Penthouse, Finca, otro)(Solo una);
                                                            Si cuenta con parqueadero (Si, No) y; 
                                                            si cuenta con Ascensor  (Si,No) de la vivienda con la siguiente descripción.
                                                            Separa los valores por punto y coma. Solo dame los valores. Deben estár todos. 
                                                            Si no puedes deducir alguno o no se encuentra la información segistra el valor como un 'NA': ", texto_completo)))
                   ))
  
  # Procesar la respuesta
  response_content <- content(response, as = "text",encoding = 'UTF-8')
  result <- fromJSON(response_content)
  
  # Extraer el contenido de la respuesta generada por ChatGPT
  structured_data <- result$choices$message$content
  
  # Opcional: Si el resultado es una tabla en texto, puedes procesarlo para convertirlo en un data.frame en R
  
  # x = list(resultado = structured_data,respuesta = response)
  return(structured_data)
}


# Loop train df -----------------------------------------------------------

ids_train = train_df_valor_referencia %>% 
  as.data.frame() %>% 
  mutate(total = paste(title,description,sep = ':')) %>%
  select(property_id,total)

variables_complementarias = data.frame(
  property_id = ids_train$property_id
)

lista_almacenamiento = list()
# lista_almacenamiento = readRDS('train.rds')

for (i in setdiff(ids_train$property_id,names(lista_almacenamiento))) {
  tryCatch(expr = {
    descripcion_propiedad  = ids_train %>%
      filter(property_id == i) %>% 
      select(total) %>%
      unlist()
    
    valores = caracteristicas_vivienda(descripcion_propiedad)
    
    # lista_almacenamiento[[i]] = valores[['resultado']]
    lista_almacenamiento[[i]] = valores
    
    # variables_complementarias[variables_complementarias$property_id == i,'metros_2_propiedad'] = valores[['resultado']]
    variables_complementarias[variables_complementarias$property_id == i,'metros_2_propiedad'] = valores
  },
  error = function(e){
    print(e)
    print(i)
  }
  )
}

saveRDS(lista_almacenamiento,'train.rds')

# Loop test df ------------------------------------------------------------


ids_test = test_df_valor_referencia %>% 
  as.data.frame() %>% 
  mutate(total = paste(title,description,sep = ':')) %>%
  select(property_id,total)

variables_complementarias_test = data.frame(
  property_id = ids_test$property_id
)

lista_almacenamiento_test = readRDS('test.rds')

for (i in setdiff(ids_test$property_id,names(lista_almacenamiento_test))) {
  tryCatch(expr = {
    descripcion_propiedad  = ids_test %>%
      filter(property_id == i) %>% 
      select(total) %>%
      unlist()
    
    valores = caracteristicas_vivienda(descripcion_propiedad)
    
    lista_almacenamiento_test[[i]] = valores
    
    variables_complementarias_test[variables_complementarias_test$property_id == i,'metros_2_propiedad'] = valores
    
  },
  error = function(e){
    print(e)
    print(i)
  }
  )
}

saveRDS(lista_almacenamiento_test,'test.rds')

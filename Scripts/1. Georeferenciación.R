
# Librerías, datos y environment ------------------------------------------

rm(list = ls())

library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# Leer las localidades. 
localidades = read_sf('Data/loca')

# Leer los barrios de Bogotá
catastro = read_sf('Data/catastro')

# Leer los estratos por cuadra
estratificacion = read_sf('Data/manzanaestratificacion')

# Leer datos de predios por cuadra
predios_residenciales_cuadra = read.csv('Data/numero_predios_residenciales.csv',dec = '.',colClasses = c('character','character','character','character',
                                                                                                         'character','character')) %>% 
  mutate(n_predios_manzana = as.numeric(str_replace_all(Cantidad.de.predios,',',''))) %>% 
  select(Código.manzana,n_predios_manzana) %>% 
  rename(MANCODIGO = Código.manzana) %>% 
  unique() 

# Leer los m2 residenciales construidos por cuadra
m2_construidos_cuadra = read.csv('Data/area_construida_residencial_m2.csv',dec = '.',colClasses = c('character','character','character','character',
                                                                                                    'character','character','character')) %>% 
  mutate(m2_manzana = as.numeric(str_replace_all(Área.contruida..m2.,',',''))) %>% 
  select(Código.manzana,m2_manzana) %>% 
  rename(MANCODIGO = Código.manzana) %>% 
  unique() 

# Leer los valores de referencia por cuadra 2023 (media precio m2)
valor_ref_2023 = read_sf('Data/valor_ref_2023') %>% 
  st_transform(crs = 4686) %>% 
  group_by(MANCODIGO) %>% 
  filter(row_number() == 1) %>% 
  left_join(predios_residenciales_cuadra) %>% 
  left_join(m2_construidos_cuadra)

# Leer los datos de prueba
test_df = read.csv('Data/test.csv') %>% 
  st_as_sf(coords = c('lon','lat'),crs = 4686)

# Leer los datos de entrenamiento
train_df = read.csv('Data/train.csv') %>% 
  st_as_sf(coords = c('lon','lat'),crs = 4686)

# Añadir variables a las tablas -------------------------------------------


## Estrato ---------------------------------------------------------------

train_df_estrato = st_join(train_df,estratificacion %>% 
                             select(ESTRATO,geometry) %>% 
                             mutate(geometry = st_make_valid(geometry)),join = st_nearest_feature)
  
test_df_estrato = st_join(test_df,estratificacion %>% 
                             select(ESTRATO,geometry) %>% 
                             mutate(geometry = st_make_valid(geometry)),join = st_nearest_feature)


## Valor de referencia  --------------------------------------------------

train_df_valor_referencia = st_join(train_df_estrato,valor_ref_2023 %>% 
                                      select(V_REF,geometry,m2_manzana,n_predios_manzana) %>% 
                                      mutate(geometry = st_make_valid(geometry)),join = st_nearest_feature)

test_df_valor_referencia = st_join(test_df_estrato,valor_ref_2023 %>% 
                                      select(V_REF,geometry,m2_manzana,n_predios_manzana) %>% 
                                      mutate(geometry = st_make_valid(geometry)),join = st_nearest_feature)













# filtrar la localidad de chapinero
chapinero = localidades %>%
  filter(LocCodigo == '02')

barrios_chapinero = catastro %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  st_intersection(chapinero)

estratos_chapinero = st_join(barrios_chapinero,estratificacion %>% 
                             select(ESTRATO,geometry) %>% 
                             mutate(geometry = st_make_valid(geometry)),join = st_nearest_feature)

valor_referencia_chapinero = st_join(barrios_chapinero,valor_ref_2023 %>% 
                                       select(V_REF,geometry) %>% 
                                       mutate(geometry = st_make_valid(geometry)),join = st_nearest_feature)

ggplot() +
  # geom_sf(data = chapinero) +
  geom_sf(data = estratos_chapinero, aes(fill = factor(ESTRATO))) +
  theme_minimal() +
  theme(legend.position = "right") + # Posicionar la leyenda
  labs(fill = "Estrato",title = 'Estratos en Chapinero')


ggplot() +
  # geom_sf(data = chapinero) +
  geom_sf(data = valor_referencia_chapinero, aes(fill = V_REF)) +
  scale_fill_continuous(labels = label_dollar(prefix = "$", scale = 1)) + 
  theme_minimal() +
  theme(legend.position = "right") + # Posicionar la leyenda
  labs(fill = "Valores de referncia",title = 'Valores de referencia',subtitle = 'Barrios de chapinero')

# ggplot()+
#   geom_sf(data = estratificacion)
# 
# 
# ggplot()+
#   geom_sf(data = estratificacion %>%
#             filter(OBJECTID == '661444'))


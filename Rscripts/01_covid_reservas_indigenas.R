# COVID E RESERVAS INDIGENAS
# COVID Brasil
rm(list=ls()) # limpar as vari?veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')



x <- st_read('Input/shapefiles/shape_minerac_ilegal.shp')
mapview::mapview(x) # municípios com mineração ilegal em 2020

# 1) ver morte 100 mil hab covid para esses municípios que tem garimpo
muni.garimpo <- unique(x$code_mn)


covid <- read.csv2('Temp/covid.amzl.csv') %>% 
  dplyr::filter(city_ibge_code %in% muni.garimpo)

categorias <- covid %>%
  group_by(class_obit_100_mil_ha) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# De fato houveram mais mortes relativas ao tamanho da população onde tem garimpo ilegal.


# 2) ver reservas que tem garimpo 
library(mapview)
i <- geobr::read_indigenous_land()
mapview::mapview(i) # reservas indígenas
sf_use_s2(FALSE)
garimpo <- st_intersection(i,x) # terras indígenas com garimpo (fazer o mapa)
mapview::mapview(garimpo)

# não consegui os dados de mortes indígenas por reserva.

# Não precisa fazer um mapa do zero
# ver esse mapa https://covid19.socioambiental.org/
# e essa nota técnica https://drive.google.com/file/d/1H596_oDmOGf4mOTziHGIrbYM17PdycVj/view
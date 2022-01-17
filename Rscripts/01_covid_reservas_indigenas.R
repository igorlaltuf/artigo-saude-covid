# COVID E RESERVAS INDIGENAS
# COVID Brasil
rm(list=ls()) # limpar as variáveis carregadas
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
# São 71 municípios com garimpo ilegal na AMZL
qtd.muni.garimpo <- unique(covid$city_ibge_code)


# 2) ver reservas que tem garimpo 
library(mapview)
i <- geobr::read_indigenous_land()
mapview::mapview(i) # reservas indÃ­genas
sf_use_s2(FALSE)
garimpo.terra.indigena <- st_intersection(i,x) # terras indÃ­genas com garimpo (fazer o mapa)
mapview::mapview(garimpo.terra.indigena)

# municípios dentro de terra indígena com garimpo ilegal (60 no total)
muni.ind <- unique(garimpo.terra.indigena$code_mn)

# Dos 71 municípios com garimpo na AMZL, 60 estão em reservas indígenas!!!!!

# municípios em terras indígenas e covid
terras.covid <- covid %>% 
  dplyr::filter(city_ibge_code %in% muni.ind) %>% 
  arrange(desc(obitos_100_mil_ha))

categorias.terra <- terras.covid %>%
  group_by(class_obit_100_mil_ha) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# passar classificação para factor
terras.covid$class_obit_100_mil_ha <- as.factor(terras.covid$class_obit_100_mil_ha)
terras.covid$class_obit_100_mil_ha <- factor(terras.covid$class_obit_100_mil_ha, levels = c('Muito Alto','Alto','Médio Alto','Médio Baixo','Baixo','Muito Baixo'))


# Mapa covid municípios em terras indígenas
shape.muni.amzl <- read_sf('Input/shapefiles/shape.muni.amzl.shp')
shape.selec <- left_join(terras.covid,shape.muni.amzl, by = c('city_ibge_code'='cd_mn'))

shape.amzl <- read_amazon()

ggplot()+
  geom_sf(data = shape.amzl, aes(geometry = geom), fill = NA) + 
  geom_sf(data = shape.selec, aes(fill=class_obit_100_mil_ha, geometry = geometry), colour = NA) +
  scale_fill_manual(values = rev(brewer.pal(6,"Greys")))+
  #geom_point(data = coord.energia, aes(geometry = geom, col = 'Energia'), stat = "sf_coordinates", size = 1.5, colour = '#8856a7')+
  #geom_point(data = coord.mineracao, aes(geometry = geom, col = 'Mineração'), stat = "sf_coordinates", size = 1.5, colour = '#2ca25f')+
  #geom_sf_text(data = coord.energia, aes(label = name_muni), colour='grey10',vjust=1.7, size = 1.5) +
  #geom_sf_text(data = coord.mineracao, aes(label = name_muni), colour='grey10',vjust=1.7, size = 1.5) +
  labs(fill= NULL, y=NULL, x=NULL) + #Muda o nome da legenda com o fill.
  #scale_color_manual(values = c("Royalties Energia" = '#8856a7','Royalties Energia' = '#2ca25f')) +
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345"))+
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_classic()+ # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')

ggsave('Outputs/mapas/covid_muni_terras_ind_grey.png', width = 9, height = 6, dpi = 600)









# tabela municípios em terras indígenas
categorias <- categorias.terra %>% 
  arrange(desc(class_obit_100_mil_ha)) %>% 
  mutate(percentual = round(n/60,2)) %>% 
  ungroup()

tab.categorias <- gt(categorias) %>%
  cols_label(
    class_obit_100_mil_ha = 'Classificação de óbitos a cada 100 mil habitantes',
    n = 'Quantidade de municípios',
    percentual = 'Percentual'
  ) %>% 
  tab_header(
    title = 'Classificação dos óbitos por COVID-19 nas terras indígenas com presença de garimpo ilegal',
    subtitle = '2020'
  ) %>%
  cols_hide(
    columns = N_category
  ) %>% 
  fmt_markdown(
    columns = c(class_obit_100_mil_ha)
  ) %>% 
  fmt_number(
    columns = n,
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  fmt_percent(
    decimals = 1,
    columns = percentual
  ) %>% 
  tab_source_note('Fonte: RAISG (2020) e IBGE via GEOBR (2020)') 

tab.categorias

gtsave(tab.categorias, 'Outputs/tabelas/tabela.terra.ind.png')




# 39 dos 60 municípios com valores acima da média


# não consegui os dados de mortes indígenas por reserva.

# Não precisa fazer um mapa do zero
# ver esse mapa https://covid19.socioambiental.org/
# e essa nota técnica https://drive.google.com/file/d/1H596_oDmOGf4mOTziHGIrbYM17PdycVj/view
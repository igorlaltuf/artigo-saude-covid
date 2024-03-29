# Classifica��o dos dados da COVID-19 na Amaz�nia Legal

# Obs: valores daqui batem com os valores do site para o dia 12-08-2021 (inclusive os dados de 
# �bitos a cada 100 mil hab).

rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

# importar e organizar os dados
covid <- read.csv('Input/caso_full.csv',fileEncoding = 'UTF-8')
covid.amzl <- covid %>% 
  dplyr::select(city_ibge_code,city,date,last_available_deaths,estimated_population) %>% 
  dplyr::filter(city_ibge_code %in% cidades.amazonia.legal &
                date == '2021-08-12') %>% 
  mutate(obitos_100_mil_ha = (last_available_deaths/estimated_population) * 100000) %>% 
  classificar.variavel('obitos_100_mil_ha', 'class_obit_100_mil_ha')

write.csv2(covid.amzl, file = 'Temp/covid.amzl.csv', row.names = F)

# quantidade de munic�pios em cada classe
x <- covid.amzl %>% 
  group_by(class_obit_100_mil_ha) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# Dados das cidades intermedi�rias
intermed <- covid.amzl %>% 
            dplyr::filter(city_ibge_code %in% cidades.intermediadoras) %>% 
            arrange(desc(obitos_100_mil_ha))
  

# Tabela da COVID-19 na Amaz�nia Legal com classifica��o restrita a essa regi�o
tabela.covid <- gt(intermed) %>%
    cols_label(
      city = 'Munic�pio',
      last_available_deaths = 'Quantidade de �bitos',
      obitos_100_mil_ha = 'Quantidade de �bitos a cada 100 mil habitantes',
      class_obit_100_mil_ha = 'Classifica��o'
    ) %>% 
    cols_hide(
      columns = c('city_ibge_code','date','estimated_population')
    ) %>% 
    tab_header(
      title = 'Mortalidade do COVID-19 na Amaz�nia Legal',
      subtitle = 'Entre 17/03/2020 e 12/08/2021'
    ) %>%
    fmt_markdown(
      columns = c(city,class_obit_100_mil_ha)
    ) %>% 
    fmt_number(
      columns = c(last_available_deaths,obitos_100_mil_ha),
      decimals = 0,
      sep_mark = '.',
      dec_mark = ','
    ) %>% 
    cols_align(
      align = 'center'
    ) %>% 
    tab_source_note('Fonte: Elaborado com base nos dados dispon�veis no dia 13 de agosto de 2021 de COVID-19 (AJ et al.): agregador desenvolvido por �lvaro Justen e colaboradores (https://brasil.io/dataset/covid19/boletim).')

tabela.covid
gtsave(tabela.covid, 'Outputs/tabelas/tabela_covid_intermediadoras.png')


# dados dos 6 maiores

x <- covid.amzl %>% 
  dplyr::filter(city_ibge_code %in% c(1100205,1505064,1508100,1503093,1500602,1508357,1505536,1502152,1504208,1505502,1600402,1505304))












# Mapa da COVID-19 na Amaz�nia Legal com classifica��o restrita a essa regi�o
shape.muni.amzl <- read_sf('Input/shapefiles/shape.muni.amzl.shp')
shape.muni.amzl <- left_join(covid.amzl,shape.muni.amzl, by = c('city_ibge_code'='cd_mn'))

# transforma character em factors
shape.muni.amzl$class_obit_100_mil_ha <- as.factor(shape.muni.amzl$class_obit_100_mil_ha)
# define a ordem dos factors (em 6 n�veis)
shape.muni.amzl$class_obit_100_mil_ha <- factor(shape.muni.amzl$class_obit_100_mil_ha, levels = c('Muito Alto','Alto','M�dio Alto','M�dio Baixo','Baixo','Muito Baixo'))
# coordenadas dos pontos
coord.cidades <- st_read('Input/shapefiles/coord.cidades.shp')


coord.energia <- geobr::read_municipal_seat(showProgress = T) %>% 
  dplyr::filter(code_muni %in% c(1100205,1505064,1508100,1503093,1500602,1508357))
coord.mineracao <- geobr::read_municipal_seat(showProgress = T) %>%
  dplyr::filter(code_muni %in% c(1505536,1502152,1504208,1505502,1600402,1505304))

#geobr::lookup_muni('parauapebas')

# plotar mapa com �bitos a cada 100 mil hab
ggplot(shape.muni.amzl)+
  geom_sf(aes(fill=class_obit_100_mil_ha, geometry = geometry), colour = NA)+
  scale_fill_manual(values = rev(brewer.pal(6,"YlOrRd")))+
  #geom_point(data = coord.energia, aes(geometry = geom, col = 'Energia'), stat = "sf_coordinates", size = 1.5, colour = '#8856a7')+
  #geom_point(data = coord.mineracao, aes(geometry = geom, col = 'Minera��o'), stat = "sf_coordinates", size = 1.5, colour = '#2ca25f')+
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

ggsave('Outputs/mapas/covid_obitos_amzl.png', width = 9, height = 6, scale = 1.5)


# �bitos da COVID nas cidades selecionadas
# Energia: Porto Velho (RO) e Altamira (PA) 
# Minera��o: Marab� (PA), Parauapebas (PA), Cana� dos Caraj�s (PA), Oriximin� (PA), Juruti (PA), Manaus (AM), Ouril�ndia do Norte (PA) e Itaituba (PA).
lista.cidades <- c(1100205,1500602,1504208,1505536,1502152,1505304,1503903,1503606)
shape.muni.amzl %>% dplyr::filter(city_ibge_code %in% lista.cidades)

# Taxa de Mortalidade 
tx.mortalidade <- covid %>% 
  dplyr::select(city_ibge_code,city,date,last_available_deaths,last_available_confirmed,estimated_population) %>% 
  dplyr::filter(city_ibge_code %in% cidades.amazonia.legal &
                  date == '2021-08-12') %>% 
  mutate(tx_mortalidade = (last_available_deaths/last_available_confirmed)) %>% 
  classificar.variavel('tx_mortalidade', 'tx_mortalidade_class') 

tx.mortalidade <- left_join(tx.mortalidade,shape.muni.amzl, by = 'city_ibge_code')

# transforma character em factors
tx.mortalidade$tx_mortalidade_class <- as.factor(tx.mortalidade$tx_mortalidade_class)
# define a ordem dos factors (em 6 níveis)
tx.mortalidade$tx_mortalidade_class <- factor(tx.mortalidade$tx_mortalidade_class, levels = c('Muito Alto','Alto','M�dio Alto','M�dio Baixo','Baixo','Muito Baixo'))

ggplot(tx.mortalidade)+
  geom_sf(aes(fill=tx_mortalidade_class, geometry = geometry), colour = NA)+
  scale_fill_manual(values = rev(brewer.pal(6,"YlOrRd")))+
  # geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates")+
  # geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 1.8) +
  labs(fill= 'Classifica��o da taxa\nde mortalidade', y=NULL, x=NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_classic()+ # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')

ggsave('Outputs/mapas/tx_mortalidade_amzl.png', width = 9, height = 6)


# tabela s�ntese
tx.mort.munis <- tx.mortalidade %>% 
  dplyr::filter(city_ibge_code %in% lista.cidades) %>% 
  arrange(desc(tx_mortalidade)) %>% 
  select(2,7,8,13,14) %>% 
  as.data.frame() %>% 
  mutate(tipo = ifelse(city.x %in% c('Porto Velho','Altamira'),'Royalties de Energia', 'Royalties da Minera��o')) %>% 
  select(1,6,2,4,3,5)


tabela.mortalidade <- gt(tx.mort.munis) %>%
  cols_label(
    tipo = 'Tipo de recurso recebido',
    city.x = 'Munic�pio',
    obitos_100_mil_ha = '�bitos a cada 100 mil habitantes',
    class_obit_100_mil_ha = 'Classifica��o de �bitos a cada 100 mil habitantes',
    tx_mortalidade = 'Taxa de Mortalidade',
    tx_mortalidade_class = 'Classifica��o da taxa de mortalidade'
  ) %>% 
  tab_header(
    title = 'Taxa de Mortalidade e �bitos da COVID-19 na Amaz�nia Legal',
    subtitle = 'Entre 17/03/2020 e 12/08/2021'
  ) %>%
  fmt_markdown(
    columns = c(tipo, city.x, class_obit_100_mil_ha, tx_mortalidade_class)
  ) %>% 
  fmt_number(
    columns = c(obitos_100_mil_ha),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  fmt_percent(
    columns = tx_mortalidade
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elaborado com base nos dados dispon�veis no dia 13 de agosto de 2021 de COVID-19 (AJ et al.): agregador desenvolvido por �lvaro Justen e colaboradores (https://brasil.io/dataset/covid19/boletim).')

tabela.mortalidade
gtsave(tabela.mortalidade, 'Outputs/tabelas/tabela_covid_munis.png')



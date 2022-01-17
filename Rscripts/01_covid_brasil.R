# COVID Brasil
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

covid <- read.csv('Input/caso_full.csv', encoding = 'UTF-8')

covid <- covid %>% 
  dplyr::select(city_ibge_code,city,place_type,state,date,last_available_deaths,estimated_population) %>% 
  dplyr::filter(place_type == 'city' &
                  date == '2021-08-12') %>% 
  mutate(obitos_100_mil_ha = (last_available_deaths/estimated_population)*100000) %>% 
  na.omit()

covid <- classificar.variavel(covid,'obitos_100_mil_ha','class_obit_100_mil_ha')

am <- covid %>% 
  dplyr::filter(state %in% 'AM')

grandes.muni <- covid %>% 
  arrange(desc(estimated_population))
  
intermed <- covid %>% 
  dplyr::filter(city_ibge_code %in% cidades.intermediadoras) %>% 
  arrange(desc(obitos_100_mil_ha))

x <- am %>% 
  group_by(class_obit_100_mil_ha) %>%
  mutate(N_category = n()) %>%
  count(N_category)

mean(covid$obitos_100_mil_ha)

# Tabela das cidades intermediadoras e classificação nacional
tabela.covid <- gt(intermed) %>%
  cols_label(
    city = 'Município',
    last_available_deaths = 'Quantidade de Óbitos',
    obitos_100_mil_ha = 'Quantidade de Óbitos a cada 100 mil habitantes',
    class_obit_100_mil_ha = 'Classificação'
  ) %>% 
  cols_hide(
    columns = c('city_ibge_code','date','estimated_population','place_type','state')
  ) %>% 
  tab_header(
    title = 'Mortalidade por Covid-19 nas cidades intermediárias da Amazônia Legal',
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
  tab_source_note('Fonte: Elaborado com base nos dados disponíveis no dia 13 de agosto de 2021 de COVID-19 (AJ et al.): agregador desenvolvido por Álvaro Justen e colaboradores (https://brasil.io/dataset/covid19/boletim).')

tabela.covid
gtsave(tabela.covid, 'Outputs/tabelas/Covid_intermediadoras_classif_brasil.png')

# write.csv(covid,'Outputs/03_mapas/Saúde/covid_classificacao_brasil.csv')
mean(covid$obitos_100_mil_ha)

# mapa Brasil por município
shape.muni <- st_read('Input/shapefiles/shape.muni.shp')
covid.shape <- left_join(covid, shape.muni, by = c('city_ibge_code' = 'cd_mn'))

# transforma character em factors
covid.shape$class_obit_100_mil_ha <- as.factor(covid.shape$class_obit_100_mil_ha)

# define a ordem dos factors (em 6 levels)
covid.shape$class_obit_100_mil_ha <- factor(covid.shape$class_obit_100_mil_ha, levels = c('Muito Alto','Alto','Médio Alto','Médio Baixo','Baixo','Muito Baixo'))

# coord dos pontos
coord.cidades <- st_read('Input/shapefiles/coord.cidades.shp')

# ver os maiores 20 municípios que receberam royalties
dados.royalties <- read_csv2('Temp/maiores20royalties.csv')

maiores.20 <- covid %>% 
  dplyr::filter(city_ibge_code %in% dados.royalties$codigo_ibge) %>% 
  arrange(desc(obitos_100_mil_ha))

y <- maiores.20 %>% 
  group_by(class_obit_100_mil_ha) %>%
  mutate(N_category = n()) %>%
  count(N_category)

e <- dados.royalties %>%  # vetor com os muni de energia
  dplyr::filter(transferencia %in% 'Royalties - CFH')

m <- dados.royalties %>%  # vetor com os muni de mineração
  dplyr::filter(transferencia %in% 'Royalties - CFM')

coord.minerac <- geobr::read_municipal_seat() %>% 
  dplyr::filter(code_muni %in% e$codigo_ibge)

coord.energia <- geobr::read_municipal_seat() %>% 
  dplyr::filter(code_muni %in% m$codigo_ibge)

# Brasil
# Óbitos a cada 100 mil hab
ggplot(covid.shape)+
  geom_sf(aes(fill = class_obit_100_mil_ha, geometry = geometry), colour = NA) + # inclui a legenda automaticamente
  scale_fill_manual(values = rev(brewer.pal(6,"YlOrRd"))) + # define as cores do fill
  geom_point(data = coord.energia, aes(geometry = geom, col = 'Royalties de Energia'), stat = "sf_coordinates", size = 1.3)+
  geom_point(data = coord.minerac, aes(geometry = geom, col = 'Royalties de Mineração'), stat = "sf_coordinates", size = 1.3)+
  scale_colour_manual(values = c("Royalties de Energia" = "#8856a7", "Royalties de Mineração" = "#2ca25f"), name = NULL) + # legenda os pontos (o nome da legenda aqui deve ser o mesmo no atributo col em geom_point(aes(col)))
  labs(fill= NULL, y=NULL, x=NULL) + #Muda o nome da legenda com o fill e retira nomes dos eixos.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_classic()+ # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')+
  guides(col=guide_legend(ncol = 1))

ggsave('Outputs/mapas/covid_obitos_brasil.png', width = 9, height = 6)

# Fazer tabela com classificação do covid para os 38 maiores e colocar no artigo
energia.tab <- read_csv2('Temp/royalties_energia_maiores.csv') %>% 
  left_join(maiores.20, by = c('codigo_ibge' = 'city_ibge_code')) %>% 
  select(2,8,12,13,4) %>% 
  arrange(desc(obitos_100_mil_ha))

tabela.energia <- gt(energia.tab) %>%
  cols_label(
    municipio = 'Município',
    state = 'UF',
    val_acum_defl_2020 = 'Valor acumulado dos Royalties',
    obitos_100_mil_ha = 'Óbitos a cada 100 mil habitantes',
    class_obit_100_mil_ha = 'Classificação dos óbitos'
  ) %>% 
  tab_header(
    title = 'Ranking de municípios da Amazônia Legal por royalties recebidos via CFH e comparação com dados da COVID-19',
    subtitle = 'Valor acumulado em reais entre 2011 e 2020 deflacionado em valores de 2020'
  ) %>%
  fmt_markdown(
    columns = c(municipio,state,class_obit_100_mil_ha)
  ) %>% 
  fmt_number(
    columns = c(val_acum_defl_2020,obitos_100_mil_ha),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elaborado com base nos dados do Tesouro Nacional (2021) e nos dados 
                  disponíveis no dia 13 de agosto de 2021 de COVID-19 (AJ et al.): agregador 
                  desenvolvido por Álvaro Justen e colaboradores (https://brasil.io/dataset/covid19
                  /boletim) e dados do Tesouro Nacional de 2021')
tabela.energia
gtsave(tabela.energia, 'Outputs/tabelas/tabela.energia.png')


minerac.tab <- read_csv2('Temp/royalties_minerac_maiores.csv') %>% 
  left_join(maiores.20, by = c('codigo_ibge' = 'city_ibge_code')) %>% 
  select(2,8,12,13,4) %>% 
  arrange(desc(obitos_100_mil_ha))

tabela.minerac <- gt(minerac.tab) %>%
  cols_label(
    municipio = 'Município',
    state = 'UF',
    val_acum_defl_2020 = 'Valor acumulado dos Royalties',
    obitos_100_mil_ha = 'Óbitos a cada 100 mil habitantes',
    class_obit_100_mil_ha = 'Classificação dos óbitos'
  ) %>% 
  tab_header(
    title = 'Ranking de municípios da Amazônia Legal por royalties recebidos via CFM e comparação com dados da COVID-19',
    subtitle = 'Valor acumulado em reais entre 2011 e 2020 deflacionado em valores de 2020'
  ) %>%
  fmt_markdown(
    columns = c(municipio,state,class_obit_100_mil_ha)
  ) %>% 
  fmt_number(
    columns = c(val_acum_defl_2020,obitos_100_mil_ha),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elaborado com base nos dados do Tesouro Nacional (2021) e nos dados 
                  disponíveis no dia 13 de agosto de 2021 de COVID-19 (AJ et al.): agregador 
                  desenvolvido por Álvaro Justen e colaboradores (https://brasil.io/dataset/covid19
                  /boletim) e dados do Tesouro Nacional de 2021')

tabela.minerac
gtsave(tabela.minerac, 'Outputs/tabelas/tabela.minerac.png')


# incluir legenda dos pontos no mapa!

# Exatamente 19 dos 38 município (50%) ficaram acima da média nacional na quantidade de óbitos a cada 100 mil habitantes.
# Não mostra uma melhora em relação ao panorama nacional
# Presidente Figueiredo e Aripuanã figuram nas duas listas, por isso tem apenas 38 municípios no lugar de 40






# lista.cidades <- c(1100205,1500602,1504208,1505536,1502152,1505304,1503903,1302603,1505437,1503606)
# covid.shape %>% dplyr::filter(city_ibge_code %in% lista.cidades)

cidades <- covid %>% 
  dplyr::filter(city_ibge_code %in% c(1100205,1505064,1508100,1503093,1500602,1508357,1505536,1502152,1504208,1505502,1600402,1505304))


# Filtrar TODOS OS QUE RECEBERAM ROYALTIES NO BRASIL! COMPARAR COM O MAPA DAQUI!! VER QUANTOS EM CADA CATEGORIA!!!


# municípios energia
Porto velho, altamira, tucurui,novo repartimento, goianesia do pará, vitória do xingu
# mineração
excluir Mazagão (só recebeu valor alto em 2020) e incluir São Luis

# incluir tabela com municípios que mais receberam royalties no brasil nos últimos 10 anos




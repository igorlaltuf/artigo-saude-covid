# Rodovias e hidrovias 2016 IBGE
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Importar dados do IBGE
shape <- st_read("Input/rodovias e hidrovias 2016 IBGE/LRH2016_00_Base_Completa.shp", options = "ENCODING=WINDOWS-1252") # Este encoding ENCODING=WINDOWS-1252 foi a forma dele conseguir ler o os acentos do shapefile.

# Baixar e organizar coordenadas das cidades brasileiras
coordMunicipal <- read_municipal_seat(year = 2010, showProgress = T) # baixar todas as coordenadas das cidades brasileiras
coordMunicipal$lat <- as.numeric(st_coordinates(coordMunicipal$geom)[,2])
coordMunicipal$lng <- as.numeric(st_coordinates(coordMunicipal$geom)[,1])
coordMunicipal <- as.data.frame(coordMunicipal)

# Estabelecer parâmetros (tempo e custo)
tempo <- 9000 # limite de tempo (máx 9000 min) padrão 120 min
custo <- 950 # limite de custo (máx R$ 950) padrão 35 reais
rodo <- 1
hidro <- 1

# var05 é quantidade de saídas semanais via hidrovias
# var06 é quantidade de saídas semanais via rodovias
# filtrar separadamente acima de 100 saídas semanais

# Filtrar as 34 cidades intermediadoras como sendo a origem e quanto ao tempo
cid_inter <- c('1302603') # Manaus

# Hidrovias 2016
origem_intermed_lines <- shape %>% 
  dplyr::filter(codmundv_a %in% cid_inter & var04 <= tempo & var03 <= custo & var05 > hidro) # & var06 > rodo) 
# Os pares de ligação nessa pesquisa não tem "direção", então a posição do par de Municípios A-B não significa que B seja destino necessariamente,
# B é nesse caso tanto origem quanto destino. Por isso eu também filtro procurando as cidades intermediadoras na variável codmundv_b
origem_intermed_lines2 <- shape %>% 
  dplyr::filter(codmundv_b %in% cid_inter & var04 <= tempo & var03 <= custo & var05 > hidro) # & var06 > rodo)
origem_intermed_lines <- rbind(origem_intermed_lines,origem_intermed_lines2) # E agora junto as duas bases

# Regic de manaus que usa infra de saúde
regic.manaus.saude <-  c("1301605", "1300029", "1302553", "1300060", "1300086", "1300102", "1300144", "1300201", "1300300", "1300409", "1300631", "1300508",
                         "1300607", "1300680", "1300805", "1300839", "1300904", "1301001", "1301100", "1301159", "1301209", "1301407", "1301308", "1301654",
                         "1301803", "1301852", "1301902", "1301951", "1303536", "1302009", "1302108", "1302801", "1302207", "1302306", "1302405", "1302504",
                         "1303569", "1302702", "1303106", "1302900", "1303007", "1303205", "1303304", "1303403", "1303601", "1303700", "1303809", "1304062",
                         "1303908", "1303957", "1304005", "1304104", "1304203", "1304237", "1304260", "1304302", "1304401", "1500404", "1506807")



a <- origem_intermed_lines %>% 
  dplyr::filter(codmundv_a %in% regic.manaus.saude)

b <- origem_intermed_lines %>% 
  dplyr::filter(codmundv_b %in% regic.manaus.saude)

origem_intermed_lines <- rbind(a,b) # dados apenas dos municípios que constam na regic de saúde de manaus


# Mapa estático das hidrovias
sf_use_s2(FALSE) 
mapa <- read_amazon()
hidrovias.navegaveis <- read_sf('Input/shapes logística/hidrovias/Hidrovias.shp') %>% 
  dplyr::filter(cla_icacao %in% c('Navegável', 'Navegação sazonal'))

x <- origem_intermed_lines %>% 
  mutate(class_var05 = cut(var05, breaks=c(0,40,80,120,Inf), labels = c('0-40 saídas','40-80 saídas','80-120 saídas','120 ou mais saídas')))

cidades <- unique(append(x$codmundv_a, x$codmundv_b)) # cidades que tem ligações hidro ou rodoviárias
cidades <- paste(cidades, collapse = '|')

cidades.ponto <- st_read("Input/REGIC2018_cidades_ponto/REGIC2018_Cidades_ponto.shp", options = "ENCODING=UTF-8") %>% 
  dplyr::filter(str_detect(cod_cidade, cidades)) 
cidades.ponto <- cidades.ponto %>%  
  mutate(lat = unlist(map(cidades.ponto$geometry, 1)),
         lng = unlist(map(cidades.ponto$geometry, 2)))

# mapa base
grafico <- ggplot() +
  geom_sf(data = mapa, aes(geometry = geom)) +
  geom_sf(data = hidrovias.navegaveis, aes(geometry = geometry, color = 'Hidrovias'), size = .6, show.legend = 'line') +
  geom_sf(data = x, aes(geometry = geometry, color = class_var05), size = 0.5, show.legend = 'line') +
  geom_sf(data = cidades.ponto, aes(geometry = geometry), size = .5, shape = 16) +
  scale_color_manual(values = c('Hidrovias' = '#2c7fb8',"0-40 saídas" = "#ffffb2", "40-80 saídas" = "#fecc5c", '80-120 saídas' = '#fd8d3c', '120 ou mais saídas' = '#e31a1c'), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid", "solid", "solid","solid", "solid")))) +
  coord_sf(crs = 4674) +
  annotation_scale(location='br') +
  annotation_north_arrow(location='tl',
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom')


# Mapas das rotas próximas com frequência das saídas semanais
# Manaus
grafico +
  coord_sf(xlim = c(-71.2,-54), ylim = c(-8,1), expand = FALSE) +
  geom_text(
    data = cidades.ponto,
    label = cidades.ponto$label, 
    nudge_x = 0.05, nudge_y = -0.09, 
    aes(x = lat, y = lng),
    check_overlap = T,
    size = 2.5, 
    fontface = "bold")
ggsave('Outputs/03_mapas/Outros/frequencia_saídas_hidro_manaus.png', dpi = 600)




# dados para os municípios da REGIC de Manaus que fazem o uso da infra de saúde e que estão no estudo de hidrovias e rodovias
# tempo médio via hidrovia (var04) em horas
mean(origem_intermed_lines$var04)/60 # 20h09m
# mediana via hidrovia (var04) em horas
median(origem_intermed_lines$var04)/60 # 12h

# custo médio (var03)
mean(origem_intermed_lines$var03) # R$ 122,52
# custo mediano (var03)
median(origem_intermed_lines$var03) # R$ 120,00


# Rodovias 2016 
origem_intermed_lines <- shape %>% 
  dplyr::filter(codmundv_a %in% cid_inter & var04 <= tempo & var03 <= custo & var06 > rodo) 
origem_intermed_lines2 <- shape %>% 
  dplyr::filter(codmundv_b %in% cid_inter & var04 <= tempo & var03 <= custo & var06 > rodo)
origem_intermed_lines <- rbind(origem_intermed_lines,origem_intermed_lines2) # E agora junto as duas bases


a <- origem_intermed_lines %>% 
  dplyr::filter(codmundv_a %in% regic.manaus.saude)

b <- origem_intermed_lines %>% 
  dplyr::filter(codmundv_b %in% regic.manaus.saude)

origem_intermed_lines <- rbind(a,b) # dados apenas dos municípios que constam na regic de saúde de manaus


# Mapa estático das rodovias
x <- origem_intermed_lines %>% 
  mutate(class_var06 = cut(var06, breaks=c(0,400,Inf), labels = c('até 400 saídas','acima de 400 saídas')))
cidades <- unique(append(x$codmundv_a, x$codmundv_b)) # cidades que tem ligações hidro ou rodoviárias
cidades <- paste(cidades, collapse = '|')

cidades.ponto <- st_read("Input/REGIC2018_cidades_ponto/REGIC2018_Cidades_ponto.shp", options = "ENCODING=UTF-8") %>% 
  dplyr::filter(str_detect(cod_cidade, cidades))  

cidades.ponto <- cidades.ponto %>% 
  mutate(lat = unlist(map(cidades.ponto$geometry, 1)),
         lng = unlist(map(cidades.ponto$geometry, 2)))

# mapa base
shape.muni.uf <- st_read('Outputs/00_shapes_e_dados/shape.estad.amzl.shp')

grafico <- ggplot() +
  geom_sf(data = shape.muni.uf, aes(geometry = geometry), colour = NA) +
  geom_sf(data = hidrovias.navegaveis, aes(geometry = geometry, color = 'Hidrovias'), size = .6, show.legend = 'line') +
  geom_sf(data = x, aes(geometry = geometry, color = class_var06), size = .6, show.legend = 'line') +
  geom_sf(data = cidades.ponto, aes(geometry = geometry), size = .7, shape = 16) +
  scale_color_manual(values = c('Hidrovias' = '#2c7fb8','até 400 saídas' = '#2ca25f',"acima de 400 saídas" = "#f03b20"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid","solid","solid")))) +
  coord_sf(crs = 4674) +
  annotation_scale(location='br') +
  annotation_north_arrow(location='tl',
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom')

y <- cidades.ponto %>% 
  dplyr::filter(cod_cidade %in% cid_inter)

# gráfico geral
grafico +
  geom_sf_text(data = y, aes(label = label), colour='grey10', vjust = 1.3, size = 2.2) +
  coord_sf(xlim = c(-62,-57), ylim = c(-4,-1), expand = FALSE) +
  geom_text(
    data = cidades.ponto,
    label = cidades.ponto$label, 
    nudge_x = 0.05, nudge_y = 0, 
    aes(x = lat, y = lng),
    check_overlap = T,
    size = 2.5, 
    fontface = "bold")

ggsave('Outputs/03_mapas/Outros/frequencia_saídas_rodo_amzl.png')


# dados para os municípios da REGIC de Manaus que fazem o uso da infra de saúde e que estão no estudo de hidrovias e rodovias
# tempo médio via Rodovia (var04) em horas até Manaus dos municípios da REGIC de saúde
mean(origem_intermed_lines$var04)/60 # 5h18m
# mediana via hidrovia (var04) em horas
median(origem_intermed_lines$var04)/60 # 4h

# custo médio (var03)
mean(origem_intermed_lines$var03) # R$ 58,5
# custo mediano (var03)
median(origem_intermed_lines$var03) # R$ 35,00



# municípios da REGIC que aparecem na pesquisa de hidrovias e rodovias (51 no total)
cid <- c("1303205", "1304005", "1303569", "1303536",
"1301852", "1302405", "1302009", "1301902", "1302504","1301001", "1302306", "1302108", 
"1300029", "1300904", "1300300", "1300839", "1300631",
"1301308", "1302207", "1300102", "1301209", "1301159", "1300607", "1301605", "1301852",
"1300060", "1302009", "1301902", "1300086", "1300409", "1302504", "1300680", "1302553",
"1300805", "1300508", "1303809", "1303601", "1304260", "1302801", "1500404", "1304104", "1303007",
"1506807", "1302900", "1304062", "1303205", "1303106", "1302702", "1303403", "1303908",
"1303700", "1304203", "1304237", "1304302", "1303957", "1303304")
cid <- unique(cid)

# 
# 
# 
# # Rio Branco
# grafico +
#   coord_sf(xlim = c(-69,-67), ylim = c(-10.5,-8.5), expand = FALSE) +
#   geom_text(
#     data = cidades.ponto,
#     label = cidades.ponto$label, 
#     nudge_x = 0.05, nudge_y = 0.05, 
#     aes(x = lat, y = lng),
#     check_overlap = T,
#     size = 2.8, 
#     fontface = "bold")
# 
# ggsave('Outputs/03_mapas/Outros/frequencia_saídas_rodo_rio_branco.png')

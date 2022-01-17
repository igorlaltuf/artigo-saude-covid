# Respiradores por contaminados

rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

resp <- read_csv2('Input/distribuicao_respiradores.csv', 
                  col_names = T, col_types = cols(.default = "c", QUANTIDADE = 'n', VALOR = 'n')) %>%  
  janitor::clean_names() %>% 
  dplyr::filter(destino == 'AMAZONAS',
                estado_municipio == 'ESTADO') %>% 
  mutate(data = as_date(my(substr(data, 4, 10)))) %>% 
  group_by(data, destinatario, destino) %>% 
  summarise(quantidade = sum(quantidade)) %>% 
  dplyr::filter(destinatario %in% c('SECRETARIA ESTADUAL DE SAÚDE DO AMAZONAS',
                                    'Secretaria Estadual de Saúde',
                                    'SES AMAZONAS',
                                    'SECRETARIA ESTADUAL DE SAUDE DO AMAZONAS'))



resp2 <- read_csv2('Input/distribuicao_respiradores.csv', 
                  col_names = T, col_types = cols(.default = "c", QUANTIDADE = 'n', VALOR = 'n')) %>% 
  janitor::clean_names() %>% 
  group_by(destinatario, destino) %>% 
  summarise(qtd_respiradores = sum(quantidade)) %>% 
  arrange(desc(qtd_respiradores))
     


# 100 munis mais pop     
pop <- read_csv2('Temp/pop_muni_br_2000-20.csv', col_names = TRUE) %>%   
  dplyr::filter(ano == 2020) %>% 
  arrange(desc(populacao)) %>% 
  slice(1:100)

# Analisar evolução da distribuição de respiradores por estados e municípios
# calcular a proporção de respiradores per capita e respiradores por óbito
# regressão




# sobre oxigênio
# https://app.powerbi.com/view?r=eyJrIjoiZDE0MGZiMjItYzA1Zi00NjAwLWJmMGUtZjM4MDk3MDNjOGY0IiwidCI6ImI2N2FmMjNmLWMzZjMtNGQzNS04MGM3LWI3MDg1ZjVlZGQ4MSJ9
                 
covid <- read_csv2('Input/caso_full.csv', col_names = T)


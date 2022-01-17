# Evolução populacional por município - 2001 - 2020

rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

# Importar estimativas populacionais de 2005 a 2019, dados do censo de 2010 e da contagem populacional de 2007
# contagem 2007 https://www.ibge.gov.br/estatisticas/sociais/habitacao/9065-contagem-da-populacao.html?edicao=10189&t=resultados
pop.2010 <- read_excel('Input/tabela202.xlsx', skip = 4) %>% 
  select(1,2,4) %>% 
  janitor::clean_names()

pop.2005.2019 <- read_excel('Input/tabela6579.xlsx', skip = 3) %>% 
  janitor::clean_names()

pop.2007 <- read_excel('Input/popmunic2007layoutTCU14112007.xls', skip = 2, range = 'A3:E5567') %>% 
  janitor::clean_names() %>% 
  unite("cod_muni", 2:3,sep = '') %>% 
  rename('2007' = 'populacoes')

populacao <- left_join(pop.2005.2019,pop.2010) %>% 
  left_join(pop.2007, by = c('cod' = 'cod_muni')) %>% 
  rename('cod_muni' = 'cod',
         'muni' = 'municipio') %>% 
  select(1,2,22,3:8,24,9,10,21,11:20) %>% 
  janitor::clean_names() 

# %>% 
#   dplyr::filter(cod_muni %in% cidades.amazonia.legal)

# converter em numeric
populacao[4:23] <- lapply(populacao[4:23], as.numeric)

# descobrir quantos NAs existem em cada coluna do Dataframe
sapply(populacao, function(x) sum(is.na(x)))

# reorganizar dataframe como se fosse uma tabela dinÃ¢mica
populacao <- populacao %>% 
  pivot_longer(!c(cod_muni,muni,sigla_uf), names_to = "ano", values_to = "populacao") 

# remover o x antes dos anos
populacao$ano<-as.numeric(sub(".", "", populacao$ano))

# exportar csv com dados populacionais
con <- file('Temp/pop_muni_br_2000-20.csv', encoding="UTF-8") # para exportar em utf-8
write.csv2(populacao, file = con, row.names = F)

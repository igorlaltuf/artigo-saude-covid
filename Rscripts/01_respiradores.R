# Respiradores por contaminados

rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

resp <- read_csv2('Input/distribuicao_respiradores.csv', 
                  col_names = T, col_types = cols(.default = "c", QUANTIDADE = 'n', VALOR = 'n')) %>% 
  janitor::clean_names()
          
          
                 
covid <- read_csv2('Input/caso_full.csv', col_names = T)


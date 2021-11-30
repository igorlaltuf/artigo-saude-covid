# Royalties Ranking
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

options(scipen = 999) # remove a notação científica

# Ranking de royalties acumulados recebidos pelas prefeituras da AMZL de 2011 a 2020 em valores de 2020
royalties.amzl <- read.csv2('Temp/base_royalties_amzl.csv') %>% 
  dplyr::filter(ano %in% c(2011:2020),
                codigo_ibge %in% cidades.amazonia.legal) %>% 
  group_by(codigo_ibge, municipio, transferencia) %>% 
  summarise(val_acum_defl_2020 = sum(valor_real))

roy.energia <- royalties.amzl %>% 
  dplyr::filter(transferencia %in% 'Royalties - CFH') %>% 
  arrange(desc(val_acum_defl_2020)) %>% 
  classificar.variavel('val_acum_defl_2020','class_roy_energia')

roy.mineracao <- royalties.amzl %>% 
  dplyr::filter(transferencia %in% 'Royalties - CFM') %>% 
  arrange(desc(val_acum_defl_2020)) %>% 
  classificar.variavel('val_acum_defl_2020','class_roy_mineracao')
# Dos 773 municípios estudados, 76 municípios da AMZL que recebem royalties de energia e 457 de mineração
# foram selecionados os 20 municípios que mais receberam os dois tipos de royalties nos últimos 10 anos
# valor de corte:
# mineração R$ 26,5 mi em valores de 2020, ou seja, em média R$ 2,6 mi por ano.
# energia R$ 23,4 mi em valores de 2020, ou seja, em média R$ 2,3 mi por ano.

muni.e <- roy.energia[1:20,1:3]
muni.m <- roy.mineracao[1:20,1:3]
muni <- rbind(muni.e,muni.m)
write_csv2(x = muni,append = F,path = 'Temp/maiores20royalties.csv')


roy.energia <- roy.energia[1:20,1:5]
roy.mineracao <- roy.mineracao[1:20,1:5]

write_csv2(x = roy.energia, append = F, path = 'Temp/royalties_energia_maiores.csv')
write_csv2(x = roy.mineracao, append = F, path = 'Temp/royalties_minerac_maiores.csv')

# Ver dados de saúde dos 6 maiores de cada 
# fazer correlação entre todos q recebem royalties comparar royalties com longevidade 2010 e 2020


# Royalties Ranking
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

options(scipen = 999) # remove a nota��o cient�fica

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
# Dos 773 munic�pios estudados, 76 munic�pios da AMZL que recebem royalties de energia e 457 de minera��o
# foram selecionados os 20 munic�pios que mais receberam os dois tipos de royalties nos �ltimos 10 anos
# valor de corte:
# minera��o R$ 26,5 mi em valores de 2020, ou seja, em m�dia R$ 2,6 mi por ano.
# energia R$ 23,4 mi em valores de 2020, ou seja, em m�dia R$ 2,3 mi por ano.

muni.e <- roy.energia[1:20,1:3]
muni.m <- roy.mineracao[1:20,1:3]
muni <- rbind(muni.e,muni.m)
write_csv2(x = muni,append = F,path = 'Temp/maiores20royalties.csv')


roy.energia <- roy.energia[1:20,1:5]
roy.mineracao <- roy.mineracao[1:20,1:5]

write_csv2(x = roy.energia, append = F, path = 'Temp/royalties_energia_maiores.csv')
write_csv2(x = roy.mineracao, append = F, path = 'Temp/royalties_minerac_maiores.csv')

# Ver dados de sa�de dos 6 maiores de cada 
# fazer correla��o entre todos q recebem royalties comparar royalties com longevidade 2010 e 2020


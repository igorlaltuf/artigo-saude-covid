# Rodar todos os scripts

rm(list=ls()) 
# -- scripts base
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

#--- scripts das funções
source('Rscripts/01_datasus.R')
source('Rscripts/01_despesas_municipais_saude.R')
source('Rscripts/01_royalties.R')

# Municípios
# Porto Velho (RO) - Royalties CFH
# Altamira (PA) - Royalties CFH
# Marabá (PA) - Royalties CFM
# Parauapebas (PA) - Royalties CFM
# Canaã dos Carajás (PA) - Royalties CFM
# Oriximiná (PA) - Royalties CFM
# Juruti (PA) - Royalties CFM
# Manaus (AM) - Royalties CFM
# Ourilândia do Norte (PA) - Royalties CFM
# Itaituba (PA) - Royalties CFM


# geobr::lookup_muni(name_muni = 'Itaituba')
# Seis municípios que receberam mais royalties de energia
muni.energia <- c(1100205,1505064,1508100,1503093,1500602,1508357)
# Seis municípios que receberam mais royalties de mineração
muni.mineracao <- c(1505536,1502152,1504208,1505502,1600402,1505304)

# seleção nova (6 maiores de cada):
municipios <- c(1100205,1505064,1508100,1503093,1500602,1508357,1505536,
                1502152,1504208,1505502,1600402,1505304)

# seleção antiga de municípios
# municipios <- c(1100205,1500602,1504208,1505536,1502152,1505304,1503903,1302603,1505437,1503606,1508357)


Dados.Royalties(municipios,'Royalties - CFM') # transferências com royalties
Dados.Royalties(municipios,'Royalties - CFH') # transferências com royalties
Saude.Muni(municipios,'10 - Saúde') # despesas municipais com saúde

municipios <-  as.numeric(substr(municipios,1,6)) # muda para 6 dígitos para passar no datasus
Estab.Muni(municipios)  # estabelecimentos de saúde
Leitos.Muni(municipios) # leitos do sus (de todos os tipos)
Medicos.Muni(municipios) # quantidade de médicos por municipio

# incluir parallel processing (preciso fazer loops for no lugar do while)
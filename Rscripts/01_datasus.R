# Dados de saúde pública - DataSUS
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

# Pacote microdatasus
# 1 - Dados de estabelecimentos (2005 - 2020)
var.cnes.st <- c('CNES','CODUFMUN','COD_CEP','VINC_SUS','TPGESTAO','TP_UNID','TURNO_AT','GESPRG1E', 
                 'GESPRG1M','GESPRG2E','GESPRG2M','GESPRG4E','GESPRG4M','GESPRG5E','GESPRG5M', 
                 'GESPRG6E','GESPRG6M','NIVATE_A','NIVATE_H','URGEMERG')
ano <- 2005 # ano inicial
mes <- 12 # mês em que os dados serão coletados
while(ano <= 2020){
  dados <- fetch_datasus(year_start = ano ,
                         year_end = ano, 
                         month_start = mes, 
                         month_end = mes, 
                         uf = uf.amz.legal, 
                         information_system = "CNES-ST", 
                         vars = var.cnes.st) %>% 
    mutate(ano = ano)
  assign(paste("estabelec", ano, sep="."), dados)
  ano = ano + 1
}

dados.st <- rbind(estabelec.2005,estabelec.2006,estabelec.2007,estabelec.2008,
              estabelec.2009,estabelec.2010,estabelec.2011,estabelec.2012,
              estabelec.2013,estabelec.2014,estabelec.2015,estabelec.2016,
              estabelec.2017, estabelec.2018,estabelec.2019,estabelec.2020) %>% 
  janitor::clean_names()



# colocar isso dentro de uma função: exportar gráficos e tabelas via recortes

# quantos estabelecimentos de saúde tem em Porto Velho?
pv.m <- dados.st %>% 
  dplyr::filter(codufmun %in% 110020,
                vinc_sus == 1, # vinculados ao SUS
                nivate_h == 1, # sobre o atendimento: Tem atendimento hospitalar municipal ou estadual? exclui farmácias etc
                tpgestao == 'M', # administração municipal 
                gesprg6m == 1) %>% # hospitais que permitem internações
  group_by(ano) %>% 
  count()

pv.e <- dados.st %>% 
  dplyr::filter(codufmun %in% 110020,
                vinc_sus == 1, # vinculados ao SUS
                nivate_h == 1, # sobre o atendimento: Tem atendimento hospitalar municipal ou estadual? exclui farmácias etc
                tpgestao == 'E', # administração municipal 
                gesprg6e == 1) %>% # hospitais que permitem internações
  group_by(ano) %>% 
  count()

# Só tinha 1 hospital Municipal até 2019. Maior parte são estaduais.
# mesmo na pandemia, nenhum hospital municipal de PV era de alta intensidade
# Quando aos estaduais, nem todos são de alta intensidade (remover o atributo gesprg6e e comparar)

# gesprg6e  Alta complexidade estadual = 1
# gesprg6m  Alta complexidade municipal = 1












write.csv(x,'Outputs/00_shapes_e_dados/00_cnes_st_2015.csv', row.names = F)





# 2 - Dados de Leitos (2005 - 2020)
var.cnes.lt <- c('CNES','CODUFMUN','TP_UNID','TP_LEITO','CODLEITO','QT_EXIST','QT_CONTR','QT_SUS','QT_NSUS')

y <- fetch_datasus(year_start = 2019,
                   year_end = 2019, 
                   month_start = 12, 
                   month_end = 12, 
                   uf = uf.amz.legal, 
                   information_system = "CNES-LT",
                   var = var.cnes.lt)



# Cruzar dados abaixo com tipo de estabelecimento de sa?de do datasus para saber onde est?o os hospitais, upas etc
# algumas colunas s? aparecem quando eu fa?o a requisi??o especificamente pelo seu nome (NIVATE_H e URGEMERG).
# Por isso eu selecionei as colunas que preciso.





Incapacidade do SUS de atender as pessoas


# aumento de acidentes de transito, 
# médicos a cada 100 mil habitantes, leitos de uti a cada 100 mil habitantes


Regic de municípios que vão para PV e Altamira buscar tratamento.
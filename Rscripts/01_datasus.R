# Dados de saúde pública - DataSUS
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

pop <- read_csv('Temp/populacao_amzl_2001-20.csv') # População - 2001 - 2020

# Pacote microdatasus
# 1 - Dados de estabelecimentos (2005 - 2020)
# Baixar e limpar os dados
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

unique(dados.st$tpgestao)

# Função que retorna os dados # código do município apenas com 6 dígitos
# Função que retorna a evolução da quantidade de hospitais municipais e estaduais
Estab.Muni <- function(codigo_municipio){
  for(i in codigo_municipio){
  dados <- dados.st %>% 
    dplyr::filter(codufmun %in% i,
                  nivate_h == 1, # sobre o atendimento: Tem atendimento hospitalar municipal ou estadual? exclui farmácias etc
                  tpgestao %in% c('M','E','D'), # M, E ou D (dupla) administração municipal, estadual ou dupla
                  vinc_sus == 1) %>%  # vinculados ao SUS
    group_by(ano) %>% 
    count()
  
  label.muni <- cidades.brasil.nome[substr(cidades.brasil.nome$cod_muni,1,6) == i,2] # transformar em vetor ver regic script
  label.muni <- label.muni$muni # transforma em vetor
  label.muni <- as.character(str_replace_all(label.muni, "[[:punct:]]","")) # essa vari?vel deve receber o nome da cidade de acordo com o c?digo colocado
  arquivo.hospitais <- paste('Hosp muni e est em ', label.muni,'.png')
  diretorio <- paste0('Outputs/dados por municipio/',label.muni)
  dir.create(diretorio)
  
  # gerar gráfico
  grafico.estabelecimentos <- ggplot() +
    geom_bar(dados, mapping = aes(x = ano, y = n), col = 'blue', stat = 'identity') +
    ggtitle(paste("Hospitais vinculados ao SUS com gestão\nmunicipal, estadual ou dupla - ", label.muni)) +
    labs(y = 'Quantidade de hospitais', x = 'Ano', caption = 'Fonte: Elaboração própria. SALDANHA Et al.(2019).') +
    theme_classic()+
    theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
          plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(breaks = seq(0, max(dados$n), 1))+
    scale_x_continuous(breaks = seq(2005, 2020, 1))
    
  grafico.estabelecimentos
  ggsave(plot = grafico.estabelecimentos, path = diretorio, filename = arquivo.hospitais, width = 9, height = 6)
  
  }}

# teste
amostra <- c(110020,150060) # Lembrar que o código aqui é de 6 dígitos
Estab.Muni(amostra)



# Função com dados sobre os leitos por 100 mil habitantes
var.cnes.lt <- c('CNES','CODUFMUN','TP_UNID','TP_LEITO','CODLEITO','QT_EXIST','QT_CONTR','QT_SUS','QT_NSUS')
ano <- 2005 # ano inicial
mes <- 12 # mês em que os dados serão coletados
while(ano <= 2020){
  dados <- fetch_datasus(year_start = ano ,
                         year_end = ano, 
                         month_start = mes, 
                         month_end = mes, 
                         uf = uf.amz.legal, 
                         information_system = "CNES-LT", 
                         vars = var.cnes.lt) %>% 
    mutate(ano = ano)
  assign(paste("leito", ano, sep="."), dados)
  ano = ano + 1
}

dados.lt <- rbind(leito.2005,leito.2006,leito.2007,leito.2008,
                  leito.2009,leito.2010,leito.2011,leito.2012,
                  leito.2013,leito.2014,leito.2015,leito.2016,
                  leito.2017, leito.2018,leito.2019,leito.2020) %>% 
  janitor::clean_names()

Leitos.Muni <- function(codigo_municipio){
  for(i in codigo_municipio){
    dados <- dados.lt %>% 
      dplyr::filter(codufmun %in% i) %>% 
      select('ano','qt_sus') %>% 
      group_by(ano) %>% 
      summarise(leitos_sus = sum(qt_sus))
    
    label.muni <- cidades.brasil.nome[substr(cidades.brasil.nome$cod_muni,1,6) == i,2] # transformar em vetor ver regic script
    label.muni <- label.muni$muni # transforma em vetor
    label.muni <- as.character(str_replace_all(label.muni, "[[:punct:]]","")) # essa vari?vel deve receber o nome da cidade de acordo com o c?digo colocado
    arquivo.leitos <- paste('Leitos do SUS em ', label.muni,'.png')
    arquivo.leitos.pop <- paste('Leitos do SUS 100 mil hab ', label.muni,'.png')
    diretorio <- paste0('Outputs/dados por municipio/',label.muni)
    dir.create(diretorio)
    
    # gerar gráfico qtd leitos
    grafico.leitos <- ggplot() +
      geom_bar(dados, mapping = aes(x = ano, y = leitos_sus), col = 'blue', stat = 'identity') +
      ggtitle(paste("Evolução dos leitos do SUS - ", label.muni)) +
      labs(y = 'Quantidade de leitos', x = 'Ano', caption = 'Fonte: Elaboração própria. SALDANHA Et al.(2019).') +
      theme_classic()+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
            plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(breaks = seq(0, max(dados$leitos_sus), 50))+
      scale_x_continuous(breaks = seq(2005, 2020, 1))
    
    grafico.leitos
    ggsave(plot = grafico.leitos, path = diretorio, filename = arquivo.leitos, width = 9, height = 6)
    
    pop$cod_muni <- substr(pop$cod_muni, 1, 6)
    
    pop <- pop %>% 
      dplyr::filter(cod_muni == i) 
    
    dados <- left_join(dados, pop, by = 'ano') %>%     
      mutate(leitos_cada_100_mil_ha = (leitos_sus/populacao)*100000) %>% 
      select(1,2,6,7)
      
      
    # leitos do sus cada 100 mil hab (independentemente se é municipal ou não)
    grafico.leitos.pop <- ggplot() +
      geom_bar(dados, mapping = aes(x = ano, y = leitos_cada_100_mil_ha), col = 'blue', stat = 'identity') +
      ggtitle(paste("Evolução dos leitos do SUS a cada 100 mil habitantes - ", label.muni)) +
      labs(y = 'Quantidade de leitos a\ncada 100 mil habitantes', x = 'Ano', caption = 'Fonte: Elaboração própria. SALDANHA Et al.(2019).') +
      theme_classic()+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
            plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(breaks = seq(0, max(dados$leitos_sus), 50))+
      scale_x_continuous(breaks = seq(2005, 2020, 1))
    
    grafico.leitos.pop
    ggsave(plot = grafico.leitos.pop, path = diretorio, filename = arquivo.leitos.pop, width = 9, height = 6)
    
    
  }}

# teste
Leitos.Muni(110020)



# profissionais de saúde

# Tentar novamente depois, caso não dê, usar o tabnet com dados sem estarem separados pelo que é público
# https://datasus.saude.gov.br/transferencia-de-arquivos/# 
# https://www.youtube.com/watch?v=KoikseQAXpg 
# http://tabnet.datasus.gov.br/cgi/cnes/NT_RecursosHumanos.htm#origem 
ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/ colar no windows explorer
contribuir para adicionar outras bases





# Cruzar dados abaixo com tipo de estabelecimento de sa?de do datasus para saber onde est?o os hospitais, upas etc
# algumas colunas s? aparecem quando eu fa?o a requisi??o especificamente pelo seu nome (NIVATE_H e URGEMERG).
# Por isso eu selecionei as colunas que preciso.

















# Função com dados sobre os médicos por 100 mil habitantes













# Porque todo hospital municipal tpgestao == 'M' não tem vinculo com o SUS vinc_sus == 1?
  # Diferença nos números (em 2020 em PV, 8 com vínculo e 21 sem)
  
  # nivate_h é a variável que uso para selecionar os hospitais independente do nível de atendimento (ambulatorial ou ...)
  
  # fazer outros filtros reunindo dados por ano e um left join com uma tabela no final com as colunas por tipo de hospital
  # a importância do município
  


#  Fazer função para hospitais estaduais comparar com os estaduais


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
# VER E-SIC E FAZER PERGUNTA
# gesprg6e  Alta complexidade estadual = 1
# gesprg6m  Alta complexidade municipal = 1
write.csv(x,'Outputs/00_shapes_e_dados/00_cnes_st_2015.csv', row.names = F)


# 2 - Dados de Leitos (2005 - 2020)





Incapacidade do SUS de atender as pessoas


# aumento de acidentes de transito, 
# médicos a cada 100 mil habitantes, leitos de uti a cada 100 mil habitantes


Regic de municípios que vão para PV e Altamira buscar tratamento.
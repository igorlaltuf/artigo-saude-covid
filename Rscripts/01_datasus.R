# Dados de Sa�de P�blica - DataSUS

# carregar scripts
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

# dados populacionais
pop <- read_csv('Temp/populacao_amzl_2001-20.csv') # Popula��o - 2001 - 2020


# Pacote microdatasus
# 1 - Dados de estabelecimentos (2005 - 2020)
# Baixar e limpar os dados
var.cnes.st <- c('CNES','CODUFMUN','COD_CEP','VINC_SUS','TPGESTAO','TP_UNID','TURNO_AT','GESPRG1E', 
                 'GESPRG1M','GESPRG2E','GESPRG2M','GESPRG4E','GESPRG4M','GESPRG5E','GESPRG5M', 
                 'GESPRG6E','GESPRG6M','NIVATE_A','NIVATE_H','URGEMERG')
ano <- 2005 # ano inicial
mes <- 12 # m�s em que os dados ser�o coletados
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

dados.st <- do.call(rbind, lapply(paste0("estabelec.", 2005:2020),get)) %>% 
  janitor::clean_names()

# unique(dados.st$tpgestao)

# Fun��o que retorna os dados: usar o c�digo do munic�pio com apenas com 6 d�gitos
# Fun��o que retorna a evolu��o da quantidade de hospitais municipais e estaduais

Estab.Muni <- function(codigo_municipio){
  for(i in codigo_municipio){
  dados <- dados.st %>% 
    dplyr::filter(codufmun %in% i,
                  nivate_h == 1, # sobre o atendimento: Tem atendimento hospitalar municipal ou estadual? exclui farm�cias etc
                  tpgestao %in% c('M','E','D'), # M, E ou D (dupla) administra��o municipal, estadual ou dupla
                  vinc_sus == 1) %>%  # vinculados ao SUS
    group_by(ano) %>% 
    count()
  
  label.muni <- cidades.brasil.nome[substr(cidades.brasil.nome$cod_muni,1,6) == i,2] # transformar em vetor ver regic script
  label.muni <- label.muni$muni # transforma em vetor
  label.muni <- as.character(str_replace_all(label.muni, "[[:punct:]]","")) # essa vari�vel deve receber o nome da cidade de acordo com o c�digo colocado
  arquivo.hospitais <- paste('Hosp muni e est em ', label.muni,'.png')
  diretorio <- paste0('Outputs/dados por municipio/',label.muni)
  dir.create(diretorio)
  
  # gerar  gr�fico
  grafico.estabelecimentos <- ggplot() +
    geom_bar(dados, mapping = aes(x = ano, y = n), col = '#00a2ed', fill = '#00a2ed', stat = 'identity') +
    ggtitle(paste("Hospitais vinculados ao SUS com gest�o\nmunicipal, estadual ou dupla - ", label.muni)) +
    labs(y = 'Quantidade de hospitais', x = 'Ano', caption = 'Fonte: Elabora��o pr�pria. SALDANHA Et al.(2019).') +
    theme_classic()+
    theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
          plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(breaks = seq(0, max(dados$n), 1))+
    scale_x_continuous(breaks = seq(2005, 2020, 1))
    
  grafico.estabelecimentos
  ggsave(plot = grafico.estabelecimentos, path = diretorio, filename = arquivo.hospitais, width = 9, height = 6)
  
  }}


# amostra <- c(110020,150060) # Lembrar que o c�digo aqui � de 6 d�gitos
# Estab.Muni(amostra)


# 2 - Dados de leitos (2005 - 2020) - valores para todos os leitos, e n�o apenas os leitos de interna��o.

var.cnes.lt <- c('CNES','CODUFMUN','TP_UNID','TP_LEITO','CODLEITO','QT_EXIST','QT_CONTR','QT_SUS','QT_NSUS')
ano <- 2005 # ano inicial
mes <- 12 # m�s em que os dados ser�o coletados
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

dados.lt <- do.call(rbind, lapply(paste0("leito.", 2005:2020),get)) %>%
  janitor::clean_names() %>%
  mutate(codufmun = as.numeric(as.character(codufmun)))

pop$cod_muni <- as.numeric(substr(pop$cod_muni,1,6))

dados.lt <- left_join(dados.lt, pop, by = c('codufmun' = 'cod_muni', 'ano')) %>%  
  select('ano','codufmun','qt_sus','populacao') %>% 
  group_by(ano,codufmun,populacao) %>%   
  summarise(leitos_sus = sum(qt_sus)) %>% 
  mutate(leitos_cada_100_mil_ha = (leitos_sus/populacao)*100000)



Leitos.Muni <- function(codigo_municipio){
  for(i in codigo_municipio){
    dados <- dados.lt %>% 
      dplyr::filter(codufmun %in% i)
    
    label.muni <- cidades.brasil.nome[substr(cidades.brasil.nome$cod_muni,1,6) == i,2] # transformar em vetor ver regic script
    label.muni <- label.muni$muni # transforma em vetor
    label.muni <- as.character(str_replace_all(label.muni, "[[:punct:]]","")) # essa vari�vel deve receber o nome da cidade de acordo com o c�digo colocado
    arquivo.leitos <- paste('Leitos do SUS em ', label.muni,'.png')
    arquivo.leitos.pop <- paste('Leitos do SUS 100 mil hab ', label.muni,'.png')
    diretorio <- paste0('Outputs/dados por municipio/',label.muni)
    dir.create(diretorio)
    
    # gerar gr�fico qtd leitos
    grafico.leitos <- ggplot() +
      geom_bar(dados, mapping = aes(x = ano, y = leitos_sus), col = '#00a2ed', fill = '#00a2ed', stat = 'identity') +
      ggtitle(paste("Evolu��o dos leitos do SUS - ", label.muni)) +
      labs(y = 'Quantidade de leitos', x = 'Ano', caption = 'Fonte: Elabora��o pr�pria. SALDANHA Et al.(2019).') +
      theme_classic()+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
            plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = seq(2005, 2020, 1))
    
    grafico.leitos
    ggsave(plot = grafico.leitos, path = diretorio, filename = arquivo.leitos, width = 9, height = 6)
    
   
    # leitos do sus cada 100 mil hab (independentemente se s�o do munic�pio ou n�o)
    grafico.leitos.pop <- ggplot() +
      geom_bar(dados, mapping = aes(x = ano, y = leitos_cada_100_mil_ha), col = '#00a2ed', fill = '#00a2ed', stat = 'identity') +
      ggtitle(paste("Evolu��o dos leitos do SUS a cada 100 mil habitantes - ", label.muni)) +
      labs(y = 'Quantidade de leitos a\ncada 100 mil habitantes', x = 'Ano', caption = 'Fonte: Elabora��o pr�pria. SALDANHA Et al.(2019).') +
      theme_classic()+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
            plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = seq(2005, 2020, 1))
    
    grafico.leitos.pop
    ggsave(plot = grafico.leitos.pop, path = diretorio, filename = arquivo.leitos.pop, width = 9, height = 6)
    
  }}

#Leitos.Muni(110020)
#debug(Leitos.Muni)


# 3 - Dados de m�dicos do sus (2005 - 2020)
# url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/"

cbo.2002 <- read.csv2('Input/CBO2002 - Ocupacao.csv') %>%   
  janitor::clean_names() %>% 
  dplyr::filter(codigo >= 225103 & # filtrar cargos de médicos independente da especialização
                codigo <= 225355) %>% 
  mutate(codigo = as.character(codigo)) 
  
cbo.1994 <- read.csv2('Input/cbo94-datasus.csv', encoding = 'UTF-8') %>%     
  filter(str_detect(profissao, "M�dico|Medicos")) 

cbo.2007.10 <- read.csv2('Input/cbo2007-10-conv.csv', encoding = 'UTF-8') 


lista.de.arquivos <- list.files(path = "Input/CNES-PF/", recursive = TRUE,
                                pattern = "\\.dbc$", 
                                full.names = TRUE)

# dados do m�s de dezembro para cada ano de 2006 a 2020
# ano <- c(2006:2020)
marc <- c(1:135)
i <- 1
while (i<=length(lista.de.arquivos)) {
  #pop$cod_muni6 <- as.numeric(substr(pop$cod_muni,1,6))
  
  x <- read.dbc::read.dbc(lista.de.arquivos[i]) %>%  
    janitor::clean_names() %>%
    mutate(codufmun = as.numeric(as.character(codufmun)),
           tpgestao = as.character(tpgestao),
           cbo = as.character(cbo),
           prof_sus = as.numeric(as.character(prof_sus)),
           competen = as.character(competen)) %>% 
    mutate(ano = as.numeric(substr(competen,1,4))) %>% 
    select(codufmun, ano, tpgestao, cbo, prof_sus)

  # classifica��o pela cbo94
  ano.2006 <- x %>%
    dplyr::filter(cbo %in% cbo.1994$cod_cbo_94) %>% # filtrar cargos de m�dicos independente da especializa��o
    group_by(codufmun, ano) %>%
    summarise(qtd_med_sus = sum(prof_sus)) %>%
    left_join(pop, by = c('ano', 'codufmun' = 'cod_muni')) %>%    
    mutate(med_sus_100_mil_hab = (qtd_med_sus/populacao)*100000)

  # classifica��o segundo a Tabela de convers�o da classifica��o Brasileira de Ocupa��es 
  # disponibilizada pelo Minist�rio do Trabalho e Emprego (MTE).
  # http://www.sbpc.org.br/upload/noticias_setor/320110927123631.pdf
  anos2007.2010 <- x %>%
    dplyr::filter(cbo %in% cbo.2007.10$cod_antigo) %>% # filtrar cargos de m�dicos independente da especializa��o
    group_by(codufmun, ano) %>% # usar tpgestao aqui para saber os m�dicos do municipio, estado ou duplo
    summarise(qtd_med_sus = sum(prof_sus)) %>%
    left_join(pop, by = c('ano', 'codufmun' = 'cod_muni')) %>%    
    mutate(med_sus_100_mil_hab = (qtd_med_sus/populacao)*100000)
  
  anos2011.2020 <- x %>%
    dplyr::filter(cbo %in% cbo.2002$codigo) %>% # filtrar cargos de m�dicos independente da especializa��o
    group_by(codufmun, ano) %>%
    summarise(qtd_med_sus = sum(prof_sus)) %>%
    left_join(pop, by = c('ano', 'codufmun' = 'cod_muni')) %>%   
    mutate(med_sus_100_mil_hab = (qtd_med_sus/populacao)*100000)

  x <- rbind(ano.2006, anos2007.2010, anos2011.2020)
  
  assign(paste("cnes.pf",marc[i], sep="."), x)
  i <- i + 1
}

dados <- do.call(rbind, lapply(paste0("cnes.pf.",1:135),get))


# m�dicos do sus por munic�pio (estadual, municipal ou dupla)
Medicos.Muni <- function(codigo_municipio){
  for(i in codigo_municipio){
    
    dados.muni <- dados %>% 
      dplyr::filter(codufmun %in% i)
   
    label.muni <- cidades.brasil.nome[substr(cidades.brasil.nome$cod_muni,1,6) == i,2] # transformar em vetor ver regic script
    label.muni <- label.muni$muni # transforma em vetor
    label.muni <- as.character(str_replace_all(label.muni, "[[:punct:]]","")) # essa vari�vel deve receber o nome da cidade de acordo com o c�digo colocado
    arquivo.med <- paste('M�dicos do SUS', label.muni,'.png')
    arquivo.med.pop <- paste('M�dicos do SUS 100 mil hab ', label.muni,'.png')
    diretorio <- paste0('Outputs/dados por municipio/',label.muni)
    dir.create(diretorio)
    
    # qts m�dicos por munic�pio
    grafico.medico <- ggplot() +
      geom_bar(dados.muni, mapping = aes(x = ano, y = qtd_med_sus), col = '#00a2ed', fill = '#00a2ed', stat = 'identity') +
      ggtitle(paste("Evolu��o da quantidade de m�dicos vinculados ao SUS - \n", label.muni)) +
      labs(y = 'Quantidade de m�dicos', x = 'Ano', caption = 'Fonte: Elabora��o pr�pria. DataSUS(2021).') +
      theme_classic()+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
            plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = seq(2006, 2020, 1))
    
    #grafico.medico 
    
    # qts m�dicos a cada 100 mil habitantes por munic�pio
    grafico.med.100.mil <- ggplot() +
      geom_bar(dados.muni, mapping = aes(x = ano, y = med_sus_100_mil_hab), col = '#00a2ed', fill = '#00a2ed', stat = 'identity') +
      ggtitle(paste("Evolu��o da quantidade de m�dicos vinculados \n ao SUS a cada 100 mil habitantes -", label.muni)) +
      labs(y = 'Quantidade de m�dicos a cada\n100 mil habitantes', x = 'Ano', caption = 'Fonte: Elabora��o pr�pria. DataSUS(2021).') +
      theme_classic()+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
            plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = seq(2006, 2020, 1))
    
    #grafico.med.100.mil
    
    # salvar gr�ficos
    ggsave(plot = grafico.medico, path = diretorio, filename = arquivo.med, width = 9, height = 6)
    ggsave(plot = grafico.med.100.mil, path = diretorio, filename = arquivo.med.pop, width = 9, height = 6)
  }
}

# municipios <- c(110020,150060)
# Medicos.Muni(municipios) #cod com 6 d�gitos
# Leitos.Muni(municipios)
# Estab.Muni(municipios)

# Fonte: ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/ colar no windows explorer
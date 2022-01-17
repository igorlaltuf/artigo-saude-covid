# Dados de Saúde Pública - DataSUS

# carregar scripts
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

# dados populacionais
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

dados.st <- do.call(rbind, lapply(paste0("estabelec.", 2005:2020),get)) %>% 
  janitor::clean_names()

# unique(dados.st$tpgestao)

# Função que retorna os dados: usar o código do município com apenas com 6 dígitos
# Função que retorna a evolução da quantidade de hospitais municipais e estaduais

Estab.Muni.Sintese <- function(codigo_municipio){
  contador = 1
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
  label.muni <- as.character(str_replace_all(label.muni, "[[:punct:]]","")) # essa variável deve receber o nome da cidade de acordo com o código colocado
  arquivo.hospitais <- paste('Hosp muni e est em ', label.muni,'.png')
  arquivo.hosp.sintese <- paste('Hosp Munic e Estad.png')
  diretorio <- paste0('Outputs/dados por municipio/')
  dir.create(diretorio)
 
  label.title <- cidades.brasil.nome[substr(cidades.brasil.nome$cod_muni,1,6) == i,2]
 
  
  # gerar  gráfico
  graf.estab <- ggplot() +
    geom_bar(dados, width = 0.8, mapping = aes(x = ano, y = n), col = '#bdd7e7', fill = '#bdd7e7', stat = 'identity') +
    ggtitle(label.title) +
    labs(y = 'Quantidade\nde hospitais', x = 'Ano') +
    theme_classic() +
    theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(2006, 2020, 2))
    # scale_y_continuous(breaks = seq(0, max(dados$n), 1)) 
    
  # ifelse(max(dados$n)>5, assign(breaks.eixo.y,2), assign(breaks.eixo.y,1))
  
  breaks.eixo.y <- if(max(dados$n)>5) 2 else 1
  
  
  graf.estab <- graf.estab +
    scale_y_continuous(breaks = seq(0, max(dados$n), breaks.eixo.y))
  
  assign(paste0("graf.estab", contador), graf.estab)
  contador = contador + 1
  }
  
  graf.sintese <- (graf.estab1|graf.estab2)/
                  (graf.estab3|graf.estab4)/
                  (graf.estab5|graf.estab6)
  
  graf.sintese

  ggsave(plot = graf.sintese, path = diretorio, filename = arquivo.hosp.sintese, width = 7.5, height = 9)
  
  }


 amostra <- c(150553,150215,150420,150550,160040,150530) # Lembrar que o código aqui é de 6 dígitos
 Estab.Muni.Sintese(amostra)


# 2 - Dados de leitos (2005 - 2020) - valores para todos os leitos, e não apenas os leitos de internação.

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

dados.lt <- do.call(rbind, lapply(paste0("leito.", 2005:2020),get)) %>%
  janitor::clean_names() %>%
  mutate(codufmun = as.numeric(as.character(codufmun)))

pop$cod_muni <- as.numeric(substr(pop$cod_muni,1,6))

dados.lt <- left_join(dados.lt, pop, by = c('codufmun' = 'cod_muni', 'ano')) %>% 
  select('ano','codufmun','muni','qt_sus','populacao') %>% 
  group_by(ano,codufmun,muni,populacao) %>%   
  summarise(leitos_sus = sum(qt_sus)) %>% 
  mutate(leitos_cada_100_mil_ha = (leitos_sus/populacao)*100000)


seis.maiores.minerac <- c(150553,150215,150420,150550,160040,150530)

dados.lt.minerac <- dados.lt %>% 
  dplyr::filter(codufmun %in% seis.maiores.minerac)

x <- ggplot() +
     geom_line(data = dados.lt.minerac, aes(x=ano, y=leitos_cada_100_mil_ha, group = muni, colour = muni)) +
     geom_point(data = dados.lt.minerac, aes(x=ano, y=leitos_cada_100_mil_ha, group = muni), size = .8) +
     labs(x = 'Ano', y = "Leitos a cada 100 mil habitantes") +
     scale_x_continuous(breaks = seq(2005, 2020, 3)) +
     scale_y_continuous(breaks = seq(0, max(dados.lt.minerac$leitos_cada_100_mil_ha), 50)) +
     theme_classic() +
     theme(legend.position = "bottom",
           legend.title = element_blank())
  

# salvar
ggsave(plot = x, path = diretorio, filename = 'evol leitos minerac.png', width = 9, height = 6)


seis.maiores.energia <- c(110020,150506,150810,150309,150060,150835)
dados.lt.energia <- dados.lt %>% 
  dplyr::filter(codufmun %in% seis.maiores.energia)

y <- ggplot() +
     geom_line(data = dados.lt.energia, aes(x=ano, y=leitos_cada_100_mil_ha, group = muni, colour = muni)) +
     geom_point(data = dados.lt.energia, aes(x=ano, y=leitos_cada_100_mil_ha, group = muni), size = .8) +
     labs(x = 'Ano', y = "Leitos a cada 100 mil habitantes") +
     scale_x_continuous(breaks = seq(2005, 2020, 3)) +
     scale_y_continuous(breaks = seq(0, max(dados.lt.energia$leitos_cada_100_mil_ha), 50)) +
     theme_classic() +
     theme(legend.position = "bottom",
           legend.title = element_blank())

# salvar
ggsave(plot = y, path = diretorio, filename = 'evol leitos energia.png', width = 9, height = 6)


# 3 - Dados de médicos do sus (2005 - 2020)
# url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/"

cbo.2002 <- read.csv2('Input/CBO2002 - Ocupacao.csv') %>%   
  janitor::clean_names() %>% 
  dplyr::filter(codigo >= 225103 & # filtrar cargos de mÃ©dicos independente da especializaÃ§Ã£o
                codigo <= 225355) %>% 
  mutate(codigo = as.character(codigo)) 
  
cbo.1994 <- read.csv2('Input/cbo94-datasus.csv', encoding = 'UTF-8') %>%     
  filter(str_detect(profissao, "Médico|Medicos")) 

cbo.2007.10 <- read.csv2('Input/cbo2007-10-conv.csv', encoding = 'UTF-8') 


lista.de.arquivos <- list.files(path = "Input/CNES-PF/", recursive = TRUE,
                                pattern = "\\.dbc$", 
                                full.names = TRUE)

# dados do mês de dezembro para cada ano de 2006 a 2020
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

  # classificação pela cbo94
  ano.2006 <- x %>%
    dplyr::filter(cbo %in% cbo.1994$cod_cbo_94) %>% # filtrar cargos de médicos independente da especialização
    group_by(codufmun, ano) %>%
    summarise(qtd_med_sus = sum(prof_sus)) %>%
    left_join(pop, by = c('ano', 'codufmun' = 'cod_muni')) %>%    
    mutate(med_sus_100_mil_hab = (qtd_med_sus/populacao)*100000)

  # classificação segundo a Tabela de conversão da classificação Brasileira de Ocupações 
  # disponibilizada pelo Ministério do Trabalho e Emprego (MTE).
  # http://www.sbpc.org.br/upload/noticias_setor/320110927123631.pdf
  anos2007.2010 <- x %>%
    dplyr::filter(cbo %in% cbo.2007.10$cod_antigo) %>% # filtrar cargos de médicos independente da especialização
    group_by(codufmun, ano) %>% # usar tpgestao aqui para saber os médicos do municipio, estado ou duplo
    summarise(qtd_med_sus = sum(prof_sus)) %>%
    left_join(pop, by = c('ano', 'codufmun' = 'cod_muni')) %>%    
    mutate(med_sus_100_mil_hab = (qtd_med_sus/populacao)*100000)
  
  anos2011.2020 <- x %>%
    dplyr::filter(cbo %in% cbo.2002$codigo) %>% # filtrar cargos de médicos independente da especialização
    group_by(codufmun, ano) %>%
    summarise(qtd_med_sus = sum(prof_sus)) %>%
    left_join(pop, by = c('ano', 'codufmun' = 'cod_muni')) %>%   
    mutate(med_sus_100_mil_hab = (qtd_med_sus/populacao)*100000)

  x <- rbind(ano.2006, anos2007.2010, anos2011.2020)
  
  assign(paste("cnes.pf",marc[i], sep="."), x)
  i <- i + 1
}

dados <- do.call(rbind, lapply(paste0("cnes.pf.", 1:135), get))


dados.prof.energia <- dados %>% 
  dplyr::filter(codufmun %in% seis.maiores.energia)

m <- ggplot() +
  geom_line(data = dados.prof.energia, aes(x=ano, y=med_sus_100_mil_hab, group = muni, colour = muni)) +
  geom_point(data = dados.prof.energia, aes(x=ano, y=med_sus_100_mil_hab, group = muni), size = .8) +
  labs(x = 'Ano', y = "Médicos a cada 100 mil habitantes") +
  scale_x_continuous(breaks = seq(2005, 2020, 3)) +
  scale_y_continuous(breaks = seq(0, max(dados.prof.energia$med_sus_100_mil_hab), 50)) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

m

# salvar
ggsave(plot = m, path = diretorio, filename = 'evol medicos energia.png', width = 9, height = 6)



dados.prof.minerac <- dados %>% 
  dplyr::filter(codufmun %in% seis.maiores.minerac)

n <- ggplot() +
  geom_line(data = dados.prof.minerac, aes(x=ano, y=med_sus_100_mil_hab, group = muni, colour = muni)) +
  geom_point(data = dados.prof.minerac, aes(x=ano, y=med_sus_100_mil_hab, group = muni), size = .8) +
  labs(x = 'Ano', y = "Médicos a cada 100 mil habitantes") +
  scale_x_continuous(breaks = seq(2005, 2020, 3)) +
  scale_y_continuous(breaks = seq(0, max(dados.prof.minerac$med_sus_100_mil_hab), 50)) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

n

# salvar
ggsave(plot = n, path = diretorio, filename = 'evol medicos minerac.png', width = 9, height = 6)




# médicos do sus por município (estadual, municipal ou dupla)
Medicos.Muni <- function(codigo_municipio){
  for(i in codigo_municipio){
    
    dados.muni <- dados %>% 
      dplyr::filter(codufmun %in% i)
   
    label.muni <- cidades.brasil.nome[substr(cidades.brasil.nome$cod_muni,1,6) == i,2] # transformar em vetor ver regic script
    label.muni <- label.muni$muni # transforma em vetor
    label.muni <- as.character(str_replace_all(label.muni, "[[:punct:]]","")) # essa variável deve receber o nome da cidade de acordo com o código colocado
    arquivo.med <- paste('Médicos do SUS', label.muni,'.png')
    arquivo.med.pop <- paste('Médicos do SUS 100 mil hab ', label.muni,'.png')
    diretorio <- paste0('Outputs/dados por municipio/',label.muni)
    dir.create(diretorio)
    
    # qts médicos por município
    grafico.medico <- ggplot() +
      geom_bar(dados.muni, mapping = aes(x = ano, y = qtd_med_sus), col = '#00a2ed', fill = '#00a2ed', stat = 'identity') +
      ggtitle(paste("Evolução da quantidade de médicos vinculados ao SUS - \n", label.muni)) +
      labs(y = 'Quantidade de médicos', x = 'Ano', caption = 'Fonte: Elaboração própria. DataSUS(2021).') +
      theme_classic()+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
            plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = seq(2006, 2020, 1))
    
    #grafico.medico 
    
    # qts médicos a cada 100 mil habitantes por município
    grafico.med.100.mil <- ggplot() +
      geom_bar(dados.muni, mapping = aes(x = ano, y = med_sus_100_mil_hab), col = '#00a2ed', fill = '#00a2ed', stat = 'identity') +
      ggtitle(paste("Evolução da quantidade de médicos vinculados \n ao SUS a cada 100 mil habitantes -", label.muni)) +
      labs(y = 'Quantidade de médicos a cada\n100 mil habitantes', x = 'Ano', caption = 'Fonte: Elaboração própria. DataSUS(2021).') +
      theme_classic()+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
            plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = seq(2006, 2020, 1))
    
    #grafico.med.100.mil
    
    # salvar gráficos
    ggsave(plot = grafico.medico, path = diretorio, filename = arquivo.med, width = 9, height = 6)
    ggsave(plot = grafico.med.100.mil, path = diretorio, filename = arquivo.med.pop, width = 9, height = 6)
  }
}

# municipios <- c(110020,150060)
# Medicos.Muni(municipios) #cod com 6 dígitos
# Leitos.Muni(municipios)
# Estab.Muni(municipios)

# Fonte: ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/ colar no windows explorer
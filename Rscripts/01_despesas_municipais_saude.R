# Despesas municipais com sa�de na Amaz�nia Legal - Despesas empenhadas
# Dados dispon�veis at� 2012 s�o sobre as receitas empenhadas. Por isso devo filtrar apenas essas receitas para os anos posteriores.

source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
options(scipen = 999) # remove nota��o cient�fica

pop <- read_csv('Temp/populacao_amzl_2001-20.csv') # Popula��o - 2001 - 2020
ipca <- read_excel('Input/ipca_indice.xlsx') #igp-di m�dia anual
ipca.2020 <- ipca$media_numero_indice_ipca[27]

# Despesas municipais por fun��o 2004-2018 (pr�-pandemia)
# tabela para unir as duas bases
equi.cod <- read_excel('Input/equivalencia entre codigos.xlsx') %>% 
  janitor::clean_names() %>% 
  unite("codigo_antigo", 5:6,sep = '') %>% 
  select('codigo_antigo','ibge_uf_muni_d','nome_ibge')

# importar arquivos de 2004 at� 2012
lista.de.arquivos <- list.files(path = "Input/despesas municipais 2004-2012/", recursive = TRUE,
                                pattern = "\\.csv$", full.names = TRUE)
ano <- c(2004:2012)
i <- 1
while (i <= length(lista.de.arquivos)) {
  assign(paste("desp", ano[i], sep="."), read_csv2(paste(lista.de.arquivos[i])))
  i <- i + 1
}

desp.2004 <- desp.2004 %>% mutate(ano = 2004) %>% janitor::clean_names() %>% select('cd_uf','cd_mun','nome_caixa','ano','saude','atencao_basica','assistencia_hospitalar','suporte_profilatico','vigilancia_sanitaria','vigilancia_epidemiologica','alimentacao_e_nutricao','demais_subfuncoes_10') %>% rename('municipio' = 'nome_caixa')
desp.2005 <- desp.2005 %>% mutate(ano = 2005) %>% janitor::clean_names() %>% select('cd_uf','cd_mun','nome_caixa','ano','saude','atencao_basica','assistencia_hospitalar','suporte_profilatico','vigilancia_sanitaria','vigilancia_epidemiologica','alimentacao_e_nutricao','demais_subfuncoes_10') %>% rename('municipio' = 'nome_caixa')
desp.2006 <- desp.2006 %>% mutate(ano = 2006) %>% janitor::clean_names() %>% select('cd_uf','cd_mun','nome_caixa','ano','saude','atencao_basica','assistencia_hospitalar','suporte_profilatico','vigilancia_sanitaria','vigilancia_epidemiologica','alimentacao_e_nutricao','demais_subfuncoes_10') %>% rename('municipio' = 'nome_caixa')
desp.2007 <- desp.2007 %>% mutate(ano = 2007) %>% janitor::clean_names() %>% select('cd_uf','cd_mun','nome_caixa','ano','saude','atencao_basica','assistencia_hospitalar','suporte_profilatico','vigilancia_sanitaria','vigilancia_epidemiologica','alimentacao_e_nutricao','demais_subfuncoes_10') %>% rename('municipio' = 'nome_caixa')
desp.2008 <- desp.2008 %>% mutate(ano = 2008) %>% janitor::clean_names() %>% select('cd_uf','cd_mun','nome_caixa','ano','saude','atencao_basica','assistencia_hospitalar','suporte_profilatico','vigilancia_sanitaria','vigilancia_epidemiologica','alimentacao_e_nutricao','demais_subfuncoes_10') %>% rename('municipio' = 'nome_caixa')
desp.2009 <- desp.2009 %>% mutate(ano = 2009) %>% janitor::clean_names() %>% select('cd_uf','cd_mun','nome_caixa','ano','saude','atencao_basica','assistencia_hospitalar','suporte_profilatico','vigilancia_sanitaria','vigilancia_epidemiologica','alimentacao_e_nutricao','demais_subfuncoes_10') %>% rename('municipio' = 'nome_caixa')
desp.2010 <- desp.2010 %>% mutate(ano = 2010) %>% janitor::clean_names() %>% select('cd_uf','cd_mun','municipio','ano','saude','atencao_basica','assistencia_hospitalar','suporte_profilatico','vigilancia_sanitaria','vigilancia_epidemiologica','alimentacao_e_nutricao','demais_subfuncoes_10') 
desp.2011 <- desp.2011 %>% mutate(ano = 2011) %>% janitor::clean_names() %>% select('cd_uf','cd_mun','municipio','ano','saude','atencao_basica','assistencia_hospitalar','suporte_profilatico','vigilancia_sanitaria','vigilancia_epidemiologica','alimentacao_e_nutricao','demais_subfuncoes_10')
desp.2012 <- desp.2012 %>% mutate(ano = 2012) %>% janitor::clean_names() %>% select('cd_uf','cd_mun','municipio','ano','saude','atencao_basica','assistencia_hospitalar','suporte_profilatico','vigilancia_sanitaria','vigilancia_epidemiologica','alimentacao_e_nutricao','demais_subfuncoes_10')

dados.antigos <- rbind(desp.2004,desp.2005,desp.2006,desp.2007,desp.2008,desp.2009,desp.2010,desp.2011,desp.2012) %>% 
  pivot_longer(cols = 5:12, names_to = 'despesa', values_to = 'valor') %>% 
  unite("codigo_antigo", 1:2,sep = '') %>% 
  left_join(equi.cod) %>% 
  select('nome_ibge','ibge_uf_muni_d','ano','despesa','valor') %>% 
  rename('cod_ibge' = 'ibge_uf_muni_d') %>% 
  dplyr::filter(cod_ibge %in% cidades.amazonia.legal)

# importar arquivos de 2013 até 2018
lista.de.arquivos <- list.files(path = "Input/despesas municipais 2013-2018/", recursive = TRUE,
                                pattern = "\\.csv$", full.names = TRUE)
ano <- c(2013:2018)
i <- 1
while (i <= length(lista.de.arquivos)) {
  assign(paste("desp", ano[i], sep="."), read_csv2(paste(lista.de.arquivos[i]), skip = 4))
  i <- i + 1
}

# incluir ano no dados
desp.2013 <- desp.2013 %>% mutate(ano = 2013)
desp.2014 <- desp.2014 %>% mutate(ano = 2014)
desp.2015 <- desp.2015 %>% mutate(ano = 2015)
desp.2016 <- desp.2016 %>% mutate(ano = 2016)
desp.2017 <- desp.2017 %>% mutate(ano = 2017)
desp.2018 <- desp.2018 %>% mutate(ano = 2018)

dados.novos <- rbind(desp.2013,desp.2014,desp.2015,desp.2016,desp.2017,desp.2018) %>% 
  janitor::clean_names() %>% 
  dplyr::filter(coluna %in% 'Despesas Empenhadas', # assim como os dados das despesas anteriores � 2013
                conta %in% c('10 - Sa�de',
                             '10.301 - Aten��o B�sica',
                             '10.302 - Assist�ncia Hospitalar e Ambulatorial',
                             '10.303 - Suporte Profil�tico e Terapeutico',
                             '10.304 - Vigil�ncia Sanit�ria',
                             '10.305 - Vigil�ncia Epidemiol�gica',
                             '10.306 - Alimenta��o e Nutri��o',
                             '10.122 - Administra��o Geral',
                             'FU10 - Demais Subfun��es')) %>% 
  left_join(equi.cod,by = c('cod_ibge' = 'ibge_uf_muni_d')) %>% 
  select('nome_ibge','cod_ibge','ano','conta','valor') %>% 
  rename('despesa' = 'conta')

base.saude <- rbind(dados.antigos, dados.novos) %>% 
  mutate(despesa = replace(despesa, 
                           despesa == c('saude',
                                        'atencao_basica','assistencia_hospitalar',
                                        'suporte_profilatico','vigilancia_sanitaria',
                                        'vigilancia_epidemiologica','alimentacao_e_nutricao',
                                        'demais_subfuncoes_10'),
                           c('10 - Sa�de',
                             '10.301 - Aten��o B�sica',
                             '10.302 - Assist�ncia Hospitalar e Ambulatorial',
                             '10.303 - Suporte Profil�tico e Terapeutico',
                             '10.304 - Vigil�ncia Sanit�ria',
                             '10.305 - Vigil�ncia Epidemiol�gica',
                             '10.306 - Alimenta��o e Nutri��o',
                             'FU10 - Demais Subfun��es'))) %>% 
  left_join(ipca, by = 'ano') %>% 
  left_join(pop, by = c('ano', 'cod_ibge'='cod_muni')) %>% 
  rename('valor_nominal' = 'valor') %>% 
  mutate(valor_nominal_per_capita = valor_nominal/populacao,
         valor_real = valor_nominal*ipca.2020/media_numero_indice_ipca,
         valor_real_per_capita = valor_real/populacao) %>% 
  select(1:4,9,5,10,11,12)

# salvar o arquivo
write.csv2(base.saude,'Temp/base_muni_saude_amzl.csv', row.names = F)

# fun��o que consulta dados de despesas municipais na sa�de
Saude.Muni <- function(codigo_municipio, tipo_despesa){
  
  for(i in codigo_municipio) {
  
  if(tipo_despesa == '10 - Sa�de') {label.desp <- 'sa�de'}
  if(tipo_despesa == '10.301 - Aten��o B�sica') {label.desp <- 'aten��o b�sica'}
  if(tipo_despesa == '10.302 - Assist�ncia Hospitalar e Ambulatorial') {label.desp <- 'assist�ncia hospitalar e ambulatorial'}
  if(tipo_despesa == '10.303 - Suporte Profil�tico e Terapeutico') {label.desp <- 'suporte profil�tico e terapeutico'}
  if(tipo_despesa == '10.304 - Vigil�ncia Sanit�ria') {label.desp <- 'vigil�ncia sanit�ria'}
  if(tipo_despesa == '10.305 - Vigil�ncia Epidemiol�gica') {label.desp <- 'vigil�ncia epidemiol�gica'}
  if(tipo_despesa == '10.306 - Alimenta��o e Nutri��o') {label.desp <- 'alimenta��o e nutri��o'}
  if(tipo_despesa == 'FU10 - Demais Subfun��es') {label.desp <- 'demais subfun��es de sa�de'}
  
  label.muni <- cidades.brasil.nome %>% 
    dplyr::filter(cod_muni %in% i) 
  label.muni <- cidades.brasil.nome[cidades.brasil.nome$cod_muni == i,2] # transformar em vetor ver regic script
  label.muni <- label.muni$muni # transforma em vetor
  label.muni <- as.character(str_replace_all(label.muni,"[[:punct:]]","")) # essa vari�vel deve receber o nome da cidade de acordo com o c�digo colocado
  
  arquivo.graf.nom <- paste('Gr�fico desp nominal de',label.muni,'com',label.desp,'.png')
  arquivo.graf.real <- paste('Gr�fico desp real de',label.muni,'com',label.desp,'.png')
  arquivo.graf.cap <- paste('Gr�ficoo desp real per capita de',label.muni,'com',label.desp,'.png')
  arquivo.tabela <- paste('Tabela despesas de',label.muni,'.png')
  arquivo.csv <- paste('despesas de ', label.muni,'.csv', sep = '')
  diretorio <- paste0('Outputs/dados por municipio/',label.muni)
  dir.create(diretorio)

  dados <- base.saude %>% 
    dplyr::filter(cod_ibge %in% i,
                  despesa %in% tipo_despesa)  
  
  # Gr�fico do valor nominal
  graf.nominal <- ggplot(dados, aes(x=ano, y=valor_nominal/1000000)) +
    geom_line() +
    labs(x = 'Ano', y = 'Valor Nominal',
         title = paste('Evolu��o do valor empenhado das despesas municipais\ncom',label.desp,
                       'em milh�es (R$)', 'no munic�pio de', label.muni)) +
    scale_x_continuous(breaks = seq(2004, 2020, 2)) + 
    theme_classic()
   
  # Gr�fico do valor real
  graf.real <- ggplot(dados, aes(x=ano, y=valor_real/1000000)) +
    geom_line() +
    labs(x = 'Ano', y = 'Valor Real',
         title = paste('Evolu��o do valor empenhado das despesas municipais\ncom',label.desp,
                       'em milh�es (R$ em valores de 2020)\n', 'no munic�pio de', label.muni)) +
    scale_x_continuous(breaks = seq(2004, 2020, 2)) + 
    theme_classic()

  # Gr�fico do valor real per capita
  graf.real.cap <- ggplot(dados, aes(x=ano, y=valor_real_per_capita)) +
    geom_line() +
    labs(x = 'Ano', y = 'Valor Real per Capita',
         title = paste('Evolu��o do valor empenhado das despesas municipais per capita\ncom',label.desp,
                       '\n(R$ em valores de 2020) no munic�pio de', label.muni)) +
    scale_x_continuous(breaks = seq(2004, 2020, 2)) + 
    theme_classic()
  
  # gerar tabela
  tabela.despesas <- gt(dados) %>%
    cols_label(
      nome_ibge = 'Munic�pio',
      ano = 'Ano',
      populacao = 'Popula��o',
      valor_nominal = 'Valor Nominal',
      valor_real = 'Valor Real',
      valor_real_per_capita = 'Valor Real per Capita'
    ) %>% 
    cols_hide(
      columns = c('cod_ibge','despesa','nome_ibge','valor_nominal_per_capita')
    ) %>% 
    tab_header(
      title = paste("Evolu��o das despesas empenhadas com", label.desp ,"no munic�pio de",label.muni),
      subtitle = 'valores em R$ entre 2004 e 2020.'
    ) %>%
    fmt_markdown(
      columns = c(nome_ibge,ano)
    ) %>% 
    fmt_number(
      columns = c(populacao, valor_nominal, valor_real, valor_real_per_capita),
      decimals = 0,
      sep_mark = '.',
      dec_mark = ',',
    ) %>% 
    cols_align(
      align = 'center'
    ) %>% 
    tab_source_note('Fonte: Elabora��o pr�pria. Tesouro Nacional (2021). Valor real deflacionado pela m�dia do IPCA de 2020.')
  
  tabela.despesas
  
  # Salvar os gr�ficos
  ggsave(plot = graf.nominal, path = diretorio, filename = arquivo.graf.nom, width = 9, height = 6)
  ggsave(plot = graf.real, path = diretorio, filename = arquivo.graf.real, width = 9, height = 6)
  ggsave(plot = graf.real.cap, path = diretorio, filename = arquivo.graf.cap, width = 9, height = 6)
  
  # salvar a tabela
  gtsave(data = tabela.despesas, path = diretorio, filename = arquivo.tabela)

  # salvar csv com dados temp
  write.csv2(x = dados, file = paste('Temp/',arquivo.csv,sep = ''), row.names = F)
  
  }
}


 # amostra <- c(1100205,1500602)
 # Saude.Muni(amostra,'10 - Sa�de')


# Fonte anterior a 2013
# https://www.tesourotransparente.gov.br/publicacoes/finbra-dados-contabeis-dos-municipios-1989-a-2012/2012/26
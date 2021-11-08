# Despesas municipais com saúde na Amazônia Legal - Despesas empenhadas
# Dados disponíveis até 2012 são sobre as receitas empenhadas. Por isso devo filtrar apenas essas receitas para os anos posteriores.

source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
options(scipen = 999) # remove notação científica

pop <- read_csv('Temp/populacao_amzl_2001-20.csv') # População - 2001 - 2020
ipca <- read_excel('Input/ipca_indice.xlsx') #igp-di média anual
ipca.2020 <- ipca$media_numero_indice_ipca[27]

# Despesas municipais por função 2004-2018 (pré-pandemia)
# tabela para unir as duas bases
equi.cod <- read_excel('Input/equivalencia entre codigos.xlsx') %>% 
  janitor::clean_names() %>% 
  unite("codigo_antigo", 5:6,sep = '') %>% 
  select('codigo_antigo','ibge_uf_muni_d','nome_ibge')

# importar arquivos de 2004 até 2012
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
  dplyr::filter(coluna %in% 'Despesas Empenhadas', # assim como os dados das despesas anteriores à 2013
                conta %in% c('10 - Saúde','10.301 - Atenção Básica','10.302 - Assistência Hospitalar e Ambulatorial',
                             '10.303 - Suporte Profilático e Terapêutico','10.304 - Vigilância Sanitária',
                             '10.305 - Vigilância Epidemiológica','10.306 - Alimentação e Nutrição',
                             '10.122 - Administração Geral','FU10 - Demais Subfunções')) %>% 
  left_join(equi.cod,by = c('cod_ibge' = 'ibge_uf_muni_d')) %>% 
  select('nome_ibge','cod_ibge','ano','conta','valor') %>% 
  rename('despesa' = 'conta')

base.saude <- rbind(dados.antigos, dados.novos) %>% 
  mutate(despesa = replace(despesa, 
                           despesa == c('saude','atencao_basica','assistencia_hospitalar',
                                        'suporte_profilatico','vigilancia_sanitaria',
                                        'vigilancia_epidemiologica','alimentacao_e_nutricao',
                                        'demais_subfuncoes_10'),
                           c('10 - Saúde', '10.301 - Atenção Básica','10.302 - Assistência Hospitalar e Ambulatorial',
                             '10.303 - Suporte Profilático e Terapêutico','10.304 - Vigilância Sanitária',
                             '10.305 - Vigilância Epidemiológica','10.306 - Alimentação e Nutrição',
                             'FU10 - Demais Subfunções'))) %>% 
  left_join(ipca, by = 'ano') %>% 
  left_join(pop, by = c('ano', 'cod_ibge'='cod_muni')) %>% 
  rename('valor_nominal' = 'valor') %>% 
  mutate(valor_nominal_per_capita = valor_nominal/populacao,
         valor_real = valor_nominal*ipca.2020/media_numero_indice_ipca,
         valor_real_per_capita = valor_real/populacao) %>% 
  select(1:4,9,5,10,11,12)

# salvar o arquivo
write.csv2(base.saude,'Temp/base_muni_saude_amzl.csv', row.names = F)

# função que consulta dados de despesas municipais na saúde
Saude.Muni <- function(codigo_municipio, tipo_despesa){
  
  for(i in codigo_municipio) {
  
  if(tipo_despesa == '10 - Saúde') {label.desp <- 'saúde'}
  if(tipo_despesa == '10.301 - Atenção Básica') {label.desp <- 'atenção básica'}
  if(tipo_despesa == '10.302 - Assistência Hospitalar e Ambulatorial') {label.desp <- 'assistência hospitalar e ambulatorial'}
  if(tipo_despesa == '10.303 - Suporte Profilático e Terapêutico') {label.desp <- 'suporte profilático e terapêutico'}
  if(tipo_despesa == '10.304 - Vigilância Sanitária') {label.desp <- 'vigilância sanitária'}
  if(tipo_despesa == '10.305 - Vigilância Epidemiológica') {label.desp <- 'vigilância epidemiológica'}
  if(tipo_despesa == '10.306 - Alimentação e Nutrição') {label.desp <- 'alimentação e nutrição'}
  if(tipo_despesa == 'FU10 - Demais Subfunções') {label.desp <- 'demais subfunções de saúde'}
  
  label.muni <- cidades.brasil.nome %>% 
    dplyr::filter(cod_muni %in% i) 
  label.muni <- cidades.brasil.nome[cidades.brasil.nome$cod_muni == i,2] # transformar em vetor ver regic script
  label.muni <- label.muni$muni # transforma em vetor
  label.muni <- as.character(str_replace_all(label.muni,"[[:punct:]]","")) # essa vari?vel deve receber o nome da cidade de acordo com o c?digo colocado
  
  arquivo.graf.nom <- paste('Gráfico desp nominal de',label.muni,'com',label.desp,'.png')
  arquivo.graf.real <- paste('Gráfico desp real de',label.muni,'com',label.desp,'.png')
  arquivo.graf.cap <- paste('Gráfico desp real per capita de',label.muni,'com',label.desp,'.png')
  arquivo.tabela <- paste('Tabela despesas de',label.muni,'.png')
  arquivo.csv <- paste('despesas de ', label.muni,'.csv', sep = '')
  diretorio <- paste0('Outputs/dados por municipio/',label.muni)
  dir.create(diretorio)

  dados <- base.saude %>% 
    dplyr::filter(cod_ibge %in% i,
                  despesa %in% tipo_despesa)  
  
  # gráfico do valor nominal
  graf.nominal <- ggplot(dados, aes(x=ano, y=valor_nominal/1000000)) +
    geom_line() +
    labs(x = 'Ano', y = 'Valor Nominal',
         title = paste('Evolução do valor empenhado das despesas municipais\ncom',label.desp,
                       'em milhões (R$)', 'no município de', label.muni)) +
    scale_x_continuous(breaks = seq(2004, 2020, 2)) + 
    theme_classic()
   
  # gráfico do valor real
  graf.real <- ggplot(dados, aes(x=ano, y=valor_real/1000000)) +
    geom_line() +
    labs(x = 'Ano', y = 'Valor Real',
         title = paste('Evolução do valor empenhado das despesas municipais\ncom',label.desp,
                       'em milhões (R$ em valores de 2020)\n', 'no município de', label.muni)) +
    scale_x_continuous(breaks = seq(2004, 2020, 2)) + 
    theme_classic()

  # gráfico do valor real per capita
  graf.real.cap <- ggplot(dados, aes(x=ano, y=valor_real_per_capita)) +
    geom_line() +
    labs(x = 'Ano', y = 'Valor Real per Capita',
         title = paste('Evolução do valor empenhado das despesas municipais per capita\ncom',label.desp,
                       '\n(R$ em valores de 2020) no município de', label.muni)) +
    scale_x_continuous(breaks = seq(2004, 2020, 2)) + 
    theme_classic()
  
  # gerar tabela
  tabela.despesas <- gt(dados) %>%
    cols_label(
      nome_ibge = 'Município',
      ano = 'Ano',
      populacao = 'População',
      valor_nominal = 'Valor Nominal',
      valor_real = 'Valor Real',
      valor_real_per_capita = 'Valor Real per Capita'
    ) %>% 
    cols_hide(
      columns = c('cod_ibge','despesa','nome_ibge','valor_nominal_per_capita')
    ) %>% 
    tab_header(
      title = paste("Evolução das despesas empenhadas com", label.desp ,"no município de",label.muni),
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
    tab_source_note('Fonte: Elaboração própria. Tesouro Nacional (2021). Valor real deflacionado pela média do IPCA de 2020.')
  
  tabela.despesas
  
  # Salvar os gráficos
  ggsave(plot = graf.nominal, path = diretorio, filename = arquivo.graf.nom, width = 9, height = 6)
  ggsave(plot = graf.real, path = diretorio, filename = arquivo.graf.real, width = 9, height = 6)
  ggsave(plot = graf.real.cap, path = diretorio, filename = arquivo.graf.cap, width = 9, height = 6)
  
  # salvar a tabela
  gtsave(data = tabela.despesas, path = diretorio, filename = arquivo.tabela)

  # salvar csv com dados temp
  write.csv2(x = dados, file = paste('Temp/',arquivo.csv,sep = ''), row.names = F)
  
  }
}


# falta apenas melhorar o alinhamento de títulos, fontes etc
# amostra <- c(1100205,1500602)
# Saude.Muni(amostra,'10 - Saúde')

# estabelecer metas para essa semana (DATASUS, receitas? etc)
# filtrar a AMZL quando fizer a correlação, remover zeros
# juntar resultado daqui com dos royalties e calcular regressão

# https://people.duke.edu/~rnau/411seas.htm
# Continuar daqui: criar função para automatizar


# os gastos com saúde aumentaram, mesmo verificando per capita e deflacionando. Isso teria sido geral? (ver mais abaixo)
# e para essas contas?
# 10.302 - Assistência Hospitalar e Ambulatorial
# 10.305 - Vigilância Epidemiológica

# evolução do gasto em saúde sobre o total da arrecadação do município (receita tributária)

# pegar um ano e classificar a proporção do gasto com saúde sobre a receita tributária, classificar e comparar com outras cidades
# classificar com cut()
# Usar como exemplo:
# https://www.r-graph-gallery.com/web-line-chart-with-labels-at-end-of-line.html


# ver como fazer isso de forma paralela usando o processador
# Importante: os dados de saúde não incluem gastos com pessoal até 2012
# 
# Fonte anterior a 2013
# https://www.tesourotransparente.gov.br/publicacoes/finbra-dados-contabeis-dos-municipios-1989-a-2012/2012/26
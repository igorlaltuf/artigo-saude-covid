# Despesas municipais com saúde na Amazônia Legal - Despesas empenhadas
# Dados disponíveis até 2012 são sobre as receitas empenhadas. Por isso devo filtrar apenas essas receitas para os anos posteriores.

rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
options(scipen = 999) # remove notação científica

pop <- read_csv('Temp/populacao_amzl_2001-20.csv') # População - 2001 - 2020
ipca <- read_excel('Input/ipca_indice.xlsx') #igp-di média anual

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
  dplyr::filter(cod_ibge %in% cidades.amazonia.legal,
                coluna %in% 'Despesas Empenhadas', # assim como os dados das despesas anteriores à 2013
                conta %in% c('10 - Saúde','10.301 - Atenção Básica','10.302 - Assistência Hospitalar e Ambulatorial',
                             '10.303 - Suporte Profilático e Terapêutico','10.304 - Vigilância Sanitária',
                             '10.305 - Vigilância Epidemiológica','10.306 - Alimentação e Nutrição',
                             '10.122 - Administração Geral','FU10 - Demais Subfunções')) %>% 
  left_join(equi.cod,by = c('cod_ibge' = 'ibge_uf_muni_d')) %>% 
  select('nome_ibge','cod_ibge','ano','conta','valor') %>% 
  rename('despesa' = 'conta')


base.saude.amzl <- rbind(dados.antigos, dados.novos) %>% 
  mutate(despesa = replace(despesa, 
                           despesa == c('saude','atencao_basica','assistencia_hospitalar',
                                        'suporte_profilatico','vigilancia_sanitaria',
                                        'vigilancia_epidemiologica','alimentacao_e_nutricao',
                                        'demais_subfuncoes_10'),
                           c('10 - Saúde', '10.301 - Atenção Básica','10.302 - Assistência Hospitalar e Ambulatorial',
                             '10.303 - Suporte Profilático e Terapêutico','10.304 - Vigilância Sanitária',
                             '10.305 - Vigilância Epidemiológica','10.306 - Alimentação e Nutrição',
                             'FU10 - Demais Subfunções')))

# salvar o arquivo
write.csv2(base.saude.amzl,'Temp/base_muni_saude_amzl.csv', row.names = F)


# Continuar daqui: criar função para automatizar
# revisar ipca abaixo

# Porto Velho (RO)
pv.pop <- pop %>% 
  dplyr::filter(cod_muni %in% 1100205)

ipca.2020 <- ipca$media_numero_indice_ipca[27]

pv <- base.saude.amzl %>% 
  dplyr::filter(cod_ibge %in% 1100205,
                despesa %in% '10 - Saúde') %>% 
  left_join(pv.pop) %>% 
  left_join(ipca) %>% 
  mutate(valor_per_cap_n = valor/populacao,
         valor_real = valor*ipca.2020/igp_di_media_anual,
         valor_real_per_cap = valor_real/populacao) 
         

ggplot(pv, aes(x=ano, y=valor/1000000)) +
  geom_line(col = 'blue') +
  labs(x = 'ano', y = 'Despesas municipais empenhadas com saúde\n em milhões (R$)')

ggplot(pv, aes(x=ano, y=valor_per_cap_n)) +
  geom_line(col = 'blue') +
  labs(x = 'ano', y = 'Despesa municipal empenhada per capita \n com saúde (R$)')

ggplot(pv, aes(x=ano, y=valor_real/1000000)) +
  geom_line(col = 'blue') +
  labs(x = 'ano', y = 'Despesas municipais empenhadas com saúde\n em milhões (R$) - valores de 2020')

ggplot(pv, aes(x=ano, y=valor_real_per_cap)) +
  geom_line(col = 'blue') +
  labs(x = 'ano', y = 'Despesa municipal empenhada per capita \n com saúde (R$) - valores de 2020')



# os gastos com saúde aumentaram, mesmo verificando per capita e deflacionando. Isso teria sido geral? (ver mais abaixo)
# e para essas contas?
# 10.302 - Assistência Hospitalar e Ambulatorial
# 10.305 - Vigilância Epidemiológica

# evolução do gasto em saúde sobre o total da arrecadação do município (receita tributária)

# pegar um ano e classificar a proporção do gasto com saúde sobre a receita tributária, classificar e comparar com outras cidades
# classificar com cut()



# ver como fazer isso de forma paralela usando o processador
# Importante: os dados de saúde não incluem gastos com pessoal até 2012
# 
# Fonte anterior a 2013
# https://www.tesourotransparente.gov.br/publicacoes/finbra-dados-contabeis-dos-municipios-1989-a-2012/2012/26



# Receitas via transferências da União - Royalties
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

options(scipen = 999) # remove notação científica
igpdi <- read_excel('Input/IGP-DI.xlsx')
igpdi.2020 <- igpdi$igp_di_media_anual[26]

# Royalties de mineração, energia e petróleo (exclui FEP) de 1990 até 2020
royalties <- read_csv2('Input/transferencias_para_municípios_1990_2020_utf8.csv') %>% 
  janitor::clean_names() %>% 
  mutate(valor_consolidado = as.numeric(gsub('[[:punct:] ]+','', substr(valor_consolidado, 3, 100)))/100) %>%  # remove o R$, ponto e vírgula da string, converte em número e divide por 100 p/ incluir decimal
  dplyr::filter(codigo_ibge %in% cidades.amazonia.legal,
                !transferencia %in% c('Royalties - FEP', 'Royalties - PEA')) %>%  # exclui estas duas categorias
  left_join(igpdi) %>% 
  mutate(valor_real = valor_consolidado * igpdi.2020 / igp_di_media_anual) %>% # deflacionar
  select(5,1,2,3,4,8,7) 

unique(royalties$transferencia) # tipos de royalties

# função que gera gráficos dos Royalties
Dados.Royalties <- function(codigo_municipio, tipo) {
  
  dados <- royalties %>% 
    dplyr::filter(codigo_ibge %in% codigo_municipio,
                  transferencia %in% tipo)
  
if (!nrow(dados) > 0) return (paste("Erro: nenhum resultado para code:", codigo_municipio,"ou tipo:", tipo))
  # gerar gráfico
x <- ggplot() +
    geom_line(dados, mapping = aes(x = ano, y = valor_consolidado / em_milhoes), col = 'blue') +
    geom_line(dados, mapping = aes(x = ano, y = valor_real / em_milhoes), col = 'red') 

# melhorar estética dos gráficos

 # criar pasta com o nome do município 

 # salvar o mapa na pasta criada


# colocar nome do município e tipo de royaltie no título da tabela e remover a coluna dele
# Tabela
tabela.covid <- gt(dados) %>%
  cols_label(
    municipio = 'Município',
    ano = 'Ano',
    valor_consolidado = 'Valor Nominal',
    valor_real = 'Valor Real (2020)'
  ) %>% 
  cols_hide(
    columns = c('codigo_ibge','transferencia','igp_di_media_anual')
  ) %>% 
  tab_header(
    title = 'Royalties recebidos pelo município',
    subtitle = '2000-2020'
  ) %>%
  fmt_markdown(
    columns = c(municipio,ano)
  ) %>% 
  fmt_number(
    columns = c(valor_consolidado,valor_real),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elaboração própria. Tesouro Nacional (2021).')

# salvar a tabela na pasta criada
# return(tabela.covid)
# return(x) 
}

# testar função (passar mais de um os município)
Dados.Royalties(1100205,'Royalties - CFH')









# Análises de casos: 
# Porto Velho (RO) - Royalties CFH
pv.royalties <- royalties %>% 
  dplyr::filter(codigo_ibge == 1100205,
                transferencia == 'Royalties - CFH') %>% 
  left_join(igpdi) %>% 
  mutate(valor_real = valor_consolidado*igpdi.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(pv.royalties, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(pv.royalties, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')


# Altamira (PA) - Royalties CFH
alta <- royalties %>% 
  dplyr::filter(codigo_ibge == 1500602,
                transferencia == 'Royalties - CFH') %>% 
  left_join(igpdi) %>% 
  mutate(valor_real = valor_consolidado*igpdi.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(alta, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(alta, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')
  

# Cidades da Mineração (CFEM)



# Marabá (PA)
mara.royalties <- royalties %>% 
  dplyr::filter(codigo_ibge == 1504208,
                transferencia == 'Royalties - CFM') %>% 
  left_join(igpdi) %>% 
  mutate(valor_real = valor_consolidado*igpdi.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(mara.royalties, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(mara.royalties, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')


# Parauapebas (PA)
parau.royalties <- royalties %>% 
  dplyr::filter(codigo_ibge == 1505536,
                transferencia == 'Royalties - CFM') %>% 
  left_join(igpdi) %>% 
  mutate(valor_real = valor_consolidado*igpdi.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(parau.royalties, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(parau.royalties, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')


# Canaã dos Carajás (PA)
parau.canaa <- royalties %>% 
  dplyr::filter(codigo_ibge == 1502152,
                transferencia == 'Royalties - CFM') %>% 
  left_join(igpdi) %>% 
  mutate(valor_real = valor_consolidado*igpdi.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(parau.canaa, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(parau.canaa, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')


# Canaã dos Carajás (PA)
parau.canaa <- royalties %>% 
  dplyr::filter(codigo_ibge == 1502152,
                transferencia == 'Royalties - CFM') %>% 
  left_join(igpdi) %>% 
  mutate(valor_real = valor_consolidado*igpdi.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(parau.canaa, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(parau.canaa, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')





, Oriximiná (PA), Juruti (PA), Manaus (AM), Ourilândia do Norte (PA) e Itaituba (PA).




# deflacionar os valores dos Royalties
# continuar daqui (analisar dados, ou melhor, a evolução das cidades)
# cidades da mineração que foram escolhidas.


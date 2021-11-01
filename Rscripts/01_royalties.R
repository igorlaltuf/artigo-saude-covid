# Receitas via transferências da União - Royalties
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

options(scipen = 999) # remove notação científica
ipca <- read_excel('Input/ipca_indice.xlsx')
ipca.2020 <- ipca$media_numero_indice_ipca[27]

# Royalties de mineração, energia e petróleo (exclui FEP) de 1990 até 2020
royalties <- read_csv2('Input/transferencias_para_municípios_1990_2020_utf8.csv') %>% 
  janitor::clean_names() %>% 
  mutate(valor_consolidado = as.numeric(gsub('[[:punct:] ]+','', substr(valor_consolidado, 3, 100)))/100) %>%  # remove o R$, ponto e vírgula da string, converte em número e divide por 100 p/ incluir decimal
  dplyr::filter(codigo_ibge %in% cidades.amazonia.legal,
                !transferencia %in% c('Royalties - FEP', 'Royalties - PEA')) %>%  # exclui estas duas categorias
  left_join(ipca) %>% 
  mutate(valor_real = valor_consolidado * ipca.2020 / media_numero_indice_ipca) %>% # deflacionar
  select(5,1,2,3,4,8,7) 

unique(royalties$transferencia) # tipos de royalties

# função que gera gráficos dos Royalties
Dados.Royalties <- function(codigo_municipio, tipo) {
  
  dados <- royalties %>% 
    dplyr::filter(codigo_ibge %in% codigo_municipio,
                  transferencia %in% tipo)
# check da função
if (!nrow(dados) > 0) return (paste("Erro: nenhum resultado para o município código", codigo_municipio,"ou valores referentes aos", tipo))

# automatizar criação de diretórios e títulos do gráfico e da tabela
  label.muni <- cidades.brasil.nome %>% 
    dplyr::filter(cod_muni %in% codigo_municipio) 
  
  label.muni <- cidades.brasil.nome[cidades.brasil.nome$cod_muni == codigo_municipio,2] # transformar em vetor ver regic script
  label.muni <- label.muni$muni # transforma em vetor
  label.muni <- as.character(str_replace_all(label.muni,"[[:punct:]]","")) # essa vari?vel deve receber o nome da cidade de acordo com o c?digo colocado
  titulo.roy <- ifelse(tipo == "Royalties - CFM", "da mineração",
                       ifelse(tipo == "Royalties - ANP", "do petróleo",
                              ifelse(tipo == "Royalties - CFH", "da geração de energia elétrica", "ERRO")))    
  arquivo.grafico <- paste('Gráfico - royalties',titulo.roy,'de',label.muni,'.png')
  arquivo.tabela <- paste('Tabela - royalties',titulo.roy,'de',label.muni,'.png')
  diretorio <- paste0('Outputs/dados por municipio/',label.muni)
  dir.create(diretorio)

# gerar gráfico
grafico.royalties <- ggplot() +
  geom_line(dados, mapping = aes(x = ano, y = valor_consolidado / em_milhoes), col = 'blue') +
  geom_line(dados, mapping = aes(x = ano, y = valor_real / em_milhoes), col = 'red') +
  ggtitle(paste("Royalties", titulo.roy ,"recebidos pela prefeitura de",label.muni)) +
  
  labs(y = 'Valor dos Royalties (em R$ milhões)', x = 'Ano', caption = 'Fonte: Elaboração própria. Tesouro Nacional (2021).  Valor real deflacionado pela média do IPCA de 2020.') +
  theme_classic()+
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title = element_text(hjust = 0.5))

# gerar tabela
tabela.royalties <- gt(dados) %>%
  cols_label(
    municipio = 'Município',
    ano = 'Ano',
    valor_consolidado = 'Valor Nominal',
    valor_real = 'Valor Real'
  ) %>% 
  cols_hide(
    columns = c('codigo_ibge','transferencia','media_numero_indice_ipca','municipio')
  ) %>% 
  tab_header(
    title = paste("Royalties", titulo.roy ,"recebidos pela\n prefeitura de",label.muni),
    subtitle = 'valores em reais entre 2000 e 2020.'
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
  tab_source_note('Fonte: Elaboração própria. Tesouro Nacional (2021). Valor real deflacionado pela média do IPCA de 2020.')

# salvar
ggsave(plot = grafico.royalties, path = diretorio, filename = arquivo.grafico, width = 9, height = 6)
gtsave(data = tabela.royalties, path = diretorio, filename = arquivo.tabela)
}

# debug(Dados.Royalties)
# geobr::lookup_muni('rio de janeiro')[,1]

# testar função (passar mais de um município)
teste <- c(1100205,1500602)
Dados.Royalties(teste,'Royalties - CFH')






# ao aumento dos royalties é acompanhado por um aumento na mesma magnitude na saúde? Comparar antes e depois dos royalties
# Debug a função https://www.youtube.com/watch?v=x7BdImJ6loA
# debug(Dados.Royalties)
# Dados.Royalties(1100205,'Royalties - CFH') # rodar para debugar depois de rodar o debug()
# teste para rodar o código dentro da função (funcionou depois do debug)
# codigo_municipio <- 1100205
# tipo <- 'Royalties - CFH'


# Análises de casos: 
# Porto Velho (RO) - Royalties CFH
pv.royalties <- royalties %>% 
  dplyr::filter(codigo_ibge == 1100205,
                transferencia == 'Royalties - CFH') %>% 
  left_join(ipca) %>% 
  mutate(valor_real = valor_consolidado*ipca.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(pv.royalties, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(pv.royalties, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')


# Altamira (PA) - Royalties CFH
alta <- royalties %>% 
  dplyr::filter(codigo_ibge == 1500602,
                transferencia == 'Royalties - CFH') %>% 
  left_join(ipca) %>% 
  mutate(valor_real = valor_consolidado*ipca.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(alta, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(alta, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')
  

# Cidades da Mineração (CFEM)



# Marabá (PA)
mara.royalties <- royalties %>% 
  dplyr::filter(codigo_ibge == 1504208,
                transferencia == 'Royalties - CFM') %>% 
  left_join(ipca) %>% 
  mutate(valor_real = valor_consolidado*ipca.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(mara.royalties, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(mara.royalties, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')


# Parauapebas (PA)
parau.royalties <- royalties %>% 
  dplyr::filter(codigo_ibge == 1505536,
                transferencia == 'Royalties - CFM') %>% 
  left_join(ipca) %>% 
  mutate(valor_real = valor_consolidado*ipca.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(parau.royalties, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(parau.royalties, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')


# Canaã dos Carajás (PA)
parau.canaa <- royalties %>% 
  dplyr::filter(codigo_ibge == 1502152,
                transferencia == 'Royalties - CFM') %>% 
  left_join(ipca) %>% 
  mutate(valor_real = valor_consolidado*ipca.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(parau.canaa, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(parau.canaa, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')


# Canaã dos Carajás (PA)
parau.canaa <- royalties %>% 
  dplyr::filter(codigo_ibge == 1502152,
                transferencia == 'Royalties - CFM') %>% 
  left_join(ipca) %>% 
  mutate(valor_real = valor_consolidado*ipca.2020/igp_di_media_anual) # deflacionar

ggplot() +
  geom_line(parau.canaa, mapping = aes(x = ano, y = valor_consolidado/em_milhoes), col = 'blue') +
  geom_line(parau.canaa, mapping = aes(x = ano, y = valor_real/em_milhoes), col = 'red')





, Oriximiná (PA), Juruti (PA), Manaus (AM), Ourilândia do Norte (PA) e Itaituba (PA).

# continuar daqui (analisar dados, ou melhor, a evolução das cidades)
# cidades da mineração que foram escolhidas.


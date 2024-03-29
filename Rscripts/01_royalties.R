# Receitas via transfer�ncias da Uni�o - Royalties

source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

options(scipen = 999) # remove a nota��o cient�fica
ipca <- read_excel('Input/ipca_indice.xlsx')
ipca.2020 <- ipca$media_numero_indice_ipca[27]

# Royalties de minera��o, energia e petr�leo (exclui FEP) de 1990 at� 2020
royalties <- read_csv2('Input/transferencias_para_munic�pios_1990_2020_utf8.csv') %>% 
  janitor::clean_names() %>% 
  mutate(valor_consolidado = as.numeric(gsub('[[:punct:] ]+','', substr(valor_consolidado, 3, 100)))/100) %>%  # remove o R$, ponto e v�rgula da string, converte em n�mero e divide por 100 p/ incluir decimal
  dplyr::filter(!transferencia %in% c('Royalties - FEP', 'Royalties - PEA')) %>%  # exclui estas duas categorias
  left_join(ipca) %>% 
  mutate(valor_real = valor_consolidado * ipca.2020 / media_numero_indice_ipca) %>% # deflacionar
  select(5,1,2,3,4,8,7) 

# salvar o arquivo
write.csv2(royalties,'Temp/base_royalties_amzl.csv', row.names = F)

unique(royalties$transferencia) # tipos de royalties

# fun��o que gera gr�ficos dos Royalties
Dados.Royalties <- function(codigo_municipio, tipo) {
  for(i in codigo_municipio) { # permite passar mais de um argumento
  
  dados <- royalties %>% 
    dplyr::filter(codigo_ibge %in% i,
                  transferencia %in% tipo)
# check da fun��o
if (!nrow(dados) > 0) return (paste("Erro: nenhum resultado para o munic�pio/c�digo", i,"ou valores referentes aos", tipo))

# automatizar cria��o de diret�rios e do t�tulo dos gr�ficos e tabelas
  label.muni <- cidades.brasil.nome %>% 
    dplyr::filter(cod_muni %in% i) 
  
  label.muni <- cidades.brasil.nome[cidades.brasil.nome$cod_muni == i,2] # transformar em vetor ver regic script
  label.muni <- label.muni$muni # transforma em vetor
  label.muni <- as.character(str_replace_all(label.muni,"[[:punct:]]","")) # essa vari�vel deve receber o nome da cidade de acordo com o c�digo colocado
  titulo.roy <- ifelse(tipo == "Royalties - CFM", "da minera��o",
                       ifelse(tipo == "Royalties - ANP", "do petr�leo",
                              ifelse(tipo == "Royalties - CFH", "da gera��o de energia el�trica", "ERRO")))    
  arquivo.grafico <- paste('Gr�fico - royalties',titulo.roy,'de',label.muni,'.png')
  arquivo.tabela <- paste('Tabela - royalties',titulo.roy,'de',label.muni,'.png')
  arquivo.csv <- paste('royalties de ', label.muni,'.csv', sep = '')
  diretorio <- paste0('Outputs/dados por municipio/',label.muni)
  dir.create(diretorio)

# gerar gr�fico
grafico.royalties <- ggplot() +
  #geom_line(dados, mapping = aes(x = ano, y = valor_consolidado / em_milhoes), col = 'blue') +
  geom_line(dados, mapping = aes(x = ano, y = valor_real / em_milhoes), col = 'black') +
  ggtitle(paste("Royalties", titulo.roy ,"recebidos pela prefeitura de",label.muni, "\n(em valores de 2020)")) +
  labs(y = 'Valor dos Royalties (em R$ milh�es)', x = 'Ano', caption = 'Fonte: Elabora��o pr�pria. Tesouro Nacional (2021). Valor real deflacionado pela m�dia do IPCA de 2020.') +
  theme_classic()+
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title = element_text(hjust = 0.5))

# gerar tabela
tabela.royalties <- gt(dados) %>%
  cols_label(
    municipio = 'Munic�pio',
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
  tab_source_note('Fonte: Elabora��o pr�pria. Tesouro Nacional (2021). Valor real deflacionado pela m�dia do IPCA de 2020.')

# salvar
ggsave(plot = grafico.royalties, path = diretorio, filename = arquivo.grafico, width = 9, height = 6)
gtsave(data = tabela.royalties, path = diretorio, filename = arquivo.tabela)
write.csv2(x = dados, file = paste('Temp/', arquivo.csv, sep = ''), row.names = F)

}
}

# geobr::lookup_muni('rio de janeiro')[,1] # retorna o c�digo do munic�pio pelo nome
 amostra <- c(1100205,1500602)
 Dados.Royalties(amostra,'Royalties - CFH')

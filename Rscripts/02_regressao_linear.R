# Regressão Linear
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
options(scipen = 999) # remove notação científica

# Valores deflacionados em valores de 2020 para mostrar o que realmente varia (isso tira aquele problema dos anos)

# Para o ano de 2018
despesas.2018 <- read.csv2('Temp/base_muni_saude_amzl.csv') %>% 
  dplyr::filter(ano == 2018, 
                despesa %in% '10 - Saúde',
                cod_ibge %in% cidades.amazonia.legal)
royalties.2018 <- read.csv2('Temp/base_royalties_amzl.csv') %>% 
  dplyr::filter(ano == 2018,
                codigo_ibge %in% cidades.amazonia.legal,
                transferencia == 'Royalties - CFH')

dados <- left_join(royalties.2018, despesas.2018, by = c('ano','codigo_ibge' = 'cod_ibge')) %>% 
  select(1,8,6,13) %>% # selecionar valores deflacionados
  rename('valor_royalties' = 'valor_real.x',
         'valor_saude' = 'valor_real.y')

cor(dados)

modelo <- lm(formula = valor_saude~valor_royalties, data = dados)
summary(modelo)


# gráfico
library(ggplot2)
library(ggpubr) # pacote para colocar a equação no gráfico
x <- as.data.frame(dados)
ggplot(x, aes(x = valor_royalties, y = valor_saude)) +
  geom_point(shape = 21,
             color = 'dodgerblue4',
             fill = 'deepskyblue1',
             size = 3) +
  geom_smooth(method = 'lm', # adiciona a reta de regressão pelo método de regressão linear
              formula = y~x, # como eu já informei x e y em ggplot(x, aes(x = DriversKilled, y = PetrolPrice)), eu só preciso informar que y está em função de x
              # ou seja, DriversKilled está em função de PetroPrice
              se = F, # faixa sombreada em volta da reta é o intervalo de confiança (remove com se = F).
              col = 'darkred') + # cor da reta
  # mostrar a equação da reta no gráfico com a função stat_regline_equation()
  stat_regline_equation(aes(label = paste(..eq.label.., # palavra reservada do R para equação 
                                          ..rr.label.., # palavra reservada do R para o R² 
                                          sep = '~'))) + # ~ é o símbolo usado para dar espaço nas equações
  theme_classic()














# altamira (teste, pois tenho apenas 3 dados)
desp.alta <- read.csv2('Temp/despesas de Altamira PA.csv')
royalt.alta <- read.csv2('Temp/royalties de Altamira PA.csv')

dados.alta <- left_join(desp.alta, royalt.alta, by = 'ano') %>% 
  select('ano','valor_real.x','valor_real.y') %>%
  na.omit() %>% 
  rename('desp_saude_real' = 'valor_real.x',
         'royalties_real' = 'valor_real.y')
  
# confirmar se fiz correto!
lm(data = dados.alta, formula = desp_saude_real~royalties_real)



# pv
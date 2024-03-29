# Regress�o Linear
rm(list=ls()) 
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
options(scipen = 999) # remove nota��o cient�fica

# Valores deflacionados em valores de 2020 para mostrar o que realmente varia (isso tira aquele problema dos anos)

# Para o ano de 2018
despesas.2018 <- read.csv2('Temp/base_muni_saude_amzl.csv') %>% 
  dplyr::filter(ano == 2018, 
                despesa %in% '10 - Sa�de',
                cod_ibge %in% cidades.amazonia.legal)

royalties.2018 <- read.csv2('Temp/base_royalties_amzl.csv') %>% 
  dplyr::filter(ano == 2018,
                codigo_ibge %in% cidades.amazonia.legal,
                transferencia == 'Royalties - CFH')

dados <- left_join(royalties.2018, despesas.2018, by = c('ano', 'codigo_ibge' = 'cod_ibge')) %>% 
  select(1,8,6,13) %>% # selecionar valores deflacionados
  rename('valor_royalties' = 'valor_real.x',
         'valor_saude' = 'valor_real.y')

cor(dados)

# log1p() natural logarithm of a given number or set of numbers apliquei pq 
modelo <- lm(formula = log1p(valor_saude)~log1p(valor_royalties), data = dados)
summary(modelo)


# gr�fico
library(ggplot2)
library(ggpubr) # pacote para colocar a equa��o no gr�fico
x <- as.data.frame(dados)
ggplot(x, aes(x = valor_royalties, y = valor_saude)) +
  geom_point(shape = 21,
             color = 'dodgerblue4',
             fill = 'deepskyblue1',
             size = 3) +
  geom_smooth(method = 'lm', # adiciona a reta de regress�o pelo m�todo de regress�o linear
              formula = y~x, # como eu j� informei x e y em ggplot(x, aes(x = DriversKilled, y = PetrolPrice)), eu apenas preciso informar que y est� em fun��o de x
              # ou seja, DriversKilled est� em fun��o de PetroPrice
              se = F, # faixa sombreada em volta da reta � o intervalo de confian�a (remove com se = F).
              col = 'darkred') + # cor da reta
  # mostrar a equa��o da reta no gr�fico com a fun��o stat_regline_equation()
  stat_regline_equation(aes(label = paste(..eq.label.., # palavra reservada do R para equa��o 
                                          ..rr.label.., # palavra reservada do R para o R� 
                                          sep = '~'))) + # ~ � o simbolo usado para dar espa�o nas equa��es
  theme_classic()+
  # scale_y_continuous(trans = 'log10') + # experimento de colocar as escalas na base de log 10
  # scale_x_continuous(trans = 'log10') 
  scale_x_continuous(trans = scales::log_trans()) + # logaritmo neperiano
  scale_y_continuous(trans = scales::log_trans())   # logaritmo neperiano










# altamira (teste, pois tenho apenas 3 dados)
desp.alta <- read.csv2('Temp/despesas de Altamira PA.csv')
royalt.alta <- read.csv2('Temp/royalties de Altamira PA.csv')

dados.alta <- left_join(desp.alta, royalt.alta, by = 'ano') %>% 
  select('ano','valor_real.x','valor_real.y') %>%
  na.omit() %>% 
  rename('desp_saude_real' = 'valor_real.x',
         'royalties_real' = 'valor_real.y')
  
# teste
lm(data = dados.alta, formula = desp_saude_real~royalties_real)



# pv
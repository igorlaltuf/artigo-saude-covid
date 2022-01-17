# Leitos de internação vinculados ao SUS 
# Leitos de internação são aqueles para quem precisa ficar mais de 24h no hospital (COVID).
# https://portal.cfm.org.br/noticias/covid-19-interrompe-decada-de-queda-em-leitos-de-internacao-no-sistema-unico-de-saude-sus/

# Remover dados de 2021, já que não tenho a estimativa populacional


rm(list=ls()) 
# carregar scripts
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

# dados populacionais
pop <- read_csv('Temp/populacao_amzl_2001-20.csv') # População - 2001 - 2020
cod_muni <- as.character(str_sub(pop$cod_muni, 1, 6))
pop$cod_muni <- cod_muni
pop$ano <- as.character(pop$ano)

# obs: os dados são do mês de dezembro de cada ano, com exceção de 2021 em que os dados usados foram do mês de novembro.
# leitos de internação da CNES
leitos <- read_csv2('Input/leitos_de_internação_2005-2021.csv') %>% 
  janitor::clean_names() %>% 
  select(1:17) # remove 2021 (não tenho dados populacionais ainda)

cod_muni <- str_sub(leitos$municipio, 1, 6)
leitos$municipio <- str_sub(leitos$municipio, 8, 50)
leitos$cod_muni <- cod_muni

leitos <- leitos[, c(18,1:17)] # reordenar colunas
colnames(leitos) <- c('cod_muni','muni','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020')
leitos <- pivot_longer(leitos,
            cols = starts_with("20"),
            names_to = 'ano',
            values_to = 'leitos_internacao_sus') 

leitos$leitos_internacao_sus <- as.numeric(leitos$leitos_internacao_sus)

# remover NAs
is.na(leitos$leitos_internacao_sus)
leitos$leitos_internacao_sus[is.na(leitos$leitos_internacao_sus)] <- 0


# reunir os dados
dados <- left_join(leitos, pop, by = c('cod_muni','ano')) %>% 
  rename('muni' = 'muni.x', 'muni_uf' = 'muni.y') %>% 
  mutate(leitos_cada_100_mil_ha = (leitos_internacao_sus/populacao)*100000)


# Municípios na REGIC de Manaus para atend de saúde de alta complexidade (não inclui Manaus em si)
regic.de.manaus.saude <-  c("1301605", "1300029", "1302553", "1300060", "1300086", "1300102", "1300144", "1300201", "1300300", "1300409", "1300631", "1300508",
                            "1300607", "1300680", "1300805", "1300839", "1300904", "1301001", "1301100", "1301159", "1301209", "1301407", "1301308", "1301654",
                            "1301803", "1301852", "1301902", "1301951", "1303536", "1302009", "1302108", "1302801", "1302207", "1302306", "1302405", "1302504",
                            "1303569", "1302702", "1303106", "1302900", "1303007", "1303205", "1303304", "1303403", "1303601", "1303700", "1303809", "1304062",
                            "1303908", "1303957", "1304005", "1304104", "1304203", "1304237", "1304260", "1304302", "1304401", "1500404", "1506807") %>%
  substr(1,6) 

# Adicionar Manaus à lista de municípios acima (Manaus + sua regic)
regic.com.manaus.saude <- append(regic.de.manaus.saude, '130260')

# Qtd de leitos em Manaus (AM)
leitos.manaus <- dados %>% 
  dplyr::filter(cod_muni %in% "130260") %>% 
  group_by(ano) %>% 
  summarise(populacao = sum(populacao),
            leitos_internacao_sus = sum(leitos_internacao_sus)) %>% 
  mutate(leitos_cada_100_mil_ha = (leitos_internacao_sus/populacao)*100000)

# Qtd de leitos em Manaus (AM) + sua REGIC de atendimentos de saúde de alta complexidade
leitos.manaus.e.regic <- dados %>% 
  dplyr::filter(cod_muni %in% regic.com.manaus.saude) %>% 
  group_by(ano) %>% 
  summarise(populacao = sum(populacao),
            leitos_internacao_sus = sum(leitos_internacao_sus)) %>% 
  mutate(leitos_cada_100_mil_ha = (leitos_internacao_sus/populacao)*100000)

# leitos da regic de Manaus sem Manaus
leitos.regic.sem.manaus <- dados %>% 
  dplyr::filter(cod_muni %in% regic.de.manaus.saude) %>% 
  group_by(ano) %>% 
  summarise(populacao = sum(populacao),
            leitos_internacao_sus = sum(leitos_internacao_sus)) %>% 
  mutate(leitos_cada_100_mil_ha = (leitos_internacao_sus/populacao)*100000)



# fazer gráfico
grafico.leitos <- ggplot() +
  geom_bar(leitos.manaus, mapping = aes(x = ano, y = leitos_cada_100_mil_ha), col = 'grey', fill = 'grey', stat = 'identity') +
  labs(y = 'Quantidade de leitos a cada 100 mil habitantes', x = 'Ano') +
  theme_classic()+
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title = element_text(hjust = 0.5))

grafico.leitos 
ggsave(plot = grafico.leitos,'Outputs/manaus_leitos_internação.png', width = 9, height = 6, dpi = 600)




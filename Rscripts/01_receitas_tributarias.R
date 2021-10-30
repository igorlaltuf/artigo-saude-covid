# Receitas tributárias
# FINBRA


# receitas <- read.csv2('Input/finbra receitas 2017.txt', skip = 3)
# receitas <- receitas %>% 
#   dplyr::filter(Cod.IBGE %in% cidades.amazonia.legal,
#                 Coluna %in% 'Receitas Brutas Realizadas',
#                 Conta %in% '1.1.0.0.00.00.00 - Receita Tribut?ria') %>% 
#   select(Cod.IBGE, Valor) %>% 
#   mutate(Cod.IBGE = as.character(Cod.IBGE))
# 
# itr.2017 <- left_join(itr.2017, receitas, by = c('cod_muni'='Cod.IBGE')) %>% 
#   rename(receitas_tributarias = Valor) %>% 
#   mutate(perc_total = cota_parte_itr/receitas_tributarias) %>% 
#   arrange(desc(perc_total))
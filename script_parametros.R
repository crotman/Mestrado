library(tidyverse)
library(sidrar)
library(lubridate)

parametros <- read_csv2("D:\\Mestrado Bruno\\Dados\\ParametrosResiduos.csv",  )
dados <- read_csv2("D:\\Mestrado Bruno\\Dados\\temporarias.csv",  quote = "\"")
junta <- read_csv2("D:\\Mestrado Bruno\\HPC Parametrizado\\junta.csv",  quote = "\"")


parametros_ajustado <- parametros %>% 
    mutate(tbst =  as.double(str_replace(tbst,",",".") )) %>% 
    mutate(Disp =  as.double(str_replace(Disp,",",".") )) %>% 
    mutate(PCI =  as.double(str_replace(PCI,",",".") )) 


dados_ajustado <- dados %>% 
  filter(Municipio != "") %>% 
  filter(!is.na(Cod)) %>% 
  filter(Cod != 0) %>% 
  select(Cod,Municipio,Algodao, Arroz, Feijao, Mandioca, Milho, Soja, Trigo  ) %>% 
  mutate(UF = str_trim(str_sub(str_extract(Municipio, "\\(.{2}\\)"), 2, 3) )) %>% 
  mutate(Nome = str_trim(str_replace(Municipio, "\\(.{2}\\)", "" ))) %>% 
  select(-Municipio) %>% 
  mutate_at(vars(-Cod, -UF, -Nome), as.character  ) %>% 
  mutate_at(vars(-Cod, -UF, -Nome), as.integer  )  %>% 
  replace(is.na(.), 0) %>% 
  gather(cultura, producao, Algodao, Arroz, Feijao, Mandioca, Milho, Soja, Trigo ) %>% 
  left_join(parametros, by = c("cultura" = "Produto")   )  %>% 
  mutate(Carga = producao * tbst * Disp  ) %>% 
  mutate(Energia = producao * tbst * Disp * PCI * 0.2 ) %>% 
  rename (CD = Cod) %>% 
  select(UF, Nome, CD, Carga, Energia  ) %>% 
  group_by(UF, Nome, CD) %>% 
  summarise(Carga = sum(Carga), Energia = sum(Energia)) 


ipca <- get_sidra(api ="/t/1737/n1/all/v/2266/p/all/d/v2266%2013") %>% 
  mutate(data = as.character(.$"Mês (Código)")) %>% 
  mutate(data = parse_date_time(data,"Ym") ) %>% 
  select(data, Valor) 

  
ipca_inicio <- ipca %>% 
  filter( year(data) == 2010 & month(data) == 6 ) %>% 
  .$Valor

ipca_fim <- ipca %>% 
  filter( year(data) == 2017 & month(data) == 6 ) %>% 
  .$Valor

Correcao <- ipca_fim / ipca_inicio

receita <- tibble( receita = c(175, 800), tipo_receita = c("preco_leilao", "preco_resolucao 482")  )

premio_produtor_esc = 0.075

alfa_custo_esc = 69000000

beta_custo_usina_esc = 3500000

custos_biomassa = tibble(
  tipo_custo = c("custo-obt-minimo_oliveira", "custo-obt-medio_oliveira", "custo-obt-maximo_oliveira"), 
  colheita = c(19.95, 37.08, 57.84) ,
  armazenagem = c(5.69, 13.81, 22.73) ,
  carga = c(3.69, 7.84, 13.88) 
)

#https://www.tabelasdefrete.com.br/p/calculo-carreteiro?1

custo_transporte_esc = 0.315

percentual_OPEX_esc = 0.03

fator_disponibilidade_esc = 0.9


custos_biomassa <- custos_biomassa %>% 
  mutate( colheita = colheita * Correcao ) %>% 
  mutate( armazenagem = armazenagem * Correcao ) %>% 
  mutate( carga = carga * Correcao ) 
  

custos_biomassa_conab = tibble(
  tipo_custo = c("conab"), 
  colheita = c(31.0) ,
  armazenagem = c(0.0) ,
  carga = c(0.0) 
)

custos_biomassa <- custos_biomassa_conab %>% 
  bind_rows(custos_biomassa)


taxas <- tibble( tipo_receita = c(rep("preco_leilao",8), rep("preco_resolucao 482",8)), taxa = c(c(8:15), seq(15, by = 2, length.out = 8 )  ))


parametros <- 
  receita %>% 
  inner_join(taxas, by = c("tipo_receita" = "tipo_receita")) %>% 
  crossing(custos_biomassa) %>% 
  rename(receita_por_mwh = receita, 
         custo_colheita_por_t = colheita, 
         custo_carga_por_t = carga, 
         custo_armazenamento_por_t = armazenagem
  ) %>% 
  mutate(
    premio_produtor = premio_produtor_esc,
    alfa_custo_usina= alfa_custo_esc	,
    beta_custo_usina = beta_custo_usina_esc,
    custo_transporte_por_t_km = custo_transporte_esc ,
    percentual_OPEX	 = percentual_OPEX_esc,
    fator_disponibilidade = fator_disponibilidade_esc,
    cenario = paste(tipo_receita, tipo_custo)
  ) %>% 
  arrange(tipo_custo) %>%  
  mutate(unitario = 1) %>% 
  mutate(cod_cenario = cumsum(unitario)) %>% 
  select(-unitario)



write_csv(dados_ajustado,"c:\\temp\\carga_energia.csv")
write_csv(parametros,"c:\\temp\\parametros_novo.csv")












  
  







  
  
    
    

    

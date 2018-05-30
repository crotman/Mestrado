

library(tidyverse)


#Lendo municípios e informações de energia e carga

municipios <-  read.csv("D:\\Mestrado Bruno\\HPC Parametrizado\\junta.csv",sep = ";", encoding = "UTF-8",dec = "," ) %>% as_tibble()

str(municipios)

head(municipios)


#Lendo dados de distâncias reais entre municípios


distancias <-  read.csv("C:\\temp\\distanciarealmetros.csv",sep = "," ) %>% 
  as_tibble() 

distancias_inv <- distancias %>% 
  rename(CD_yold = CD_y) %>%
  rename(CD_y = CD_x) %>%
  rename(CD_x = CD_yold)

distancias <- distancias %>% union(distancias_inv)

str(distancias)

head(distancias)



#Lendo distâncias em linha reta entre municípios


distancias_retas <-  read.csv("C:\\temp\\distancia.csv",sep = "," ) %>% 
  as_tibble() %>% 
  rename(distancia_reta = "Distância") %>% 
  rename(cod1 = cod)




str(distancias_retas)

head(distancias_retas)





#Lendo parâmetros adicionais

MWH_por_GJ = 0.27777777777777777777777778

anos_vida_util_usina = 30 

parametros <-  read.csv("C:\\temp\\parametros.csv",sep = ";", encoding = "UTF-8",dec = "," ) %>% as_tibble()

str(parametros)

head(parametros)



#Escolhendo escopo de local da execução


municipios_escopo <- municipios %>% 
  filter( X.U.FEFF.UF == "SP" )


str(municipios_escopo)

head(municipios_escopo)


#Criando uma matriz de distâncias


matriz <- municipios_escopo %>%
  mutate(unidade = 1) %>% #Para faxer um full outer join
  full_join(.,., by = c("unidade" = "unidade"), suffix = c("_x", "_y") ) %>% 
  left_join( distancias  ) %>% 
  left_join( distancias_retas ) %>%
  #distâncias sem 
  replace_na( list("Distância" = 10000) ) %>% 
  rename( Distancia  =  "Distância" )




str(matriz)
head(matriz)



#Funções usadas no algoritmo

insere_receita_custo_lucro <- function(num_cenario) {
  
  custo_colheita = as.double( parametros[num_cenario,"custo_colheita_por_t"] )
  custo_armazenamento = as.double( parametros[num_cenario,"custo_armazenamento_por_t"])
  custo_carga = as.double( parametros[num_cenario,"custo_carga_por_t"])
  custo_transporte = as.double( parametros[num_cenario,"custo_transporte_por_t_km"])
  premio_produtor = as.double( parametros[num_cenario,"premio_produtor"])
  receita_por_mwh = as.double( parametros[num_cenario,"receita_por_mwh"])
  fator_disponibilidade = as.double(parametros[num_cenario,"fator_disponibilidade"])
  
  
  matriz <- matriz %>% 
    mutate( 
      custo = 
        as.double(
          (
            custo_colheita * Carga_y +
              custo_armazenamento * Carga_y +
              custo_carga * Carga_y +
              custo_transporte * Carga_y * Distancia
          ) * ( 1 + premio_produtor)
        )) %>% 
    
    mutate(  
      
      receita = as.double(Energia_y * MWH_por_GJ * receita_por_mwh * fator_disponibilidade)
    ) %>% 
    
    mutate ( lucro = receita - custo) %>% 
    
    filter (lucro >0) %>% 
    
    arrange( desc(lucro) ) 
  
}


calcula_heuristica_1 <- function(municipios_escopo) {
  
  #Heurística:
  #Enquanto houver municipios
  #Cria uma Partição com o município de maior energia disponível
  #Adiciona vizinhos deste municipio à partição, em ordem decrescente de lucro, até que não seja possível inclur devido ao máximo de produção
  
  
  municipios_escopo_h <- municipios_escopo %>% arrange(desc(Energia)) 
  
  municipios_escopo_h_desalocados <- municipios_escopo_h
  
  
  #inserindo a partição vazia
  particoes_h <- tibble(sede = integer(), cidades = integer()  )
  
  fim <- FALSE
  
  cod_municipio_tratar <- as.integer(municipios_escopo_h[1,"CD"])
  
  while (!fim)
  {
    print(particoes_h)
    
    
    #pegando os vizinhos do município tratado ainda não alocados que dão lucro
    vizinhos <- matriz %>% 
      filter(CD_x == cod_municipio_tratar) %>% 
      inner_join( municipios_escopo_h_desalocados, by = c("CD_y" = "CD")  ) %>% 
      select (CD_y)
    
    
    if (count(vizinhos) > 0)
    {
      #Criando a partição com os vizinhos que dão lucro
      particoes_h = add_row(particoes_h, sede = cod_municipio_tratar, cidades = vizinhos$CD_y ) 
      #Retirando os alocados
      municipios_escopo_h_desalocados <- municipios_escopo_h_desalocados %>% 
        anti_join( vizinhos, by = c("CD" = "CD_y" ))
      #Pegando o próximo não alocado 
      cod_municipio_tratar <- as.integer(municipios_escopo_h_desalocados[1,"CD"])
    }
    else
    {
      #Se nem o próprio município é viável, alocamos todos os não alocados numa partição vazia
      resto <- (municipios_escopo_h_desalocados %>% select(CD))$CD
      particoes_h <- add_row(particoes_h, sede = -1, cidades = resto ) 
      fim <- TRUE
    }
    
  }
  
  particoes_h  
  
}

calcula_custo_usina <- function(energia_GJ, num_cenario){
  
  fator_disponibilidade = as.double(parametros[num_cenario,"fator_disponibilidade"])
  alfa = as.double( parametros[num_cenario,"alfa_custo_usina"])
  beta = as.double( parametros[num_cenario,"beta_custo_usina"])
  perc_opex = as.double( parametros[num_cenario,"percentual_OPEX"])
  
  #capacidade necessária é a potência que a usina tem que ter para fazer frente a energia disponibilizada
  capacidade_necessaria = energia_GJ * MWH_por_GJ / fator_disponibilidade / (24*365)
  
  (alfa + beta * capacidade_necessaria) * ( 1 + perc_opex )  / anos_vida_util_usina
  
}


calcula_lucro_varias_especificacoes <- function(particoes){
  
  
  #Recebe um dataframe com as colunas sede e cidades para cada configuração, indexada pela coluna indice
  
  
  particoes_matriz <-  particoes %>%
  inner_join(matriz, c("sede" = "CD_x", "cidades" = "CD_y" )) 

  #agrupando para calcular, para cada candidata, o lucro e carga da região (sem custo da usina), depois o cutso da usina
  especificacoes_limpa_negativas <- particoes_matriz %>% 
    group_by(sede, indice) %>% 
    summarise( lucro_total_sem_usina = sum(lucro), energia_total = sum(Energia_y)) %>% 
    mutate ( custo_usina = calcula_custo_usina(energia_GJ = energia_total, num_cenario = i)  ) %>% 
    mutate ( lucro_final_sede = lucro_total_sem_usina - custo_usina) %>% 
    mutate ( lucro_final_sede = ifelse(lucro_final_sede < 0 , 0, lucro_final_sede) )
  
  #escolhendo o melhor lucro
  melhor_lucro <- especificacoes_limpa_negativas %>% 
    group_by( indice ) %>% 
    summarise( lucro = sum(lucro_final_sede)  ) %>% 
    top_n( 1, lucro)
  
  resposta <- especificacoes_limpa_negativas %>% 
    filter( indice == melhor_lucro$indice ) %>% 
    mutate( lucro = melhor_lucro$lucro )
  
  
  resposta

}
  



calcula_lucro_escolhendo_sede <- function(particoes) {
  
  #Recebe um dataframe com as colunas sede e cidades para uma configuração
  
  #Devolve o dataframe com as melhores sedes. E o lucro repetido na coluna lucro
  
  
  
  
  #self join para testar todas as cidades como sede
  particoes_com_candidatas <- particoes %>% 
    inner_join( particoes, by = c("sede" = "sede")) %>% 
    rename( sede_candidata = cidades.x, cidade = cidades.y  ) %>% 
    rename( sede_original = sede )
  
  #join com a matriz de informações para calcular o lucro de cada cidade
  particoes_com_candidatas_matriz <-  particoes_com_candidatas %>%
    inner_join(matriz, c("sede_candidata" = "CD_x", "cidade" = "CD_y" )) 
  
  #agrupando para calcular, para cada candidata, o lucro e carga da região (sem custo da usina), depois o cutso da usina
  lucro_candidatas <- particoes_com_candidatas_matriz %>% 
    group_by(sede_original, sede_candidata) %>% 
    summarise( lucro_total_sem_usina = sum(lucro), energia_total = sum(Energia_y)) %>% 
    mutate ( custo_usina = calcula_custo_usina(energia_GJ = energia_total, num_cenario = i)  ) %>% 
    mutate ( lucro_final = lucro_total_sem_usina - custo_usina)
  
  maiores_lucros <- lucro_candidatas %>% 
    group_by(sede_original) %>% 
    summarise(maior_lucro = max(lucro_final))
  
  #selecionando as candidatas de maior lucro
  candidatas_vencedoras <- lucro_candidatas %>% 
    inner_join(maiores_lucros) %>% 
    filter( lucro_final == maior_lucro ) %>% 
    select( sede_original, sede_candidata, lucro_final ) %>% 
    #selecionando uma só candidata arbitrariamente se duas tiverem o mesmo lucro
    group_by( sede_original ) %>% 
    summarise( candidata_vencedora = min(sede_candidata), lucro = max(lucro_final) )
  
  #substituindo a sede pela mais lucrativa
  particoes <- particoes %>% 
    inner_join( candidatas_vencedoras, by = (c("sede" = "sede_original" ))) %>% 
    mutate( sede = ifelse(sede != -1, candidata_vencedora, -1 )) %>% 
    select( sede, cidades, lucro) %>% 
    #limpando as partições negativas para a vazia
    mutate( sede = ifelse(lucro < 0, -1, sede) ) %>% 
    #o lucro da vazia é zero
    mutate( lucro = ifelse( sede == -1, 0, lucro )  )  %>% 
    arrange( sede )
  
  lucro_escalar <- particoes %>% 
    select(sede, lucro) %>% 
    distinct (sede, lucro) 
    
  lucro_escalar <- sum(lucro_escalar$lucro)
  
  particoes <- particoes %>% 
    mutate(lucro = lucro_escalar)
  
  
  
}





#Execução do algoritmo


#Loop principal 

heuristicas = c("H1", "H2", "HRand")

for (i in 1:1) #nrow(parametros) )
{
  
  
  for (h in (1:1))
  {
    
    #Inserindo os custos, receitas e lucros para cada par de cidades
    matriz <- insere_receita_custo_lucro(i)

    #Preparando as partições após heurística    
    particoes <- calcula_heuristica_1(municipios_escopo)
    
    #escolhendo melhor sede
    particoes <- calcula_lucro_escolhendo_sede(particoes)
    
    particoes_com_unitario <- particoes %>% 
      mutate( unitario = 1) %>% 
      mutate( indice = cumsum(unitario) )
    
    sedes <- particoes %>% 
      select(sede) %>% 
      distinct (sede) %>% 
      mutate( unitario = 1) %>% 
      mutate( indice = cumsum(unitario) )
    
    
    #montando as trocas
    #cada municipio troca para cada região
    ind_municipios <- particoes_com_unitario %>% 
      select( indice, unitario ) %>% 
      rename (ind_municipio = indice)

    ind_sedes <- sedes %>% 
      select( indice, unitario, sede ) %>% 
      #adicionando uma sede "zero", que significa levar o município para uma sede dele
      add_row( indice = 0, unitario = 1  ) %>% 
      rename (ind_sede = indice)
    
    trocas <- inner_join( ind_sedes, ind_municipios ) %>% 
      mutate (ind_troca = cumsum(unitario) )
    
    particoes_com_unitario <- particoes_com_unitario %>% 
      rename (ind_municipio = indice) 

    particoes_com_troca <- particoes_com_unitario %>% 
      inner_join(trocas, by = c("unitario" = "unitario")) %>% 
      #efetuando a troca de sede
      mutate ( sede_nova = ifelse(ind_municipio.x == ind_municipio.y, sede.y, sede.x ) ) %>% 
      #as trocas para sede 0 são trocas para a própria cidade
      mutate ( sede_nova = ifelse(sede_nova != 0, sede_nova, cidades ) ) %>% 
      rename (sede = sede_nova, indice = ind_troca) %>%
      select (sede, cidades, indice)  
    
    particoes <- calcula_lucro_varias_especificacoes(particoes_com_troca)  
      

    
    
    aaaa <- 0

  

        
    
    
    
    
  }
    
    
    
    
      
    
    
    
    
    
    

    
    
    
    
    
    
  
}




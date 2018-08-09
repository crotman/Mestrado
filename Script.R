library(data.table)
library(pryr)
library(maxmatching)
library(igraph)
library(tidyverse)
library(brazilmaps)
library(tmap)



#Lendo municipios e informacoes de energia e carga

municipios <-  read.csv("D:\\Mestrado Bruno\\HPC Parametrizado\\junta.csv",sep = ";", encoding = "UTF-8",dec = "," ) %>% as_tibble()

str(municipios)

head(municipios)


#Lendo dados de dist√¢ncias reais entre municipios

distancias <-  read.csv("C:\\temp\\distanciarealmetrossede.csv",sep = "," ) %>% 
  as_tibble() 

distancias_inv <- distancias %>% 
  rename(CD_yold = CD_y) %>%
  rename(CD_y = CD_x) %>%
  rename(CD_x = CD_yold)

distancias <- distancias %>% union(distancias_inv)

str(distancias)

head(distancias)



#Lendo distancias em linha reta entre municipios

distancias_retas <-  read.csv("C:\\temp\\distancia.csv",sep = "," ) %>% 
  as_tibble() %>% 
  rename(distancia_reta = "Dist‚ncia") %>% 
  rename(cod1 = cod)

str(distancias_retas)

head(distancias_retas)





#Lendo par‚metros adicionais

MWH_por_GJ = 0.27777777777777777777777778

anos_vida_util_usina = 30 

parametros <-  read.csv("C:\\temp\\parametros.csv",sep = ";", encoding = "UTF-8",dec = "," ) %>% as_tibble()

str(parametros)

head(parametros)



#Escolhendo escopo de local da execucao

municipios_escopo_original <- municipios %>% 
  filter( X.U.FEFF.UF == "SP" )

str(municipios_escopo)

head(municipios_escopo)


#Criando uma matriz de distancias estrada

#matriz <- municipios_escopo_original %>%
#  mutate(unidade = 1) %>% #Para faxer um full outer join
#  full_join(.,., by = c("unidade" = "unidade"), suffix = c("_x", "_y") ) %>% 
#  left_join( distancias  ) %>% 
#  left_join( distancias_retas ) %>%
#  #dist√¢ncias sem 
#  replace_na( list("Dist‚ncia" = 10000) ) %>% 
#  rename( Distancia  =  "Dist‚ncia" ) %>% 
#  mutate( Distancia = ifelse (Distancia <0, 10000, Distancia ))


#Criando uma matriz de distancias retas

matriz <- municipios_escopo_original %>%
  mutate(unidade = 1) %>% #Para faxer um full outer join
  full_join(.,., by = c("unidade" = "unidade"), suffix = c("_x", "_y") ) %>% 
  left_join( distancias  ) %>% 
  left_join( distancias_retas ) %>%
  #dist√¢ncias sem 
  replace_na( list(distancia_reta = 10000) ) %>% 
  rename( Distancia  =  distancia_reta ) %>% 
  mutate( Distancia = ifelse (Distancia <0, 10000, Distancia ))


#matriz_ordem <- matriz %>% 
#  arrange(CD_X,CD_Y)



str(matriz)
head(matriz)



#Funcoes usadas no algoritmo

insere_receita_custo_lucro <- function(num_cenario) {

  
  #inserindo a receita e o custo relacionados aa biomassa. Falta o custo da usina
    
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
              custo_carga * Carga_y 
          ) * ( 1 + premio_produtor)
          + custo_transporte * Carga_y * Distancia
        )) %>% 
    
    mutate(  
      
      receita = as.double(Energia_y * MWH_por_GJ * receita_por_mwh * fator_disponibilidade)
    ) %>% 
    
    mutate ( lucro = receita - custo) %>% 
    
    filter (lucro >0) %>% 
    
    arrange( desc(lucro) ) 
  
}


calcula_heuristica_1 <- function(municipios_escopo) {
  
  #Heuristica:
  #Enquanto houver municipios
  #Cria uma Particao com o municipio de maior energia disponivel
  #Adiciona vizinhos deste municipio aa particao, em ordem decrescente de lucro, ate que nao seja possivel inclur devido ao maximo de producao
  
  
  municipios_escopo_h <- municipios_escopo %>% arrange(desc(Energia)) 
  
  municipios_escopo_h_desalocados <- municipios_escopo_h
  
  
  #inserindo a particao vazia
  particoes_h <- tibble(sede = integer(), cidades = integer(), label = integer()  )
  
  fim <- FALSE
  
  cod_municipio_tratar <- as.integer(municipios_escopo_h[1,"CD"])
  
  label <- 1
  
  while (!fim)
  {
    print(particoes_h)
    
    
    #pegando os vizinhos do municipio tratado ainda nao alocados que dao lucro
    vizinhos <- matriz %>% 
      filter(CD_x == cod_municipio_tratar) %>% 
      inner_join( municipios_escopo_h_desalocados, by = c("CD_y" = "CD")  ) %>% 
      select (CD_y)
    
    
    if (count(vizinhos) > 0)
    {
      #Criando a particao com os vizinhos que dao lucro
      particoes_h = add_row(particoes_h, sede = cod_municipio_tratar, cidades = vizinhos$CD_y, label = label )
      label <- label + 1
      #Retirando os alocados
      municipios_escopo_h_desalocados <- municipios_escopo_h_desalocados %>% 
        anti_join( vizinhos, by = c("CD" = "CD_y" ))
      #Pegando o proximo nao alocado 
      cod_municipio_tratar <- as.integer(municipios_escopo_h_desalocados[1,"CD"])
    }
    else
    {
      #Se nem o proprio municipio e viavel, alocamos todos os nao alocados numa particao vazia
      resto <- (municipios_escopo_h_desalocados %>% select(CD))$CD
      if (length(resto) > 0)
      {
        particoes_h <- add_row(particoes_h, sede = -1, cidades = resto, label = 0 ) 
      }
      fim <- TRUE
    }
    
  }
  
  particoes_h <- particoes_h %>% 
    mutate(sede = as.integer(sede), label = as.integer(label))
  
  particoes_h
  
}



calcula_heuristica_2 <- function(municipios_escopo, cenario) {
  
  #Heuristica:
  #Caminhando pelos municipios em ordem de energia.
  #Calcular o aumento de lucro que o municipio daria a cada particao, guardar o maior aumento de lucro e a maior particao
  #Comparar com o lucro que o municipio daria com sua propria particao
  #Escolher se cria uma nova particao ou coloca  municipio na particao onde ele aumentaria mais o lucro
  #Se todos os lucros forem negativos, coloca numa particao vazia
  
  
  municipios_escopo_h <- municipios_escopo %>% arrange(desc(Energia)) 
  
  alfa = as.double( parametros[cenario,"alfa_custo_usina"])
  
  #inserindo a particao vazia
  particoes_h <- tibble(sede = integer(), cidades = integer()  )
  
  fim <- FALSE
  
  
  for (m in 1:nrows(municipios_escopo_h)){
    

    #pegando o municipio e vendo, para cada parti√ß√£o, qual seria o incremento de lucro
    #depois pegando o maior lucro adicionado
    
    #lucros_adicionados_por_particao <- municipios_escopo_h[m] %>% 
    #  inner_join(matriz, by = c("CD" = "CD_y")) %>%
    #  left_join(particoes_h, )
    #  mutate(custo_adic_usina = calcula_custo_usina(energia_GJ = energia, num_cenario = cenario) ) %>% 
    #  #retirando o alfa quando a sede eh a propria cidade
    #  mutate(custo_adic_usina = ifelse(CD_y == CD_x, custo_adic_usina - alfa, custo_adic_usina )) %>% 
    #  mutate(lucro_final = lucro - custo_adic_usina) %>% 
    #  top_n()

    
      
      
      
    
    
  }
  
  
  cod_municipio_tratar <- as.integer(municipios_escopo_h[1,"CD"])
  
  while (!fim)
  {
    print(particoes_h)
    
    
    #pegando os vizinhos do municipio tratado ainda nao alocados que dao lucro
    vizinhos <- matriz %>% 
      filter(CD_x == cod_municipio_tratar) %>% 
      inner_join( municipios_escopo_h_desalocados, by = c("CD_y" = "CD")  ) %>% 
      select (CD_y)
    
    
    if (count(vizinhos) > 0)
    {
      #Criando a particao com os vizinhos que dao lucro
      particoes_h = add_row(particoes_h, sede = cod_municipio_tratar, cidades = vizinhos$CD_y ) 
      #Retirando os alocados
      municipios_escopo_h_desalocados <- municipios_escopo_h_desalocados %>% 
        anti_join( vizinhos, by = c("CD" = "CD_y" ))
      #Pegando o pr√≥ximo n√£o alocado 
      cod_municipio_tratar <- as.integer(municipios_escopo_h_desalocados[1,"CD"])
    }
    else
    {
      #Se nem o proprio municipio √© viavel, alocamos todos os nao alocados numa particao vazia
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
  
  #capacidade necessaria a potencia que a usina tem que ter para fazer frente a energia disponibilizada
  capacidade_necessaria = energia_GJ * MWH_por_GJ / fator_disponibilidade / (24*365)
  
  (alfa + beta * capacidade_necessaria) * ( 1 + perc_opex )  / anos_vida_util_usina
  
}


calcula_custo_usina_v2 <- function(energia_GJ, num_cenario, fator_disponibilidade, alfa, beta, perc_opex){
  
  #fator_disponibilidade = as.double(parametros[num_cenario,"fator_disponibilidade"])
  #alfa = as.double( parametros[num_cenario,"alfa_custo_usina"])
  #beta = as.double( parametros[num_cenario,"beta_custo_usina"])
  #perc_opex = as.double( parametros[num_cenario,"percentual_OPEX"])
  
  #capacidade necessaria a potencia que a usina tem que ter para fazer frente a energia disponibilizada
  capacidade_necessaria = energia_GJ * MWH_por_GJ / fator_disponibilidade / (24*365)
  
  (alfa + beta * capacidade_necessaria) * ( 1 + perc_opex )  / anos_vida_util_usina
  
}


calcula_lucro_varias_especificacoes <- function(particoes){
  
  
  #Recebe um dataframe com as colunas sede e cidades para cada configuracao, indexada pela coluna indice
  
  #matriz_para_join <- matriz %>% 
  #  select(CD_x, CD_y, lucro, Energia_y )

  matriz_para_join_ordenada <- matriz_ordenada %>% 
    select(CD_x, CD_y, lucro, Energia_y )

  #matriz_para_join_ordenada <- matriz_para_join_ordenada %>% 
  #  mutate(CD_x = as.factor(CD_x), CD_y = as.factor(CD_y))
  
  #particoes <- particoes %>% 
  #  mutate(sede = as.factor(sede), cidades = as.factor(cidades))
    
  particoes_matriz <-  particoes %>%
    inner_join(matriz_para_join_ordenada, c("sede" = "CD_x", "cidades" = "CD_y" )) 

  #particoes_matriz <- particoes_matriz %>% 
  #  mutate(sede = as.integer(sede), cidades = as.integer(cidades)) 
    
  #matriz_para_join_ordenada <- matriz_para_join_ordenada %>% 
  #  mutate(CD_x = as.integer(CD_x), CD_y = as.integer(CD_y))
  
  #particoes <- particoes %>% 
  #  mutate(sede = as.integer(sede), cidades = as.integer(cidades))
  
  
  
  fator_disponibilidade = as.double(parametros[i,"fator_disponibilidade"])
  alfa = as.double( parametros[i,"alfa_custo_usina"])
  beta = as.double( parametros[i,"beta_custo_usina"])
  perc_opex = as.double( parametros[i,"percentual_OPEX"])
  
  
  #agrupando para calcular, para cada candidata, o lucro e carga da regi√£o (sem custo da usina), depois o cutso da usina
  #especificacoes_limpa_negativas <- particoes_matriz %>% 
  #  group_by(sede, indice) %>% 
  #  summarise( lucro_total_sem_usina = sum(lucro), energia_total = sum(Energia_y)) %>% 
  #  mutate ( custo_usina = calcula_custo_usina(energia_GJ = energia_total, num_cenario = i)  ) %>% 
  #  mutate ( lucro_final_sede = lucro_total_sem_usina - custo_usina) %>% 
  #  mutate ( lucro_final_sede = ifelse(lucro_final_sede < 0 , 0, lucro_final_sede) )

  #agrupando para calcular, para cada candidata, o lucro e carga da regi√£o (sem custo da usina), depois o cutso da usina
  especificacoes_limpa_negativas <- particoes_matriz %>% 
    group_by(sede, indice) %>% 
    summarise( lucro_total_sem_usina = sum(lucro), energia_total = sum(Energia_y)) %>% 
    mutate ( custo_usina = calcula_custo_usina_v2(energia_GJ = energia_total, num_cenario = i, fator_disponibilidade = fator_disponibilidade, alfa = alfa, beta = beta, perc_opex = perc_opex )  ) %>% 
    mutate ( lucro_final_sede = lucro_total_sem_usina - custo_usina) %>% 
    mutate ( lucro_final_sede = if_else(lucro_final_sede < 0 , 0, lucro_final_sede) )
  

  #escolhendo o melhor lucro
  melhor_lucro <- especificacoes_limpa_negativas %>% 
    group_by( indice ) %>% 
    summarise( lucro = sum(lucro_final_sede)  ) %>% 
    top_n( 1, lucro)
  

  resposta <- especificacoes_limpa_negativas %>% 
    filter( indice == first(melhor_lucro$indice) ) %>% 
    mutate( lucro = first(melhor_lucro$lucro) ) %>% 
    top_n(1)
  

  particoes <- particoes %>%
    filter( indice == first(resposta$indice) ) %>%
    mutate( lucro = first(resposta$lucro) ) %>% 
    select( sede, cidades, lucro, label )
  
  particoes
  

}
  

calcula_lucro_escolhendo_sede <- function(particoes) {
  
  #Recebe um dataframe com as colunas sede e cidades para uma configuracao
  
  #Devolve o dataframe com as melhores sedes. E o lucro repetido na coluna lucro
  

  #self join para testar todas as cidades como sede
  particoes_com_candidatas <- particoes %>% 
    inner_join( particoes, by = c("sede" = "sede")) %>% 
    rename( sede_candidata = cidades.x, cidade = cidades.y  ) %>% 
    rename( sede_original = sede )
  
  #join com a matriz de informacoes para calcular o lucro de cada cidade
  particoes_com_candidatas_matriz <-  particoes_com_candidatas %>%
    inner_join(matriz_so_lucro_energia_ordenada, c("sede_candidata" = "CD_x", "cidade" = "CD_y" )) 

  
  #agrupando para calcular, para cada candidata, o lucro e carga da regi√£o (sem custo da usina), depois o cutso da usina
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
    #selecionando uma s√≥ candidata arbitrariamente se duas tiverem o mesmo lucro
    group_by( sede_original ) %>% 
    summarise( candidata_vencedora = min(sede_candidata), lucro = max(lucro_final) )
  
  #substituindo a sede pela mais lucrativa
  particoes <- particoes %>% 
    inner_join( candidatas_vencedoras, by = (c("sede" = "sede_original" ))) %>% 
    mutate( sede = if_else(sede != as.integer(-1), as.integer(candidata_vencedora), as.integer(-1) )) %>% 
    select( sede, cidades, lucro, label) %>% 
    #limpando as particoes negativas para a vazia
    mutate( sede = if_else(lucro < 0, as.integer(-1), sede) ) %>% 
    mutate( label = if_else(lucro < 0, as.integer(0), label) ) %>% 
    #o lucro da vazia eh zero
    mutate( lucro = if_else( sede == -1, 0, lucro )  )  %>% 
    arrange( sede )
  
  #Jogando as cidades sem lucro para a regi„o para a partiÁ„o vazia
  #particoes <- particoes %>% 
  #  left_join(matriz_so_lucro_energia, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
  #  mutate(sede = if_else(is.na(lucro.y), as.integer(-1), sede ), label = if_else(is.na(lucro.y), as.integer(0), label ), lucro.x = if_else(is.na(lucro.y), 0, lucro.x ) ) %>% 
  #  rename(lucro = lucro.x) %>% 
  #  select(sede, cidades, lucro, label) %>% 
  #  identity()
      

  lucro_escalar <- particoes %>% 
    select(sede, lucro) %>% 
    distinct (sede, lucro) 
    
  lucro_escalar <- sum(lucro_escalar$lucro)
  
  particoes <- particoes %>% 
    mutate(lucro = lucro_escalar)
  
  
  
}

realiza_pedaco_passo_busca_local <- function(sedes, ind_municipios, particoes_com_unitario, max_label)
{
  

  ind_sedes <- sedes %>% 
    select( indice, unitario, sede, label ) %>% 
    #adicionando uma sede "zero", que significa levar o municipio para uma sede dele
    add_row( indice = as.integer(0), unitario = as.integer(1), sede = as.integer(0), label = as.integer(-1)  ) %>% 
    rename (ind_sede = indice)
  
  trocas <- inner_join( ind_sedes, ind_municipios ) %>% 
    mutate (ind_troca = cumsum(unitario) )
  
  particoes_com_unitario <- particoes_com_unitario %>% 
    rename (ind_municipio = indice) 
  
  
  trocas <- trocas %>% 
    select(ind_municipio, sede, label, ind_troca)
  
  particoes_com_unitario <- particoes_com_unitario %>% 
    select(sede, ind_municipio, label, cidades)
  
  
  particoes_com_troca_pre <- particoes_com_unitario %>% 
    crossing(trocas)  
  
  particoes_com_troca_pre <- particoes_com_troca_pre %>% 
    rename( sede.x = sede, sede.y = sede1, ind_municipio.x = ind_municipio, ind_municipio.y = ind_municipio1, label.x = label, label.y = label1 )
  

  label_mais_1 <- as.integer(max_label + 1)
  

#  particoes_com_troca_pre <- particoes_com_troca_pre %>% 
#    mutate ( sede_nova = if_else(ind_municipio.x != ind_municipio.y,  sede.x, if_else(sede.y != 0, sede.y, cidades)))
  
  particoes_com_troca_pre <- particoes_com_troca_pre %>% 
    mutate ( sede_nova = (ind_municipio.x != ind_municipio.y) * sede.x + (ind_municipio.x == ind_municipio.y) * (sede.y != 0) * sede.y + (sede.y == 0) * cidades, label_novo = (ind_municipio.x != ind_municipio.y) * label.x + (ind_municipio.x == ind_municipio.y) * (label.y != -1) * label.y + (ind_municipio.x == ind_municipio.y) * ( label.y == -1) * (cidades == sede.x) * label.x +  (ind_municipio.x == ind_municipio.y) * ( label.y == -1) * (cidades != sede.x) * label_mais_1 )
  
  
  
  #particoes_com_troca_pre <- particoes_com_troca_pre %>% 
  #  mutate(label_novo = if_else(ind_municipio.x != ind_municipio.y, label.x, if_else (label.y != -1, label.y, if_else(cidades == sede.x, label.x, label_mais_1  ) )))
  

#  particoes_com_troca_pre <- particoes_com_troca_pre %>% 
#    mutate(label_novo = (ind_municipio.x != ind_municipio.y) * label.x + (ind_municipio.x == ind_municipio.y) * (label.y != -1) * label.y + (ind_municipio.x == ind_municipio.y) * ( label.y == -1) * (cidades == sede.x) * label.x +  (ind_municipio.x == ind_municipio.y) * ( label.y == -1) * (cidades != sede.x) * label_mais_1)
  
  
  
  

  particoes_com_troca_pre <- particoes_com_troca_pre %>% 
        rename (sede = sede_nova, indice = ind_troca, label = label_novo) 
  
  
  particoes_com_troca <- particoes_com_troca_pre %>%  
    select (sede, cidades, indice, label)  
  
  
  particoes <- calcula_lucro_varias_especificacoes(particoes_com_troca)  
  
  #particoes_com_troca <- particoes_com_troca %>% 
  
  
  
  #  mutate(sede= as.factor(sede),cidades = as.factor(cidades))
  
  #particoes <- calcula_lucro_varias_especificacoes(particoes_com_troca)  
  
  
  
}

junta_pedacos_busca_local <- function(sedes, ind_municipios, particoes_com_unitario, max_label, n_pedacos)
{
  
  tam_pedaco <- as.integer(count(ind_municipios) %/% n_pedacos + 1)
  
  lucro_melhor_pedaco <- 0
  
  for (i in 1:n_pedacos)
  {
    ind_municipios_pedaco <- ind_municipios %>% 
      mutate(divisao = ind_municipio %/% tam_pedaco + 1) %>% 
      filter(divisao == i) %>% 
      select(ind_municipio, unitario)
    
    melhor_do_pedaco <- realiza_pedaco_passo_busca_local(sedes = sedes, ind_municipios = ind_municipios_pedaco, particoes_com_unitario = particoes_com_unitario, max_label = max_label)
    if (max(melhor_do_pedaco$lucro) > lucro_melhor_pedaco)
    {
      resposta <- melhor_do_pedaco
      lucro_melhor_pedaco <- max(melhor_do_pedaco$lucro)
    }
    

  }
  
  resposta
  
}


realiza_passo_busca_local <- function(particoes) {
  
  
  max_label <- max(particoes$label)
  

  particoes_com_unitario <- particoes %>% 
    mutate( unitario = as.integer(1)) %>% 
    mutate( indice = cumsum(unitario) )


  sedes <- particoes %>% 
    select(sede, label) %>% 
    distinct (sede, label) %>% 
    mutate( unitario = as.integer(1)) %>% 
    mutate( indice = cumsum(unitario) )

  
  #montando as trocas
  #cada municipio troca para cada regiao
  ind_municipios <- particoes_com_unitario %>% 
    select( indice, unitario ) %>% 
    rename (ind_municipio = indice)
  
  
  #particoes_2 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 2)
  
  particoes_1 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 1)
  
  #particoes_2 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 5)
  
  #particoes_3 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 10)
  
  #particoes_4 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 20)
  
  particoes_1
      
}


  




gera_mapa_da_particao <- function(particoes){
  
  
  
  particoes_com_nome <- particoes %>%
    mutate( iteracao_perturbacao = perturbacao * 1000 + iteracao  ) %>% 
    left_join( municipios_escopo, by = c("sede" = "CD") ) %>% 
    select (sede, cidades, Nome, iteracao, iteracao_perturbacao) %>% 
    rename( regiao = Nome ) %>% 
    mutate (regiao = fct_drop( regiao )) %>% 
    mutate( nome_sede = regiao ) %>% 
    mutate( nome_sede = (ifelse(sede == cidades, as.character(nome_sede), NA )) ) %>% 
    mutate(eh_sede = ifelse(sede == cidades, 1, 0) ) %>% 
    filter (iteracao %% intervalo == 1 | iteracao == n_distinct(.$iteracao)   )
  
  
  cidades <- get_brmap(geo = "City")
  
  UFs <- get_brmap(geo = "State")
  
  cidades <- sp::merge(cidades, particoes_com_nome, by.x = "City", by.y = "cidades"  )
  
  sedes_das_particoes <- particoes %>% 
    group_by(sede) %>% 
    select(sede, iteracao) %>% 
    filter(sede != -1) %>% 
    distinct(sede)
  
  
  
  sedes <- sp::merge(cidades, sedes_das_particoes, by.x = "City", by.y = "sede"  )
  
  #  obj_tm_shape <- tm_shape(cidades) +
  #    tm_fill(col ="regiao" ) +
  #    tm_text(text = "nome_sede") +
  #    tm_facets(along = "iteracao") +
  #    tm_shape(sedes) +
  #    tm_borders() +
  #    tm_facets(along = "iteracao")
  
  
  obj_tm_shape <- tm_shape(cidades) +
    tm_fill(col ="regiao" ) +
    tm_bubbles(size ="eh_sede", size.lim = c(0.1, 1.1), scale = 0.5  ) +     
    tm_text(text = "nome_sede") +
    tm_facets(along = "iteracao_perturbacao")
  
  
  
  
  
  animation_tmap(obj_tm_shape, "c:\\temp\\graficoanimado.mpg")
  
  
  
  
}

  
  

gera_video_das_particoes<- function(particoes, intervalo){
  
  
  particoes_grupo <- particoes %>% 
    group_by(iteracao, perturbacao) %>% 
    summarise(lucro_group = max(lucro)) %>% 
    arrange( desc(lucro_group) )
  
  ultima_iteracao_das_perturbacoes <- particoes %>% 
    group_by(perturbacao) %>% 
    summarise(ultima_iteracao = max(iteracao), lucro_perturbacao = max(lucro) ) %>% 
    select(perturbacao, ultima_iteracao, lucro_perturbacao )
    
  
  particoes_com_nome <- particoes %>%
    mutate( iteracao_perturbacao = perturbacao * 1000 + iteracao  ) %>% 
    left_join( municipios_escopo, by = c("sede" = "CD") ) %>% 
    select (sede, cidades, Nome, iteracao, iteracao_perturbacao, perturbacao, label) %>% 
    rename( regiao = Nome ) %>% 
    mutate (regiao = fct_drop( regiao )) %>% 
    mutate( nome_sede = regiao ) %>% 
    mutate( nome_sede = (ifelse(sede == cidades, as.character(nome_sede), NA )) ) %>% 
    mutate(eh_sede = ifelse(sede == cidades, 1, 0) ) %>% 
    left_join( ultima_iteracao_das_perturbacoes  ) %>% 
    filter (iteracao %% intervalo == 1 | iteracao ==  ultima_iteracao ) %>% 
    mutate (label = as.factor(label))
    
  
  cidades <- get_brmap(geo = "City")
  
  UFs <- get_brmap(geo = "State")
  
  cidades <- sp::merge(cidades, particoes_com_nome, by.x = "City", by.y = "cidades"  )
  
  sedes_das_particoes <- particoes %>% 
    group_by(sede) %>% 
    select(sede, iteracao) %>% 
    filter(sede != -1) %>% 
    distinct(sede)
  
  
  
  sedes <- sp::merge(cidades, sedes_das_particoes, by.x = "City", by.y = "sede"  )
  
#  obj_tm_shape <- tm_shape(cidades) +
#    tm_fill(col ="regiao" ) +
#    tm_text(text = "nome_sede") +
#    tm_facets(along = "iteracao") +
#    tm_shape(sedes) +
#    tm_borders() +
#    tm_facets(along = "iteracao")

  
  obj_tm_shape <- tm_shape(cidades) +
    tm_fill(col ="label" ) +
    tm_bubbles(size ="eh_sede", size.lim = c(0.1, 1.1), scale = 0.5  ) +     
    tm_text(text = "nome_sede") +
    tm_facets(along = "iteracao_perturbacao")
  
    
  print(particoes_grupo)   
  
  print(ultima_iteracao_das_perturbacoes)

  animation_tmap(obj_tm_shape, "c:\\temp\\graficoanimado.gif", delay = 50)
  
  
  
  
}


limpa_particoes <- function(particoes){
  #Jogando as cidades sem lucro para a regi„o para a partiÁ„o vazia
  #particoes <- particoes %>% 
  #  left_join(matriz_so_lucro_energia, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
  #  mutate(sede = if_else(is.na(lucro.y), as.integer(-1), sede ), label = if_else(is.na(lucro.y), as.integer(0), label ) ) %>% 
  #  rename(lucro = lucro.x) %>% 
  #  select(sede, cidades, lucro, label, iteracao, perturbacao, MoJo_ate_melhor, inv_prob_sede_existente) %>% 
  #  identity()

  
  particoes <- particoes %>% 
    left_join(matriz_so_lucro_energia, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
    mutate(sede = is.na(lucro.y) *as.integer(-1) + !is.na(lucro.y) * sede , label = is.na(lucro.y) *  as.integer(0) + !is.na(lucro.y) + label )  %>% 
    rename(lucro = lucro.x) %>% 
    select(sede, cidades, lucro, label, iteracao, perturbacao, MoJo_ate_melhor, inv_prob_sede_existente) %>% 
    identity()
  
  
  particoes
  
}
  
  

  

calculaMoJo <- function(A, B){
  
  
  A <- A %>% 
    mutate (sede = dense_rank(label), cidades = dense_rank(cidades))
  
  B <- B %>% 
    mutate (sede = dense_rank(label), cidades = dense_rank(cidades))
  
    
  #colocando as tags de B em A
  #Figura 2
  A <- A %>% 
    inner_join(B,by = c("cidades", "cidades")) %>% 
    transmute(sede = sede.x, tag = sede.y, cidades = cidades) 
  
  #calculando as interseÁıes v_i,j
  
  
  i <- new_tibble(list(i = 1:max(A$sede), unitario = rep(1,max(A$sede)))) 
  
  j <- new_tibble(list(j = 1:max(B$sede), unitario = rep(1,max(B$sede)))) 
  
  
  
  v <- i %>% 
    inner_join(j) %>% 
    left_join(A, c("i" = "sede", "j" = "tag" ) ) %>% 
    mutate(bateu = if_else(is.na(cidades),0,1)) %>% 
    group_by(i,j) %>% 
    summarise( v = sum(bateu)) %>% 
    identity()
  
  
  vmax <- v %>% 
    group_by(i) %>% 
    summarise(max = max(v))
  
  #Figura 4
  
  Ai_em_Gk <- v %>% 
    inner_join(vmax, by = c("i"="i", "v"="max" )) %>% 
    ungroup(i) %>% 
    transmute( i = i, k = j  ) %>% 
    mutate(unitario = 1) %>% 
    identity()
  
  
  vertices_bipartido <- Ai_em_Gk %>% 
    mutate(k = -k)
  
  
  grafo <- graph_from_data_frame(vertices_bipartido, directed = FALSE)
  
  
  matches <- maxmatching(grafo)
  
  matches.tibble <- as.tibble(matches$matching) %>% 
    rownames_to_column( var = "i") %>% 
    transmute( i = as.integer(i), k = as.integer(value) ) %>% 
    filter( i > 0) %>% 
    mutate( k = -1 * k) %>% 
    mutate(i = as.integer(i)) %>% 
    mutate(k = as.integer(k)) %>% 
    identity()
  
  As_sem_match <- matches.tibble %>% 
    filter ( (is.na(k)) ) %>% 
    left_join(Ai_em_Gk, by = c("i","i")) %>% 
    group_by(i) %>% 
    summarise( k = max(k.y) ) %>% 
    mutate( i = as.integer(i)) %>% 
    mutate(k = as.integer(k)) %>% 
    identity()
  
  #Figura 6
  
  Gs <- matches.tibble %>% 
    filter( !is.na(k)) %>% 
    union(As_sem_match) %>% 
    transmute(Ai = i, Gk = k ) %>% 
    identity()
  
  n_groups_nao_vazios <- Gs %>% 
    select(Gk) %>% 
    distinct() %>% 
    nrow()
  
  
  #empty cluster to each empty group
  
  
  maior_sede_A <- A %>% 
    select(sede) %>% 
    top_n(1, sede) %>% 
    distinct() %>%
    as.integer()
  
  grupos_vazios <- B %>% 
    select(sede) %>% 
    distinct() %>% 
    anti_join(Gs,by = c("sede" = "Gk") ) %>% 
    mutate(unitario = 1, contador = cumsum(unitario)) %>% 
    identity()
  
  clusters_vazios <- grupos_vazios %>% 
    transmute(unitario = 1) %>% 
    transmute(Ai = maior_sede_A + cumsum(unitario), contador = cumsum(unitario) ) %>% 
    identity()
  
  clusters_em_grupos_adicionais <- grupos_vazios %>% 
    inner_join(clusters_vazios) %>% 
    transmute(Ai = Ai, Gk = sede) %>% 
    identity()
  
  Gs <- Gs %>% 
    union(clusters_em_grupos_adicionais)
  
  #Figura 6 com elementos
  tags_com_Gs <- A %>% 
    inner_join(Gs, by = c("sede" = "Ai") )
  
  
  #For each group Gk, 
  #we move all objects tagged with Tk that belong to clusters in other groups to any cluster in Gk
  
  moves <- tags_com_Gs %>% 
    filter (tag != Gk) %>% 
    inner_join(Gs) %>% 
    group_by(cidades) %>% 
    summarise(Ai = min(Ai)) 
  
  n_moves <- nrow(moves)
  
  n_moves_formula <- nrow(A) - sum(vmax$max)
  
  
  #Calculando o n˙mero de joins
  
  n_clusters_A <- A %>% 
    select(sede) %>%
    distinct() %>% 
    nrow()
  
  n_joins <- n_clusters_A - n_groups_nao_vazios
  
  
  MoJo <- n_joins + n_moves
  

  MoJo  
  
  
}
  



#Execucao do algoritmo

#Loop principal 

#heuristicas = c("H1", "H2", "HRand")


particoes_iteracoes <- tibble(iteracao = integer(), perturbacao = integer(), sede = integer(), cidades = integer(), lucro = double(), label = integer(), MoJo_ate_melhor = integer(), inv_prob_sede_existente = integer())

#inv_prob_sede_existente_params <- c(5,7,10,15,20)

inv_prob_sede_existente_params <- c(8,10,15)

particoes_com_troca_pre <- tibble()

for (inv_prob_sede_existente in inv_prob_sede_existente_params)
{

  for (i in 1:1 ) #nrow(parametros) )
  {
    
    
    for (h in (1:1))
    {
      
      #Inserindo os custos, receitas e lucros para cada par de cidades
      matriz <- insere_receita_custo_lucro(i)

      
      matriz_so_lucro_energia <- matriz %>% 
        select(CD_x, CD_y, lucro, Energia_y)
      
      matriz_ordenada <- matriz %>% 
        arrange(CD_x, CD_y)
      
      matriz_so_lucro_energia_ordenada <- matriz_so_lucro_energia %>% 
        arrange(CD_x, CD_y)
      
      
      municipios_com_lucro <- matriz %>% 
        select(CD_y) %>% 
        distinct()
      
      municipios_escopo <- municipios_escopo_original %>% 
        semi_join(municipios_com_lucro, by = c("CD" = "CD_y"))
  
      #Preparando as particoes apos heuristica    
      particoes <- calcula_heuristica_1(municipios_escopo)
      
      #escolhendo melhor sede
      particoes <- calcula_lucro_escolhendo_sede(particoes)
      
      continua <- TRUE
      maior_lucro_busca_local<- first(particoes$lucro)
      maior_lucro <- first(particoes$lucro)
      melhor_solucao <- particoes
      
      iteracao <- 0
      
      
      
      perturbacao <- 0
      
      continua_perturbacao <- TRUE
      
      while (continua_perturbacao)
      {
        continua = TRUE
      
        maior_lucro_busca_local <- 0
        ##busca local
        while (continua){
          
        
          iteracao <- iteracao + 1
          
          particoes <- realiza_passo_busca_local( particoes )
          particoes <- particoes %>% select( sede, cidades, label)
          particoes <- calcula_lucro_escolhendo_sede( particoes )
          lucro_atual <- first( particoes$lucro )    
          if (lucro_atual > maior_lucro_busca_local){
            print("lucro atual")
            print(lucro_atual)
            print("maior_lucro_busca_local")
            print(maior_lucro_busca_local)
            print("param")
            print(inv_prob_sede_existente)
            print("perturba")
            print(perturbacao)
            print("iteracao")
            print(iteracao)
            maior_lucro_busca_local = lucro_atual
            melhor_solucao_busca_local <- particoes
            continua = TRUE 
            MoJo <- calculaMoJo(particoes, melhor_solucao)
            particoes <- particoes %>% 
              mutate(iteracao = iteracao) %>% 
              mutate(perturbacao = perturbacao) %>% 
              mutate(MoJo_ate_melhor = MoJo) %>% 
              mutate(inv_prob_sede_existente = inv_prob_sede_existente)
            
            particoes_limpa <- limpa_particoes(particoes)
            
            particoes_iteracoes <- particoes_iteracoes %>% union(particoes_limpa)
            
          }
          else{
            continua = FALSE
            if (maior_lucro_busca_local > maior_lucro){
              maior_lucro <- maior_lucro_busca_local
              melhor_solucao <- melhor_solucao_busca_local
            }
            lucro_atual <- maior_lucro_busca_local
            
          }
          
          #if (iteracao == 40){
          #  continua = FALSE
          #  if (maior_lucro_busca_local > maior_lucro){
          #    maior_lucro <- maior_lucro_busca_local
          #    melhor_solucao <- melhor_solucao_busca_local
          
          #}
            
          
          
          
          
    
        }
      
        #perturba particao
        #Cada municipio tem 1/20 de mudar para uma sede existente 
        #e 1/300 de mudar para uma sede qualquer 
        
  
        inv_prob_sede_qualquer = inv_prob_sede_existente * 30
  
        sedes_existentes <- particoes %>% 
          select(sede, label) %>% 
          distinct(sede, label) 
        
        n_sedes_existentes <- nrow(sedes_existentes)
        
        municipios_existentes <- particoes %>% 
          select(cidades) %>% 
          distinct( cidades)
        
        n_municipios_existentes = nrow(municipios_existentes)
          
        print("perturba")
        print(perturbacao)
        print("maior lucro perturbacao")
        print(maior_lucro_busca_local)
        print("maior_lucro")
        print(maior_lucro)
        
        particoes_perturbada <- melhor_solucao %>%  
          mutate (muda_sede_existente = (sample(1:inv_prob_sede_existente,size = n(), replace = TRUE) == 1 )) %>% 
          mutate (ind_sede_existente_destino = sample(1:n_sedes_existentes,size = n(), replace = TRUE)  ) %>% 
          mutate (muda_sede_qualquer = (sample(1:inv_prob_sede_qualquer,size = n(), replace = TRUE) == 1 )) %>% 
          mutate (ind_municipio_existente_destino = sample(1:n_municipios_existentes,size = n(), replace = TRUE)  ) %>% 
          mutate (sede = ifelse(muda_sede_existente, sedes_existentes$sede[ind_sede_existente_destino], sede )) %>% 
          mutate (label = ifelse(muda_sede_existente, sedes_existentes$label[ind_sede_existente_destino], label )) %>% 
          mutate (sede = ifelse(muda_sede_qualquer, cidades, sede )) %>% 
          mutate (label = ifelse(muda_sede_qualquer, cidades, label )) %>% 
          select(sede, cidades, label)
        
        particoes <- calcula_lucro_escolhendo_sede(particoes = particoes_perturbada )    
  
        #if (lucro_atual > maior_lucro){
        #  maior_lucro_busca_local = lucro_atual
        #  print(lucro_atual)
        #  continua_perturbacao = TRUE 
        #}
        #else{
        #  continua_perturbacao = TRUE
        #}
        
        
        if (perturbacao == 50){
          continua_perturbacao = FALSE
        }
        else 
        {      
          perturbacao <- perturbacao + 1
          iteracao <- 1
          
          MoJo <- calculaMoJo(particoes, melhor_solucao)
    
          particoes <- particoes %>% 
            mutate(iteracao = iteracao) %>% 
            mutate(perturbacao = perturbacao) %>% 
            mutate(MoJo_ate_melhor = MoJo) %>% 
            mutate(inv_prob_sede_existente = inv_prob_sede_existente)
  
          particoes_limpa <- limpa_particoes(particoes)
          
          
          particoes_iteracoes <- particoes_iteracoes %>% union(particoes_limpa)
        }
      }
      

      #gera_video_das_particoes (particoes_iteracoes, intervalo = 10 )
      

    }
  }
}
      
write.csv(particoes_iteracoes,"c:\\temp\\sede-8-10-15.csv")

    
    
      
    

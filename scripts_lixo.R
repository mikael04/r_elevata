###Aqui eu estava fazendo a junção e agrupando os demais valores (fora do top10) em "OUTRA"
######################################################
##### TOP categorias por negocios feitos
ng_top10 <- ng_rj_hist_lj_ven_lj_ngp_lj_pd_top10 %>%
  select(produto_categoria_id) %>%
  group_by(produto_categoria_id) %>%
  mutate(num_negocios = n()) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  arrange(desc(num_negocios)) %>%
  collect ()

##pega só os top10
top10 <- head.matrix(ng_top10, n=10)

top10_lj <- left_join(top10, categoria, by = c("produto_categoria_id"="categoria_id"))

##Alterando por causa do caracter especial
top10_lj$categoria_nome[top10_lj$produto_categoria_id == 	120191031173021] <- "CABEÇOTE"
top10_lj <- top10_lj[, -2]
top10_lj <- as.data.frame(top10_lj)


##filtrando antes, apenas tratores e colheitadeiras (produto_categoria = 1, 2, 3 e 4, como trator, colheitadeira, pulverizador e plantadeira, respectivamente, demais, recategorizar como "outros")
##Não preciso mais fazer esse filtro pq farei um "fill" no ggplot pelo nome da categoria, através da junção com a tabela categoria
#ng_rj_hist_lj_ven_lj_ngp_lj_pd$produto_categoria_id[ng_rj_hist_lj_ven_lj_ngp_lj_pd$produto_categoria_id != c(1,2,3,4)] <- 5
#categoria <- c("Trator", "Colheitadeira", "Pulverizador", "Plantadeira", "Outros")

ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat <- left_join(ng_rj_hist_lj_ven_lj_ngp_lj_pd, top10_lj, by=c("produto_categoria_id" = "produto_categoria_id"))
ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat$produto_categoria_id[is.na(ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat$categoria_nome)] <- "-1"
ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat$categoria_nome[is.na(ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat$categoria_nome)] <- "OUTRA"


#############################################################

## Aqui eu estava usando pra gerar os gráficos com vários campos, porém não é necessário no momento
############################################################
ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat <- left_join(ng_rj_hist_lj_ven_lj_ngp_lj_pd, top10_lj, by=c("produto_categoria_id" = "produto_categoria_id"))




##não estou selecionando empresa_id nem np_ativo no momento, nome e marcaremovido já que ele só pega de um deles e estou selecionando categoria
## np_quantidade também removidosjá que estou categorizando
ng_fat_cat <- ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat %>%
  select(negocio_id, negocio_negocio_situacao_id, produto_categoria_id, categoria_nome, np_valor) %>%
  group_by(categoria_nome, negocio_negocio_situacao_id) %>%
  distinct (categoria_nome, negocio_negocio_situacao_id, .keep_all = TRUE) %>%
  collect ()


p4 <- ggplot(ng_fat_cat, aes(categoria_nome, np_valor, fill=factor(negocio_negocio_situacao_id), label = np_valor)) +
  geom_col()+
  ggtitle("Total de valor por categoria") +
  theme (axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank(), axis.text.y = element_blank(), legend.title = element_blank())
  #+geom_text(position = position_stack(vjust = +0.5))
ggplotly(p4) %>%
  layout(showlegend = FALSE)
################################################################

## Primeira vez pra verificar quais são os top10
  ng_top10 <- ng_rj_hist_lj_ven_lj_ngp_lj_pd_top10 %>%
    select(produto_categoria_id, np_valor) %>%
    group_by(produto_categoria_id) %>%
    mutate(faturamento = sum(np_valor)) %>%
    distinct (produto_categoria_id, .keep_all = TRUE) %>%
    arrange(desc(faturamento)) %>%
    collect ()

    ### Gráfico de valor total  por categoria e situação de negócio
    #######################################################################
    ##não estou selecionando empresa_id nem np_ativo no momento, nome e marcaremovido já que ele só pega de um deles e estou selecionando categoria
    ## np_quantidade também removidosjá que estou categorizando
    ng_fat_cat <- ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat %>%
      select(negocio_id, negocio_negocio_situacao_id, produto_categoria_id, categoria_nome, np_valor) %>%
      group_by(categoria_nome, negocio_negocio_situacao_id) %>%
      distinct (categoria_nome, negocio_negocio_situacao_id, .keep_all = TRUE) %>%
      collect ()


    p4 <- ggplot(ng_fat_cat, aes(categoria_nome, np_valor, fill=factor(negocio_negocio_situacao_id), label = np_valor)) +
      geom_col()+
      ggtitle("Total de valor por categoria") +
      theme (axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank(), axis.text.y = element_blank(), legend.title = element_blank())
    +geom_text(position = position_stack(vjust = +0.5))
    ggplotly(p4) %>%
      layout(showlegend = FALSE)
    ########################################################################

    Gráficos Negócios por categoria  (HTML)
    #######################################################################
    ##Negócios por vendedor por tipo de máquina
    p4 <- ggplot(ng, aes(vendedor_nome, num_negocios, fill=factor(produto_categoria_id), label = num_negocios)) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
    geom_col(position = "stack") +
    xlab("Vendedores") +
    ylab("Número de produtos") +
    ggtitle("Tipos de máquinas em negociação, em 2020") +
    theme (axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank())+
    scale_fill_brewer(palette = "Dark2")+
    geom_text(position = position_stack(vjust = +0.5))

    ##Aqui eu estou removendo as legendas criadas pelo fill já que não consegui mudar elas de posição
    ##Se usar ggplot vai pro lugar, se usar o plotly tem q mover como fiz abaixo
    #p0 <- p0+theme (legend.position = "bottom") ->
    ggplotly(p4)
    ### Ticket médio por proposta

    ### Uma proposta tem n proposta_pagamento (cuidar as ativas, pp_ativo = 1)
    ##coleta todos proposta_pagamenmto
    proposta_pagamento <- tbl(con, "proposta_pagamento") %>%
      select(pp_id, pp_proposta_id, pp_modo_id, pp_forma_id, pp_valor, pp_ativo, pp_usado_id) %>%
      collect()

    proposta_pagamento <- proposta_pagamento %>%
      filter(pp_ativo == TRUE)

    proposta_produto <- tbl(con, "proposta_produto") %>%
      select(pp_id, pp_proposta_id, pp_produto_id, pp_quantidade, pp_valor, pp_ativo) %>%
      collect()

    proposta_produto <- proposta_produto %>%
      filter(pp_ativo == TRUE)

    produto <- tbl(con, "produto") %>%
      select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
      collect()

    pprod_ij_prod <- inner_join(proposta_produto, produto, by = c("pp_produto_id" = "produto_id"))

    ##Junção pra termos valor da proposta (preciso do negócio pra filtrar empresa e vendedores se preciso)
    p_ij_n_ij_pp <- inner_join(prop_ij_neg_ij_vend, proposta_pagamento, by = c("proposta_id" = "pp_proposta_id")) %>%
      arrange(pp_valor)

    ##filtro da empresa e também propostas finalizadas
    p_ij_n_ij_pp_empresa <- p_ij_n_ij_pp %>%
      filter(vendedor_empresa_id == empresa, proposta_status == 4)

    p_ij_n_ij_pp_empresa <- p_ij_n_ij_pp_empresa %>%
      group_by(proposta_id) %>%
      mutate (valor_proposta = sum(pp_valor)) %>%
      collect ()

    ##media geral da empresa
    total_empresa <- sum(p_ij_n_ij_pp_empresa$pp_valor)
    media_empresa <- round(total_empresa/nrow(p_ij_n_ij_pp_empresa), 2)

    ##Começando as junções pra chegar nas categorias
    p_ij_pprod_ij_prod <- inner_join(proposta, pprod_ij_prod, by = c('proposta_id' = 'pp_proposta_id')) %>%
      select(proposta_id, proposta_status, pp_produto_id, pp_quantidade, pp_valor, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
      filter (proposta_status == 4)

    ## Primeira vez pra verificar quais são os top8
    pr_top8_fat <- p_ij_pprod_ij_prod %>%
      select(produto_categoria_id, pp_valor) %>%
      group_by(produto_categoria_id) %>%
      mutate(fat = sum(pp_valor)) %>%
      mutate(n = n()) %>%
      distinct (produto_categoria_id, .keep_all = TRUE) %>%
      collect ()


    ##Isso aqui tudo é pra pegar o top 8 (antes top10, mas aparecia uma categoria que não queríamos), substituir os que não estão por -Outros e depois refazer a média
    top8_fat <- head.matrix(pr_top8_fat, n=8)

    ##Vou fazer um join pra pegar os nomes de cada categoria
    categoria <- tbl(con, "categoria") %>%
      select(categoria_id, categoria_nome) %>%
      collect()

    top8_fat_ij_cat <- inner_join(top8_fat, categoria, by = c("produto_categoria_id"="categoria_id"))
    top8_fat_ij_cat <- top8_fat_ij_cat[, -2:-4]
    top8_fat_ij_cat <- as.data.frame(top8_fat_ij_cat)

    ##Aqui estou juntando para agrupar primeiro através do na
    pr_top8_fat_aux <- left_join(pr_top8_fat, top8_fat_ij_cat, by=c("produto_categoria_id" = "produto_categoria_id"))
    pr_top8_fat_aux$produto_categoria_id[is.na(pr_top8_fat_aux$categoria_nome)] <- "-1"


    ##Depois de setar as linhas q tem nome de coluna = na (não estão no top8) e ter setado todas pra id = -1, faço a soma dos n e faturamentos
    n_out <- sum(pr_top8_fat_aux$n[which(pr_top8_fat_aux$produto_categoria_id=="-1")])
    fat_out <- sum(pr_top8_fat_aux$fat[which(pr_top8_fat_aux$produto_categoria_id=="-1")])

    ##E então, repasso o valor de volta para a coluna "outros", de faturamento total (de todas as categorias menos as q estão no top8) e número de vezes que aparecem
    pr_top8_fat_aux$n[pr_top8_fat_aux$produto_categoria_id=='-1'] <- n_out
    pr_top8_fat_aux$fat[pr_top8_fat_aux$produto_categoria_id=='-1'] <- fat_out

    ##E aqui removo as demais linhas, que já foram adicionadas a "Outros"
    pr_top8_fat_aux <- pr_top8_fat_aux[!is.na(pr_top8_fat_aux$categoria_nome),]

    ##Aqui só renomeio pra ver que a categoria outros tem um * representando a soma de todas as outras categorias também
    pr_top8_fat_aux$categoria_nome[(pr_top8_fat_aux$categoria_nome == "OUTRA")] <- "OUTRA *"


    ## Aqui já estõu fazendo a média das categorias
    pr_top8_fat_med <- pr_top8_fat_aux %>%
      select(categoria_nome, produto_categoria_id, fat, n) %>%
      group_by(produto_categoria_id) %>%
      mutate(fat_med = fat/n) %>%
      distinct (produto_categoria_id, .keep_all = TRUE) %>%
      collect ()


    fat_tot_categorias <- pr_top8_fat_med

    fat_tot_categorias <- fat_tot_categorias %>%
      mutate(med_emp = media_empresa) %>%
      collect()


    ## Gráfico 6 - Ticket médio por categoria e geral da empresa
    ax <- list(
      autotick = TRUE,
      title = "",
      showticklabels = TRUE)
    p3 <- plot_ly(fat_tot_categorias, type = "bar", x = ~categoria_nome, y = ~fat_med,
                  name = 'Ticket médio por categoria',
                  marker = list(color = 'lightblue'),
                  text = ~paste(categoria_nome,'<br>' , function_format_din(fat_med)),
                  hoverinfo = "text")

    p3 <- p3 %>%
      layout(barmode = 'identity', xaxis = ax, yaxis = ax)

    p3 <- p3 %>% add_trace(type = 'scatter', mode = 'markers+line', yaxis = 'y2',
                           name = 'Ticket médio da empresa',
                           x = ~categoria_nome,
                           y = ~med_emp,
                           line = list(color = 'red'),
                           text = ~paste('Ticket médio<br>' , function_format_din(med_emp)),
                           hoverinfo = "text",
                           marker = list(color = 'orange'))

    p3 <- p3 %>%
      layout(
        yaxis = list(side = 'left', title = 'Faturamento', showgrid = TRUE, zeroline = FALSE, title = ''),
        #range nos dois eixos iguais pra ficar na mesma proporção
        yaxis2 = list(overlaying = "y", showgrid = FALSE, zeroline = FALSE, showticklabels= F, range = c(0,400000)), yaxis = list(range = c(0,400000)),
        ##aqui eu ajusto onde quero que apareça a legenda
        legend = list(x=0.8, y=0.9)#)
      )
    if(dash == F){
      p3
    }


    if(teste == F){
      #tabelas
      rm(ax, categoria, fat_tot_categorias, p_ij_n_ij_pp_empresa, p_ij_pprod_ij_prod, pprod_ij_prod, pr_top8_fat, pr_top8_fat_aux,
         pr_top8_fat_med, produto, prop_ij_neg_ij_vend,proposta_produto, top8_fat, top8_fat_ij_cat)
      #variáveis
      rm(fat_out, media_empresa, n_out, total_empresa)
    }

    ##############################################

    ###Forma 1 de fazer o gráfico de linhas com faturamento anual (cortando os meses que não aparecem no ano atual)
    #######################################################################
    ##Criando um faturamento médio
    fat_med2ant_mes <- fat_2019_mes
    fat_med2ant_mes$ym_sum <- (fat_2018_mes$ym_sum+fat_2019_mes$ym_sum)/2

    ##Vou usar isso pra remover as linhas que não existem no 2020_mes (ainda não aconteceram)
    n_linhas <- nrow(fat_2020_mes)
    fat_med2ant_mes_rem <- fat_med2ant_mes[c(1:n_linhas),]
    fat_med2ant_mes_rem <- fat_med2ant_mes_rem[,-1]

    ##Pegando o de 2020 que será o primeiro a comparar
    fat_2020_med2ant_mes <- fat_2020_mes
    ## Renomeando pra ym_sum_ant e colocando tudo no mesmo data frame
    fat_2020_med2ant_mes[, "ym_sum_2ant"]<- fat_med2ant_mes_rem$ym_sum


    ##Repetindo para anos anteriores
    fat_2018_mes_rem <- fat_2018_mes[c(1:n_linhas),]
    fat_2018_mes_rem <- fat_2018_mes_rem[,-1]

    ## Renomeando pra ym_sum_ant e colocando tudo no mesmo data frame
    fat_2020_med2ant_mes[, "ym_sum_2018"]<- fat_2018_mes_rem$ym_sum

    ##Repetindo para anos anteriores
    fat_2019_mes_rem <- fat_2019_mes[c(1:n_linhas),]
    fat_2019_mes_rem <- fat_2019_mes_rem[,-1]

    ## Renomeando pra ym_sum_ant e colocando tudo no mesmo data frame
    fat_2020_med2ant_mes[, "ym_sum_2019"]<- fat_2019_mes_rem$ym_sum

    ##terei que fazer algumas mudanças pra automatizar o processo
    meses = c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
    meses_rem = meses[1:n_linhas]
    ## alterando pra números pra poder fazer da mesma forma
    fat_2020_med2ant_mes$ym <- as.integer(fat_2020_med2ant_mes$ym)
    fat_2020_med2ant_mes$ym <- with(fat_2020_med2ant_mes, cut(ym, breaks = c(0,1,2,3,4,5,6,7,8),
                                                                                  labels = meses_rem))
    ##########################################################################################################
    ##aqui eu estou alterando o joão paulo, que havia problemas com codificação (FORMA ANTIGA)
    ng_ij_vn_ij_np_fat$vendedor_nome[ng_ij_vn_ij_np_fat$negocio_vendedor_id == 45] <- "JOÃO PAULO"
    #Arrumando encoding (FORMA NOVA, RESOLVE TUDO DE UMA VEZ)
    vendedor$vendedor_nome <- func_fmt_char_esp(vendedor$vendedor_nome)

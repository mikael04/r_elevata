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

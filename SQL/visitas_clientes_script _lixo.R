### Tabelas e graficos de Clientes

cliente <- tbl(con,'cliente') %>%
  select (cliente_id, cliente_vendedor_id, cliente_empresa_id, cliente_data_cadastro, cliente_cidade, cliente_ultima_visita) %>%
  filter(cliente_empresa_id == empresa) %>%
  collect()

##Clientes cadastrados por vendedor ate 2020
cli_p_v <- cliente %>%
  filter(cliente_data_cadastro < ano_atual) %>%
  group_by(cliente_vendedor_id) %>%
  mutate(n_clientes = n()) %>%
  distinct(cliente_vendedor_id, .keep_all = T) %>%
  select(cliente_vendedor_id, n_clientes) %>%
  ungroup() %>%
  arrange(cliente_vendedor_id)

##Juncao pra pegar nome do vendedor
#cli_p_v <- inner_join(cli_p_v, vendedor, by = c('cliente_vendedor_id' = 'vendedor_id')) %>%
#  select(cliente_vendedor_id, vendedor_nome, n_clientes) %>%
#  arrange(cliente_vendedor_id)

##Clientes cadastrados em 2020 por vendedor
cliente_2020 <- cliente %>%
  filter(cliente_data_cadastro >= ano_atual)

cli_p_v_2020 <- cliente_2020 %>%
  group_by(cliente_vendedor_id) %>%
  mutate(n_clientes_2020 = n()) %>%
  distinct(cliente_vendedor_id, .keep_all = T) %>%
  select(cliente_vendedor_id, n_clientes_2020) %>%
  ungroup() %>%
  arrange(cliente_vendedor_id)

##Juncao pra pegar nome do vendedor
cli_p_v_2020 <- inner_join(cli_p_v_2020, vendedor, by = c('cliente_vendedor_id' = 'vendedor_id')) %>%
  select(cliente_vendedor_id, vendedor_nome, n_clientes_2020)

##Juntando anos anteriores + 2020 em uma tabela (pro plotly)
cli_p_v_t_2020 <- left_join(cli_p_v_2020, cli_p_v, by = c('cliente_vendedor_id')) %>%
  select (cliente_vendedor_id, vendedor_nome, n_clientes, n_clientes_2020)

### Grafico c0 - Distribuicao de clientes cadastrados por vendedor

c0 <- plot_ly(cli_p_v_t_2020, x = ~vendedor_nome, y= ~n_clientes, type = 'bar',
              name = 'Até 2020')
c0 <- c0 %>%
  add_trace(y= ~n_clientes_2020, name = 'Em 2020')
c0 <- c0 %>%
  layout(barmode = 'grouped',
         xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
         yaxis = list(title = ''))

if(dash == F){
  c0
}

if(teste == F){
  #tabelas
  rm(cli_p_v, cli_p_v_2020, cli_p_v_t_2020);
  #variáveis
  rm();
}

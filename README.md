# r_elevata
Scripts R para análise de dados

Subdividido em:

* Dashs -> Onde são montadas as dashs (e rodado os scripts), divididos por setores (geral, marcas, negócios, propostas e visitas_clientes)

* scripts -> usados para testes (mesmos das dashs)

* scripts_geradores -> geram arquivos, podendo ser, puxar os dados do banco de dados (e escrevê-los em csvs, gerador_csvs), gerar todas as dashs (gerador_rel), gerar csv de empresas ativas

## dash_geral e script_geral
  Scripts e gráficos referentes as visões mais gerais da empresa.
  
  ### Tabelas:
  
  Usa as tabelas negocio, vendedor, negócio_produto (valores de cada negócio), historico_negocio_situacao (filtrar negócios apenas por última alteração), cliente, visita_cliente (já filtrado por ano, usado para medir número de visitas), visita_status (pegar nome do status), visita_status_empresa (combinar com visita_status para ter nome do status da visita).
  
  ### Dash (abas):
  
    - Funil de vendas.
      - Funil da empresa, em valor financeiro por status do negócio (gráfico n9)
      - Valor financeiro dos pedidos fechados no ano atual (gráfico de pizza n10)
      - Valor financeiro dos pedidos fechados no último mês (gráfico de pizza n11)
    - Faturamento ano a ano
      - Faturamento mês a mês do ano atual até dois anos anteriores (gráfico n12)
      - Distribuição de cadastro dos clientes, cadastro de visitas e cadastro de negócios no ano atual (gráfico c3)
      - Combinados (n12 e c3, subplot)
      
## dash_marcas, marcas_k e script_marcas, marcas_k
  
  Scripts, gráficos e mapas referentes as marcas. _k é para empresas que não se enquadram na categoria de tratores e colheitadeiras (atualmente 78 - komatsu e 79 - pampeiro caminhões e peças)
  
  Esta dash apresenta mapas, esses mapas usam a API openstreemap através da biblioteca leaflet, os ícones plotados (distribuição das marcas) foi pesquisado na internet e adaptado para encaixar no mapa (pasta Icons, nome da marca e _r, dentro da dash é feita a junção do ícone com a marca, através do ID, e selecionado o tamanho do ícone).
  
  ### Tabelas:
  
  Usa as tabelas cliente, parque_máquina (distribuição do parque de máquinas/inventário), marca (nome da marca), marca_categoria (categoria de cada marca), negocio (agora para pegar os faturamentos de cada marca), vendedor (para poder separar por empresa), negocio_produto (novamente, faturamento), produto (juntar a marca, categoria e empresa), historico_negocio_situacao (filtrar negócios apenas por última alteração), categoria (pega nome de categoria e apenas categorias ativas).
    
  ### Dash (abas):
  
    - Top 10 categorias
      - Valor financeiro em negócios cadastrados no ano (n4)
      - Valor financeiro em negócios faturados no ano (n5)
    - Marcas (tratores)/ para marcas_k, apenas Marcas
      - Mapa das marcas (mapa m1_t)
      - Distribuição das marcas (m2_t)
    - Marcas (colheitadeiras)/ para marcas_k, não tem uma segunda aba
      - Mapa das marcas (mapa m1_c)
      - Distribuição das marcas (m2_c)
      
## dash_negocios e script_negocios

  Scripts e gráficos referentes aos negócios.
  
  Nesta dash eu uso um pequeno "truque", para organizar a aba de Idade dos negócios, eu acabo usando uma sidebar ({.sidebar}) já que a lib usada (flexdashboard) é limitada nas opções de montar a dash em si. Também uso valueBox (pequenas caixas com título e valores) para apresentar algumas informações.
  
  ### Tabelas:
  
  Usa as tabelas negocio, vendedor, negócio_produto (valores de cada negócio), historico_negocio_situacao (filtrar negócios apenas por última alteração), produto (nome do produto).
  
  ### Dash (abas):
  
    - Valor Financeiro dos negócios
      - Valor financeiro dos negócios cadastrados por vendedor no ano atual (separados por status atual do negócio)
      - Valor financeiro dos negócios fechados por vendedor no ano atual (separados por status atual do negócio)
    - Idade dos negócios
      - Idade dos negócios abertos, por vendedor, por idade do negocio (n6)
      - valueBox (4) de cálculo de tempo médio, para os negócios, separado por status. Sendo estes, faturado, financiamento não aprovado, desistência do cliente e perdemos para a concorrência.
      - Negócios abertos da empresa agrupados por idade (n7, formato de pizza)

## dash_proposta e script_proposta

  Scripts e gráficos referentes as propostas.
  
  ### Tabelas:
    
  Usa as tabelas proposta, negocio, vendedor, proposta_pagamento (para pegar modo e forma de pagamento, além do valor da proposta), negócio_produto (valores de cada negócio), produto (nome do produto), proposta_produto (pega a id da proposta, id do produto e as quantidades/valores), categorias (nome das categorias), proposta_modo_forma (usado pra filtrar a empresa).
  ### Dash (abas):
      - Valor Financeiro de propostas
        - Ticket médio das propostas (p3 só novos, p4 novos e usados), temos as barras mostrando das categorias, e uma linha com pontos (lines+markers) mostrando o ticket médio de toda a empresa
        - Instituição financeira (proporção do valor financeiro, p6, modos de pagamento, pizza)
        - Forma de pagamento (proporção do valor financeiro, p7, formas de pagamento, pizza)
      - Volume de propostas
        - Volume de propostas por tipo, por vendedor no ano atual (p0)
        - Valor financeiro por status no ano atual (p1)
        - Proporção do recebimento de usados nas propostas (p2, pizza)

## dash_visitas_clientes e script_visitas_clientes

  Scripts e gráficos referentes as visitas e clientes.
  
  Esta dash apresenta mapas, esses mapas usam a API openstreemap através da biblioteca leaflet.
  
  ### Tabelas:
  Usa as tabelas visita_cliente (já filtrado por ano, usado para medir número de visitas), visita_resultado (nome do resultado da visita), visita_resultado_empresa (para filtrar as empresas), visita_status (pegar nome do status), visita_status_empresa (combinar com visita_status para ter nome do status da visita), vendedor, clientes (pegar informações de data de cadastro, empresa e última visita), negócio (contar negócios feitos vendedor no ano, quantas visitas cada cliente que teve negócios recebeu e quantas visitas cada cliente que não teve negócios recebeu -> histograma de distirbuição clientes por visitas, clientes com negócios (clientes com visitas em anat)).   
      
  ### Dash (abas):
      - Volume de visitas
        - Distribuição dos motivos das visitas por status, por vendedor no ano atual (v0)
        - Distribuição dos resultados das visitas por status, por vendedor no ano atual (v1)
      - Clientes
        - Sidebar explicando os histogramas
        - Histograma de clientes sem negócio (c5)
        - Histograma de clientes sem negócio (c4)

## dic_variaveis.txt

A ideia é deixar um resumo do que cada variável (geralmente dataframes) é, com um resumo, de onde vem ou para que é usada.

Atualmente incompleto.

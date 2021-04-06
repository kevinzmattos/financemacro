mat_constr <- 
  get_sidra(api='//t/3415/n1/all/v/1195/p/all/c11046/40312/d/v1195%201') %>% # Pegando a tabela atravÃ©s do API do SIDRA
  mutate(Data = parse_date(`Mês (Código)`, format = '%Y%m')) %>% # transformando o objeto em formato de data
  select(Data, Valor) %>% # seleciona apenas as colunas discriminadas
  as_tibble() # deixa no formato ideal para analise de dados



teste <- filter(mat_constr, month(Data) %in% c(1,5,9))
## Plotando o grÃ¡fico

filter(mat_constr, Data > '2011-12-01') %>%
  ggplot(aes(x=Data, y =Valor)) +
  geom_line(color = '#990000', size = 1.3) +
  theme_clean() +
  scale_x_date(labels = date_format("%b/%y"), date_breaks = "4 month") +
  coord_cartesian(xlim = c(as.Date("2012-03-01"),as.Date("2020-07-01")))+
  theme(axis.text.x = element_text(angle=90))+
  labs(x='', y='', 
       title='Materiais de construção: volume de vendas do comércio varejista',
       subtitle = 'Índice base fixa com ajuste sazonal (Base: 2014 = 100) (número índice)',
       caption='Fonte: IBGE - Elaboração: UFABC Finance')

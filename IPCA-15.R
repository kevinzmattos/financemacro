
library(sidrar)
library(tidyverse)
library(ggthemes)
library(scales)
library(tstools)



## coletando os dados e tratando

teste <- get_sidra(api = '/t/3065/n1/all/v/1117/p/all/d/v1117%2013') %>%
  mutate( date = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  rename( indice = Valor) %>%
  mutate(inflacao_mensal = (indice/lag(indice, 1)-1)*100,
         inflacao_anual = (indice/lag(indice,12)-1)*100,
         inflacao_acum = acum_i(indice,10)) %>%
  select(date,indice, inflacao_mensal, inflacao_anual, inflacao_acum)




### Metas de Inflação

meta = c(rep(4.5,12*7-11), rep(4.25, 12), rep(4, 12))
meta_max = c(rep(4.5+2,12*5-11), rep(4.5+1.5,12*2),
             rep(4.25+1.5,12), rep(4+1.5, 12))
meta_min = c(rep(4.5-2,12*5-11), rep(4.5-1.5,12*2),
             rep(4.25-1.5,12), rep(4-1.5, 12))
data_meta = 
  tibble(date = seq(as.Date('2012-12-01'), as.Date('2020-12-01'),
                    by='1 month'),
         meta_min = meta_min,
         meta = meta,
         meta_max = meta_max)

## plotando

teste %>%
  filter(date > '2017-12-01') %>%
  ggplot(aes(x= date)) +
  geom_line(aes(y=inflacao_anual, colour = 'IPCA-15 %a.a'), size = 1.5) +
  geom_ribbon(data=data_meta,
              aes(ymin=meta_min, ymax=meta_max), 
              colour='grey70', alpha=0.1, linetype='dotted', size=1.5) +
  geom_line(data=data_meta,
            aes(y=meta, colour='Meta'), size=1.5 , linetype='dotted')+
  theme_classic() +
  scale_x_date(labels = date_format("%b/%y"), 
               date_breaks = "1 month")+
  coord_cartesian(xlim = c(as.Date("2018-02-01"),as.Date("2020-09-01")))+
  scale_colour_manual('',
                      values=c('IPCA-15 %a.a'='#01a2d9',
                               'Meta' = 'red'))+
  theme(axis.text.x = element_text(angle=90), legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))+
  labs(x='', y='%', 
       title='IPCA-15 ',
       caption='Fonte: IBGE - Elaboração: UFABC Finance')

  


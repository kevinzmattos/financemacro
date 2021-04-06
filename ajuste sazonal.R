Sys.setenv(X13_PATH = 'C:/Users/kevin/OneDrive/Documentos/UFABC/x13ashtml')
setwd('C:/Users/kevin/OneDrive/Documentos/UFABC')

library(seasonal)
library(tidyverse)
library(ggthemes)
library(scales)
library(lubridate)
library(tstools)

checkX13()


ibc_br <- read_delim("ibc-br.csv", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",")) %>%
  mutate(date = dmy(data)) %>%
  select(date, valor) %>%
  as.matrix()

tabela <- ts(ibc_br, freq = 12, start =c(2003,01))

ajuste <- seas(x = tabela)

view(tabela)




ibc_saz <- series(ajuste, 's11') 
plot(series(ajuste, 's11'),col='red' ) 
par(new= TRUE)
  plot(data)
write.csv(ibc_saz, "C:/Users/kevin/OneDrive/Documentos/UFABC/Ibc-br-sazonal")

values <- seq(from = as.Date("2003-01-01"), to = as.Date("2020-08-01"), by = 'month')
  
ibc <- cbind(Ibc.br.sazonal, values, dados$valor)
ibc <- ibc[-1] 
ibc <- `colnames<-`(ibc, c('valor ajustado','data','valor sem ajuste'))

ibc %>% filter(data > '2018-08-01') %>%
ggplot(aes(x=data)) +
  geom_line(aes(y=`valor ajustado`, col= 'valor ajustado'), size = 1)+
  geom_line(aes(y=`valor sem ajuste`, col = 'valor sem ajuste'), size = 1) +
  theme_clean() +
  scale_x_date(labels = date_format("%Y/%b"), date_breaks = "3 month", limits = c(as.Date("2018-08-01"),as.Date("2020-08-01"))) +
  theme(axis.text.x = element_text(angle=90))+
  theme(legend.position = 'top')+
  scale_colour_manual('',
                      values=c('valor ajustado'='#990000',
                               'valor sem ajuste'='#014d64'))+
  labs(x='', y='', 
       title='Índice de Atividade Econômica do Banco Central –Brasil (IBC-Br)',
       caption='Grupo de estudos macroeconômicos da UFABC Finance') 




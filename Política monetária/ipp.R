## Teste Indice de preços ao produtor

### ESTE ARQUIVO POSSUI UMA SÉRE DE TESTES ###########
#### COM DADOS DO IPP, IPCA, ÍNDICE DE COMMODITIES E CÂMBIO #######

library(tidyverse)
library(sidrar)
library(ggthemes)
library(scales)

ipp <- get_sidra(api = "/t/6903/n1/all/v/10008/p/all/c842/all/d/v10008%205") %>%
  mutate(Data = parse_date(`Mês (Código)`, format="%Y%m"))

ipp_f <- ipp %>%
  mutate(Valor_2 = (Valor/105.1923)*100)%>%
  select( Data, `Indústria geral, indústrias extrativas e indústrias de transformação e atividades (CNAE 2.0)`, Valor_2) %>%
  
  spread(key = `Indústria geral, indústrias extrativas e indústrias de transformação e atividades (CNAE 2.0)`, value = Valor_2) 




ipp_f %>%
  filter(Data > "2019-11-01") %>%
  ggplot(aes(x=Data, y= `Indústria Geral`))+
  geom_line(size=1, colour = "#990000") +
  geom_label( 
    data= ipp_f %>% filter(Data == "2021-01-01"), # Filter data first
    aes(label= round(`Indústria Geral`,1))
  )+
  theme_clean()+
  theme(legend.position = 'none', axis.text.x = element_text(angle=0))+
  scale_x_date(labels = date_format("%b/%y"), date_breaks = "1 month") +
  labs(x='', y='',
       title = "Evolução do IPP",
       subtitle = "2019=100",
       caption='Fonte: IBGE - Elaboração: UFABC Finance')

### IPCA

ipca_ind <- get_sidra(api="/t/1737/n1/all/v/2266/p/all/d/v2266%2013") %>%
  mutate(Data = parse_date(`Mês (Código)`, format="%Y%m")) %>%
  filter(Data > "2019-11-01") %>%
  mutate(Valor = (Valor/5320.25)*100 ) %>%
  select(Data, Valor)

ipca_ind %>%
  ggplot(aes(x=Data, y= Valor))+
  #geom_line(size=1)+
  geom_line(size=1, colour = "#00cc66") +
  theme_clean()+
  theme(legend.position = 'none', axis.text.x = element_text(angle=90))+
  scale_x_date(labels = date_format("%b/%y"), date_breaks = "1 month") +
  labs(x='', y='', 
       title="IPCA",
       caption='Fonte: IBGE - Elaboração: UFABC Finance')
  

### apend

ipp_ipca <- 
  inner_join(ipp_f,ipca_ind, by="Data") %>%
  select(Data,`Indústria Geral`, Valor)
  #pivot_longer(cols = c(`Indústria Geral`, Valor))



ipp_ipca %>%
  ggplot(aes(x=Data))+
  geom_line(aes(y=Valor), colour = "#737373", size=1)+
  geom_line(aes(y=`Indústria Geral`), colour= "#990000", size = 1)+
  geom_label( data= ipp_ipca %>% filter(Data == "2021-01-01"), # Filter data first
    aes(label= round(`Indústria Geral`,1), y=`Indústria Geral`))+
  theme_clean()+
  geom_label( data= ipp_ipca %>% filter(Data == "2021-01-01"), # Filter data first
              aes(label= round(Valor,1), y=Valor))+
  theme_clean()+
  theme(legend.position = 'top', axis.text.x = element_text(angle=90))+
  scale_x_date(labels = date_format("%b/%y"), date_breaks = "1 month") +
  labs(x='', y='', 
       title = "IPP e IPCA",
       subtitle = "2019 = 100",
       caption='Fonte: IBGE - Elaboração: UFABC Finance')


### Cotação dolar

library(rbcb)
library(lubridate)

cambio <- rbcb::get_series(c(USDBRL = 1), last = 310) %>%
  mutate(Valor = (USDBRL/4.0307
)*100)


cambio %>%
  ggplot(aes(date, Valor))+
  geom_line()


library(lubridate)
library(dplyr)

#round dates down to month
cambio$month <- floor_date(cambio$date, "month")

#find mean sales by week

dias <- seq(as.Date("2020-01-01"),length=14,by="months")-1
dias

cambio_2 <- cambio %>%
  group_by(month) %>%
  arrange(date) %>%  
  slice(n()) %>%
  select(month,USDBRL, Valor) %>%
  `colnames<-`(c("Data","USDBRL","Cambio"))
  


### append

ipp_ipca_cb <- 
  inner_join(ipp_ipca,cambio_2, by="Data") %>%
  select(Data,`Indústria Geral`, Valor, Cambio)


ipp_ipca_cb %>%
  ggplot(aes(x=Data))+
  geom_line(aes(y=Valor, colour = "IPCA"), size=1.2)+
  geom_line(aes(y=`Indústria Geral`,colour= "IPP"),size=1.2) +
  geom_line(aes(y=Cambio, colour="USDBRL"),size=1.2)+
  theme_clean()+
  scale_color_manual('',
                     values=c('IPCA'='#0099ff',
                              'IPP' = '#00cc66',
                              'USDBRL'='red'))+
  
  theme(legend.position = 'top', axis.text.x = element_text(angle=0),
        legend.background = element_blank())+
  scale_x_date(labels = date_format("%b/%y"), date_breaks = "1 month") +
  
  labs(x='', y='',
       title= "Comparação entre Índices de preços e a cotação do dólar",
       subtitle = "Dezembro 2019 = 100",
       caption='Fonte: IBGE - Elaboração: UFABC Finance')

  
#### commodities

com <- rbcb::get_series(c("valor"=27574), last=15) %>%
  mutate(indice = (valor/205.54)*100) %>%
  select(date, indice) %>%
  `colnames<-`(c("Data","Indice"))


com %>%
  ggplot(aes(date, indice))+
  geom_line()

### append

df_comp <- ipp_ipca_cb %>%
  inner_join(com, by= "Data" )

## plotando 4 linhas ####
df_comp %>%
  ggplot(aes(x=Data))+
  geom_line(aes(y=Valor, colour = "IPCA"), size=1.2)+
  geom_line(aes(y=`Indústria Geral`,colour= "IPP"),size=1.2) +
  geom_line(aes(y=Cambio, colour="USDBRL"),size=1.2)+
  geom_line(aes(y=Indice, colour="Commodities"),size=1.2)+
  theme_clean()+
  scale_color_manual('',
                     values=c('IPCA'='#0099ff',
                              'IPP' = '#00cc66',
                              'USDBRL'='red',
                              "Commodities" = "orange"))+
  
  theme(legend.position = 'top', axis.text.x = element_text(angle=0),
        legend.background = element_blank())+
  scale_x_date(labels = date_format("%b/%y"), date_breaks = "1 month") +
  
  labs(x='', y='',
       title= "Comparação entre Índices de preços, Commodities e Dólar",
       subtitle = "Dezembro 2019 = 100",
       caption='Fonte: IBGE - Elaboração: UFABC Finance')


#### dando um gather
colnames(df_comp) <- c("Data",'IPP','IPCA',
                         'USDBRL', "Commodities")

df_longer <- df_comp %>%
  pivot_longer(c(IPP,IPCA,USDBRL,Commodities))



df_longer %>%
  transform(name = factor(name, levels = c('IPP','IPCA',
                                           'USDBRL', "Commodities"))) %>%
  ggplot(aes(x=Data, y=value,colour=name))+
  geom_line(size=1.2)+
  theme_clean()+
  facet_wrap(~name, scales= "free")+
  theme(legend.position = 'none', axis.text.x = element_text(angle=90), legend.background = element_blank(), legend.title = element_blank())+
  scale_x_date(labels = date_format("%b/%y"), date_breaks = "1 month") +
  labs(x='', y='',
       title= "Comparação entre Índices de preços, Commodities e Dólar",
       subtitle = "Dezembro 2019 = 100",
       caption='Fonte: IBGE - Elaboração: UFABC Finance')


### calculando as variações

ipp_var <-get_sidra(api="/t/6903/n1/all/v/all/p/all/c842/all/d/v1394%202,v1395%202,v1396%202,v10008%205") %>%
  mutate(Data = parse_date(`Mês (Código)`, format="%Y%m")) %>%
  select(Data,`Indústria geral, indústrias extrativas e indústrias de transformação e atividades (CNAE 2.0)`,Variável, Valor)


ipp_var_t <- ipp_var %>%
  spread(key=Variável, value = Valor) %>%
  `colnames<-`(c("Data","Setores","Índice","var_acum", "var_men","var_trim"))%>%
  filter(Setores %in% c("Indústria Geral","B Indústrias Extrativas","C Indústrias de Transformação"))

ipp_var_12<- ipp_var_t %>%
  select(Data, Setores, var_men) 


ipp_var_12 %>%
  filter(Data > "2017-12-01" & Data < "2021-01-01") %>%
  ggplot(aes(x=Data, colour=Setores))+
  geom_line(aes(y=var_trim)) +
  facet_wrap(~Setores, scales="free") +
  theme_clean()+
  theme(legend.position = 'none', axis.text.x = element_text(angle=90), legend.background = element_blank(), legend.title = element_blank())+
  scale_x_date(labels = date_format("%b/%y"), date_breaks = "4 month") +
  labs(x='', y='%',
       title= "Varição acumulada no ano do IPP ",
       caption='Fonte: IBGE - Elaboração: UFABC Finance')


ipp_var_12_long <- ipp_var_12 %>%
  spread(key= Setores, value = var_men) %>%
  `colnames<-`(c("Data","Indústrias Extrativas","Indústrias de Transformação","Indústria Geral"))

ipp_var_12 %>%
  filter(Data >"2019-12-01")%>%
  ggplot(aes(x=Data, y= var_men, fill= Setores))+
  geom_col(position = "dodge")+
  #geom_col(aes(y=`Indústria Geral`,fill= "Indústria Geral"), size=1.2)+
  #geom_col(aes(y=`Indústrias Extrativas`,fill = "Indústrias Extrativas"),size=1.1) +
  #geom_col(aes(y=`Indústrias de Transformação`, fill="Indústrias de Transformação"),size=1.1)+
  theme_clean()+
  scale_color_manual('',
                     values=c('Indústria Geral' = 'grey',
                              'Indústrias Extrativas'='#990000',
                              "Indústrias de Transformação" = "orange"))+
  
  theme(legend.position = 'top', axis.text.x = element_text(angle=0),
        legend.background = element_blank())+
  scale_x_date(labels = date_format("%b/%y"), date_breaks = "1 month") +
  
  labs(x='', y='%',
       title= "Variação mensal do IPP",
       #subtitle = "Dezembro 2019 = 100",
       caption='Fonte: IBGE - Elaboração: UFABC Finance')




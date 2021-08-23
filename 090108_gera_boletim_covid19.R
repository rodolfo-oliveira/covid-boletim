# Monirando o covid-19 na região da BR-319 #
# Eliana Lins Morandi, CeDHE-FGV #



# Dados da API covid-19 no Brasil.io
# https://github.com/turicas/covid19-br/blob/master/api.md
rm(list = ls())
#library(httr); library(jsonlite)
library(dplyr); library(sp); library(ggplot2); library(tidyverse);library(zoo)
source("901_auxiliar_functions.R")

#### COLETANDO OS DADOS ####
### coletando os dados do Amazonas ###
#am <- data.frame(matrix(nrow = 0, ncol = 12))
#url <- 'https://brasil.io/api/dataset/covid19/caso/data?is_last=False&state=AM'
#count<-0
#while(count==0) {
#  x <- fromJSON(url,
#                simplifyDataFrame = T, flatten = T)
#  if(is.null(x$`next`) == T){count <- 1}
#  url <- x$`next`
#  x <- x$results
#  am <- rbind(am, x) 
#}

### coletando os dados de rondônia ###
#ro <- data.frame(matrix(nrow = 0, ncol = 12))
#url <- 'https://brasil.io/api/dataset/covid19/caso/data?is_last=False&state=RO'
#count<-0
#while(count==0) {
#  x <- fromJSON(url,
#                simplifyDataFrame = T, flatten = T)
#  if(is.null(x$`next`) == T){count <- 1}
#  url <- x$`next`
#  x <- x$results
#  ro <- rbind(ro, x) 
#}
#rm(count, url, x)

# montando banco
#df_covid <- am %>%
#  bind_rows(ro) %>%
#  mutate(CD_GEOCODM = city_ibge_code) %>%
#  select(CD_GEOCODM, city, confirmed, confirmed_per_100k_inhabitants,
#         deaths, death_rate, estimated_population_2019, date)

#### MONTA BANCO , DEVIDO ÀS ALTERACOES NA API ####
df_covid <- baixa_dados(estados = c("11","13"))
 
#### Gráfico de linhas ####
# definindo municípios do entorno:

AMRO <- get_entorno(mapa = get_mapa())
minhalista<-AMRO$id

# empilhando todos os municípios do entorno:----
entornoBR <- data.frame()

for (i in minhalista){
  x<-df_covid[as.character(df_covid$CD_GEOCODM) == i,]
  entornoBR <- rbind(entornoBR, x)
}
rm(x,i)

entornoBR %>%
  filter(is.na(CD_GEOCODM) == F) %>%
  select(city, date, new_confirmed, confirmed, confirmed_per_100k_inhabitants, new_deaths,deaths, death_rate, estimated_population_2019) %>%
  mutate(date = as.Date(x = date),
         city = as.factor(city)) -> entornoBR

meses <- c("janeiro",
           "fevereiro",
           "março",
           "abril",
           "maio",
           "junho",
           "julho",
           "agosto",
           "setembro",
           "outubro",
           "novembro",
           "dexembro")

# comparando dados do ultimo mes (ultimos 30 dias):----


filter(entornoBR, date == max(entornoBR$date)-30 | date == max(entornoBR$date)) %>% 
  unique() %>%
  select(city, date, confirmed) %>% 
  unique() %>% # a data mais recente é sempre a de cima para cada município
  spread(key = date, value = confirmed) -> tx_evolucao
tx_evolucao$diferenca <- tx_evolucao[,3] - tx_evolucao[,2]
tx_evolucao$tx <- paste0(round(100*tx_evolucao$diferenca/tx_evolucao[,2], 2),'%')  
colnames(tx_evolucao) <- c('Município', paste0('casos até ',
                                               format(as.Date(max(entornoBR$date)-30,format="%Y-%m-%d"), format = "%d"),
                                               ' de ',
                                               meses[as.numeric(format(as.Date(max(entornoBR$date)-30,format="%Y-%m-%d"), format = "%m"))]), 
                           paste0('casos até ',
                                  format(as.Date(max(entornoBR$date),format="%Y-%m-%d"), format = "%d"),
                                  ' de ',
                                  meses[as.numeric(format(as.Date(max(entornoBR$date),format="%Y-%m-%d"), format = "%m"))]), 
                           'Nº de casos novos','Taxa de crescimento - casos') 
tx_evolucao_casos <- tx_evolucao

filter(entornoBR, date == max(entornoBR$date)-30 | date == max(entornoBR$date)) %>%
  unique() %>%
  select(city, date, deaths) %>% # a data mais recente é sempre a de cima para cada município
  unique()%>%
  spread(key = date, value = deaths) -> tx_evolucao


tx_evolucao$diferenca <- tx_evolucao[,3] - tx_evolucao[,2]
tx_evolucao$tx <- paste0(round(100*tx_evolucao$diferenca/tx_evolucao[,2], 2),'%')  
colnames(tx_evolucao) <- c('Município', paste0('mortes até ',
                                               format(as.Date(max(entornoBR$date)-30,format="%Y-%m-%d"), format = "%d"),
                                               ' de ',
                                               meses[as.numeric(format(as.Date(max(entornoBR$date)-30,format="%Y-%m-%d"), format = "%m"))]), 
                           paste0('mortes até ',
                                  format(as.Date(max(entornoBR$date),format="%Y-%m-%d"), format = "%d"),
                                  ' de ',
                                  meses[as.numeric(format(as.Date(max(entornoBR$date),format="%Y-%m-%d"), format = "%m"))]), 
                           'Nº de novas mortes','Taxa de crescimento - mortes') 


tx_evolucao <- tx_evolucao_casos %>%
  left_join(tx_evolucao, by = 'Município')
rm(tx_evolucao_casos)

# Municípios com mais de 100 casos novos:
casos <- tx_evolucao %>% 
  select(Município, `Nº de casos novos`) %>%
  filter( `Nº de casos novos`>100)
casos<-casos[order(casos$`Nº de casos novos`, decreasing = T),]
write.csv2(casos, 'analises/boletim_covid19/tabela_casos_202107.csv')

aux <- paste0('mortes até ',
       format(as.Date(max(entornoBR$date),format="%Y-%m-%d"), format = "%d"),
       ' de ',
       meses[as.numeric(format(as.Date(max(entornoBR$date),format="%Y-%m-%d"), format = "%m"))])

# Municípios com mortes:
sum(tx_evolucao$`Nº de novas mortes`, na.rm = T)
sum(tx_evolucao[,names(tx_evolucao)==aux], na.rm = T)

#mortes <- tx_evolucao %>% 
#  select(Município, `Nº de novas mortes`) %>%
#  filter( `Nº de novas mortes` > 0)
#mortes<-mortes[order(mortes$`Nº de novas mortes`, decreasing = T),]
#write.csv2(mortes, 'analises/boletim_covid19/tabela_mortes_202103.csv')



# gráfico de linhas: casos confirmados no interior ----

# Arrumando erro de digitação em Humaita, de 2021-02-06 a 2021-02-08, de 6919 para 6019:
entornoBR$confirmed[entornoBR$city == 'Humaitá' & entornoBR$date == '2021-02-06' | 
                      entornoBR$city == 'Humaitá' & entornoBR$date == '2021-02-07' | 
                      entornoBR$city == 'Humaitá' & entornoBR$date == '2021-02-08'] <- 6019

banco1<-entornoBR %>% # banco 1 com o número de casos confirmados por município ao longo do tempo.
  select(city, date, confirmed)
summary(as.factor(as.character(banco1$city))) # para checar se há os 13 municípios

#media movel

#estou zerando casos negativos
entornoBR %>%
  mutate(new_confirmed = (abs(new_confirmed) + new_confirmed)/2) %>%
  group_by(city) %>%
  select(city,date,new_confirmed)%>%
  unique() %>%
  mutate(rollAvg = rollapply(new_confirmed, 7, mean,na.rm=T, fill=NA,align='right')) -> casosMovel






p1 <- entornoBR %>%
  filter(city != 'Manaus' & city != 'Porto Velho') %>%
  ggplot(aes(x=date, y=confirmed, group = city, colour = city)) +
  geom_line( size=0.8, alpha=0.9, linetype=1) + 
  geom_point(size = 2) +
  scale_x_date(date_labels = "%b %Y") + # ajusta label do eixo x (data)
  xlab('Dia') + ylab('Número de casos') +
  scale_color_brewer(palette = 'Paired') + # escala de cores
  labs(caption = 'Fonte dos dados: Brasil.IO. Elaboração: FGV CeDHE.') +
  theme(legend.title = element_blank(), # tira título da legenda
        plot.subtitle = element_text(hjust = 0.5), # centraliza subtítulo
        plot.title = element_text(hjust = 0.5)) # centraliza título

p1A <- casosMovel %>%
  filter(city != 'Manaus' & city != 'Porto Velho') %>%
  ggplot(aes(x=date, y=rollAvg, group = city, colour = city)) +
  geom_line( size=0.5, alpha=0.7, linetype=1) + 
  #geom_point(size = 2) +
  scale_x_date(date_labels = "%b %Y") + # ajusta label do eixo x (data)
  xlab('Dia') + ylab('Média Móvel 7 dias - Número de casos novos') +
  scale_color_brewer(palette = 'Paired') + # escala de cores
  labs(caption = 'Fonte dos dados: Brasil.IO. Elaboração: FGV CeDHE.') +
  theme(legend.title = element_blank(), # tira título da legenda
        plot.subtitle = element_text(hjust = 0.5), # centraliza subtítulo
        plot.title = element_text(hjust = 0.5)) # centraliza título


# gráfico de linhas: casos confirmados nas capitais:----
p2 <- entornoBR %>%
  filter(city == 'Manaus'|city == 'Porto Velho') %>%
  ggplot(aes(x=date, y=confirmed/1000, group = city, colour = city)) +
  geom_line( size=0.8, alpha=0.9, linetype=1) + 
  geom_point(size = 2) +
  scale_x_date(date_labels = "%b %Y") + # ajusta label do eixo x (data)
  xlab('Dia') + ylab('Mil casos') +
  scale_color_brewer(palette = 'Set2') + # escala de cores
  labs(caption = 'Fonte dos dados: Brasil.IO. Elaboração: FGV CeDHE.') +
  theme(legend.position = 'right',
        legend.title = element_blank(), # tira título da legenda
        plot.subtitle = element_text(hjust = 0.5), # centraliza subtítulo
        plot.title = element_text(hjust = 0.5)) # centraliza título

p2A <- casosMovel %>%
  filter(city == 'Manaus' | city == 'Porto Velho') %>%
  ggplot(aes(x=date, y=rollAvg, group = city, colour = city)) +
  geom_line( size=0.8, alpha=0.9, linetype=1) + 
  #geom_point(size = 2) +
  scale_x_date(date_labels = "%b %Y") + # ajusta label do eixo x (data)
  xlab('Dia') + ylab('Média Móvel 7 dias - Número de casos novos') +
  scale_color_brewer(palette = 'Set2') + # escala de cores
  labs(caption = 'Fonte dos dados: Brasil.IO. Elaboração: FGV CeDHE.') +
  theme(legend.title = element_blank(), # tira título da legenda
        plot.subtitle = element_text(hjust = 0.5), # centraliza subtítulo
        plot.title = element_text(hjust = 0.5)) # centraliza título


ggsave(p1, path = 'analises/boletim_covid19/', filename = 'graficointerior_boletim_202107.png',
       width = 20, height = 12, units = 'cm')
ggsave(p2, path = 'analises/boletim_covid19/', filename = 'graficocapitais_boletim_202107.png',
       width = 15, height = 10, units = 'cm')


  #### GERANDO MAPA E BARPLOT ####
data_do_mapa1 <- '2021-07-15'
data_do_mapa2 <- '2021-07-15'

# Barplot numero de casos acumulados até a data de interesse:----
banco2 <- entornoBR %>%
  filter(date == data_do_mapa1 | date == data_do_mapa2) %>%
        #selecionando a data mais recente para cada município:
        group_by(city) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
  mutate(confirmados_por_mil_hab = confirmed_per_100k_inhabitants/100) %>%
  select(city, confirmed, confirmados_por_mil_hab) %>% 
  distinct()

summary(as.factor(as.character(banco2$city)))

# barplot capitais:
p3A <- entornoBR %>%
  filter(date == data_do_mapa1 | date == data_do_mapa2) %>%
  filter(city == 'Porto Velho' | city == 'Manaus') %>%
  distinct() %>%
  #selecionando a data mais recente para cada município:
  group_by(city) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  # plotando:
  ggplot(aes(x = reorder(city, confirmed) , y = confirmed/1000)) + 
  geom_bar(fill = 'red4', stat = 'identity') + 
  geom_text(aes(y = confirmed/1000 + 10, label=round(confirmed/1000, digits = 1)), vjust = 0, col='red4', size=4.5) +
  ylab('Mil casos') + xlab('') + coord_flip() + 
  labs(caption = 'Fonte dos dados: Brasil.IO. Elaboração: FGV CeDHE.')

# barplot interior:
p3B <- entornoBR %>%
  filter(date == data_do_mapa1 | date == data_do_mapa2) %>%
  filter(city != 'Porto Velho' & city != 'Manaus') %>%
  distinct() %>%
  #selecionando a data mais recente para cada município:
  group_by(city) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  # plotando:
  ggplot(aes(x = reorder(city, confirmed) , y = confirmed/1000)) + 
  geom_bar(fill = 'red4', stat = 'identity') + 
  geom_text(aes(y = confirmed/1000 + 0.3, label=round(confirmed/1000, digits = 1)), vjust = 0.5, col='red4', size=4.5) +
  ylab('Mil casos') + xlab('') + coord_flip() + 
  labs(caption = 'Fonte dos dados: Brasil.IO. Elaboração: FGV CeDHE.')


# Filtrando para mapa: ----
df_covid <- df_covid %>%
  filter(date == data_do_mapa1 | date == data_do_mapa2) %>%
  unique() %>%
  filter(str_length(CD_GEOCODM)>2) %>%
  #selecionando a data mais recente para cada município:
  group_by(city) %>%
  filter(date == max(date)) %>%
  ungroup()

# verificando número de datas diferentes representadas no mapa:
df_covid %>% select(date) %>% unique() %>% nrow()

# Verificando se temos um município por linha:
df_covid %>% select(CD_GEOCODM) %>% distinct() %>% nrow() # igual ao df_covid inteiro, então ok!

# Importando camada BR-319 p/ mapa:
BRmeio <- rgdal::readOGR('dados_tratados/Ministerio_Infraestrutura/',
                       'trecho_meio_BR319_desenhado', stringsAsFactors = F) # trecho do meio da BR

# Adiciona ID na tabela de atributos:
AMRO@data$id <- 1:(dim(AMRO@data)[1])

# Transformando o objeto em data.frame para conseguirmos plotar o mapa:
AMRO2 <- fortify(AMRO)

# Join por ID, para acrescentar os atributos no novo data.frame:
AMRO2 = plyr::join(x = AMRO2, y = AMRO@data, by="id")

# fazendo join com da camada com o banco de covid-19:
df_covid$CD_GEOCODM <- as.character(df_covid$CD_GEOCODM)
AMRO2 <- left_join(AMRO2, df_covid, 'CD_GEOCODM')

#### Plotando mapa casos por 100k hab ####
p <- AMRO2 %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, 
                   fill = confirmed_per_100k_inhabitants/100), # fill= colore pelo número de casos confirmados
               colour = "darkgrey", size = 0.20) +
  # deixando sem grid:
  theme_void() + 
  #xlab('Longitude') + ylab('Latitude') +
  
  # Escala de cores:
  scale_fill_gradient(high = 'red4', 
                      low = 'white', 
                      na.value = 'gray70') +
  # contorno municípios do entorno:
  geom_polygon(data = entorno, aes(x = long, y = lat, group = group),
               colour = "gray20", size = 0.6, alpha = 0, linetype = 3) + 
  
  # trecho do meio da BR
  #geom_line(data = BRmeio, aes(x = long, y = lat, group = group),
  #         color = 'firebrick', size = 1.2) +
  
  # títulos / etiquetas:
  theme( plot.title = element_text(hjust = 0.5), # centraliza título
         plot.subtitle = element_text(hjust = 0.5)) + # centraliza subtítulo
  labs(fill = 'Casos por mil hab.\n acumulado até \n 15 julho 2021') # legenda
       #title = paste('Casos confirmados de covid-19'), # título
       #subtitle = 'Municípios dos estados do Amazonas e de Rondônia \n com municípios do entorno da BR-319 em destaque', # subtítulo
       #caption = 'Fonte dos dados: Brasil.IO. Elaboração: CeDHE-FGV.') # roda-pé

# para ver as taxas:
entornoBR %>% filter(date == data_do_mapa1 | date == data_do_mapa2) %>% unique()

# salvando gráficos de barras:
ggsave(plot = p3A, path = 'analises/boletim_covid19/',
       filename = 'barras_acumulado_capitais_202107.png',
       width = 15, height = 5, units = 'cm')
ggsave(plot = p3B, path = 'analises/boletim_covid19/',
       filename = 'barras_acumulado_interior_202107.png',
       width = 15, height = 10, units = 'cm')

# salvando mapa:
ggsave(plot = p, path = 'analises/boletim_covid19/',
       filename = 'mapa_boletim_202107.png',
       width = 30, height = 23, units = 'cm')
  
# salvando bancos para o designer:
# write.csv2(banco1, 'analises/boletim_covid19/bancos_para_designer/banco1_evolucao_casos_acumulados_20210315.csv')
write.csv2(banco2, 'analises/boletim_covid19/bancos_para_designer/banco2_casos_acumulados_ate_20210715.csv')

rm(list = ls())

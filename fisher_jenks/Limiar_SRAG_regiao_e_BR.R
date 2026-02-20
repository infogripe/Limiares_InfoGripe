library(tidyr)
library(dplyr)
library(tidyverse)
library(vroom)
library(mem)
library(readxl)
library(zoo)
library(Hmisc)
require(geofacet)
library(cartography)

##Lendo dados UF
path <- "https://raw.githubusercontent.com/infogripe/Boletim_InfoGripe/main/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv"
dados <- vroom(path)

###População

pop<-read_xlsx("populacao/projecoes_2024_tab1_idade_simples.xlsx", skip = 5)

#############################################################################
##########Limiar UF SRAG pós covid###########################################
#############################################################################

pop_reg<- pop %>% filter (LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul")) %>%
  filter(SEXO=="Ambos") %>%
  select(LOCAL, IDADE, '2022':'2025') %>%
  group_by(LOCAL)  %>%
  summarise(across('2022':'2025',\(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer('2022':'2025',names_to = "epiyear", values_to = "populacao") %>%
  mutate(epiyear=as.double(epiyear)) %>%
  rename(regioes=LOCAL)

###Dados SRAG

dados_SRAG<- dados %>% 
  as.data.frame()%>%
  filter(SG_UF_NOT!=0)%>%
  filter (fx_etaria=="Total", epiyear>=2022 & epiyear<=2025) %>%
  group_by(regioes=floor(as.integer(SG_UF_NOT)/10),
           epiweek, epiyear) %>%
  summarise(casos=sum(SRAG, na.rm=T)) %>%
  mutate(regioes=factor(regioes,
                        levels=c(1,2,5,3,4),
                        labels=c('Norte', 'Nordeste',
                                 'Centro-Oeste', 'Sudeste',
                                 'Sul'))) %>%
  left_join(pop_reg, by=c("epiyear", "regioes")) %>%
  mutate(inci=casos*100000/populacao) %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(date>"2022-03-13" & date<"2025-07-12") %>% 
  filter(!is.na(regioes))

nomes_ref<- as.list(unique(dados_SRAG$regioes)) 

limiar2<- data.frame(Muito_baixo= numeric(),
                     baixo=numeric(),
                     moderado=numeric(),
                     alto=numeric(),
                     muito_alto=numeric(),
                     regiao = character(),
                     cod_regiao=character())


for (i in nomes_ref) {
  
  sub<- dados_SRAG %>% filter(regioes==i) 
  
  epi <- getBreaks(v = sub$inci, nclass = 5, method = "fisher")
  
  
  # return dos dados calculados
  df_limi<-  data.frame(data.frame(Muito_baixo= epi[1],
                                   baixo=epi[2],
                                   moderado=epi[3],
                                   alto=epi[4],
                                   muito_alto=epi[5],
                                   regiao = i))
  
  limiar2=rbind(limiar2,df_limi)
  
}

#write.csv(limiar2, "output/fisher_limiares_SRAG_UF_2022_2024.csv", row.names = FALSE)

###Calculando o valor dos limiares em população

##População de 2025
pop_reg25<- pop %>% filter (LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul")) %>%
  filter(SEXO=="Ambos") %>%
  select(LOCAL, CÓD., IDADE, '2026') %>%
  rename(pop='2026')%>%
  group_by(LOCAL, CÓD.)  %>%
  summarise(pop= sum(pop, na.rm = TRUE)) %>%
  rename(regiao=LOCAL) %>%
  arrange(regiao)

limiar_reg_pop<- limiar2 %>%
  arrange(regiao)%>%
  mutate_at(1:5,function(col){round(pop_reg25$pop*col/100000)}) %>%
  mutate(escala="casos",
         escala_reg="Região") %>%
  mutate(across(1:5, as.character))


limiar_reg<- limiar2 %>%
  mutate(escala="incidencia",
         escala_reg="Região") %>%
  mutate(across(1:5, as.character))  %>%
  bind_rows(limiar_reg_pop)


#################################################
################# LIMIAR BRASIL #################
#################################################

pop_br<- pop %>% filter (LOCAL %in% c("Brasil")) %>%
  filter(SEXO=="Ambos") %>%
  select(LOCAL, IDADE, '2022':'2025') %>%
  group_by(LOCAL)  %>%
  summarise(across('2022':'2025',\(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer('2022':'2025',names_to = "epiyear", values_to = "populacao") %>%
  mutate(epiyear=as.double(epiyear)) %>%
  mutate(regioes="BR")

###Dados SRAG

dados_SRAG_br<- dados %>% 
  as.data.frame()%>%
  filter(SG_UF_NOT==0)%>%
  filter (fx_etaria=="Total", epiyear>=2022 & epiyear<=2025) %>%
  mutate(regioes="BR") %>%
  group_by(regioes,
           epiweek, epiyear) %>%
  summarise(casos=sum(SRAG, na.rm=T)) %>%
  left_join(pop_br, by=c("epiyear", "regioes")) %>%
  mutate(inci=casos*100000/populacao) %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(date>"2022-03-13" & date<"2025-07-12") %>% 
  filter(!is.na(regioes))
  
  
  epi <- getBreaks(v = dados_SRAG_br$inci, nclass = 5, method = "fisher")
  
  
  # return dos dados calculados
  limiar2<-  data.frame(data.frame(Muito_baixo= epi[1],
                                   baixo=epi[2],
                                   moderado=epi[3],
                                   alto=epi[4],
                                   muito_alto=epi[5],
                                   regiao = "BR"))


###Calculando o valor dos limiares em população

##População de 2025
pop_br25<- pop %>% filter (LOCAL %in% c("Brasil")) %>%
  filter(SEXO=="Ambos") %>%
  select(LOCAL, CÓD., IDADE, '2026') %>%
  rename(pop='2026')%>%
  group_by(LOCAL, CÓD.)  %>%
  summarise(pop= sum(pop, na.rm = TRUE)) %>%
  mutate(regiao="BR") 


limiar_br_pop<- limiar2 %>%
  mutate_at(1:5,function(col){round(pop_br25$pop*col/100000)}) %>%
  mutate(escala="casos",
         escala_reg="Nacional") %>%
  mutate(across(1:5, as.character))


limiar_br<- limiar2 %>%
  mutate(escala="incidencia",
         escala_reg="Nacional") %>%
  mutate(across(1:5, as.character))  %>%
  bind_rows(limiar_br_pop)


casos_reg_br<- limiar_reg %>%
   bind_rows(limiar_br)

write.csv(casos_reg_br, "output/limiares_SRAG_casos_regiao_BR.csv", row.names = FALSE)

################################################################
#####################Limiar óbitos #############################
################################################################

path = "https://raw.githubusercontent.com/infogripe/Boletim_InfoGripe/main/Dados/InfoGripe/obitos_semanais_fx_etaria_virus_sem_filtro_febre.csv"
dados_obt <- vroom(file = path)

###Dados SRAG
dados_SRAG_obt<- dados_obt %>% 
  as.data.frame()%>%
  filter(SG_UF_NOT!=0)%>%
  filter (fx_etaria=="Total", epiyear>=2022 & epiyear<=2025) %>%
  group_by(regioes=floor(as.integer(SG_UF_NOT)/10),
           epiweek, epiyear) %>%
  summarise(casos=sum(SRAG, na.rm=T)) %>%
  mutate(regioes=factor(regioes,
                        levels=c(1,2,5,3,4),
                        labels=c('Norte', 'Nordeste',
                                 'Centro-Oeste', 'Sudeste',
                                 'Sul'))) %>%
  left_join(pop_reg, by=c("epiyear", "regioes")) %>%
  mutate(inci=casos*100000/populacao) %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(date>"2022-03-13" & date<"2025-07-12") %>% 
  filter(!is.na(regioes))

nomes_ref<- as.list(unique(dados_SRAG_obt$regioes)) 

limiar2<- data.frame(Muito_baixo= numeric(),
                     baixo=numeric(),
                     moderado=numeric(),
                     alto=numeric(),
                     muito_alto=numeric(),
                     regiao = character())


for (i in nomes_ref) {
  
  sub<- dados_SRAG_obt %>% filter(regioes==i) 
  
  reg<-unique(sub$regioes)
  
  
  epi <- getBreaks(v = sub$inci, nclass = 5, method = "fisher")
  
  
  # return dos dados calculados
  df_limi<-  data.frame(data.frame(Muito_baixo= epi[1],
                                   baixo=epi[2],
                                   moderado=epi[3],
                                   alto=epi[4],
                                   muito_alto=epi[5],
                                   regiao = i))
  
  limiar2=rbind(limiar2,df_limi)
  
}


limiar_reg_pop_obt<- limiar2 %>%
  arrange(regiao)%>%
  mutate_at(1:5,function(col){round(pop_reg25$pop*col/100000)}) %>%
  mutate(escala="obitos",
         escala_reg="Região") %>%
  mutate(across(1:5, as.character))


limiar_reg_obt<- limiar2 %>%
  mutate(escala="mortalidade",
         escala_reg="Região") %>%
  mutate(across(1:5, as.character))  %>%
  bind_rows(limiar_reg_pop_obt)


#######################################################
################# LIMIAR BRASIL obito #################
#######################################################


###Dados SRAG

dados_SRAG_obt<- dados_obt %>% 
  as.data.frame()%>%
  filter(SG_UF_NOT==0)%>%
  filter (fx_etaria=="Total", epiyear>=2022 & epiyear<=2025) %>%
  mutate(regioes="BR") %>%
  group_by(regioes,
           epiweek, epiyear) %>%
  summarise(casos=sum(SRAG, na.rm=T)) %>%
  left_join(pop_br, by=c("epiyear", "regioes")) %>%
  mutate(inci=casos*100000/populacao) %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(date>"2022-03-13" & date<"2025-07-12") %>% 
  filter(!is.na(regioes))


  
  epi <- getBreaks(v = dados_SRAG_obt$inci, nclass = 5, method = "fisher")
  
  
  # return dos dados calculados
  limiar2<-  data.frame(data.frame(Muito_baixo= epi[1],
                                   baixo=epi[2],
                                   moderado=epi[3],
                                   alto=epi[4],
                                   muito_alto=epi[5],
                                   regiao = "BR"))
  
  


limiar_br_pop_obt<- limiar2 %>%
  mutate_at(1:5,function(col){round(pop_br25$pop*col/100000)}) %>%
  mutate(escala="obitos",
         escala_reg="Nacional") %>%
  mutate(across(1:5, as.character))

limiar_br_obt<- limiar2 %>%
  mutate(escala="mortalidade",
         escala_reg="Nacional") %>%
  mutate(across(1:5, as.character))  %>%
  bind_rows(limiar_br_pop_obt)

###Juntando tudo

obitos_reg<-bind_rows(limiar_reg_obt, limiar_br_obt)

write.csv(obitos_reg, "output/limiares_SRAG_obitos_regiao_BR.csv", row.names = FALSE)


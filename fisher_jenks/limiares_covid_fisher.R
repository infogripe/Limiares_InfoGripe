######Covid-19

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


##########Limiar UF SRAG pós covid

pop_br<- pop %>% filter (!LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul", "Brasil")) %>%
  filter(SEXO=="Ambos") %>%
  select(SIGLA, IDADE, '2022':'2025') %>%
  group_by(SIGLA)  %>%
  summarise(across('2022':'2025',\(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer('2022':'2025',names_to = "epiyear", values_to = "populacao") %>%
  mutate(epiyear=as.double(epiyear)) %>%
  rename(DS_UF_SIGLA=SIGLA)

###Dados SRAG

dados_SRAG<- dados %>% 
  as.data.frame()%>%
  filter (fx_etaria=="Total", epiyear>=2022 & epiyear<=2025) %>%
  filter(SG_UF_NOT!=0)%>%
  group_by(SG_UF_NOT, DS_UF_SIGLA,
           epiweek, epiyear) %>%
  summarise(casos=sum(SARS2, na.rm=T)) %>%
  left_join(pop_br, by=c("epiyear", "DS_UF_SIGLA")) %>%
  mutate(inci=casos*100000/populacao) %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(date>"2022-03-13" & date<"2025-07-12") %>% 
  filter(!is.na(DS_UF_SIGLA))

nomes_ref<- as.list(unique(dados_SRAG$DS_UF_SIGLA)) 

limiar2<- data.frame(Muito_baixo= numeric(),
                     baixo=numeric(),
                     moderado=numeric(),
                     alto=numeric(),
                     muito_alto=numeric(),
                     regiao = character(),
                     cod_regiao=character())


for (i in nomes_ref) {
  
  sub<- dados_SRAG %>% filter(DS_UF_SIGLA==i) 
  
  reg<-unique(sub$SG_UF_NOT)
  
  
  epi <- getBreaks(v = sub$inci, nclass = 5, method = "fisher")
  
  
  # return dos dados calculados
  df_limi<-  data.frame(data.frame(Muito_baixo= epi[1],
                                   baixo=epi[2],
                                   moderado=epi[3],
                                   alto=epi[4],
                                   muito_alto=epi[5],
                                   regiao = i,
                                   cod_regiao= reg))
  
  limiar2=rbind(limiar2,df_limi)
  
}

#write.csv(limiar2, "output/fisher_limiares_SRAG_UF_2022_2024.csv", row.names = FALSE)

###Calculando o valor dos limiares em população

##População de 2025
pop_uf26<- pop %>% filter (!LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul", "Brasil")) %>%
  filter(SEXO=="Ambos") %>%
  select(SIGLA, CÓD., IDADE, '2026') %>%
  rename(pop='2026')%>%
  group_by(SIGLA, CÓD.)  %>%
  summarise(pop= sum(pop, na.rm = TRUE)) %>%
  rename(regiao=SIGLA) %>%
  arrange(CÓD.)

limiar_uf_pop<- limiar2 %>%
  arrange(cod_regiao)%>%
  mutate_at(1:5,function(col){round(pop_uf26$pop*col/100000)}) %>%
  mutate(escala="casos",
         escala_reg="UF") %>%
  mutate(across(1:5, as.character))


limiar_uf<- limiar2 %>%
  mutate(escala="incidencia",
         escala_reg="UF") %>%
  mutate(across(1:5, as.character))  %>%
  bind_rows(limiar_uf_pop)


write.csv(limiar_uf, "output/limiares_COVID_casos_UF.csv", row.names = FALSE)


####òbitos

##Lendo dados UF
path = "https://raw.githubusercontent.com/infogripe/Boletim_InfoGripe/main/Dados/InfoGripe/obitos_semanais_fx_etaria_virus_sem_filtro_febre.csv"
dados <- vroom(file = path)

###População

pop<-read_xlsx("populacao/projecoes_2024_tab1_idade_simples.xlsx", skip = 5)


##########Limiar UF SRAG pós covid

pop_br<- pop %>% filter (!LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul", "Brasil")) %>%
  filter(SEXO=="Ambos") %>%
  select(SIGLA, IDADE, '2022':'2025') %>%
  group_by(SIGLA)  %>%
  summarise(across('2022':'2025',\(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer('2022':'2025',names_to = "epiyear", values_to = "populacao") %>%
  mutate(epiyear=as.double(epiyear)) %>%
  rename(DS_UF_SIGLA=SIGLA)

###Dados SRAG

dados_SRAG<- dados %>% 
  as.data.frame()%>%
  filter (fx_etaria=="Total", epiyear>=2022 & epiyear<=2025) %>%
  filter(SG_UF_NOT!=0)%>%
  group_by(SG_UF_NOT, DS_UF_SIGLA,
           epiweek, epiyear) %>%
  summarise(casos=sum(SARS2, na.rm=T)) %>%
  left_join(pop_br, by=c("epiyear", "DS_UF_SIGLA")) %>%
  mutate(inci=casos*100000/populacao) %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(date>"2022-03-13" & date<"2025-07-12") %>% 
  filter(!is.na(DS_UF_SIGLA))

nomes_ref<- as.list(unique(dados_SRAG$DS_UF_SIGLA)) 

limiar2<- data.frame(Muito_baixo= numeric(),
                     baixo=numeric(),
                     moderado=numeric(),
                     alto=numeric(),
                     muito_alto=numeric(),
                     regiao = character(),
                     cod_regiao=character())


for (i in nomes_ref) {
  
  sub<- dados_SRAG %>% filter(DS_UF_SIGLA==i) 
  
  reg<-unique(sub$SG_UF_NOT)
  
  
  epi <- getBreaks(v = sub$inci, nclass = 5, method = "fisher")
  
  
  # return dos dados calculados
  df_limi<-  data.frame(data.frame(Muito_baixo= epi[1],
                                   baixo=epi[2],
                                   moderado=epi[3],
                                   alto=epi[4],
                                   muito_alto=epi[5],
                                   regiao = i,
                                   cod_regiao= reg))
  
  limiar2=rbind(limiar2,df_limi)
  
}



#write.csv(limiar2, "output/fisher_limiares_SRAG_UF_2022_2024.csv", row.names = FALSE)

###Calculando o valor dos limiares em população

##População de 2025
pop_uf26<- pop %>% filter (!LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul", "Brasil")) %>%
  filter(SEXO=="Ambos") %>%
  select(SIGLA, CÓD., IDADE, '2026') %>%
  rename(pop='2026')%>%
  group_by(SIGLA, CÓD.)  %>%
  summarise(pop= sum(pop, na.rm = TRUE)) %>%
  rename(regiao=SIGLA) %>%
  arrange(CÓD.)



limiar_uf_pop<- limiar2 %>%
  arrange(cod_regiao)%>%
  mutate_at(1:5,function(col){round(pop_uf26$pop*col/100000)}) %>%
  mutate(escala="casos",
         escala_reg="UF") %>%
  mutate(across(1:5, as.character))


limiar_uf<- limiar2 %>%
  mutate(escala="incidencia",
         escala_reg="UF") %>%
  mutate(across(1:5, as.character))  %>%
  bind_rows(limiar_uf_pop)


write.csv(limiar_uf, "output/limiares_COVID_obitos_UF.csv", row.names = FALSE)




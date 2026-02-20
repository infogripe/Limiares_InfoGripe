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
  summarise(casos=sum(SRAG, na.rm=T)) %>%
  left_join(pop_br, by=c("epiyear", "DS_UF_SIGLA")) %>%
  mutate(inci=casos*100000/populacao) %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(date>"2022-03-13" & date<"2025-07-12") %>% ##excluindo período covid e últimos dois meses
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

###Calculando o valor dos limiares em população

##População de 2025
pop_uf25<- pop %>% filter (!LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul", "Brasil")) %>%
  filter(SEXO=="Ambos") %>%
  select(SIGLA, CÓD., IDADE, '2026') %>%
  rename(pop='2026')%>%
  group_by(SIGLA, CÓD.)  %>%
  summarise(pop= sum(pop, na.rm = TRUE)) %>%
  rename(regiao=SIGLA) %>%
  arrange(CÓD.)



limiar_uf_pop<- limiar2 %>%
  arrange(cod_regiao)%>%
  mutate_at(1:5,function(col){round(pop_uf25$pop*col/100000)}) %>%
  mutate(escala="casos",
         escala_reg="UF") %>%
  mutate(across(1:5, as.character))


limiar_uf<- limiar2 %>%
  mutate(escala="incidencia",
         escala_reg="UF") %>%
  mutate(across(1:5, as.character))  %>%
  bind_rows(limiar_uf_pop)


#write.csv(limiar2, "output/fisher_limiares_SRAG_UF_fx_etaria_2022_2024.csv", row.names = FALSE)

rm(pop_br, dados_SRAG, df_limi, limiar2, sub)

#####Limiar SRAG UF por faixa etária pós covid

pop_br<- pop %>% filter (!LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul", "Brasil")) %>%
  filter(SEXO=="Ambos") %>%
  mutate(fx_etaria=case_when(
    IDADE<2 ~ "< 2",
    IDADE>=2 & IDADE<=4 ~ "2 a 4",
    IDADE>=5 & IDADE<=14 ~ "5 a 14",
    IDADE>=15 & IDADE<=49 ~ "15 a 49",
    IDADE>=50 & IDADE<=64 ~ "50 a 64",
    IDADE >= 65 ~ "65+",
  )) %>%
  select(SIGLA, fx_etaria, IDADE, '2022':'2025') %>%
  group_by(SIGLA, fx_etaria)  %>%
  summarise(across('2022':'2025',\(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer('2022':'2025',names_to = "epiyear", values_to = "populacao") %>%
  mutate(epiyear=as.double(epiyear)) %>%
  rename(DS_UF_SIGLA=SIGLA)


dados_SRAG<- dados %>% 
  as.data.frame()%>%
  filter (fx_etaria!="Total",  epiyear>=2022 & epiyear<=2025) %>%
  filter(SG_UF_NOT!=0)%>%
  group_by(SG_UF_NOT, DS_UF_SIGLA, fx_etaria,
           epiweek, epiyear) %>%
  summarise(casos=sum(SRAG, na.rm=T)) %>%
  left_join(pop_br, by=c("epiyear", "DS_UF_SIGLA", "fx_etaria")) %>%
  mutate(inci=casos*100000/populacao) %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(date>"2022-03-13" & date<"2025-07-12") %>%
  filter(!is.na(DS_UF_SIGLA))


####Calculando limiar por faixa etaria

nomes_ref<- as.list(unique(dados_SRAG$DS_UF_SIGLA)) 
idade<- as.list(unique(dados_SRAG$fx_etaria))

limiar2<- data.frame(Muito_baixo= numeric(),
                     baixo=numeric(),
                     moderado=numeric(),
                     alto=numeric(),
                     muito_alto=numeric(),
                     regiao = character(),
                     fx_etaria= character())




for (i in nomes_ref) {
  
  sub<- dados_SRAG %>% filter(DS_UF_SIGLA==i) 
  
  for (j in idade) {
    
    sub2<- sub %>% filter(fx_etaria==j) 
    
    
    epi <- getBreaks(v = sub2$inci, nclass = 5, method = "fisher")
    
    
    # return dos dados calculados
    df_limi<-  data.frame(data.frame(Muito_baixo= epi[1],
                                     baixo=epi[2],
                                     moderado=epi[3],
                                     alto=epi[4],
                                     muito_alto=epi[5],
                                     regiao = i,
                                     fx_etaria=j))
    
    limiar2=rbind(limiar2,df_limi)
    
  }
  
}


write.csv(limiar2, "output/limiares_SRAG_casos_UF_fx_etaria.csv", row.names = FALSE)

########################################
#############  LImiar capitais ########
#######################################
library(geobr)

capitais<-read_capitals()

capitais2<- capitais %>% select(code_muni, name_muni, abbrev_state) %>%
  mutate (CO_MUN_RES=as.numeric(substr(code_muni,1,6))) %>%
  as.data.frame() %>%
  select(!geom)

cod<-substr(capitais$code_muni,1,6)

#teste<-read_population(year=2024)

###Lendo a população por municipio 2022

pop_22<-read_xls("populacao/POP_TCU_2023_Municipios_POP2022_Malha2023.xls", skip=1)
pop_22<-pop_22[-c(5571:5603),]


cap_22<- pop_22 %>% mutate(CO_MUN_RES=substr(paste0(`COD. UF`, `COD. MUNIC`),1,6)) %>%
  filter(CO_MUN_RES %in% cod) %>%
  dplyr::rename(p_2022= `POPULAÇÃO APURADA IBGE 
- CENSO DEMOGRÁFICO 2022 E MALHA TERRITORIAL 2023 -`) %>%
  select(CO_MUN_RES, UF,  p_2022) %>%
  mutate(p_2022=as.numeric(p_2022))


####Lendo a população por municipio 2024
pop_24<-read_xls("populacao/estimativa_dou_2024.xls", sheet=2, skip=1)
pop_24<-pop_24[-c(5571:5572),]

cap_24<- pop_24 %>% mutate(CO_MUN_RES=substr(paste0(`COD. UF`, `COD. MUNIC`),1,6)) %>%
  filter(CO_MUN_RES %in% cod) %>%
  dplyr::rename(p_2024=`POPULAÇÃO ESTIMADA`) %>%
  select(CO_MUN_RES, UF,p_2024) %>%
  mutate(p_2024=as.numeric(p_2024))

######Capital 2025
pop_25<-read_xls("populacao/estimativa_dou_2025.xls", sheet=2, skip=1)
pop_25<-pop_25[-c(5572:5573),]

cap_25<- pop_25 %>% mutate(CO_MUN_RES=substr(paste0(`COD. UF`, `COD. MUNIC`),1,6)) %>%
  filter(CO_MUN_RES %in% cod) %>%
  dplyr::rename(p_2025=`POPULAÇÃO ESTIMADA`) %>%
  select(CO_MUN_RES, UF,p_2025) %>%
  mutate(p_2025=as.numeric(p_2025))

cap <- cap_22 %>%
  full_join(cap_24, by=c("CO_MUN_RES", "UF")) %>%
  full_join(cap_25, by=c("CO_MUN_RES", "UF"))

###Calculando a população para 2023

cap2<- cap %>%  
  pivot_longer(3:5, names_to = "epiyear", values_to="pop") %>%
  mutate(epiyear=as.numeric(substr(epiyear,3,7))) %>%
  arrange(epiyear) %>%
  group_by(CO_MUN_RES, UF) %>%
  complete(epiyear=full_seq(2022:2025,1)) %>%
  group_by(CO_MUN_RES, UF) %>%
  mutate(pop=approxExtrap(x=epiyear[!is.na(pop)], y=pop[!is.na(pop)], xout = c(2022:2025), method = "linear")$y) %>%
  mutate(CO_MUN_RES=as.numeric(substr(CO_MUN_RES,1,6)))

###Dados

path = "dados/clean_data_srag_sragnofever_epiweek.csv.gz" 
dados <- vroom(file = path)

#teste<- dados[1:100,]

srag<- dados %>% 
  filter(CO_MUN_RES %in% cod) %>%
  mutate(epiyear=DT_SIN_PRI_epiyear,
         epiweek=DT_SIN_PRI_epiweek) %>%
  filter(epiyear>=2022 & epiyear<=2025) %>%
  group_by(CO_MUN_RES,
           epiweek, epiyear) %>%
  summarise(casos=n()) %>%
  left_join(cap2, by=c("CO_MUN_RES", "epiyear")) %>%
  mutate(inci=casos*100000/pop,
         epiyear=as.numeric(epiyear),
         epiweek=as.numeric(epiweek)) %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(date>"2022-03-13" & date<"2025-07-12") %>%
  left_join(capitais2, by="CO_MUN_RES") %>%
  mutate(capital=paste(name_muni, "-", abbrev_state))

nomes_ref<- as.list(unique(srag$CO_MUN_RES)) 

limiar_cap<- data.frame(Muito_baixo= numeric(),
                     baixo=numeric(),
                     moderado=numeric(),
                     alto=numeric(),
                     muito_alto=numeric(),
                     regiao = character(),
                     cod_muni= character())


for (i in nomes_ref) {
  
  sub<- srag %>% filter(CO_MUN_RES==i) 
  
  capital<-unique(sub$capital)
  
  
  epi <- getBreaks(v = sub$inci, nclass = 5, method = "fisher")
  
  
  # return dos dados calculados
  df_limi<-  data.frame(data.frame(Muito_baixo= epi[1],
                                   baixo=epi[2],
                                   moderado=epi[3],
                                   alto=epi[4],
                                   muito_alto=epi[5],
                                   regiao = capital,
                                   cod_regiao= i))
  
  
  limiar_cap=rbind(limiar_cap,df_limi)
  
}


#####juntando os limiares e calculando o número de casos considerando a população do ano vigente

####Considerando a população de 2025 para as capitais, já que ainda não tem a de 2026
cap_25<- cap_25 %>%
  rename(cod_regiao=CO_MUN_RES) %>%
  mutate(cod_regiao=as.numeric(cod_regiao))%>%
  select(!UF) 

#limi_pop_cap<- limi_pop_cap %>% 
limiar_cap_pop<-  limiar_cap %>%
  mutate_at(1:5,function(col){round(cap_25$p_2025*col/100000)}) %>%
  mutate(escala="casos",
         escala_reg="capital")%>%
  mutate(across(1:5, as.character))


limiar_cap2<- limiar_cap %>%
  mutate(escala="incidencia",
         escala_reg="capital")%>%
  mutate(across(1:5, as.character))

limiar_cap3<-bind_rows(limiar_cap2, limiar_cap_pop)

###Juntando tudo e salvando

junt<-bind_rows(limiar_uf, limiar_cap3)


write.csv(junt, "output/limiares_SRAG_casos_UF_capitais.csv", row.names = FALSE)

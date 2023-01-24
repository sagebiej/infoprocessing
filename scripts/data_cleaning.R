######################################################################
### Project       : Information processing in stated preference surveys
### Description    : Data cleaning and preparation                 
### Date          : 12.08.2021
### Version       : 2

######################################################################



#====================================
# Prepare data
#====================================


data <- read_sav("data/data_raw.sav" ) %>%  ### import data
  filter(compl==1,sample!=3) %>%  ## sample 3 is another choice exepriment not relevant here
  mutate(stuttgart=if_else(q2_1<=70629&q2_1>70000,1,0)) %>% ## postal codes to city
select(-c('userid', 'studNumb', 'Title', 'Client', 'compl', 'interrupt', 'dDay', 'dMonth', 'dYear', 'startTime', 'startTimel', 'endTime',  'city', 'device_str', 'osName_str', 'osVersion_str', 'agentName_str', 'agentVersion_str', 'devManu_str', 'devName_str', 'vpWidth_str', 'vpHeight_str', 'flashVersion_str', 'useragent_str'))%>%
  mutate(across(matches("^q6_[0-9]$") , ~ replace_na(., 3) ), ## NA in to 3
         across(c("q7_7_a","q7_7_c","q7_7_d","q7_7_e","q7_7_f","q7_7_i"), ~ recode(as.numeric(.), '1'=0,'2'=1,'3'=0, .missing = 0)) , #recode quiz  
         across(c("q7_7_b","q7_7_g","q7_7_h") , ~ recode(as.numeric(.),'1'=1,'2'=0,'3'=0 , .missing = 0) ) #recode quiz
         ) %>% 
  mutate(korrekt=rowSums(select(.,c("q7_7_a" , "q7_7_b" ,"q7_7_c" ,"q7_7_e", "q7_7_f", "q7_7_g", "q7_7_h", "q7_7_i")),na.rm=TRUE)  # number of correct answers in quiz
         ) %>% 
  {. ->> dataN } %>%   #here is save
  select(c('serial', 'korrekt' , 'sample', 'sample2', 'q3_3_zeit', 'q6_blck', 'q6_set_1', 'q6_set_2', 'q6_set_3', 'q6_set_4', 'q6_set_5', 'q6_set_6', 'q6_set_7', 'q6_set_8', 'q6_1', 'q6_2', 'q6_3', 'q6_4', 'q6_5', 'q6_6', 'q6_7', 'q6_8')) %>% 
  {. ->> widedata } %>%   #here is save


rename_with(function(x) c("ID", "korrekt" ,"sample","subsample","Zeit","block","set_1","set_2","set_3","set_4","set_5","set_6","set_7","set_8","choice_1","choice_2","choice_3","choice_4","choice_5","choice_6","choice_7","choice_8") ) %>%  ### rename variables
  as.data.frame()%>% 
reshape(varying=7:22, timevar="choiceset", idvar="ID", direction="long", sep="_")  %>%  # to long format for apollo %>% 
arrange(ID) %>% 
  {. ->> apollodata }  

#### Prepare design and merge with data

design12<-read_excel("data/design_hauptstudie.xlsx") %>% 
  mutate(across(where(is.character) , ~ tidyr::replace_na(., "0") ) ,
         L_GROESSE=GROESSE, GROESSE =  as.numeric(str_remove_all(GROESSE,"[^0-9]"))/1000 ,
         L_ENTFERNUNG = ENTFERNUNG , ENTFERNUNG=as.numeric(str_remove(ENTFERNUNG,"[^0-9].*?(\\((.*?)\\))")) ,  ENTFERNUNG=if_else(ENTFERNUNG>99,ENTFERNUNG/1000,ENTFERNUNG) ,
         L_GEMEINSCHAFTSAKTIVITAETEN=GEMEINSCHAFTSAKTIVITAETEN , GEMEINSCHAFTSAKTIVITAETEN=recode(GEMEINSCHAFTSAKTIVITAETEN,  "Gemeinschaftsaktivitäten" = 1 , .default=0),
         L_KULTURVERANSTALTUNGEN=KULTURVERANSTALTUNGEN , KULTURVERANSTALTUNGEN=recode(KULTURVERANSTALTUNGEN,  "Kulturveranstaltungen" = 1 , .default=0) ,
         L_UMWELTBILDUNG=UMWELTBILDUNG , UMWELTBILDUNG=recode(UMWELTBILDUNG,  "Umweltbildung" = 1 , .default=0),
         L_GESTALTUNG=GESTALTUNG , GESTALTUNG=recode(GESTALTUNG,  "eher geordnet" = 0 , "eher naturnah" = 1 , .default=999), 
         L_ZUGANG=ZUGANG, ZUGANG =  as.numeric(str_remove_all(ZUGANG,"[^0-9]"))-7, 
         L_BEITRAG=BEITRAG, BEITRAG =  as.numeric(str_remove_all(BEITRAG,"[^0-9]"))/10
         )%>%    ### NAs in 0 umwandeln
  as.data.frame()  %>%
  reshape(timevar = "alt", idvar = "choice_set", direction = "wide" ) %>% ## daten in wide format
  mutate(GROESSE.3=0, ENTFERNUNG.3=0,GEMEINSCHAFTSAKTIVITAETEN.3=0, KULTURVERANSTALTUNGEN.3=0, UMWELTBILDUNG.3=0, GESTALTUNG.3=0, ZUGANG.3=0, BEITRAG.3=0)  %>% 
  rename(set=choice_set) %>% 
right_join(data, by="set") %>% 
  arrange(ID,set) %>% 
  mutate(choice=as.numeric(choice),
         Zeit=Zeit/10 #for time models
         ) %>% 
  select(!matches("Block.[1-2]")) %>% 
  relocate(ID,sample, subsample, block, Zeit, choiceset, set, choice) %>% 
  relocate(starts_with("L_"), .after = last_col())
  

#data for apollo


## create means by treatment for time and knowledge questions, to be used later on in time and knowledge interacting choice models.

means <- design12 %>%
  group_by(subsample) %>%
  summarize(Tmean_Zeit=mean(Zeit) , Tmean_knowl =mean(korrekt))


database <- design12 %>%
  select(-starts_with("L_"))%>%
  left_join(means, by="subsample") %>%
  mutate(
    across(matches("^GROESSE.[1-2]") |matches("^BEITRAG.[1-2]")  , ~./10  )  ,
    info=if_else(subsample>=4,1,0) , 
    wissen=if_else(subsample==2|subsample==5,1,0) , 
    selbst =if_else(subsample==3|subsample==6,1,0) ,
    across(everything(), as.numeric),
    Zeit_alt =Zeit, Zeit=Zeit- Tmean_Zeit ,
    knowledge_old=korrekt , korrekt = knowledge_old-Tmean_knowl
  ) %>%
  as.data.frame()

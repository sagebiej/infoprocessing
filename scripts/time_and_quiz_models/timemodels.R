
# 
# this is not required anymore, as now handled in file data_cleaning.R. Delete later

# ## create relevant interaction variables
# 
# design12<- design12 %>%
#   mutate(Zeit=Zeit/10)
# 
# means <- design12 %>%
#   group_by(subsample) %>%
#   summarize(Tmean_Zeit=mean(Zeit) , Tmean_knowl =mean(korrekt))
#  
# 
# 
# database <- design12 %>%
#   left_join(means, by="subsample") %>%
#   mutate(across(everything(), as.numeric) ,
#          Zeit_alt =Zeit, Zeit=Zeit- Tmean_Zeit ,
#          knowledge_old=korrekt , korrekt = knowledge_old-Tmean_knowl) %>%
#   as.data.frame



#### Model 1 No time interactions ####

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "simple_model",
  modelDescr ="Conditional Logit with no interactions",
  indivID = "ID",
  mixing = FALSE ,
  outputDirectory = "modeloutput/"
)



# ################################################################# #
#### DEFINE apollo_beta()                                       
# ################################################################# #

### Startwerte festlegen
apollo_beta=c(asc_gemeinschaft = 0,
              asc_klein = 0,
              b_groesse = 0,
              b_entfernung = 0,
              b_gemeinschaft = 0,
              b_kultur = 0,
              b_umweltbildung = 0,
              b_zugang = 0,
              b_gestaltung = 0,
              b_beitrag = 0
)





# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                     
# ################################################################# #



### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate" ){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = (-b_beitrag)*(asc_gemeinschaft + b_groesse * GROESSE.1 + b_entfernung * ENTFERNUNG.1 +
                                b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 + b_kultur * KULTURVERANSTALTUNGEN.1 +
                                b_umweltbildung * UMWELTBILDUNG.1 + b_zugang * ZUGANG.1 +
                                b_gestaltung * GESTALTUNG.1 - BEITRAG.1 )
  
  V[['alt2']] = (-b_beitrag)*(asc_klein + b_groesse * GROESSE.2 + b_entfernung * ENTFERNUNG.2 +
                                b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.2 + b_kultur * KULTURVERANSTALTUNGEN.2 +
                                b_umweltbildung * UMWELTBILDUNG.2 + b_zugang * ZUGANG.2 +
                                b_gestaltung * GESTALTUNG.2 - BEITRAG.2 )
  V[['alt3']] = 0
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                            
# ################################################################# #

simple_model = apollo_estimate(apollo_beta, apollo_fixed,
                               apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))


apollo_saveOutput(simple_model)


#### Model 2  With all time interactions ####

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "prefspace",
  modelDescr ="Conditional Logit with normal time interactions with all attributes",
  indivID = "ID",
  mixing = FALSE,
  outputDirectory = "modeloutput/"
)



# ################################################################# #
#### DEFINE apollo_beta()                                   
# ################################################################# #

### Startwerte festlegen
apollo_beta=c(asc_gemeinschaft = 0,
              asc_klein = 0,
              b_groesse = 0,
              b_entfernung = 0,
              b_gemeinschaft = 0,
              b_kultur = 0,
              b_umweltbildung = 0,
              b_zugang = 0,
              b_gestaltung = 0,
              b_beitrag = 0,
              asc_gemeinschaftZ=0,
              asc_kleinZ=0,
              b_groesseZ = 0,
              b_entfernungZ = 0,
              b_gemeinschaftZ = 0,
              b_kulturZ = 0,
              b_umweltbildungZ = 0,
              b_zugangZ = 0,
              b_gestaltungZ = 0)





# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                      
# ################################################################# #



### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate" ){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = (-b_beitrag)*(asc_gemeinschaft + b_groesse * GROESSE.1 + b_entfernung * ENTFERNUNG.1 +
                                b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 + b_kultur * KULTURVERANSTALTUNGEN.1 +
                                b_umweltbildung * UMWELTBILDUNG.1 + b_zugang * ZUGANG.1 +
                                b_gestaltung * GESTALTUNG.1 - BEITRAG.1 + asc_gemeinschaftZ*Zeit +b_groesseZ * GROESSE.1*Zeit + b_entfernungZ * ENTFERNUNG.1*Zeit +
                                b_gemeinschaftZ * GEMEINSCHAFTSAKTIVITAETEN.1*Zeit + b_kulturZ * KULTURVERANSTALTUNGEN.1*Zeit +
                                b_umweltbildungZ * UMWELTBILDUNG.1*Zeit + b_zugangZ * ZUGANG.1*Zeit +
                                b_gestaltungZ * GESTALTUNG.1*Zeit )
  V[['alt2']] = (-b_beitrag)*(asc_klein + b_groesse * GROESSE.2 + b_entfernung * ENTFERNUNG.2 +
                                b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.2 + b_kultur * KULTURVERANSTALTUNGEN.2 +
                                b_umweltbildung * UMWELTBILDUNG.2 + b_zugang * ZUGANG.2 +
                                b_gestaltung * GESTALTUNG.2 - BEITRAG.2 + asc_kleinZ*Zeit+b_groesseZ * GROESSE.2*Zeit + b_entfernungZ * ENTFERNUNG.2*Zeit +
                                b_gemeinschaftZ * GEMEINSCHAFTSAKTIVITAETEN.2*Zeit + b_kulturZ * KULTURVERANSTALTUNGEN.2*Zeit +
                                b_umweltbildungZ * UMWELTBILDUNG.2*Zeit + b_zugangZ * ZUGANG.2*Zeit +
                                b_gestaltungZ * GESTALTUNG.2*Zeit)
  V[['alt3']] = 0
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                      
# ################################################################# #

prefspace = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))

apollo_saveOutput(prefspace)







#### Model 3  Heteroscedastic with no time interactions ####

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "heteros_model",
  modelDescr ="Conditional Logit with normal time interactions with all attributes",
  indivID = "ID",
  mixing = FALSE,  # True waere Mixed Logit
  outputDirectory = "modeloutput/" 
  )



# ################################################################# #
#### DEFINE apollo_beta()                                  
# ################################################################# #

### Startwerte festlegen
apollo_beta=c(asc_gemeinschaft = 0,
              asc_klein = 0,
              b_groesse = 0,
              b_entfernung = 0,
              b_gemeinschaft = 0,
              b_kultur = 0,
              b_umweltbildung = 0,
              b_zugang = 0,
              b_gestaltung = 0,
              b_beitrag = 0,
              lambda = 0)





# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                
# ################################################################# #



### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate" ){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = exp(lambda*Zeit)*(-b_beitrag)*(asc_gemeinschaft + b_groesse * GROESSE.1 + b_entfernung * ENTFERNUNG.1 +
                                                 b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 + b_kultur * KULTURVERANSTALTUNGEN.1 +
                                                 b_umweltbildung * UMWELTBILDUNG.1 + b_zugang * ZUGANG.1 +
                                                 b_gestaltung * GESTALTUNG.1 - BEITRAG.1 )
  
  V[['alt2']] = exp(lambda*Zeit)*(-b_beitrag)*(asc_klein + b_groesse * GROESSE.2 + b_entfernung * ENTFERNUNG.2 +
                                                 b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.2 + b_kultur * KULTURVERANSTALTUNGEN.2 +
                                                 b_umweltbildung * UMWELTBILDUNG.2 + b_zugang * ZUGANG.2 +
                                                 b_gestaltung * GESTALTUNG.2 -BEITRAG.2 )
  V[['alt3']] = 0
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                       
# ################################################################# #

heteros_model = apollo_estimate(apollo_beta, apollo_fixed,
                                apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))


apollo_saveOutput(heteros_model)


#### Model 4 Heteroscedastic with all time interactions ### 

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "fullmodel",
  modelDescr ="Conditional Logit with normal time interactions with all attributes",
  indivID = "ID",
  mixing = FALSE, # True waere Mixed Logit
  outputDirectory = "modeloutput/"
  )

# ################################################################# #
#### DEFINE apollo_beta()                                        ####
# ################################################################# #

### Startwerte festlegen
apollo_beta=c(asc_gemeinschaft = 0,
              asc_klein = 0,
              b_groesse = 0,
              b_entfernung = 0,
              b_gemeinschaft = 0,
              b_kultur = 0,
              b_umweltbildung = 0,
              b_zugang = 0,
              b_gestaltung = 0,
              b_beitrag = 0,
              asc_gemeinschaftZ=0,
              asc_kleinZ=0,
              b_groesseZ = 0,
              b_entfernungZ = 0,
              b_gemeinschaftZ = 0,
              b_kulturZ = 0,
              b_umweltbildungZ = 0,
              b_zugangZ = 0,
              b_gestaltungZ = 0,
              lambda = 0)

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #



### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate" ){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = exp(lambda*Zeit)*(-b_beitrag)*(asc_gemeinschaft + b_groesse * GROESSE.1 + b_entfernung * ENTFERNUNG.1 +
                                                 b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 + b_kultur * KULTURVERANSTALTUNGEN.1 +
                                                 b_umweltbildung * UMWELTBILDUNG.1 + b_zugang * ZUGANG.1 +
                                                 b_gestaltung * GESTALTUNG.1 - BEITRAG.1 + asc_gemeinschaftZ*Zeit +b_groesseZ * GROESSE.1*Zeit + b_entfernungZ * ENTFERNUNG.1*Zeit +
                                                 b_gemeinschaftZ * GEMEINSCHAFTSAKTIVITAETEN.1*Zeit + b_kulturZ * KULTURVERANSTALTUNGEN.1*Zeit +
                                                 b_umweltbildungZ * UMWELTBILDUNG.1*Zeit + b_zugangZ * ZUGANG.1*Zeit +
                                                 b_gestaltungZ * GESTALTUNG.1*Zeit )
  V[['alt2']] = exp(lambda*Zeit)*(-b_beitrag)*(asc_klein + b_groesse * GROESSE.2 + b_entfernung * ENTFERNUNG.2 +
                                                 b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.2 + b_kultur * KULTURVERANSTALTUNGEN.2 +
                                                 b_umweltbildung * UMWELTBILDUNG.2 + b_zugang * ZUGANG.2 +
                                                 b_gestaltung * GESTALTUNG.2 - BEITRAG.2 + asc_kleinZ*Zeit+b_groesseZ * GROESSE.2*Zeit + b_entfernungZ * ENTFERNUNG.2*Zeit +
                                                 b_gemeinschaftZ * GEMEINSCHAFTSAKTIVITAETEN.2*Zeit + b_kulturZ * KULTURVERANSTALTUNGEN.2*Zeit +
                                                 b_umweltbildungZ * UMWELTBILDUNG.2*Zeit + b_zugangZ * ZUGANG.2*Zeit +
                                                 b_gestaltungZ * GESTALTUNG.2*Zeit )
  V[['alt3']] = 0
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

fullmodel = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))

apollo_saveOutput(fullmodel)


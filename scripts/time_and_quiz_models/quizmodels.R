

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "Clogit",
  modelDescr ="Conditional Logit with normal quiz interactions with all attributes",
  indivID = "ID",
  mixing = FALSE
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
              b_gestaltungZ = 0)





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
  V[['alt1']] = (-b_beitrag)*(asc_gemeinschaft + b_groesse * GROESSE.1 + b_entfernung * ENTFERNUNG.1 +
                                b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 + b_kultur * KULTURVERANSTALTUNGEN.1 +
                                b_umweltbildung * UMWELTBILDUNG.1 + b_zugang * ZUGANG.1 +
                                b_gestaltung * GESTALTUNG.1 - BEITRAG.1 + asc_gemeinschaftZ*korrekt +b_groesseZ * GROESSE.1*korrekt + b_entfernungZ * ENTFERNUNG.1*korrekt +
                                b_gemeinschaftZ * GEMEINSCHAFTSAKTIVITAETEN.1*korrekt + b_kulturZ * KULTURVERANSTALTUNGEN.1*korrekt +
                                b_umweltbildungZ * UMWELTBILDUNG.1*korrekt + b_zugangZ * ZUGANG.1*korrekt +
                                b_gestaltungZ * GESTALTUNG.1*korrekt )
  V[['alt2']] = (-b_beitrag)*(asc_klein + b_groesse * GROESSE.2 + b_entfernung * ENTFERNUNG.2 +
                                b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.2 + b_kultur * KULTURVERANSTALTUNGEN.2 +
                                b_umweltbildung * UMWELTBILDUNG.2 + b_zugang * ZUGANG.2 +
                                b_gestaltung * GESTALTUNG.2 - BEITRAG.2 + asc_kleinZ*korrekt+b_groesseZ * GROESSE.2*korrekt + b_entfernungZ * ENTFERNUNG.2*korrekt +
                                b_gemeinschaftZ * GEMEINSCHAFTSAKTIVITAETEN.2*korrekt + b_kulturZ * KULTURVERANSTALTUNGEN.2*korrekt +
                                b_umweltbildungZ * UMWELTBILDUNG.2*korrekt + b_zugangZ * ZUGANG.2*korrekt +
                                b_gestaltungZ * GESTALTUNG.2*korrekt)
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

interactmodelquiz = apollo_estimate(apollo_beta, apollo_fixed,
                                    apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))



### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "Clogit",
  modelDescr ="Conditional Logit with normal time interactions with all attributes",
  indivID = "ID",
  mixing = FALSE # True waere Mixed Logit
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
  V[['alt1']] = exp(lambda*korrekt)*(-b_beitrag)*(asc_gemeinschaft + b_groesse * GROESSE.1 + b_entfernung * ENTFERNUNG.1 +
                                                    b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 + b_kultur * KULTURVERANSTALTUNGEN.1 +
                                                    b_umweltbildung * UMWELTBILDUNG.1 + b_zugang * ZUGANG.1 +
                                                    b_gestaltung * GESTALTUNG.1 - BEITRAG.1 )
  
  V[['alt2']] = exp(lambda*korrekt)*(-b_beitrag)*(asc_klein + b_groesse * GROESSE.2 + b_entfernung * ENTFERNUNG.2 +
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
#### MODEL ESTIMATION                                            ####
# ################################################################# #

heteros_modelquiz = apollo_estimate(apollo_beta, apollo_fixed,
                                    apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))





### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "Clogit",
  modelDescr ="Conditional Logit with quiz interactions with all attributes",
  indivID = "ID",
  mixing = FALSE # True waere Mixed Logit
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
  V[['alt1']] = exp(lambda*korrekt)*(-b_beitrag)*(asc_gemeinschaft + b_groesse * GROESSE.1 + b_entfernung * ENTFERNUNG.1 +
                                                    b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 + b_kultur * KULTURVERANSTALTUNGEN.1 +
                                                    b_umweltbildung * UMWELTBILDUNG.1 + b_zugang * ZUGANG.1 +
                                                    b_gestaltung * GESTALTUNG.1 - BEITRAG.1 + asc_gemeinschaftZ*korrekt +b_groesseZ * GROESSE.1*korrekt + b_entfernungZ * ENTFERNUNG.1*korrekt +
                                                    b_gemeinschaftZ * GEMEINSCHAFTSAKTIVITAETEN.1*korrekt + b_kulturZ * KULTURVERANSTALTUNGEN.1*korrekt +
                                                    b_umweltbildungZ * UMWELTBILDUNG.1*korrekt + b_zugangZ * ZUGANG.1*korrekt +
                                                    b_gestaltungZ * GESTALTUNG.1*korrekt )
  V[['alt2']] = exp(lambda*korrekt)*(-b_beitrag)*(asc_klein + b_groesse * GROESSE.2 + b_entfernung * ENTFERNUNG.2 +
                                                    b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.2 + b_kultur * KULTURVERANSTALTUNGEN.2 +
                                                    b_umweltbildung * UMWELTBILDUNG.2 + b_zugang * ZUGANG.2 +
                                                    b_gestaltung * GESTALTUNG.2 - BEITRAG.2 + asc_kleinZ*korrekt+b_groesseZ * GROESSE.2*korrekt + b_entfernungZ * ENTFERNUNG.2*korrekt +
                                                    b_gemeinschaftZ * GEMEINSCHAFTSAKTIVITAETEN.2*korrekt + b_kulturZ * KULTURVERANSTALTUNGEN.2*korrekt +
                                                    b_umweltbildungZ * UMWELTBILDUNG.2*korrekt + b_zugangZ * ZUGANG.2*korrekt +
                                                    b_gestaltungZ * GESTALTUNG.2*korrekt )
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

fullmodelquiz = apollo_estimate(apollo_beta, apollo_fixed,
                                apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))



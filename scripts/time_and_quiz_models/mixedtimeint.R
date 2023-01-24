

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "mixedtimeint",
  modelDescr ="Mixed Logit with normal time interactions with all attributes",
  indivID = "ID",
  mixing = TRUE, 
  HB=FALSE,
  nCores=11,
  outputDirectory = "modeloutput/"
  )

# ################################################################# #
#### DEFINE apollo_beta()                                        #
# ################################################################# #

### Startwerte festlegen
apollo_beta=c(mean_asc_gemeinschaft = 0.61,
              mean_asc_klein = 0.43,
              mean_b_groesse = 0.0,
              mean_b_entfernung = -0.17,
              mean_b_gemeinschaft = 0.06,
              mean_b_kultur = 0.06,
              mean_b_umweltbildung = 0.13,
              mean_b_zugang = 0.05,
              mean_b_gestaltung = 0.31,
              mean_b_beitrag = -0.12,
              sd_asc_gemeinschaft = 1,
              sd_asc_klein = 1,
              sd_b_groesse = 1,
              sd_b_entfernung = 1,
              sd_b_gemeinschaft = 1,
              sd_b_kultur = 1,
              sd_b_umweltbildung = 1,
              sd_b_zugang = 1,
              sd_b_gestaltung = 1,
              sd_b_beitrag = 1,
              asc_gemeinschaftZ=-0.4,
              asc_kleinZ=-0.03,
              b_groesseZ = -0.08,
              b_entfernungZ = -0.03,
              b_gemeinschaftZ = 0.2,
              b_kulturZ = 0.1,
              b_umweltbildungZ = -0.02,
              b_zugangZ = 0.2,
              b_gestaltungZ = -0.03)


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ##
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = draws, #50 für Code testen, min. 500 für verlässliche Ergebnisse
  interUnifDraws = c(),
  # nur normal distributed: (für cost  lognormal transformieren)
  interNormDraws = c("draws_asc_gemeinschaft",
                     "draws_asc_klein",
                     "draws_groesse",
                     "draws_entfernung",
                     "draws_gemeinschaft",
                     "draws_kultur",
                     "draws_umweltbildung",
                     "draws_zugang",
                     "draws_gestaltung",
                     "draws_beitrag"),
  # keine Intra-Individuen Heterogenität: (das wären abweichende Präferenzen selbe Individuen zwischen verschiedenen Choices)
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["asc_klein"]] = mean_asc_klein + sd_asc_klein * draws_asc_klein
  randcoeff[["asc_gemeinschaft"]] = mean_asc_gemeinschaft + sd_asc_gemeinschaft * draws_asc_gemeinschaft
  randcoeff[["b_groesse"]] = mean_b_groesse + sd_b_groesse * draws_groesse
  randcoeff[["b_entfernung"]] = mean_b_entfernung + sd_b_entfernung * draws_entfernung
  randcoeff[["b_gemeinschaft"]] = mean_b_gemeinschaft + sd_b_gemeinschaft * draws_gemeinschaft
  randcoeff[["b_kultur"]] = mean_b_kultur + sd_b_kultur * draws_kultur
  randcoeff[["b_umweltbildung"]] = mean_b_umweltbildung + sd_b_umweltbildung * draws_umweltbildung
  randcoeff[["b_zugang"]] = mean_b_zugang + sd_b_zugang * draws_zugang
  randcoeff[["b_gestaltung"]] = mean_b_gestaltung + sd_b_gestaltung * draws_gestaltung
  randcoeff[["b_beitrag"]] = - exp(mean_b_beitrag + sd_b_beitrag * draws_beitrag)
  
  return(randcoeff)
}




# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        #
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
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            #
# ################################################################# #


mixedtimeint = apollo_estimate(apollo_beta, apollo_fixed,
                                         apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=400, estimationRoutine="bfgs",
                                                                                                     hessianRoutine="analytic"))

apollo_saveOutput(mixedtimeint)


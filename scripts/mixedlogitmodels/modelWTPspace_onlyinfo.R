#### Model 3: Info Interactions only                            ####
# ################################################################# #

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "modelWTPspace_onlyinfo",
  modelDescr ="Mixed Logit in willingness-to-pay space with only infoscript interactions",
  indivID = "ID",
  mixing = TRUE, # Mixed Logit
  HB=FALSE,
  nCores=11,
  outputDirectory = "modeloutput/"
)



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ##
# ################################################################# #

### Startwerte festlegen (gerundete Koeffizienten aus conditional logit)
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
              asc_gemeinschaft_i = -0.11,
              asc_klein_i = -0.18,
              b_groesse_i = 0.08,
              b_entfernung_i = -0.07,
              b_gemeinschaft_i = 0.86,
              b_kultur_i = 0.01,
              b_umweltbildung_i = -0.03,
              b_zugang_i = 0.01,
              b_gestaltung_i = 0.08
)

### keine Parameter fix halten
apollo_fixed = c()

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
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ##
# ################################################################# #



### validieren
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = - (b_beitrag) *
    (asc_gemeinschaft + asc_gemeinschaft_i * info +
       
       b_groesse * GROESSE.1 + b_groesse_i * GROESSE.1 * info +
       
       b_entfernung * ENTFERNUNG.1 + b_entfernung_i * ENTFERNUNG.1 * info +
       
       b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 + b_gemeinschaft_i * GEMEINSCHAFTSAKTIVITAETEN.1 * info +
       
       b_kultur * KULTURVERANSTALTUNGEN.1 + b_kultur_i * KULTURVERANSTALTUNGEN.1 * info +
       
       b_umweltbildung * UMWELTBILDUNG.1 + b_umweltbildung_i * UMWELTBILDUNG.1 * info +
       
       b_zugang * ZUGANG.1 + b_zugang_i * ZUGANG.1 * info +
       
       b_gestaltung * GESTALTUNG.1 + b_gestaltung_i * GESTALTUNG.1 * info  -
       
       BEITRAG.1)
  
  V[['alt2']] = - (b_beitrag) *
    (asc_klein + asc_klein_i * info +
       
       b_groesse * GROESSE.2 + b_groesse_i * GROESSE.2 * info +
       
       b_entfernung * ENTFERNUNG.2 + b_entfernung_i * ENTFERNUNG.2 * info +
       
       b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.2 + b_gemeinschaft_i * GEMEINSCHAFTSAKTIVITAETEN.2 * info  +
       
       b_kultur * KULTURVERANSTALTUNGEN.2 + b_kultur_i * KULTURVERANSTALTUNGEN.2 * info +
       
       b_umweltbildung * UMWELTBILDUNG.2 + b_umweltbildung_i * UMWELTBILDUNG.2 * info +
       
       b_zugang * ZUGANG.2 + b_zugang_i * ZUGANG.2 * info +
       
       b_gestaltung * GESTALTUNG.2 + b_gestaltung_i * GESTALTUNG.2 * info  -
       
       BEITRAG.2)
  
  V[['alt3']] = 0
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V#,  # tell function to use list vector defined above
    #rows          = subsample==i
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
#### MODEL ESTIMATION                                            ##
# ################################################################# #

modelWTPspace_onlyinfo = apollo_estimate(apollo_beta, apollo_fixed,
                                         apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=400, estimationRoutine="bfgs",
                                                                                                     hessianRoutine="analytic"))


apollo_saveOutput(modelWTPspace_onlyinfo)

















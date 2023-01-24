#### Model 4: Question interactions only                           ####
# ################################################################# #

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "modelWTPspace_quesioninteraction",
  modelDescr ="Mixed Logit in willingness-to-pay space with question interactions only",
  indivID = "ID",
  mixing = TRUE, # Mixed Logit
  HB=FALSE,
  nCores=11,
  outputDirectory = "modeloutput/"
  
)



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ##
# ################################################################# #


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
              asc_gemeinschaft_w = -0.16,
              asc_klein_w = -0.14,
              b_groesse_w = 0.01,
              b_entfernung_w = -0.01,
              b_gemeinschaft_w = 0.04,
              b_kultur_w = 0.18,
              b_umweltbildung_w = 0.03,
              b_zugang_w = 0.01,
              b_gestaltung_w = -0.04,
              asc_gemeinschaft_s = -0.12,
              asc_klein_s = -0.06,
              b_groesse_s = 0.01,
              b_entfernung_s = -0.03,
              b_gemeinschaft_s = 0.10,
              b_kultur_s = 0.08,
              b_umweltbildung_s = -0.04,
              b_zugang_s = 0.03,
              b_gestaltung_s = -0.04
)

### keine Parameter fix halten
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ##
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = draws, 
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
    (asc_gemeinschaft +
       asc_gemeinschaft_w * wissen + asc_gemeinschaft_s * selbst +
       
       b_groesse * GROESSE.1 +
       b_groesse_w * GROESSE.1 * wissen + b_groesse_s * GROESSE.1 * selbst +
       
       b_entfernung * ENTFERNUNG.1 +
       b_entfernung_w * ENTFERNUNG.1 * wissen + b_entfernung_s * ENTFERNUNG.1 * selbst +
       
       
       
       b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 +
       b_gemeinschaft_w * GEMEINSCHAFTSAKTIVITAETEN.1 * wissen + b_gemeinschaft_s * GEMEINSCHAFTSAKTIVITAETEN.1 * selbst +
       
       b_kultur * KULTURVERANSTALTUNGEN.1 +
       b_kultur_w * KULTURVERANSTALTUNGEN.1 * wissen + b_kultur_s * KULTURVERANSTALTUNGEN.1 * selbst +
       
       b_umweltbildung * UMWELTBILDUNG.1 +
       b_umweltbildung_w * UMWELTBILDUNG.1 * wissen + b_umweltbildung_s * UMWELTBILDUNG.1 * selbst +
       
       b_zugang * ZUGANG.1 +
       b_zugang_w * ZUGANG.1 * wissen + b_zugang_s * ZUGANG.1 * selbst +
       
       b_gestaltung * GESTALTUNG.1 +
       b_gestaltung_w * GESTALTUNG.1 * wissen + b_gestaltung_s * GESTALTUNG.1 * selbst -
       
       BEITRAG.1)
  
  V[['alt2']] = - (b_beitrag) *
    (asc_klein +
       asc_klein_w * wissen + asc_klein_s * selbst +
       
       b_groesse * GROESSE.2 +
       b_groesse_w * GROESSE.2 * wissen + b_groesse_s * GROESSE.2 * selbst +
       
       b_entfernung * ENTFERNUNG.2 +
       b_entfernung_w * ENTFERNUNG.2 * wissen + b_entfernung_s * ENTFERNUNG.2 * selbst +
       
       b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.2 +
       b_gemeinschaft_w * GEMEINSCHAFTSAKTIVITAETEN.2 * wissen + b_gemeinschaft_s * GEMEINSCHAFTSAKTIVITAETEN.2 * selbst +
       
       b_kultur * KULTURVERANSTALTUNGEN.2 +
       b_kultur_w * KULTURVERANSTALTUNGEN.2 * wissen + b_kultur_s * KULTURVERANSTALTUNGEN.2 * selbst +
       
       b_umweltbildung * UMWELTBILDUNG.2 +
       b_umweltbildung_w * UMWELTBILDUNG.2 * wissen + b_umweltbildung_s * UMWELTBILDUNG.2 * selbst +
       
       b_zugang * ZUGANG.2 +
       b_zugang_w * ZUGANG.2 * wissen + b_zugang_s * ZUGANG.2 * selbst +
       
       b_gestaltung * GESTALTUNG.2 +
       b_gestaltung_w * GESTALTUNG.2 * wissen + b_gestaltung_s * GESTALTUNG.2 * selbst  -
       
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

modelWTPspace_quesioninteraction = apollo_estimate(apollo_beta, apollo_fixed,
                                                   apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=400, estimationRoutine="bfgs",
                                                                                                               hessianRoutine="analytic"))



# ################################################################# #
#### MODEL OUTPUTS                                               ##
# ################################################################# #

apollo_saveOutput(modelWTPspace_quesioninteraction)

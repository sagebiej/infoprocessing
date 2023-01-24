# ################################################################# #



#### Model 1: Full interaction terms                            ####
# ################################################################# #

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "modelWTPspace",
  modelDescr ="Mixed Logit in willingness-to-pay space with all interactions",
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
              asc_gemeinschaft_i = -0.11,
              asc_klein_i = -0.18,
              b_groesse_i = 0.08,
              b_entfernung_i = -0.07,
              b_gemeinschaft_i = 0.86,
              b_kultur_i = 0.01,
              b_umweltbildung_i = -0.03,
              b_zugang_i = 0.01,
              b_gestaltung_i = 0.08,
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
              b_gestaltung_s = -0.04,
              asc_gemeinschaft_iw = 0.06,
              asc_klein_iw = 0.18,
              b_groesse_iw = -0.01,
              b_entfernung_iw = 0.10,
              b_gemeinschaft_iw = -0.06,
              b_kultur_iw = -0.10,
              b_umweltbildung_iw = -0.05,
              b_zugang_iw = -0.02,
              b_gestaltung_iw = -0.08,
              asc_gemeinschaft_is = 0.24,
              asc_klein_is = 0.24,
              b_groesse_is = -0.01,
              b_entfernung_is = 0.01,
              b_gemeinschaft_is = -0.15,
              b_kultur_is = -0.04,
              b_umweltbildung_is = 0.11,
              b_zugang_is = -0.02,
              b_gestaltung_is = -0.14)

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
       asc_gemeinschaft_w * wissen + asc_gemeinschaft_s * selbst +
       asc_gemeinschaft_iw * info * wissen + asc_gemeinschaft_is * info * selbst +
       
       b_groesse * GROESSE.1 + b_groesse_i * GROESSE.1 * info +
       b_groesse_w * GROESSE.1 * wissen + b_groesse_s * GROESSE.1 * selbst +
       b_groesse_iw * GROESSE.1 * info * wissen + b_groesse_is * GROESSE.1 * info * selbst +
       
       b_entfernung * ENTFERNUNG.1 + b_entfernung_i * ENTFERNUNG.1 * info +
       b_entfernung_w * ENTFERNUNG.1 * wissen + b_entfernung_s * ENTFERNUNG.1 * selbst +
       b_entfernung_iw * ENTFERNUNG.1 * info * wissen + b_entfernung_is * ENTFERNUNG.1 * info * selbst +
       
       b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.1 + b_gemeinschaft_i * GEMEINSCHAFTSAKTIVITAETEN.1 * info +
       b_gemeinschaft_w * GEMEINSCHAFTSAKTIVITAETEN.1 * wissen + b_gemeinschaft_s * GEMEINSCHAFTSAKTIVITAETEN.1 * selbst +
       b_gemeinschaft_iw * GEMEINSCHAFTSAKTIVITAETEN.1 * info * wissen + b_gemeinschaft_is * GEMEINSCHAFTSAKTIVITAETEN.1 * info * selbst +
       
       b_kultur * KULTURVERANSTALTUNGEN.1 + b_kultur_i * KULTURVERANSTALTUNGEN.1 * info +
       b_kultur_w * KULTURVERANSTALTUNGEN.1 * wissen + b_kultur_s * KULTURVERANSTALTUNGEN.1 * selbst +
       b_kultur_iw * KULTURVERANSTALTUNGEN.1 * info * wissen + b_kultur_is * KULTURVERANSTALTUNGEN.1 * info * selbst +
       
       b_umweltbildung * UMWELTBILDUNG.1 + b_umweltbildung_i * UMWELTBILDUNG.1 * info +
       b_umweltbildung_w * UMWELTBILDUNG.1 * wissen + b_umweltbildung_s * UMWELTBILDUNG.1 * selbst +
       b_umweltbildung_iw * UMWELTBILDUNG.1 * info * wissen + b_umweltbildung_is * UMWELTBILDUNG.1 * info * selbst +
       
       b_zugang * ZUGANG.1 + b_zugang_i * ZUGANG.1 * info +
       b_zugang_w * ZUGANG.1 * wissen + b_zugang_s * ZUGANG.1 * selbst +
       b_zugang_iw * ZUGANG.1 * info * wissen + b_zugang_is * ZUGANG.1 * info * selbst +
       
       b_gestaltung * GESTALTUNG.1 + b_gestaltung_i * GESTALTUNG.1 * info +
       b_gestaltung_w * GESTALTUNG.1 * wissen + b_gestaltung_s * GESTALTUNG.1 * selbst +
       b_gestaltung_iw * GESTALTUNG.1 * info * wissen + b_gestaltung_is * GESTALTUNG.1 * info * selbst -
       
       BEITRAG.1)
  
  V[['alt2']] = - (b_beitrag) *
    (asc_klein + asc_klein_i * info +
       asc_klein_w * wissen + asc_klein_s * selbst +
       asc_klein_iw * info * wissen + asc_klein_is * info * selbst +
       
       b_groesse * GROESSE.2 + b_groesse_i * GROESSE.2 * info +
       b_groesse_w * GROESSE.2 * wissen + b_groesse_s * GROESSE.2 * selbst +
       b_groesse_iw * GROESSE.2 * info * wissen + b_groesse_is * GROESSE.2 * info * selbst +
       
       b_entfernung * ENTFERNUNG.2 + b_entfernung_i * ENTFERNUNG.2 * info +
       b_entfernung_w * ENTFERNUNG.2 * wissen + b_entfernung_s * ENTFERNUNG.2 * selbst +
       b_entfernung_iw * ENTFERNUNG.2 * info * wissen + b_entfernung_is * ENTFERNUNG.2 * info * selbst +
       
       b_gemeinschaft * GEMEINSCHAFTSAKTIVITAETEN.2 + b_gemeinschaft_i * GEMEINSCHAFTSAKTIVITAETEN.2 * info +
       b_gemeinschaft_w * GEMEINSCHAFTSAKTIVITAETEN.2 * wissen + b_gemeinschaft_s * GEMEINSCHAFTSAKTIVITAETEN.2 * selbst +
       b_gemeinschaft_iw * GEMEINSCHAFTSAKTIVITAETEN.2 * info * wissen + b_gemeinschaft_is * GEMEINSCHAFTSAKTIVITAETEN.2 * info * selbst +
       
       b_kultur * KULTURVERANSTALTUNGEN.2 + b_kultur_i * KULTURVERANSTALTUNGEN.2 * info +
       b_kultur_w * KULTURVERANSTALTUNGEN.2 * wissen + b_kultur_s * KULTURVERANSTALTUNGEN.2 * selbst +
       b_kultur_iw * KULTURVERANSTALTUNGEN.2 * info * wissen + b_kultur_is * KULTURVERANSTALTUNGEN.2 * info * selbst +
       
       b_umweltbildung * UMWELTBILDUNG.2 + b_umweltbildung_i * UMWELTBILDUNG.2 * info +
       b_umweltbildung_w * UMWELTBILDUNG.2 * wissen + b_umweltbildung_s * UMWELTBILDUNG.2 * selbst +
       b_umweltbildung_iw * UMWELTBILDUNG.2 * info * wissen + b_umweltbildung_is * UMWELTBILDUNG.2 * info * selbst +
       
       b_zugang * ZUGANG.2 + b_zugang_i * ZUGANG.2 * info +
       b_zugang_w * ZUGANG.2 * wissen + b_zugang_s * ZUGANG.2 * selbst +
       b_zugang_iw * ZUGANG.2 * info * wissen + b_zugang_is * ZUGANG.2 * info * selbst +
       
       b_gestaltung * GESTALTUNG.2 + b_gestaltung_i * GESTALTUNG.2 * info +
       b_gestaltung_w * GESTALTUNG.2 * wissen + b_gestaltung_s * GESTALTUNG.2 * selbst +
       b_gestaltung_iw * GESTALTUNG.2 * info * wissen + b_gestaltung_is * GESTALTUNG.2 * info * selbst -
       
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

modelWTPspace = apollo_estimate(apollo_beta, apollo_fixed,
                                apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=400, estimationRoutine="bfgs",
                                                                                            hessianRoutine="analytic"))



# ################################################################# #
#### MODEL OUTPUTS                                               ##
# ################################################################# #



apollo_saveOutput(modelWTPspace)
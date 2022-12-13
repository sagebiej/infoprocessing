## functions related to apollo

##### LR test modified function ####

apollo_lrTest_mod <-function (model1, model2){
  modelNames = list()
  LL = list()
  obs = list()
  k = list()
  inputs = list(model1, model2)
  for (i in 1:2) {
    modeluse = inputs[[i]]
    if (is.character(modeluse)) {
      filename = paste(paste(modeluse, "_output.txt", sep = ""))
      if (!file.exists(filename))
        stop("File ", filename, " not found!")
      lines = tryCatch(readLines(filename), warning = function(w) x = FALSE,
                       error = function(e) x = FALSE)
      if (is.logical(lines) && lines == FALSE)
        stop("Could not open file ", filename)
      id <- grepl(paste0("Model name"), lines)
      value = lines[which(id)]
      position = gregexpr(pattern = ":", value)[[1]][1]
      modelNames[[i]] = (substr(value, position + 2, nchar(value)))
      id <- grepl(paste0("Number of modelled outcomes"),
                  lines)
      if (any(id)) {
        value = lines[which(id)]
      }
      else {
        stop("Number of observations not found in ",
             filename)
      }
      position = gregexpr(pattern = ":", value)[[1]][1]
      obs[[i]] = as.double(substr(value, position + 1,
                                  nchar(value)))
      id1 <- grepl("LL\\(final, whole model)", lines)
      id2 <- grepl("LL\\(final)", lines)
      if (any(id1)) {
        value = lines[which(id1)]
      }
      else if (any(id2)) {
        value = lines[which(id2)]
      }
      else {
        stop("No final LL found in ", filename)
      }
      position = gregexpr(pattern = ":", value)[[1]][1]
      LL[[i]] = as.double(substr(value, position + 1, nchar(value)))
      id <- grepl("Estimated parameters", lines)
      if (any(id)) {
        value = lines[which(id)]
      }
      else {
        stop("Number of estimated parameters not found in ",
             filename)
      }
      position = gregexpr(pattern = ":", value)[[1]][1]
      k[[i]] = as.double(substr(value, position + 1, nchar(value)))
    }
    else {
      modelNames[[i]] = modeluse$apollo_control$modelName
      if (is.null(modeluse$maximum)) {
        stop("No LL found in ", paste0(modeluse))
      }
      else {
        LL[[i]] = modeluse$maximum
      }
      if (is.null(modeluse$nObs) || anyNA(modeluse$nObs[1])) {
        stop("Number of observations not found in ",
             paste0(modeluse))
      }
      else {
        obs[[i]] = modeluse$nObs[1]
      }
      nParams <- length(modeluse$apollo_beta)
      nFreeParams <- nParams
      if (!is.null(modeluse$apollo_fixed))
        nFreeParams <- nFreeParams - length(modeluse$apollo_fixed)
      k[[i]] = nFreeParams
    }
  }
  if (obs[[1]] != obs[[2]])
    stop("The two models to be compared were not estimated on the same number of observations. A likelihood ratio test cannot be used!")
  if (k[[1]] == k[[2]])
    stop("The two models to be compared have the same number of parameters. A likelihood ratio test cannot be used!")
  if (((k[[1]] - k[[2]]) * (LL[[1]] - LL[[2]])) < 0)
    stop("The model with more parameters does not have a better log-likelihood. A likelihood ratio test cannot be used!")
  if (LL[[2]] < LL[[1]]) {
    apollo_print(paste0("The order of your two models will be reversed in the output as model 1 has better fit than model 2."))
    apollo_print("\n")
    LL = LL[c(2, 1)]
    k = k[c(2, 1)]
    inputs = inputs[c(2, 1)]
    modelNames = modelNames[c(2, 1)]
  }
  output = matrix(0, nrow = 3, ncol = 2)
  output[1:2, 1] = round(unlist(LL), 2)
  output[1:2, 2] = round(unlist(k), 2)
  output[3, ] = output[2, ] - output[1, ]
  colnames(output) = c("LL", "par")
  rownames(output) = c(unlist(modelNames), "Difference")
  LR_test_value = 2 * output[3, 1]
  df = output[3, 2]
  p = stats::pchisq(LR_test_value, df, lower.tail = FALSE)
  print(output)
  cat("\nLikelihood ratio test-value:   ", round(LR_test_value,
                                                 2), "\n")
  cat("Degrees of freedom:            ", df, "\n")
  cat("Likelihood ratio test p-value: ", formatC(p), "\n")
  return(results = data.frame(LR = LR_test_value , df = as.character(as.integer(df)) , pvalue =p))
}



###### Regression Tables from Apollo Output with kable ####

# make.apollo.table.kable <- function(modelname, cols , colnames)
# 
# model <- modelname
# 
# cols <- length(colnames) + 3 
# 
# # Dataframe fuer Flextable erstellen
# dat <- data.frame(matrix(NA,nrow=29,ncol=cols))
# dat[,1] <- c("asc Gemeinschaftsgarten", "asc1", "asc Kleingarten", "asc3","Größe\npro 1000 m²","1",
#              "Entfernung\npro 1000 m","2","Gemeinschaftsaktivitäten","Gemeinschaft1",
#              "Kulturveranstaltungen","Kultur1","Umweltbildungsangebote","Bildung1",
#              "Zugang\nje zusätzlich geöffnetem Tag","3", "Gestaltung\nje Stufe naturnäher","4","Beitrag\npro 1 €","5",
#              "LL(start)","LL(final)","Rho-square","Adj. Rho-square","AIC","BIC",
#              "Anzahl Beobachtungen","Anzahl Befragte","Anzahl Parameter")
# colnames(dat) <- c("","main effects (mean)","main effects (standard deviations)","TIME")
# 
# ### relevante variablen aus Model-Output extrahieren
# z <- length(model$estimate)
# est <- model$estimate
# 
# est[grepl("sd_b",names(est))] <- abs(est[grepl("sd_b",names(est))]) # make sure all standard deviations are positive
# 
# names(est[c(10,20)])
# 
# timesten <- names(est[grepl("groesse",names(est))])
# timeshundred<-names(est[grepl("gemeinschaft|klein|entfernung|kultur|umwelt|zugang|gestaltung",names(est))])
# 
# 
# 
# est[timeshundred] <- est[timeshundred]*100
# est[timesten] <- est[timesten]*10 # groesse nur mit 10 skalieren
# 
# 
# se <- model$se
# se[timeshundred] <- se[timeshundred]*100
# se[c(3,13,23,32,41,50,59)] <- se[c(3,13,23,32,41,50,59)]*10
# est[c("mean_b_groesse","sd_b_groesse")] <- est[c("mean_b_groesse","sd_b_groesse")]*10 ## NEU! Groesse bei main effects noch mal mit 10 (also 100) skalieren
# 
# # yearly fee Koeffizienten anpassen
# est[c("mean_b_beitrag" , "sd_b_beitrag")] <- -exp(est[c("mean_b_beitrag" , "sd_b_beitrag")])/100 ## NEU! Beitrag wieder zurückskalieren
# se[c("mean_b_beitrag" , "sd_b_beitrag")] <- exp(se[c("mean_b_beitrag" , "sd_b_beitrag")])/100
# se <- abs(se) # alle standard deviations positiv
# s <- summary(model)
# p <- s$estimate[,4]
# n <- model$nIndivs
# 
# nParams     <- length(model$apollo_beta)
# nFreeParams <- nParams
# if(!is.null(model$apollo_fixed)) nFreeParams <- nFreeParams - length(model$apollo_fixed)
# 
# # Schleife fuer Signifikanz-Sterne
# p_wert <- function(p){
#   if(p < 0.01){print("***")}
#   else{if(p < 0.05){print("**")}
#     else{if(p < 0.1){print("*")}
#       else{print("")}}}}
# 
# # Tabelle erstellen
# df <- as.data.frame(matrix(data=NA,nrow=29,ncol=3))
# rownames(df) <- dat[,1]
# 
# 
# for (column in cols-1){
# 
# # Tabelle auffuellen
# for(j in 1:10){# Anzahl der Parameter
#   df[j*2-1,1] <- paste(round(est[j],2),p_wert(p[j]))
#   df[j*2,1] <- paste0("(",round(se[j],2),")")
#   df[j*2-1,2] <- paste(round(est[j+10],2),p_wert(p[j+10]))
#   df[j*2,2] <- paste0("(",round(se[j+10],2),")")
# }
# for(j in 1:9){
#   df[j*2-1,3] <- paste(round(est[j+20],2),p_wert(p[j+20]))
#   df[j*2,3] <- paste0("(",round(se[j+20],2),")")
#   
# }
# 
#   
# }  
#   
# # Model measures fuer Fusszeile in Tabelle ergaenzen
# df[21,1] <- round(model$LLStart,3)
# #df[23,1] <- round(model$LL0,3)
# df[22,1] <- round(model$LLout,3)
# df[23,1] <- round(1-(model$maximum/model$LL0),3)
# df[24,1] <- round(1-((model$maximum-nFreeParams)/model$LL0),3)
# df[25,1] <- round(-2*model$maximum + 2*nFreeParams,3)
# df[26,1] <- round(-2*model$maximum + nFreeParams*log(model$nObs),3)
# df[27,1] <- model$nObs
# df[28,1] <- model$nIndivs
# df[29,1] <- nFreeParams
# 
# dat[,2:4] <- df[,1:3]
# 
# # rownames
# rownames(dat) <- NULL
# dat[,1] <- c("ASC community garden", "", "ASC allotment garden", "","size (per 1,000 m²)","",
#              "distance (per 1,000 m)","","community activities","",
#              "cultural events","","environmental education","",
#              "access (per open day)","", " near-natural garden layout","",
#              "yearly fee (per 1 Euro)","",
#              "Log-likelihood at convergence", "Log-likelihood at constant(s)","Rho-square",
#              "Adj. Rho-square","AIC","BIC","Number of observations", "Number of respondents", "Number of parameters")
# 
# # colnames
# colnames(dat) <- c("","main effects (mean)","main effects (standard deviations)",# !!! NEU: Spalte drei neu eingefügt
#                    "TIME")
# 
# # replace NA
# dat[is.na(dat)] <- ""
# 
# 
# return(dat)
#}


###### Regression Tables from Apollo Output with texreg ####

## this function can be used to make a publication like table from models estimated with gmnl

make.apollo.table <- function(model) {
  l_table <- apollo_modelOutput(model,modelOutput_settings = list(printPVal=T) )
  
  createTexreg( coef = l_table[,"Estimate"] ,
                coef.names = rownames(l_table) ,
                se = l_table[,"Rob.s.e."],
                pvalues = l_table[,7] ,
                gof.names = c( "Log-likelihood(NULL)" , "Log-likelihood at convergence" , "Number of observations" , "Number of respondents") , gof.decimal = c(TRUE,TRUE,FALSE,FALSE) ,
                gof = c(model$LL0,model$LLout,model$nObs,model$nIndivs)
  )
}
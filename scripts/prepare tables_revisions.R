######################################################################
### Project       : Information processing in stated preference surveys
### Description    : Prepare tables included in paper                 
### Date          : 12.08.2021
### Version       : 2

######################################################################





#### Socio-demographic characteristics table 2 #### 



n_sample <- table(dataN$sample2)


# AGE

age_m <- aggregate((2020-q9_2)~ sample2,data=dataN,mean)[,2]
age_s <- aggregate((2020-q9_2)~ sample2,data=dataN,sd)[,2]
age <-data.frame(age_m,age_s)

# GENDER

male <- aggregate(q9_1==1~sample2,data=dataN,sum)[,2]/n_sample*100
female <- aggregate(q9_1>=2~sample2,data=dataN,sum)[,2]/n_sample*100

dataN$dummy_female <- ifelse(dataN$q9_1 ==2, 1, 0) #create dummy for female

# HOUSEHOLD SIZE

hh1 <- aggregate(q9_3==1~sample2,data=dataN,sum)[,2]/n_sample*100
hh2 <- aggregate(q9_3==2~sample2,data=dataN,sum)[,2]/n_sample*100
hh3 <- aggregate(q9_3==3~sample2,data=dataN,sum)[,2]/n_sample*100
hh4 <- aggregate(q9_3>=4~sample2,data=dataN,sum)[,2]/n_sample*100

hh12 <- hh1 + hh2 #hh size <= 2

hh_m <- aggregate((q9_3)~ sample2,data=dataN,mean)[,2]
hh_s <- aggregate((q9_3)~ sample2,data=dataN,sd)[,2]
hh <-data.frame(hh_m,hh_s)

# CHILDREN UNDER 14

chi1 <- aggregate(q9_4==1~sample2,data=dataN,sum)[,2]/n_sample*100
chi2 <- aggregate(q9_4==2~sample2,data=dataN,sum)[,2]/n_sample*100
chi3 <- aggregate(q9_4==3~sample2,data=dataN,sum)[,2]/n_sample*100
chi4 <- aggregate(q9_4>=4~sample2,data=dataN,sum)[,2]/n_sample*100
chi0 <- aggregate(q9_4==0~sample2,data=dataN,sum)[,2]/n_sample*100

chi1234 <- chi1 + chi2 + chi3 + chi4 #children under 14 

chi_m <- aggregate((q9_4)~ sample2,data=dataN,mean)[,2]
chi_s <- aggregate((q9_4)~ sample2,data=dataN,sd)[,2]
chi <-data.frame(chi_m,chi_s)


# HOUSEHOLD MONTHLY INCOME

in1 <- aggregate(q9_6==1~sample2,data=dataN,sum)[,2]/n_sample*100
in2 <- aggregate(q9_6==2~sample2,data=dataN,sum)[,2]/n_sample*100
in3 <- aggregate(q9_6==3~sample2,data=dataN,sum)[,2]/n_sample*100
in4 <- aggregate(q9_6==4~sample2,data=dataN,sum)[,2]/n_sample*100
in5 <- aggregate(q9_6==5~sample2,data=dataN,sum)[,2]/n_sample*100
in6 <- aggregate(q9_6==6~sample2,data=dataN,sum)[,2]/n_sample*100

in123 <- in1 + in2 + in3 #income less than 3000
in456 <- in4 + in5 + in6 #income 3000 or more 

dataN$dummy_income3000plus <- ifelse(dataN$q9_6 <=3, 0, 1) #create dummy for income

# EDUCATION


ed1 <- aggregate(q9_5==7~sample2,data=dataN,sum)[,2]/n_sample*100
ed2 <- aggregate(q9_5<7~sample2,data=dataN,sum)[,2]/n_sample*100

dataN$dummy_unidegree <- ifelse(dataN$q9_5 ==7, 1, 0) #create dummy for university degree

# COMMUNITY GARDENS

al1 <- aggregate(q3_2_a==3~sample2,data=dataN,sum)[,2]/n_sample*100
al2 <- aggregate(q3_2_a<3~sample2,data=dataN,sum)[,2]/n_sample*100

dataN$dummy_communitygarden <- ifelse(dataN$q3_2_a ==3, 1, 0) #create dummy for community garden

# ALLOTMENT GARDENS

co1 <- aggregate(q3_2_b==3~sample2,data=dataN,sum)[,2]/n_sample*100
co2 <- aggregate(q3_2_b<3~sample2,data=dataN,sum)[,2]/n_sample*100

dataN$dummy_allotmentgarden <- ifelse(dataN$q3_2_b ==3, 1, 0) #create dummy for allotment garden

# merge data

# merge data given in percent
soziodem_merge <- data.frame(matrix(c(female, male, 
                                      in123, in456,
                                      ed1, ed2,
                                      al1, al2, 
                                      co1, co2),
                                    ncol=6,byrow=TRUE))
soziodem_merge <- data.frame(c("Female*", "Male",
                               "Less than 3,000", "3,000 or more", 
                               "University degree", "No university degree",
                               "Regular use of community gardens", "No use of community gardens",
                               "Regular use of allotment gardens", "No use of allotment gardens"),
                             matrix(apply(soziodem_merge,1,function(x) paste0(round(x,1),"%")),
                                    ncol=6,byrow=TRUE))

# merge age
age <- c("Age",apply(age,1,function(x)paste0(round(x[1],1)," (",round(x[2],1),")")))

#merge householdsize
hh <- c("Household size",apply(hh,1,function(x)paste0(round(x[1],1)," (",round(x[2],1),")")))

#merge children
chi <- c("Number of children under 14",apply(chi,1,function(x)paste0(round(x[1],1)," (",round(x[2],1),")")))

# merge all data
soziodem <- rbind(age,
                  c("Gender",rep("",6)),soziodem_merge[1:2,],
                  hh,
                  chi,
                  c("Household monthly income [EUR]",rep("",6)),soziodem_merge[3:4,],
                  c("University education",rep("",6)),soziodem_merge[5:6,],
                  c("Use of community gardens",rep("",6)),soziodem_merge[7:8,],
                  c("Use of allotment gardens",rep("",6)),soziodem_merge[9:10,],
                  c("Number of respondents",n_sample))

colnames(soziodem) <- c("Characteristic","No additional info or questions","Quiz","Self-reference","Information","Info x quiz","Info x self-reference")
rownames(soziodem) <- NULL

##Test for balance of the samples
#prepare for testing 
attach(dataN)
testlist<- list(q9_2, q9_3, q9_4)
testlist2 <- list(dummy_female, dummy_unidegree, dummy_income3000plus, dummy_communitygarden, dummy_allotmentgarden)
nameslist <- as.data.frame(c("Age", "Household size", "Children under 14"))
nameslist2 <- as.data.frame(c("Gender", "University degree", "Income", "Community gardens", "Allotment gardens"))
colnames(nameslist) <- c("characteristic")
colnames(nameslist2) <- c("characteristic")

##Chi-squared Test#
chisq_results <- c()
k=1
for (i in testlist2) {
  chisq_results[k]<- ((chisq.test(i, dataN$sample2)$p.value))
  k=k+1
}
#merge test results 
chisq_results <- as.data.frame(round(chisq_results, 2))
colnames(chisq_results) <- c("pvalue_chisq")
chisq_results <- cbind(nameslist2, chisq_results)
print(chisq_results)

#Kruskal-Wallis-Test# (relativ langsam)
kruskal_results <- c()
k=1
for (i in testlist) {
  kruskal_results[k] <- ((kruskal.test(i ~ sample2)$p.value))
  k=k+1
}
kruskal_results <- as.data.frame(round(kruskal_results, 2))
colnames(kruskal_results) <- c("pvalue_kruskal")

#merge test results 
kruskal_results <- cbind(nameslist, kruskal_results)
print(kruskal_results)

detach(dataN)


#### Mixed Logit Model with treatment interactions ####

model <- modelWTPspace

# make dataframe
dat <- data.frame(matrix(NA,nrow=29,ncol=8))


### get estimates from model output
z <- length(model$estimate)
est <- model$estimate

est[grepl("sd_b",names(est))] <- abs(est[grepl("sd_b",names(est))]) # make sure all standard deviations are positive

names(est[c(10,20)])

timesten <- names(est[grepl("groesse",names(est))])
timeshundred<-names(est[grepl("gemeinschaft|klein|entfernung|kultur|umwelt|zugang|gestaltung",names(est))])



est[timeshundred] <- est[timeshundred]*100
est[timesten] <- est[timesten]*10 # groesse nur mit 10 skalieren


se <- model$se
se[timeshundred] <- se[timeshundred]*100
se[c(3,13,23,32,41,50,59)] <- se[c(3,13,23,32,41,50,59)]*10






s <- summary(model)
p <- s$estimate[,4]
n <- model$nIndivs



nParams     <- length(model$apollo_beta)
nFreeParams <- nParams
if(!is.null(model$apollo_fixed)) nFreeParams <- nFreeParams - length(model$apollo_fixed)

# loop for significance stars
p_wert <- function(p){
  if(p < 0.01){print("***")}
  else{if(p < 0.05){print("**")}
    else{if(p < 0.1){print("*")}
      else{print("")}}}}

# make table
df <- as.data.frame(matrix(data=NA,nrow=29,ncol=7))
#rownames(df) <- dat[,1]



# fill table
for(j in 1:10){# number of parameters (mean and sd of main effects)
  df[j*2-1,1] <- paste(round(est[j],2),p_wert(p[j]))
  df[j*2,1] <- paste0("(",round(se[j],2),")")
  df[j*2-1,2] <- paste(round(est[j+10],2),p_wert(p[j+10]))
  df[j*2,2] <- paste0("(",round(se[j+10],2),")")
}
for(j in 1:9){
  df[j*2-1,3] <- paste(round(est[j+20],2),p_wert(p[j+20]))
  df[j*2,3] <- paste0("(",round(se[j+20],2),")")
  df[j*2-1,4] <- paste(round(est[j+29],2),p_wert(p[j+29]))
  df[j*2,4] <- paste0("(",round(se[j+29],2),")")
  df[j*2-1,5] <- paste(round(est[j+38],2),p_wert(p[j+38]))
  df[j*2,5] <- paste0("(",round(se[j+38],2),")")
  df[j*2-1,6] <- paste(round(est[j+47],2),p_wert(p[j+47]))
  df[j*2,6] <- paste0("(",round(se[j+47],2),")")
  df[j*2-1,7] <- paste(round(est[j+56],2),p_wert(p[j+56]))
  df[j*2,7] <- paste0("(",round(se[j+56],2),")")
}

# Model measures fuer Fusszeile in Tabelle ergaenzen
df[21,1] <- round(model$LLStart,3)
#df[23,1] <- round(model$LL0,3)
df[22,1] <- round(model$LLout,3)
df[23,1] <- round(1-(model$maximum/model$LL0),3)
df[24,1] <- round(1-((model$maximum-nFreeParams)/model$LL0),3)
df[25,1] <- round(-2*model$maximum + 2*nFreeParams,3)
df[26,1] <- round(-2*model$maximum + nFreeParams*log(model$nObs),3)
df[27,1] <- model$nObs
df[28,1] <- model$nIndivs
df[29,1] <- nFreeParams

dat[,2:8] <- df[,1:7]

# rownames
rownames(dat) <- NULL
dat[,1] <- c("ASC community garden", "", "ASC allotment garden", "","Size (per 1,000 m²)","",
             "Distance (per 1,000 m)","","Community activities","",
             "Cultural events","","Environmental education","",
             "Access (per open day)","", "Near-natural garden layout","",
             "Yearly fee (per 1 Euro)","",
             "Log-likelihood at convergence", "Log-likelihood at constant(s)","Rho-square",
             "Adj. Rho-square","AIC","BIC","Number of observations", "Number of respondents", "Number of parameters")

# colnames
colnames(dat) <- c("","Main effects (mean)","Main effects (standard deviations)",# !!! NEU: Spalte drei neu eingefügt
                   "INFO","QUIZ","SELF","INFO x QUIZ","INFO x SELF")

# replace NA
dat[is.na(dat)] <- ""

mixedtable <- dat



#### Mixed logit with time interactions #### 

model <- mixedtimeinthet

# Dataframe fuer Flextable erstellen
dat <- data.frame(matrix(NA,nrow=31,ncol=4))


### get parameters
z <- length(model$estimate)
est <- model$estimate

est[grepl("sd_b",names(est))] <- abs(est[grepl("sd_b",names(est))]) # make sure all standard deviations are positive



timesten <- names(est[grepl("groesse",names(est))])
timeshundred<-names(est[grepl("gemeinschaft|klein|entfernung|kultur|umwelt|zugang|gestaltung",names(est))])



est[timeshundred] <- est[timeshundred]*100
est[timesten] <- est[timesten]*10 # scale size


se <- model$se
se[timeshundred] <- se[timeshundred]*100
se[timesten] <- se[timesten]*10


s <- summary(model)
p <- s$estimate[,4]
n <- model$nIndivs

nParams     <- length(model$apollo_beta)
nFreeParams <- nParams
if(!is.null(model$apollo_fixed)) nFreeParams <- nFreeParams - length(model$apollo_fixed)

# loop for significance stars
p_wert <- function(p){
  if(p < 0.01){print("***")}
  else{if(p < 0.05){print("**")}
    else{if(p < 0.1){print("*")}
      else{print("")}}}}

# make table
df <- as.data.frame(matrix(data=NA,nrow=31,ncol=3))




# fill table
for(j in 1:10){
  df[j*2-1,1] <- paste(round(est[j],2),p_wert(p[j]))
  df[j*2,1] <- paste0("(",round(se[j],2),")")
}  

for(j in 1:10){  
  df[j*2-1,2] <- paste(round(est[j+10],2),p_wert(p[j+10]))
  df[j*2,2] <- paste0("(",round(se[j+10],2),")")
}
for(j in 1:9){
  df[j*2-1,3] <- paste(round(est[j+20],2),p_wert(p[j+20]))
  df[j*2,3] <- paste0("(",round(se[j+20],2),")")
  
}

df[21,1] <- paste(round(est["lambda"],2),p_wert(p["lambda"]))
df[22,1] <- paste0("(",round(se["lambda"],2),")")

# Add measures of fit
df[23,1] <- round(model$LLStart,3)
#df[23,1] <- round(model$LL0,3)
df[24,1] <- round(model$LLout,3)
df[25,1] <- round(1-(model$maximum/model$LL0),3)
df[26,1] <- round(1-((model$maximum-nFreeParams)/model$LL0),3)
df[27,1] <- round(-2*model$maximum + 2*nFreeParams,3)
df[28,1] <- round(-2*model$maximum + nFreeParams*log(model$nObs),3)
df[29,1] <- model$nObs
df[30,1] <- model$nIndivs
df[31,1] <- nFreeParams

dat[,2:4] <- df[,1:3]

# rownames
rownames(dat) <- NULL
dat[,1] <- c("ASC community garden", "", "ASC allotment garden", "","Size (per 1,000 m²)","",
             "Distance (per 1,000 m)","","Community activities","",
             "Cultural events","","Environmental education","",
             "Access (per open day)","", " Near-natural garden layout","",
             "Yearly fee (per 1 Euro)","", "Time on error variance (Phi)","",
             "Log-likelihood at convergence", "Log-likelihood at constant(s)","Rho-square",
             "Adj. Rho-square","AIC","BIC","Number of observations", "Number of respondents", "Number of parameters")

# colnames
colnames(dat) <- c("","Main effects (mean)","Main effects (standard deviations)", "Time interactions")

# replace NA
dat[is.na(dat)] <- ""


mixedtime <- dat




#### Mixed logit with no interactions #### 

model <- modelWTPspacenointeractions

dat <- data.frame(matrix(NA,nrow=29,ncol=3))


### extract relevant variables
z <- length(model$estimate)
est <- model$estimate

est[grepl("sd_b",names(est))] <- abs(est[grepl("sd_b",names(est))]) # make sure all standard deviations are positive

names(est[c(10,20)])

timesten <- names(est[grepl("groesse",names(est))])
timeshundred<-names(est[grepl("gemeinschaft|klein|entfernung|kultur|umwelt|zugang|gestaltung",names(est))])



est[timeshundred] <- est[timeshundred]*100
est[timesten] <- est[timesten]*10 # scale size with 10


se <- model$se
se[timeshundred] <- se[timeshundred]*100
se[c(3,13,23,32,41,50,59)] <- se[c(3,13,23,32,41,50,59)]*10


s <- summary(model)
p <- s$estimate[,4]
n <- model$nIndivs



nParams     <- length(model$apollo_beta)
nFreeParams <- nParams
if(!is.null(model$apollo_fixed)) nFreeParams <- nFreeParams - length(model$apollo_fixed)


p_wert <- function(p){
  if(p < 0.01){print("***")}
  else{if(p < 0.05){print("**")}
    else{if(p < 0.1){print("*")}
      else{print("")}}}}


df <- as.data.frame(matrix(data=NA,nrow=29,ncol=2))




for(j in 1:10){
  df[j*2-1,1] <- paste(round(est[j],2),p_wert(p[j]))
  df[j*2,1] <- paste0("(",round(se[j],2),")")
  df[j*2-1,2] <- paste(round(est[j+10],2),p_wert(p[j+10]))
  df[j*2,2] <- paste0("(",round(se[j+10],2),")")
}




df[21,1] <- round(model$LLStart,3)
#df[23,1] <- round(model$LL0,3)
df[22,1] <- round(model$LLout,3)
df[23,1] <- round(1-(model$maximum/model$LL0),3)
df[24,1] <- round(1-((model$maximum-nFreeParams)/model$LL0),3)
df[25,1] <- round(-2*model$maximum + 2*nFreeParams,3)
df[26,1] <- round(-2*model$maximum + nFreeParams*log(model$nObs),3)
df[27,1] <- model$nObs
df[28,1] <- model$nIndivs
df[29,1] <- nFreeParams

dat[,2:3] <- df[,1:2]

# rownames
rownames(dat) <- NULL
dat[,1] <- c("ASC community garden", "", "ASC allotment garden", "","Size (per 1,000 m²)","",
             "Distance (per 1,000 m)","","Community activities","",
             "Cultural events","","Environmental education","",
             "Access (per open day)","", "Near-natural garden layout","",
             "Yearly fee (per 1 Euro)","",
             "Log-likelihood at convergence", "Log-likelihood at constant(s)","Rho-square",
             "Adj. Rho-square","AIC","BIC","Number of observations", "Number of respondents", "Number of parameters")

# colnames
colnames(dat) <- c("","Main effects (mean)","Main effects (standard deviations)")

# replace NA
dat[is.na(dat)] <- ""

nointeractionstable <- dat



######################################################################
### Project       : Information processing in stated preference surveys
### Description    : Prepare and Estimate OLS and count models                 
### Date          : 22.11.2022
### Version       : 3

######################################################################


## OLS and Count Models

data <- dataN %>%
  select(c('serial', 'korrekt', 'interviewLength', 'sample', 'sample2', 'q3_3_zeit', 'q4_zeit', 'q6_opt', 'q6_zeit',  'q8_6_a', 'q8_6_b', 'q9_1', 'q9_2', 'q9_3', 'q9_4', 'q9_5', 'q9_6', 'q9_7', 'q9_8' , "invsrc") )%>%
  filter(sample!=3) %>%
  mutate(across(everything() , ~ as.numeric(as.vector(.) ) ) ,
         across(c("q8_6_a","q8_6_b") ,~  recode(. ,'1'=1,'2'=1,'3'=1,'4'=1,'5'=0,'6'=0) ) , # Recode usage of gardens
         q9_5 = recode(q9_5,'1'=0,'2'=0,'3'=0,'4'=0,'5'=0,'6'=0,'7'=1,'8'=0),  # recode
         q9_6 = recode(q9_6,'1'=1,'2'=1,'3'=1,'4'=2,'5'=2,'6'=2) ,
         q9_1 = recode(q9_1,'1'=0,'2'=1,'3'=0 ) ,
         across(c(q9_1 , q9_5, q8_6_a, q8_6_b, invsrc), ~  .-mean(.,na.rm=T) ),  #mean center variables inv source 1=Forsa 2 = Postal invitations
         q9_2 = ((2020-q9_2)-mean((2020-q9_2),na.rm=TRUE))/10 ,
         other_time = interviewLength-q3_3_zeit,
         across(ends_with(c("zeit","time","Length")),~case_when(interviewLength>quantile(interviewLength,na.rm=TRUE)[4]+IQR(interviewLength,na.rm=TRUE) ~NaN,TRUE ~ .  ))
  ) %>%
  as.data.frame()

# data3 <- data %>% 
#   mutate(across(ends_with(c("zeit","time","Length")),~case_when(.>quantile(.,na.rm=TRUE)[4]+IQR(.,na.rm=TRUE) ~NaN,TRUE ~ .  )))
# 
# data2<-data
# # Remove speeders as in preregistration
# data2[which(data$q3_3_zeit>quantile(data$q3_3_zeit,na.rm=TRUE)[4]+IQR(data$q3_3_zeit,na.rm=TRUE)),"q3_3_zeit"]<-NA
# data[which(data$interviewLength>quantile(data$interviewLength,na.rm=TRUE)[4]+IQR(data$interviewLength,na.rm=TRUE)),"interviewLength"]<-NA
# data[which(data$q4_zeit>quantile(data$q4_zeit,na.rm=TRUE)[4]+IQR(data$q4_zeit,na.rm=TRUE)),"q4_zeit"]<-NA
# data[which(data$q6_zeit>quantile(data$q6_zeit,na.rm=TRUE)[4]+IQR(data$q6_zeit,na.rm=TRUE)),"q6_zeit"]<-NA

# Create independent variables Info etc.
for(i in 1:nrow(data)){
  if(data$sample2[i]>=4){data$info[i] <- 1}else{data$info[i] <- 0}
  if(data$sample2[i]==2 | data$sample2[i]==5){data$wissen[i] <- 1}else{data$wissen[i] <- 0}
  if(data$sample2[i]==3 | data$sample2[i]==6){data$selbst[i] <- 1}else{data$selbst[i] <- 0}
  if(data$sample2[i]==5){data$inter_info_wissen[i] <- 1}else{data$inter_info_wissen[i] <- 0}
  if(data$sample2[i]==6){data$inter_info_selbst[i] <- 1}else{data$inter_info_selbst[i] <- 0}
  
}

ols.clean <- data %>%
  select(korrekt, q3_3_zeit, interviewLength, other_time, q6_opt,q6_zeit, info, wissen,selbst,inter_info_wissen,inter_info_selbst,
         q9_1,q9_2,q9_5,q8_6_a,q8_6_b ,invsrc) %>%
  drop_na(info, wissen,selbst,inter_info_wissen,inter_info_selbst, ## drop missings casewise
          q9_1,q9_2,q9_5,q8_6_a,q8_6_b ,invsrc)

#====================================
# Regression



#### Quiz

# Quiz no socio demographics  
model_2 <- summary(lm(korrekt~info+wissen+selbst+inter_info_wissen+inter_info_selbst , data = ols.clean))

# Quiz with socio-demographic
model_2_sozio <- summary(lm(korrekt~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                              q9_1+q9_2+q9_5+q8_6_a+q8_6_b +invsrc, data = ols.clean))

##count models

Quizcountmodels<- list()

Quizcountmodels[["Poisson"]] <- (glm(korrekt~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                                    q9_1+q9_2+q9_5+q8_6_a+q8_6_b +invsrc, data = ols.clean, family = "poisson"))

Quizcountmodels[["Negative Binomial"]] <- (glm.nb(korrekt~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                                                  q9_1+q9_2+q9_5+q8_6_a+q8_6_b +invsrc, data = ols.clean, control = glm.control(maxit = 500)))



# Regression on time (q3_3_zeit - informationpage) as dependent variable



model_1 <- summary(lm(q3_3_zeit~info+wissen+selbst+inter_info_wissen+inter_info_selbst, data = ols.clean))



model_1_sozio <- summary(lm(q3_3_zeit~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                              q9_1+q9_2+q9_5+q8_6_a+q8_6_b+invsrc, data = ols.clean))


# total time minus time on info page 

model_4 <- summary(lm(other_time~info+wissen+selbst+inter_info_wissen+inter_info_selbst, data = ols.clean))

model_4_sozio <- summary(lm(other_time~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                              q9_1+q9_2+q9_5+q8_6_a+q8_6_b+invsrc, data = ols.clean))


# Time in DCE

model_4a <- summary(lm(q6_zeit~info+wissen+selbst+inter_info_wissen+inter_info_selbst, data = ols.clean))

model_4a_sozio <- summary(lm(q6_zeit~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                              q9_1+q9_2+q9_5+q8_6_a+q8_6_b+invsrc, data = ols.clean))



# Regression with  frequency of opt out choices

model_3 <- summary(lm(q6_opt~info+wissen+selbst+inter_info_wissen+inter_info_selbst, data = ols.clean))

model_3_sozio <- summary(lm(q6_opt~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                              q9_1+q9_2+q9_5+q8_6_a+q8_6_b+invsrc, data = ols.clean))

## count models for appendix

Optoutcountmodels<- list()

Optoutcountmodels[["Poisson"]] <- (glm(q6_opt~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                                         q9_1+q9_2+q9_5+q8_6_a+q8_6_b +invsrc, data = ols.clean, family = "poisson"))

Optoutcountmodels[["Negative Binomial"]] <- (glm.nb(q6_opt~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                                                      q9_1+q9_2+q9_5+q8_6_a+q8_6_b +invsrc, data = ols.clean))

Optoutcountmodels[["Zero Inflated"]] <- (zeroinfl(q6_opt~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                                                    q9_1+q9_2+q9_5+q8_6_a+q8_6_b +invsrc, data = ols.clean,  dist = "negbin"))

Optoutcountmodels[["Hurdle"]] <- (hurdle(q6_opt~info+wissen+selbst+inter_info_wissen+inter_info_selbst+
                                           q9_1+q9_2+q9_5+q8_6_a+q8_6_b +invsrc, data = ols.clean,  dist = "negbin"))



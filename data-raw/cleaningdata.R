library(rms); library(dplyr); library(ggplot2); library(survival);  library(foreign); library(haven); library(reshape); library(mosaic); library(Matching); library(Epi); library(tableone); library(faraway); library(Hmisc); library(pander);library(dplyr);library(tableone);library(Epi);library(survival);library(Matching);library(sm);library(optmatch);library(RItools);library(nnet);library(Hmisc);library(lattice);library(twang);library(survival);library(knitr);library(MatchIt)




if (!file.exists("data-raw/Telemedicine2.csv")) {
  Telemedicine <- read_csv("~/Dropbox/Dissertation_David Ngendahimana/package_development/SensR/data-raw/Telemedicine2.csv")
}

Telemedicine = read.csv("data-raw/Telemedicine2.csv")
#### ESDR
Telemedicine$ESRD.HTN = as.numeric(Telemedicine$ESRD == "HTN")
Telemedicine$ESRD.Other = as.numeric(Telemedicine$ESRD == "Other")
Telemedicine$ESRD.PKD = as.numeric(Telemedicine$ESRD == "PKD")
Telemedicine$ESRD.diab = as.numeric(Telemedicine$ESRD == "Diabetes")
Telemedicine$ESRD.glm = as.numeric(Telemedicine$ESRD == "Glomerulonephritis")

#### Induction
Telemedicine$Induction = ifelse(Telemedicine$Induction=="THYMO","THYMO","SIMULECT")
Telemedicine$Induction.SIM = as.numeric(Telemedicine$Induction == "SIMULECT")
Telemedicine$Induction.THY = as.numeric(Telemedicine$Induction == "THYMO")

###PSrisk

Telemedicine$PSrisk = as.factor(ifelse(Telemedicine$PSrisk == "LOW","LOW","MODERATE"))
Telemedicine$PSrisk.Low = as.numeric(Telemedicine$PSrisk == "LOW")
Telemedicine$PS.Moderate = as.numeric(Telemedicine$PSrisk == "MODERATE")

####Race
Telemedicine$Race1 = as.factor(ifelse(Telemedicine$Race == "WHITE","WHITE","OTHER"))
Telemedicine$Race.wht = as.numeric(Telemedicine$Race1 == "WHITE")
Telemedicine$Race.other = as.numeric(Telemedicine$Race1 == "OTHER")


### Hypertension
Telemedicine$HypertensionYES = as.numeric(Telemedicine$Hypertension == "YES")
Telemedicine$HypertensionNO = as.numeric(Telemedicine$Hypertension == "YES")


Telemedicine$Readmission = factor(Telemedicine$Readmission,level = c("YES","NO"))


Telemedicine$Surgeon.eqs <- as.numeric(Telemedicine$Surgeon=="eqs")
Telemedicine$Surgeon.kjw <- as.numeric(Telemedicine$Surgeon=="kjw")
Telemedicine$Surgeon.vrh <- as.numeric(Telemedicine$Surgeon=="vrh")

Telemedicine$GenderM = as.numeric(Telemedicine$Gender=="MALE")
Telemedicine$GenderF = as.numeric(Telemedicine$Gender=="FEMALE")

Telemedicine$HeartdzYES  = as.numeric(Telemedicine$Heartdz =="YES")
Telemedicine$HeartdzNO  = as.numeric(Telemedicine$Heartdz =="NO")

Telemedicine$TransplantTypeLiving  = as.numeric(Telemedicine$TransplantType=="Living")
Telemedicine$TransplantTypeDeceased  = as.numeric(Telemedicine$TransplantType=="Deceased")

Telemedicine$DGFYES  = as.numeric(Telemedicine$DGF=="YES")
Telemedicine$DGFNO  = as.numeric(Telemedicine$DGF=="NO")

Telemedicine$Telehealth.n=as.numeric(Telemedicine$Telehealth)



Telemedicine$Graftstatus = ifelse(Telemedicine$Graftstatus=="FAIL","FAIL","FUNCTION")

Telemedicine$Telehealth = factor(Telemedicine$Telehealth,level = c("YES","NO"))
Telemedicine$Telehealth.n =  abs(as.numeric(Telemedicine$Telehealth)-2)


psmodel1 <- glm(Telehealth.n ~ Age + Gender + Race1 + ESRD + BMI  + TransplantType + Hypertension + Dialysis + DGF + Induction + PSrisk + Surgeon, family=binomial(), data=Telemedicine)## removed all improper variables.-> ps from model makes sense i.e Patients on Telehealth have lower propensity for readmission.

Telemedicine$ps <- psmodel1$fitted
Telemedicine$linps <- psmodel1$linear.predictors
Telemedicine=subset(Telemedicine,ps >0.001)

set.seed(23547)
X <- Telemedicine$linps
Tr <- as.logical(abs(as.numeric(Telemedicine$Telehealth)-2))
match1 <- Match(Tr=Tr, X=X, M = 1, replace=FALSE, ties=FALSE)

#summary(match1)

matches <- factor(rep(match1$index.treated, 2))
Telemedicine.matchedsample <- cbind(matches, Telemedicine[c(match1$index.control, match1$index.treated),])

save(Telemedicine.matchedsample, file = "data/Telemedicine.matchedsample.csv")
save(Telemedicine,file = "data/Telemedicine.csv")
devtools::use_data(Telemedicine, compress = "xz")

# first run preparation.R, moderators.R, list.R
####################################################################################
################################  one stage masem ##################################
####################################################################################
#install.packages("metaSEM")
library(metaSEM)

#------------------------------------ PREPARE --------------------------------------------------#
# We create two dataframes, one to estimate the classical TPB (there 10 studies excluded)
dat1<-Cor2DataFrame(dataList_TPB$Data, dataList_TPB$N, acov='weighted')
dat2<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')

model1<- "B ~ IB + PBC
          IB ~ SN + PBC + ATT
          SN ~~ ATT
          SN ~~ PBC
          ATT ~~ PBC"

model2<- "B ~ IB + PBC + PN
          IB ~ SN + PBC + ATT + PN
          SN ~~ ATT
          SN ~~ PBC
          ATT ~~ PBC
          PN ~~ SN
          PN ~~ ATT
          PN ~~ PBC"

# note: we assume a one-directional effect of PN on PBC 
# (thogerson & olander use a bi-directional effect)
model3<- "B ~ IB + PBC 
          IB ~ SN + PBC + ATT
          PBC ~ PN
          ATT ~ PN
          SN ~~ ATT
          SN ~~ PBC
          ATT ~~ PBC
          PN ~~ SN"

model4<- "B ~ IB + PBC
          IB ~ PN + PBC + ATT
          PN ~ SN
          SN ~~ ATT
          SN ~~ PBC
          ATT ~~ PBC
          PN ~~ ATT
          PN ~~ PBC"


#------------------------------------ ESTIMATE --------------------------------------------------#

### Model 1
RAM1 <- lavaan2RAM(model1, obs.variables=c("B","IB","ATT","PBC","SN"))
diag(RAM1$S)[c(3:5)] <- 1 # set variances of independent variables to 1
M0 <- create.vechsR(A0=RAM1$A,S0=RAM1$S) # Mmatrix
T0 <- create.Tau2(RAM=RAM1, RE.type="Diag")# Tmatrix
model1_fit0  <- osmasem(model.name="Model 1", Mmatrix=M0, Tmatrix=T0, data=dat1, suppressWarnings=FALSE)

# Model 2
RAM2 <- lavaan2RAM(model2, obs.variables=c("B","IB","ATT", "PBC","PN","SN"))
diag(RAM2$S)[c(3:6)] <- 1 # set variances of independent variables to 1
M0 <- create.vechsR(A0=RAM2$A,S0=RAM2$S) # Mmatrix
T0 <- create.Tau2(RAM=RAM2, RE.type="Diag")#Tmatrix
model2_fit0  <- osmasem(model.name="No Moderator", Mmatrix=M0, Tmatrix=T0, data=dat2, suppressWarnings=FALSE)

# Model 3
RAM3 <- lavaan2RAM(model3, obs.variables=c("B","IB","ATT", "PBC","PN","SN"))
diag(RAM3$S)[c(5:6)] <- 1 # set variances of independent variables to 1
M0 <- create.vechsR(A0=RAM3$A,S0=RAM3$S) # Mmatrix
T0 <- create.Tau2(RAM=RAM3, RE.type="Diag")#Tmatrix
model3_fit0  <- osmasem(model.name="No Moderator", Mmatrix=M0, Tmatrix=T0, data=dat2, suppressWarnings=FALSE)

# Model 4
RAM4 <- lavaan2RAM(model4, obs.variables=c("B","IB","ATT", "PBC","PN","SN"))
diag(RAM4$S)[c(3,4,6)] <- 1 # set variances of independent variables to 1
M0 <- create.vechsR(A0=RAM4$A,S0=RAM4$S) # Mmatrix
T0 <- create.Tau2(RAM=RAM4, RE.type="Diag") #Tmatrix
model4_fit0  <- osmasem(model.name="No Moderator", Mmatrix=M0, Tmatrix=T0, data=dat2, suppressWarnings=FALSE)

#----------------------------------------------- TABLE ----------------------------------------------------------#

fit_model1<-summary(model1_fit0, fitIndices = TRUE)
fit_model2<-summary(model2_fit0, fitIndices = TRUE)
fit_model3<-summary(model3_fit0, fitIndices = TRUE)
fit_model4<-summary(model4_fit0, fitIndices = TRUE)

modelfit <- as.data.frame(matrix(0,nrow=4,ncol=7))
rownames(modelfit) <- c("classical TPB","PN affects B directly and via IB",
                        "PN affects both ATT and PBC","SN affects PN")
colnames(modelfit) <- c("Chi","ChiDoF","RMSEA","TLI","CFI","BIC","SRMR")
modelfit[1,] <- round(c(unlist(fit_model1[c("Chi","ChiDoF","RMSEA","TLI","CFI","BIC.Mx")]),osmasemSRMR(model1_fit0)),3)
modelfit[2,] <- round(c(unlist(fit_model2[c("Chi","ChiDoF","RMSEA","TLI","CFI","BIC.Mx")]),osmasemSRMR(model2_fit0)),3)
modelfit[3,] <- round(c(unlist(fit_model3[c("Chi","ChiDoF","RMSEA","TLI","CFI","BIC.Mx")]),osmasemSRMR(model3_fit0)),3)
modelfit[4,] <- round(c(unlist(fit_model4[c("Chi","ChiDoF","RMSEA","TLI","CFI","BIC.Mx")]),osmasemSRMR(model4_fit0)),3)
write.csv(modelfit, 'output/tables/table3.csv')

rm(list=setdiff(ls(), c("WIDEdat","WIDEdat_TPB","Hofstede","globe",
                        "dataList","dataList_TPB","RAM1","RAM2","RAM3","RAM4",
                        "model1_fit0","model2_fit0","model3_fit0","model4_fit0")))

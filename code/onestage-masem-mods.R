# first run preparation.R, moderators.R, list.R, onestage-masem.R
####################################################################################
################################  moderator analysis ###############################
####################################################################################
# first run onestage-masem.R
library(metaSEM)

# Ax1 <- matrix(c(0, "0*data.Mod", 0, "0*data.Mod",0,
#                 0,0,"0*data.Mod","0*data.Mod","0*data.Mod",
#                 0,0,0,0,0,
#                 0,0,0,0,0,
#                 0,0,0,0,0), ncol=5, nrow=5, byrow = TRUE)
# 
# Ax2 <- matrix(c(0, "0*data.Mod", 0, "0*data.Mod","0*data.Mod",0,
#                 0,0,"0*data.Mod","0*data.Mod","0*data.Mod","0*data.Mod",
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0), ncol=6, nrow=6, byrow = TRUE)
# Ax3 <- matrix(c(0, "0*data.Mod", 0, "0*data.Mod", 0,0,
#                 0,0,"0*data.Mod","0*data.Mod",0,"0*data.Mod",
#                 0,0,0,0,"0*data.Mod",0,
#                 0,0,0,0,"0*data.Mod",0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0), ncol=6, nrow=6, byrow = TRUE)
# Ax4 <- matrix(c(0, "0*data.Mod", 0, "0*data.Mod", 0,0,
#                 0,0,"0*data.Mod","0*data.Mod","0*data.Mod",0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,"0*data.Mod",
#                 0,0,0,0,0,0), ncol=6, nrow=6, byrow = TRUE)
####################################################################################
# only personal norms?
# Ax1 <- matrix(c(0, "0*data.Mod", 0, 0,0,
#                 0,0,0,0,"0*data.Mod",
#                 0,0,0,0,0,
#                 0,0,0,0,0,
#                 0,0,0,0,0), ncol=5, nrow=5, byrow = TRUE)
# Ax2 <- matrix(c(0,"0*data.Mod",0, 0,"0*data.Mod",0,
#                 0,0,0,0,"0*data.Mod",0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0), ncol=6, nrow=6, byrow = TRUE)
# Ax3 <- matrix(c(0,"0*data.Mod", 0,"0*data.Mod",0,0,
#                 0,0,"0*data.Mod","0*data.Mod",0,0,
#                 0,0,0,0,"0*data.Mod",0,
#                 0,0,0,0,"0*data.Mod",0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0), ncol=6, nrow=6, byrow = TRUE)
# Ax4 <- matrix(c(0, "0*data.Mod", 0, 0, 0,0,
#                 0,0,0,0,"0*data.Mod",0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0,
#                 0,0,0,0,0,0), ncol=6, nrow=6, byrow = TRUE)
Ax2 <- matrix(c(0,0,0, 0,"0*data.Mod",0,
                0,0,0,0,"0*data.Mod",0,
                0,0,0,0,0,0,
                0,0,0,0,0,0,
                0,0,0,0,0,0,
                0,0,0,0,0,0), ncol=6, nrow=6, byrow = TRUE)
Ax3 <- matrix(c(0,0,0,0,0,0,
                0,0,0,0,0,0,
                0,0,0,0,"0*data.Mod",0,
                0,0,0,0,"0*data.Mod",0,
                0,0,0,0,0,0,
                0,0,0,0,0,0), ncol=6, nrow=6, byrow = TRUE)
Ax4 <- matrix(c(0,0, 0, 0, 0,0,
                0,0,0,0,"0*data.Mod",0,
                0,0,0,0,0,0,
                0,0,0,0,0,0,
                0,0,0,0,0,0,
                0,0,0,0,0,0), ncol=6, nrow=6, byrow = TRUE)

# tau matrix 
T1 <- create.Tau2(RAM=RAM1, RE.type="Diag")
T2 <- create.Tau2(RAM=RAM2, RE.type="Diag")
T3 <- create.Tau2(RAM=RAM3, RE.type="Diag")
T4 <- create.Tau2(RAM=RAM4, RE.type="Diag")


# model parameters matrix
M1 <- create.vechsR(A0=RAM1$A, S0=RAM1$S, Ax=Ax1)
M2 <- create.vechsR(A0=RAM2$A, S0=RAM2$S, Ax=Ax2)
M3 <- create.vechsR(A0=RAM3$A, S0=RAM3$S, Ax=Ax3)
M4 <- create.vechsR(A0=RAM4$A, S0=RAM4$S, Ax=Ax4)


#******************************  results Individualism Collectivism *********************************************
# cor2dat<-Cor2DataFrame(dataList_TPB$Data, dataList_TPB$N, acov='weighted')
# cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList_TPB$IC), check.names=FALSE) 
# model1_IC <- osmasem(model.name="Model 1 (IC)", Mmatrix=M1, Tmatrix=T1, data=cor2dat)

cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList$IC), check.names=FALSE) 
model2_IC <- osmasem(model.name="Model 2 (IC)", Mmatrix=M2, Tmatrix=T2, data=cor2dat)
model3_IC <- osmasem(model.name="Model 3 (IC)", Mmatrix=M3, Tmatrix=T3, data=cor2dat)
model4_IC <- osmasem(model.name="Model 4 (IC)", Mmatrix=M4, Tmatrix=T4, data=cor2dat)


# compare moderator with non-moderator model
# fit1IC.anova<-anova(model1_IC,model1_fit0)
fit2IC.anova<-anova(model2_IC,model2_fit0)
fit3IC.anova<-anova(model3_IC,model3_fit0)
fit4IC.anova<-anova(model4_IC,model4_fit0)


#******************************  results Collectivism I *********************************************#
# cor2dat<-Cor2DataFrame(dataList_TPB$Data, dataList_TPB$N, acov='weighted')
# cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList_TPB$inst_v), check.names=FALSE)
# model1_inst_v <- osmasem(model.name="Model 1 (Inst Values)", Mmatrix=M1, Tmatrix=T1, data=cor2dat)

cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList$inst_v), check.names=FALSE)
model2_inst_v <- osmasem(model.name="Model 2 (Inst Values)", Mmatrix=M2, Tmatrix=T2, data=cor2dat)
model3_inst_v <- osmasem(model.name="Model 3 (Inst Values)", Mmatrix=M3, Tmatrix=T3, data=cor2dat)
model4_inst_v <- osmasem(model.name="Model 4 (Inst Values)", Mmatrix=M4, Tmatrix=T4, data=cor2dat)

# fit1inst_v.anova<-anova(model1_inst_v,model1_fit0)
fit2inst_v.anova<-anova(model2_inst_v,model2_fit0)
fit3inst_v.anova<-anova(model3_inst_v,model3_fit0)
fit4inst_v.anova<-anova(model4_inst_v,model4_fit0)

#******************************  results institutional coll practices *********************************************
# cor2dat<-Cor2DataFrame(dataList_TPB$Data, dataList_TPB$N, acov='weighted')
# cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList_TPB$inst_p), check.names=FALSE) 
# model1_inst_p <- osmasem(model.name="Model 1 (Inst Practices)", Mmatrix=M1, Tmatrix=T1, data=cor2dat)

cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList$inst_p), check.names=FALSE) 
model2_inst_p <- osmasem(model.name="Model 2 (Inst Practices)", Mmatrix=M2, Tmatrix=T2, data=cor2dat)
model3_inst_p <- osmasem(model.name="Model 3 (Inst Practices)", Mmatrix=M3, Tmatrix=T3, data=cor2dat)
model4_inst_p <- osmasem(model.name="Model 4 (Inst Practices)", Mmatrix=M4, Tmatrix=T4, data=cor2dat)

# fit1inst_p.anova<-anova(model1_inst_p,model1_fit0)
fit2inst_p.anova<-anova(model2_inst_p,model2_fit0)
fit3inst_p.anova<-anova(model3_inst_p,model3_fit0)
fit4inst_p.anova<-anova(model4_inst_p,model4_fit0)

#******************************  results Collectivism II *********************************************
# cor2dat<-Cor2DataFrame(dataList_TPB$Data, dataList_TPB$N, acov='weighted')
# cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList_TPB$ingr_v), check.names=FALSE)
# model1_ingr_v <- osmasem(model.name="Model 1 (Ingr Values)", Mmatrix=M1, Tmatrix=T1, data=cor2dat)

cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList$ingr_v), check.names=FALSE)
model2_ingr_v <- osmasem(model.name="Model 2 (Ingr Values)", Mmatrix=M2, Tmatrix=T2, data=cor2dat)
model3_ingr_v <- osmasem(model.name="Model 3 (Ingr Values)", Mmatrix=M3, Tmatrix=T3, data=cor2dat)
model4_ingr_v <- osmasem(model.name="Model 4 (Ingr Values)", Mmatrix=M4, Tmatrix=T4, data=cor2dat)

# fit1ingr_v.anova<-anova(model1_ingr_v,model1_fit0)
fit2ingr_v.anova<-anova(model2_ingr_v,model2_fit0)
fit3ingr_v.anova<-anova(model3_ingr_v,model3_fit0)
fit4ingr_v.anova<-anova(model4_ingr_v,model4_fit0)

#******************************  results ingroup coll practices *********************************************
# cor2dat<-Cor2DataFrame(dataList_TPB$Data, dataList_TPB$N, acov='weighted')
# cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList_TPB$ingr_p), check.names=FALSE) 
# model1_ingr_p <- osmasem(model.name="Model 1 (Ingr Practices)", Mmatrix=M1, Tmatrix=T1, data=cor2dat)

cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList$ingr_p), check.names=FALSE) 
model2_ingr_p <- osmasem(model.name="Model 2 (Ingr Practices)", Mmatrix=M2, Tmatrix=T2, data=cor2dat)
model3_ingr_p <- osmasem(model.name="Model 3 (Ingr Practices)", Mmatrix=M3, Tmatrix=T3, data=cor2dat)
model4_ingr_p <- osmasem(model.name="Model 4 (Ingr Practices)", Mmatrix=M4, Tmatrix=T4, data=cor2dat)

# fit1ingr_p.anova<-anova(model1_ingr_p,model1_fit0)
fit2ingr_p.anova<-anova(model2_ingr_p,model2_fit0)
fit3ingr_p.anova<-anova(model3_ingr_p,model3_fit0)
fit4ingr_p.anova<-anova(model4_ingr_p,model4_fit0)


#----------------------------------------------- TABLE ----------------------------------------------------------#
modelfit <- as.data.frame(matrix(0,nrow=4,ncol=11))
rownames(modelfit) <- c("Classical TPB",
                        "PN affects B directly and via IB",
                        "PN affects IB via ATT and SE",
                        "SN affects IB via PN")
colnames_m <-NULL
for(m in c("IC","inst_v","inst_p","ingr_v","ingr_p")) colnames_m<- c(colnames_m,m,"p")
colnames(modelfit)<- c("LL diff",colnames_m) # note -2LL is chisquare distributed difference when nested models

# LL test
modelfit[1,1:3]<-c(fit1IC.anova$diffLL[2], fit1IC.anova$diffdf[2],fit1IC.anova$p[2])
modelfit[2,1:3]<-c(fit2IC.anova$diffLL[2], fit2IC.anova$diffdf[2],fit2IC.anova$p[2])
modelfit[3,1:3]<-c(fit3IC.anova$diffLL[2], fit3IC.anova$diffdf[2],fit3IC.anova$p[2])
modelfit[4,1:3]<-c(fit4IC.anova$diffLL[2], fit4IC.anova$diffdf[2],fit4IC.anova$p[2])

modelfit[1,4:5]<-c(fit1inst_v.anova$diffLL[2], fit1inst_v.anova$p[2])
modelfit[2,4:5]<-c(fit2inst_v.anova$diffLL[2], fit2inst_v.anova$p[2])
modelfit[3,4:5]<-c(fit3inst_v.anova$diffLL[2], fit3inst_v.anova$p[2])
modelfit[4,4:5]<-c(fit4inst_v.anova$diffLL[2], fit4inst_v.anova$p[2])

modelfit[1,6:7]<-c(fit1inst_p.anova$diffLL[2], fit1inst_p.anova$p[2])
modelfit[2,6:7]<-c(fit2inst_p.anova$diffLL[2], fit2inst_p.anova$p[2])
modelfit[3,6:7]<-c(fit3inst_p.anova$diffLL[2], fit3inst_p.anova$p[2])
modelfit[4,6:7]<-c(fit4inst_p.anova$diffLL[2], fit4inst_p.anova$p[2])

modelfit[1,8:9]<-c(fit1ingr_v.anova$diffLL[2], fit1ingr_v.anova$p[2])
modelfit[2,8:9]<-c(fit2ingr_v.anova$diffLL[2], fit2ingr_v.anova$p[2])
modelfit[3,8:9]<-c(fit3ingr_v.anova$diffLL[2], fit3ingr_v.anova$p[2])
modelfit[4,8:9]<-c(fit4ingr_v.anova$diffLL[2], fit4ingr_v.anova$p[2])

modelfit[1,10:11]<-c(fit1ingr_p.anova$diffLL[2], fit1ingr_p.anova$p[2])
modelfit[2,10:11]<-c(fit2ingr_p.anova$diffLL[2], fit2ingr_p.anova$p[2])
modelfit[3,10:11]<-c(fit3ingr_p.anova$diffLL[2], fit3ingr_p.anova$p[2])
modelfit[4,10:11]<-c(fit4ingr_p.anova$diffLL[2], fit4ingr_p.anova$p[2])
write.csv(modelfit, 'output/tables/table4_LL.csv')

# X2
modelfit <- as.data.frame(matrix(0,nrow=15,ncol=6))
fitMeasures<-c("Chi","ChiDoF","RMSEA","TLI","CFI","BIC.Mx")

for(m in c("model2_IC","model3_IC","model4_IC",
           "model2_inst_v","model3_inst_v","model4_inst_v",
           "model2_inst_p","model3_inst_p","model4_inst_p",
           "model2_ingr_v","model3_ingr_v","model4_ingr_v",
           "model2_ingr_p","model3_ingr_p","model4_ingr_p")){
  
  results<-summary(eval(as.name(m)), fitIndices = T)
  n <- as.numeric(unlist(strsplit(strsplit(m,"_")[[1]][1],"model"))[2])-2
  
  if(grepl("IC",m)) id <-1+n 
  if(grepl("inst_v",m)) id <- 4+n
  if(grepl("inst_p",m)) id <- 7+n
  if(grepl("ingr_v",m)) id <- 10+n
  if(grepl("ingr_p",m)) id <- 13+n
  rownames(modelfit)[id] <- m
  
  modelfit[id,] <- round(unlist(results[fitMeasures]),3)
  modelfit[id,] <- round(unlist(results[fitMeasures]),3)
  modelfit[id,] <- round(unlist(results[fitMeasures]),3)
  modelfit[id,] <- round(unlist(results[fitMeasures]),3)
  modelfit[id,] <- round(unlist(results[fitMeasures]),3)
}
write.csv(modelfit, 'output/tables/table4_X2.csv')

#****************************** table 5 *********************************************
# only personal norms direct relationships
summary(model2_ingr_v) # PN on IB (-.053)
summary(model3_ingr_v) # PN on PBC (-.051) PN on ATT (-.042)
summary(model4_ingr_v) # PN on IB (-.046)
summary(model3_ingr_p) # PN on PBC (.041) PN on ATT (-.047)
summary(model3_IC) # PN on PBC (-.046) PN on ATT (.030, z=1.85)

# only personal norms, also indirect relationships (SN in for model 1)
summary(model1_ingr_v) # SN on IB (-.035)
summary(model2_ingr_v) # PN on IB (-.054)
summary(model3_ingr_v) # PN on PBC (-.043) PN on ATT (-.034, z=1.92)
summary(model4_ingr_v) # PN on IB (-.047)
summary(model3_ingr_p) # PN on PBC (.049) ATT on IB (-.033) and PN on ATT (-.037)
summary(model3_IC) # PN on PBC (-.049)

# all norms
summary(model1_ingr_v) # SN on IB (-.034)
summary(model2_ingr_v) # no sign relationships
summary(model3_ingr_v) # PN on PBC (-.042) PN on ATT (-.033, z=1.850)
summary(model4_ingr_v) # PN on IB (-.047); SN on PN (-.048, z=1.726)
summary(model3_ingr_p) # PN on PBC (.049) ATT on IB (-.035) and PN on ATT (-.038)
summary(model3_IC) # PN on PBC (-.049) (although LL test shows no significance!)

# model6_fit0.coef<-summary(model6_fit0)
# fit6IC.coef <-summary(model6_IC)
# fit6ingr_v.coef <-summary(model6_ingr_v)
# fit6ingr_p.coef <-summary(model6_ingr_p)
# parid1 <- grep("_1",fit6IC.coef$parameters$name)[1:6]
# modelfit <- as.data.frame(matrix(0,nrow=12,ncol=8))
# rownames(modelfit) <- c(fit6ingr_v.coef$parameters$name[c(1:12)])
# 
# # main effects
# modelfit[1:6,1:2] <- c(round(model6_fit0.coef$parameters$Estimate[1:6],3),round(model6_fit0.coef$parameters$`z value`[1:6],2))
# modelfit[1:6,3:4] <- c(round(fit6IC.coef$parameters$Estimate[1:6],3),round(fit6IC.coef$parameters$`z value`[1:6],2))
# modelfit[1:6,5:6] <- c(round(fit6ingr_v.coef$parameters$Estimate[1:6],3),round(fit6ingr_v.coef$parameters$`z value`[1:6],2))
# modelfit[1:6,7:8] <- c(round(fit6ingr_p.coef$parameters$Estimate[1:6],3),round(fit6ingr_p.coef$parameters$`z value`[1:6],2))
# 
# # moderator efects
# modelfit[7:12,1:2] <- NA
# modelfit[7:12,3:4] <- c(round(fit6IC.coef$parameters$Estimate[parid1],3),round(fit6IC.coef$parameters$`z value`[parid1],2))
# modelfit[7:12,5:6] <- c(round(fit6ingr_v.coef$parameters$Estimate[parid1],3),round(fit6ingr_v.coef$parameters$`z value`[parid1],2))
# modelfit[7:12,7:8] <- c(round(fit6ingr_p.coef$parameters$Estimate[parid1],3),round(fit6ingr_p.coef$parameters$`z value`[parid1],2))
# modelfit
# 
# write.csv(modelfit[c(6,1:2,5,3:4,12,7:8,11,9:10),], 'Tables/OSMASEM_onefactor_moderator_pars.csv')

rm(list=setdiff(ls(), c("WIDEdat","WIDEdat_TPB","dataList","dataList_TPB")))
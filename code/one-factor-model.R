# first run preparation.R, moderators.R, list.R
####################################################################################
############################  Campbell Paradigm ####################################
####################################################################################

dat2<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')

# latent attitude variable model following Campbell paradigm
onefactor<- "f =~ IB + PBC + SN + PN + ATT + B"

RAMx <- lavaan2RAM(onefactor)
M0 <- create.vechsR(A0=RAMx$A,S0=RAMx$S, F0=RAMx$F) # Mmatrix
T0 <- create.Tau2(RAM=RAMx, RE.type="Diag")#Tmatrix
modelx_fit0  <- osmasem(model.name="No Moderator", 
                        Mmatrix=M0, Tmatrix=T0, 
                        data=dat2, suppressWarnings=FALSE)

summary(modelx_fit0, fitIndices = TRUE)

#--------------------------- MODERATOR ANALYSIS --------------------------------------#
Axx <- matrix(c(0,0,0,0,0,0,"0*data.Mod",
                0,0,0,0,0,0,"0*data.Mod",
                0,0,0,0,0,0,"0*data.Mod",
                0,0,0,0,0,0,"0*data.Mod",
                0,0,0,0,0,0,"0*data.Mod",
                0,0,0,0,0,0,"0*data.Mod",
                0,0,0,0,0,0,0), ncol=7, nrow=7, byrow = TRUE)
Tx <- create.Tau2(RAM=RAMx, RE.type="Diag")#Tmatrix
Mx <- create.vechsR(A0=RAMx$A, S0=RAMx$S, Ax=Axx, F0=RAMx$F)

#******************************  Individualism-Collectivism *********************************************
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList$IC), check.names=FALSE) 
modelx_IC <- osmasem(model.name="One factor (IC)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_IC,modelx_fit0)

#******************************  institutional coll values *********************************************#
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(as.numeric(dataList$inst_v)), check.names=FALSE)
modelx_inst_v <- osmasem(model.name="One factor (Inst Values)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_inst_v,modelx_fit0)

#******************************  institutional coll practices *********************************************
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(as.numeric(dataList$inst_p)), check.names=FALSE) 
modelx_inst_p <- osmasem(model.name="One factor (Inst Practices)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_inst_p,modelx_fit0)

#****************************** ingroup coll values *********************************************
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(as.numeric(dataList$ingr_v)), check.names=FALSE)
modelx_ingr_v <- osmasem(model.name="One factor (Ingr Values)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_ingr_v,modelx_fit0)
summary(modelx_ingr_v)

#******************************  ingroup coll practices *********************************************
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(as.numeric(dataList$ingr_p)), check.names=FALSE) 
modelx_ingr_p <- osmasem(model.name="One factor (Ingr Practices)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_ingr_p,modelx_fit0)
summary(modelx_ingr_p)

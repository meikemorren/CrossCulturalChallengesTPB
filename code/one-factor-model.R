# first run preparation.R, moderators.R, list.R
####################################################################################
############################  Campbell Paradigm ####################################
####################################################################################
dat2<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')

# latent attitude variable model following Campbell paradigm
onefactor<- "f =~ IB + PBC + ATT + B + SN + PN"

RAMx <- lavaan2RAM(onefactor)
M0 <- create.vechsR(A0=RAMx$A,S0=RAMx$S, F0=RAMx$F) # Mmatrix
T0 <- create.Tau2(RAM=RAMx, RE.type="Diag")#Tmatrix
modelx_fit0  <- osmasem(model.name="No Moderator", 
                        Mmatrix=M0, Tmatrix=T0, 
                        data=dat2, suppressWarnings=FALSE)

summary(modelx_fit0, fitIndices = TRUE)
# BIC: -20188.22
# RMSEA: .0034
# X2(df): 22.92(9), p=.006

#--------------------------- MODERATOR ANALYSIS --------------------------------------#
Axx <- matrix(c(0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,
                0,0,0,0,0,0,"0*data.Mod",
                0,0,0,0,0,0,0), ncol=7, nrow=7, byrow = TRUE)
Tx <- create.Tau2(RAM=RAMx, RE.type="Diag")#Tmatrix
Mx <- create.vechsR(A0=RAMx$A, S0=RAMx$S, Ax=Axx, F0=RAMx$F)

#******************************  Individualism-Collectivism *********************************************
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(dataList$IC), check.names=FALSE) 
modelx_IC <- osmasem(model.name="One factor (IC)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_IC,modelx_fit0)
summary(modelx_IC)
## all relationships moderated:
# PN -.034

#******************************  institutional coll values *********************************************#
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(as.numeric(dataList$inst_v)), check.names=FALSE)
modelx_inst_v <- osmasem(model.name="One factor (Inst Values)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_inst_v,modelx_fit0)
summary(modelx_inst_v)
## all relationships moderated:
# PBC -.023 (z=1.788)

#******************************  institutional coll practices *********************************************
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(as.numeric(dataList$inst_p)), check.names=FALSE) 
modelx_inst_p <- osmasem(model.name="One factor (Inst Practices)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_inst_p,modelx_fit0)
summary(modelx_inst_p)
## only norms:
# SN -.032

## all relationships moderated:
# ATT -.032

#****************************** ingroup coll values *********************************************
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(as.numeric(dataList$ingr_v)), check.names=FALSE)
modelx_ingr_v <- osmasem(model.name="One factor (Ingr Values)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_ingr_v,modelx_fit0)
summary(modelx_ingr_v, fitIndices = T) 
# x2(df):-2.04(8), p=1
# RMSEA: Non-centrality parameter is negative
# BIC: -20201.401
# only personal norms
# PN -.052


## only norms:
# PN -.05
## all relationships moderated:
# PN -.030
# SN -.022 (z=1.851)
# B -.040

#******************************  ingroup coll practices *********************************************
cor2dat<-Cor2DataFrame(dataList$Data, dataList$N, acov='weighted')
cor2dat$data <- data.frame(cor2dat$data, Mod=scale(as.numeric(dataList$ingr_p)), check.names=FALSE) 
modelx_ingr_p <- osmasem(model.name="One factor (Ingr Practices)", Mmatrix=Mx, Tmatrix=Tx, data=cor2dat)
anova(modelx_ingr_p,modelx_fit0)
summary(modelx_ingr_p)
## only norms:
# SN -.034

## all relationships moderated:
### strangely all parameters are now significant, and very large 
### (it seems the model flips he main effects with interaction effects)

## before i corrected the datafile i got these results:
# PN -.033
# SN .022 (z=1.938)
# ATT .032



# moderators aangepast
summary(model1_ingr_v) # PBC on B .048
summary(model3_ingr_v) # PN on PBC (-.043) PN on ATT (-.033, z=1.85)
summary(model3_ingr_p) # PN on PBC (.047) PN on ATT (-.037) and ATT on IB (-.032, z=1.815)

# oude data(N=255)
summary(model3_ingr_v) # PN on PBC (-.041) PN on ATT (-.032, z=1.84)
summary(model3_ingr_p) # PN on PBC (.045) PN on ATT (-.038) and ATT on IB (-.030, sign .1)


# summary(model6_IC) # PN+ ATT- 
# # in individualist countries personal norms play a bigger role, in collectivist attitudes
# summary(model6_ingr_v) # B- PN-  
# # in countries where people believe that family relationships should be close,
# # personal norms and behavior are less important
# summary(model6_ingr_p) # PN+ ATT-
# # in countries where family relationships are experienced as close, 
# # personal norms are more important, and attitude less 
# summary(model2_ingr_v) # BonPBC+ (.06)
# summary(model3_ingr_v) # ns
# summary(model4_ingr_v) # PBConPN- (-.04)
# summary(model5_ingr_v) # BonPBC+ (.06) PNonSN- (-.04)

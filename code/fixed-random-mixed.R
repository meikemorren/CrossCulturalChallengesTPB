####################################################################################
#############################  bivariate analyses ##################################
####################################################################################
# fixed & random effects model per correlation
# we use metaSEM
library(metaSEM)

# to make inspection easier, we create a table
results<-as.data.frame(matrix(0,nrow=length(5:19), ncol=8))

for(i in 5:19){
  # fixing heterogeneity variance at 0 makes this a fixed effects model (RE.constraints=0)
  results[i-4,1:3]<- unlist(summary(meta(y=WIDEdat[,i], v=1/(WIDEdat[,4]-3), data=WIDEdat, RE.constraints=0))$Q)
  results[i-4,4]  <- unlist(summary(meta(y=WIDEdat[,i], v=1/(WIDEdat[,4]-3), data=WIDEdat, RE.constraints=0))$coefficients$Estimate)
  # inspect heterogeneity
  results[i-4,5]  <- summary(meta(y=WIDEdat[,i], v=1/(WIDEdat[,4]-3), data=WIDEdat))$I2.values[2]
  # random effects model
  results[i-4,6:7]<- unlist(summary(meta(y=WIDEdat[,i], v=1/(WIDEdat[,4]-3), data=WIDEdat))$coefficients$Estimate)
  results[i-4,8]  <- summary(meta(y=WIDEdat[,i], v=1/(WIDEdat[,4]-3), data=WIDEdat))$coefficients$`z value`[2]
}
rownames(results) <- colnames(WIDEdat)[5:19]
colnames(results) <- c("Q","N","p","Est_FE","I2","Est_RE","Tau","Tau p")

# mixed effects model 
k<-9
results<-results[,1:(k-1)]
mod <- c("IC", "inst_v","inst_p","ingr_v","ingr_p")

for(m in mod){
  id<-1
  for(i in 5:19){
    results[id,k:(k+3)]<-unlist(summary(
      meta(y=WIDEdat[,i], v=1/(WIDEdat[,4]-3), 
      x=scale(as.numeric(WIDEdat[,m]))))$coefficients[1:2,c(1,5)])[c(1,3,2,4)]
    id<-id+1
  }
  k <- k + 4
}

# make nice
colnames_m <- NULL
for(m in mod) colnames_m <- c(colnames_m,paste(m,"a"),"z",paste(m,"b"), "z")
rownames(results) <- colnames(WIDEdat)[5:19]
colnames(results) <- c(colnames(results)[1:8],colnames_m)
round(results[c(1,3,6:9),],3)
write.csv(results,"output/FE-RE-Mixed-effects.csv")

mod <- c("IC", "inst_v","inst_p","ingr_v","ingr_p")
for(m in mod){
  id<-1
  for(i in 5:19){
    fit <- summary(
      meta(y=WIDEdat[WIDEdat$Western==w,i], v=1/(WIDEdat[WIDEdat$Western==w,4]-3), 
           x=scale(as.numeric(WIDEdat[WIDEdat$Western==w,m]))))
    results[id,9]<-fit$obsStat
    results[id,k:(k+3)]<-unlist(fit$coefficients[1:2,c(1,5)])[c(1,3,2,4)]
    id<-id+1
  }
  k <- k + 4
}

# make nice
colnames_m <- NULL
for(m in mod) colnames_m <- c(colnames_m,paste(m,"a"),"z",paste(m,"b"), "z")
rownames(results) <- colnames(WIDEdat)[5:19]
colnames(results) <- c(colnames(results)[1:8],"N",colnames_m)
round(results[c(1,3,6:9),],3)

####################################################################################
#############################  publication bias ####################################
####################################################################################
library(metafor)

### set up 2x2 array for plotting
par(mfrow=c(2,3), tcl=-0.5, family="serif", mai=c(0.8,0.5,0.5,0.3))

# these will be the labels for the plots
full.corr <- c("Behavior - Intention","Behavior - PBC","Intention - Attitude",
               "Intention - PBC","Intention - Personal Norms","Intention - Subjective Norms")
name<-1 # this is how we assign a label to a plot

# standard error of within study variance
sei <- 1/sqrt((WIDEdat$N-3)) # s.e. of fisher z transformed correlation  (see eggers 2005)

for(i in x[c(1,3,6:9)]){
  
  ### fit fixed-effects model
  yi <- WIDEdat[,i] # effect sizes
  zi <- 1/2 * log((1+yi)/(1-yi)) # fisher z transformed
  res <- rma(zi, sei, measure="COR", method="FE") # estimate fixed effects
  
  ### eggers test
  # s.e. regressed on raw effect sizes
  eggerstest <- lm(yi~sei)

  ### draw funnel plots
  funnel(res, ylab="(inverted) Standard Error",main=paste(full.corr[name],"\nEgger's test = ", round(summary(eggerstest)$coef[2,1],3),
                                                          ", SE = ", round(summary(eggerstest)$coef[2,2],3), sep=""),cex.main=1.5,cex.axis=1.5,
         level=c(90, 95, 99), 
         shade=c("white", "gray55", "gray75"), 
         xlab="Fisher Z Transformed Correlation Coefficient")
  name <- name+1
}
dev.off()
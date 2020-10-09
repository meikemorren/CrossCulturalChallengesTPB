############################################################################################
#############################  creates a list of matrices ##################################
############################################################################################
# this function creates a list of matrices out of the wide datafile
# uses create_corrnames (see preparation.r)
# adds covariates to list
createList <- function(data, varnames, covariates) # exclude is not really an argument
{
  list1 <- list()
  vector2 <- vector(mode="numeric", nrow(data))
  vector3 <- vector(mode="numeric", nrow(data))
  vector4 <- vector(mode="numeric", nrow(data))
  vector5 <- vector(mode="numeric", nrow(data))
  vector6 <- vector(mode="numeric", nrow(data))
  vector7 <- vector(mode="numeric", nrow(data))
  vector8 <- vector(mode="numeric", nrow(data))
  vector9 <- vector(mode="numeric", nrow(data))
  vector10 <- vector(mode="numeric", nrow(data))
  vector11 <- vector(mode="numeric", nrow(data))
  
  for(i in seq(1, nrow(data))){
    
    # if no correlations skip 
    corr <- as.numeric(data[i,corrnames(varnames)]) 
    if(length(which(corr==0))==length(corr))    next 
    
    # create matrix
    rowmat_new <- NULL
    for (c in seq(1,length(varnames)-1)) 
    {
      rowmat <- rep(NA,length(varnames))
      rowmat[(c+1):length(varnames)] <- corr[1:(length(varnames)-c)]
      corr <- corr[-(1:(length(varnames)-c))]
      rowmat_new <- rbind(rowmat_new, rowmat)
    }
    TPBma <- rbind(rowmat_new, rep(NA,length(varnames)))
    colnames(TPBma) <-  varnames
    rownames(TPBma) <-  varnames
    TPBma[lower.tri(TPBma)] <- t(TPBma)[lower.tri(TPBma)]
    
    # diagnonal missing if no correlations
    diag(TPBma) <- 1
    for (ii in seq(1,nrow(TPBma))){
      nr_miss <- length(varnames)-1
      if(sum(is.na(TPBma[,ii]))==nr_miss & sum(is.na(TPBma[ii,]))==nr_miss){
        diag(TPBma)[ii]<-NA
      }
    }
    
    id <- length(list1) + 1
    print(length(list1))
    list1[[id]] <- TPBma
    names(list1)[id] <- as.character(data[i,"label"])
    
    vector2[id]  <- data[i,"N"]
    vector3[id]  <- as.character(data[i,"Country"])
    vector4[id]  <- as.numeric(gsub("[^0-9]","",data[i,"label"]))
    vector5[id]  <- data[i,"ID"]
    vector6[id]  <- data[i,covariates[1]]
    vector7[id]  <- data[i,covariates[2]]
    vector8[id]  <- data[i,covariates[3]]
    vector9[id]  <- data[i,covariates[4]]
    vector10[id]  <- data[i,covariates[5]]
  }
  
  # remove empty entries (i.e. studies with missing data)
  vector2 <- vector2[1:id]
  vector3 <- vector3[1:id]
  vector4 <- vector4[1:id]
  vector5 <- vector5[1:id]
  vector6 <- vector6[1:id]
  vector7 <- vector7[1:id]
  vector8 <- vector8[1:id]
  vector9 <- vector9[1:id]
  vector10 <- vector10[1:id]

  list_all <- list(list1, vector2, vector3, vector4, vector5, vector6, vector7, vector8, vector9, vector10)
  names(list_all)<-c("Data","N","Country","Year","ID",covariates)
  return(list_all)
}

### create list
# ordering is fixed otherwise createList does not work
varnames <- c("B","IB","ATT","PBC","PN","SN") 
covariates <- c("IC","inst_v","ingr_v","inst_p","ingr_p")
dataList <- createList(WIDEdat, varnames, covariates)


### inspect
summary(as.numeric(dataList$IC)) # mean 55.61 IC score
sum(dataList$N) # 114870
length(dataList$Data) # number of matrixes 225
length(unique(names(dataList$Data))) # 201 articles
nrow(table(dataList$Country)) # number of countries 46

### For classical TPB:
# use the wide file created for this purpose (see preparation.R)
# first add the moderators
ncov<-length(colnames(WIDEdat)[20:ncol(WIDEdat)]) # number of moderators
WIDEdat_TPB[,(ncol(WIDEdat_TPB)+1):(ncol(WIDEdat_TPB)+ncov)] <-NA # add empty columns
colnames(WIDEdat_TPB)[15:ncol(WIDEdat_TPB)]<- colnames(WIDEdat)[20:ncol(WIDEdat)] # add column names
for(i in seq(1,nrow(WIDEdat))){
  if(WIDEdat$ID[i] %in% WIDEdat_TPB$ID){
    # add the moderator values (match on ID)
    WIDEdat_TPB[WIDEdat_TPB$ID==WIDEdat$ID[i],15:ncol(WIDEdat_TPB)] <- 
      WIDEdat[WIDEdat$ID==WIDEdat$ID[i],20:ncol(WIDEdat)]
  }else{
    print(WIDEdat$ID[i])
    next
  } 
}

### create list
dataList_TPB <- createList(WIDEdat_TPB, c("B","IB","ATT","PBC","SN"), covariates)

### inspect
sum(dataList_TPB$N) # 105662
length(dataList_TPB$Data) # number of matrixes 215
unique(names(dataList_TPB$Data)) # 191 articles
nrow(table(dataList_TPB$Country)) # number of countries 46

rm(i,ncov,covariates)

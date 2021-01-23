############################################################################################
####################### read data from excel into long file ################################
############################################################################################
rm(list = ls())
#install.packages("devtools") # to be able to upload changes to github
library(devtools)
getwd()

# each study is a matrix, that is printed on 12 rows
data<-read.table("input/MASEM_2020.txt", sep=";",header=F) 
colnames(data) <- c("ID","label","Country","N","alpha","EC","B","IB","WTP","ATT","PBC","PN","SN","KN","F","H","PB")


### small adjustments
data$Country<-as.character(data$Country)
data$label<-as.character(data$label)
data[data$Country=="Scotland" | data$Country =="United Kingdom" | data$Country =="Great Britain"| data$Country =="England","Country"] <- as.character("UK")
data$label[data$label=="Mangafi? et al (2017)"] <- "Mangafić et al (2017)" # check!
data$label[data$label=="Whitmarsh & ONeill (2010)"]<-"Whitmarsh & O'Neill (2010)"

### inspect
length(unique(data$ID)) # 255
length(unique(data$label)) # 231 
length(unique(data$Country)) # 50

########################################################################################################################
######################################## long to wide file  ##############################################################
########################################################################################################################
# convert data to wide format (= one row per study)

# create column names for the correlations
corrnames <- function(varnames){
  names.corr <- NULL
  ii <- 1
  for (i in seq(1,length(varnames)-1))
  {
    var1 <- varnames[i]
    
    for (j in seq(i+1,length(varnames)))
    {
      var2 <- varnames[j]
      if (var1 != var2)
      {
        names.corr[ii] <- paste(var1, var2, sep="_")  
        ii <- ii + 1
      }
    }
  }
  return(names.corr)
}

# create wide file, with each correlation in a column
long2wide <- function(data, varnames) 
{
  Ncorr <- (length(varnames)*(length(varnames)-1))/2
  
  # number of entries in correlation matrix
  datafile <- as.data.frame(matrix(NA,nrow=length(unique(data$ID)),ncol=(Ncorr + 4)))
  colnames(datafile) <- c("ID","label","Country","N",corrnames(varnames))
  
  id <- 1
  for (ii in seq(1,nrow(data), by = 12)){
    datafile[id, c(1,4)] <- data[ii,c(1,4)] # ID & N
    datafile[id, c(2,3)] <- lapply(data[ii,c(2,3)], as.character) # country, label
    c <- which(colnames(data) %in% varnames) # id columns in original matrix
    ma <- do.call(cbind, lapply(data[ii+c-6,c], unlist))
    datafile[id,c(5:(5+Ncorr-1))] <- ma[lower.tri(ma, diag=FALSE)]
    id <- id + 1
  }   
  
  return(datafile)
}

# create wide file for the complete dataset
varnames <- c("B","IB","ATT","PBC","PN","SN") 
WIDEdat <-long2wide(data,varnames)

### small adjustments
WIDEdat[WIDEdat==0.000] <-NA # set to NA
# some correlations are actually zero in Wang et al 2019a:
WIDEdat[WIDEdat$ID==116,c("PBC_SN","ATT_SN","ATT_PBC")] <- 0.00 # se-sn, att-se, att-sn
# check whether there are studies without any correlation:
for(i in seq(1, nrow(WIDEdat))) if(15 == sum(is.na(WIDEdat[i,5:ncol(WIDEdat)]))) print(WIDEdat$ID) # no missings

### inspect
length(unique(WIDEdat$Country)) # 50
sum(WIDEdat$N, na.rm=T) # 130354
nrow(WIDEdat) # 255

# For classical TPB (without personal norms) we have to create another datafile:
varnames <- c("B","IB","ATT","PBC","SN")
WIDEdat_TPB <-long2wide(data,varnames)
WIDEdat_TPB[WIDEdat_TPB==0.000] <- NA # set to NA
WIDEdat_TPB[WIDEdat_TPB$ID==116,c("PBC_SN","ATT_SN","ATT_PBC")] <- 0.00 # se-sn, att-se, att-sn 

# find studies that have no correlations 
# (i.e. they only had correlations between personal norms and TPB vars)
missings <- NULL
for(i in seq(1, nrow(WIDEdat_TPB))) if(10 == sum(is.na(WIDEdat_TPB[i,5:ncol(WIDEdat_TPB)]))) missings <- c(missings,WIDEdat_TPB$ID[i]) 
WIDEdat_TPB<-WIDEdat_TPB[-missings,] # delete missings

### inspect
unique(WIDEdat_TPB$Country) # 50
sum(WIDEdat_TPB$N, na.rm=T) # 121220
nrow(WIDEdat_TPB) # 246

rm(list=setdiff(ls(), c("WIDEdat","WIDEdat_TPB",
                        "corrnames","varnames")))
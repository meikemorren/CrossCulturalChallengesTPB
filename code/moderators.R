############################################################################################
#############################  hofstede ####################################################
############################################################################################
Hofstede <- as.data.frame(read.table("input/Hofstede.txt", sep=";",quote="",header=T)) 

### small adjustments
Hofstede$Country<-as.character(Hofstede$Country)
# added, estimated by hofstede
Hofstede<-rbind(Hofstede,c("Bosnia and Herzegovina",90,22,48,87,70,44)) 
Hofstede[Hofstede$Country=="Qatar",2:5] <-c(93, 25, 55, 80) 
Hofstede[,2:7]<-apply(Hofstede[,2:7], 2, as.numeric)

# adjust country names to the ones we already have:
Hofstede$Country[Hofstede$Country=="Czechia"]  <-"Czech Republic"
Hofstede$Country[Hofstede$Country=="Luxemburg"]	<-"Luxembourg"
Hofstede$Country[Hofstede$Country=="United Kingdom"] <- "UK"
Hofstede$Country[Hofstede$Country=="United States"] <- "USA" 
Hofstede$Country[Hofstede$Country=="Fiji"] <- "Fiji Islands"

# check missing values
# (2 missing: bahrain, uganda)
WIDEdat[(WIDEdat[,"Country"] %in% Hofstede[,"Country"]==FALSE),]

# include hofstede values in the wide datafile:
WIDEdat[,20:25]<-NA
colnames(WIDEdat)[20:25] <- colnames(Hofstede)[2:7]
for(c in unique(WIDEdat$Country)){
  if(c %in% Hofstede$Country){
    rows<-WIDEdat[WIDEdat$Country==c,]
    for(r in seq(1,nrow(rows))){
      rows[r,20:25]<-Hofstede[Hofstede$Country==c,2:7]
    }
    WIDEdat[WIDEdat$Country==c,] <- rows
  }
}  
# check missing values
WIDEdat$Country[is.na(WIDEdat$IC)] # Cyprus & Uganda & Bahrain 

# impute missing values using neighboring countries
WIDEdat[WIDEdat$Country=="Uganda",20:ncol(WIDEdat)] <- Hofstede[Hofstede$Country=="East Africa",2:ncol(Hofstede)]
# uganda is part of east africa
WIDEdat[WIDEdat$Country=="Bahrain",20:ncol(WIDEdat)] <- Hofstede[Hofstede$Country=="Qatar",2:ncol(Hofstede)]
# neighbouring countries of Bharain (Saudi-Arabia, Qatar, and United Arab Emirates) all have individualism of 25
WIDEdat[WIDEdat$Country=="Cyprus",20:ncol(WIDEdat)] <- Hofstede[Hofstede$Country=="Greece",2:ncol(Hofstede)]

#############################################################################################
######################################### globe  ############################################
#############################################################################################
globe <- read.table("input/GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv",sep=",")
colnames(globe) <- c("id","Country","UA_p","FUT_p","POW_p","inst_p","HUMAN_p","PERF_p","ingr_p","EGAL_p","ASSERT_p",
                    "UA_v","FUT_v","POW_v","inst_v","HUMAN_v","PERF_v","ingr_v","EGAL_v","ASSERT_v","cluster")
globe <- as.data.frame(globe)
globe <- globe[,2:(ncol(globe)-1)]

# change country names
globe$Country[globe$Country=="Canada (English-speaking)"] <- "Canada" # no globe values for french speaking part
globe$Country[globe$Country=="England"] <- "UK"
globe$Country[globe$Country=="IRAN"] <- "Iran"
globe$Country[globe$Country=="Germany (WEST)"] <- "Germany" 

# impute missing values
# Belgium - Germanic countries (will be included in GLOBE 2020)
# Luxembourg - Germanic countries (will be included in GLOBE 2020)
Germanic <- c("Netherlands","Germany")
globe[nrow(globe)+1,1] <- "Belgium"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% Germanic,2:19],2,function(x) mean(as.numeric(x)))
globe[nrow(globe)+1,1]<-"Luxembourg"
globe[nrow(globe),2:19]<- apply(globe[globe$Country %in% Germanic,2:19],2,function(x) mean(as.numeric(x)))

# Norway - Nordic countries (will be included in GLOBE 2020)
Nordic <- c("Sweden") 
globe[nrow(globe)+1,1]<-"Norway" 
globe[nrow(globe),2:19]<- apply(globe[globe$Country %in% Nordic,2:19],2,function(x) mean(as.numeric(x)))

# Lithuania - Eastern Europe
globe[nrow(globe)+1,1]<-"Lithuania"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% c("Poland","Russia"),2:19],2,function(x) mean(as.numeric(x)))

# Bulgaria (will be included in GLOBE 2020)
globe[nrow(globe)+1,1]<-"Bulgaria"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% c("Turkey","Slovenia","Greece"),2:19],2,function(x) mean(as.numeric(x)))

# Bosnia and Herzegovina (will be included in GLOBE 2020)
Yugoslavia <- c("Slovenia","Albania") # almost neighboring countries
globe[nrow(globe)+1,1]<-"Bosnia and Herzegovina"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% Yugoslavia,2:19],2,function(x) mean(as.numeric(x)))

# Cyprus
globe[nrow(globe)+1,1]<-"Cyprus"
globe[nrow(globe),2:19]<-globe[globe$Country=="Greece",2:19]

# Chile
globe[nrow(globe)+1,1]<-"Chile"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% c("Argentina","Bolivia"),2:19],2,function(x) mean(as.numeric(x)))

# Saudi Arabia, Bahrain, Lebanon
globe[nrow(globe)+1,1]<-"Saudi Arabia"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% c("Qatar","Kuwait","Iran"),2:19],2,function(x) mean(as.numeric(x)))
globe[nrow(globe)+1,1]<-"Bahrain"
globe[nrow(globe),2:19]<-globe[globe$Country=="Qatar",2:19]
globe[nrow(globe)+1,1]<-"Lebanon"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% c("Israel","Turkey"),2:19],2,function(x) mean(as.numeric(x)))

# Pakistan, Fiji Islands 
globe[nrow(globe)+1,1]<-"Pakistan"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% c("India","Iran"),2:19],2,function(x) mean(as.numeric(x)))
globe[nrow(globe)+1,1]<-"Fiji Islands"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% c("New Zealand","Philippines"),2:19],2,function(x) mean(as.numeric(x)))

# Vietnam (will be added in GLOBE 2020)
globe[nrow(globe)+1,1]<-"Vietnam"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% c("Thailand","Philippines","Malaysia"),2:19],2,function(x) mean(as.numeric(x)))

# Uganda 
globe[nrow(globe)+1,1]<-"Uganda"
globe[nrow(globe),2:19]<-apply(globe[globe$Country %in% c("Zambia","Zimbabwe"),2:19],2,function(x) mean(as.numeric(x)))

### include GLOBE values in the wide datafile
WIDEdat[,26:43]<-NA
colnames(WIDEdat)[26:43] <- colnames(globe)[2:19]
for(c in unique(WIDEdat$Country)){
  rows<-WIDEdat[WIDEdat$Country==c,]
  if(nrow(globe[globe$Country==c,2:19])!=0){
    for(r in seq(1,nrow(rows))){
      print(r)
      rows[r,26:43]<-globe[globe$Country==c,2:19]
    }
  }  
  WIDEdat[WIDEdat$Country==c,] <- rows
}  

## EXCEPTIONS
# Switzerland, Germany, Canada, South Africa have two GLOBE values depending on the population
# The average is taken when reported as such in article (otherwise assumed):
# Germany: 2, 77 (not reported), 127 (not reported), 135 (not reported)
# Switzerland: 124 
# South africa (several urban regions, 67% black): 122
# Canada (english): 75, 128, 136
WIDEdat[WIDEdat$ID==2,26:43]  <-as.numeric(apply(globe[grepl(tolower("Germany"),tolower(globe$Country)),2:ncol(globe)], 2, mean))
WIDEdat[WIDEdat$ID==77,26:43] <-as.numeric(apply(globe[grepl(tolower("Germany"),tolower(globe$Country)),2:ncol(globe)], 2, mean))
WIDEdat[WIDEdat$ID==127,26:43]<-as.numeric(apply(globe[grepl(tolower("Germany"),tolower(globe$Country)),2:ncol(globe)], 2, mean))
WIDEdat[WIDEdat$ID==135,26:43]<-as.numeric(apply(globe[grepl(tolower("Germany"),tolower(globe$Country)),2:ncol(globe)], 2, mean))
WIDEdat[WIDEdat$ID==124,26:43]<-as.numeric(apply(globe[grepl(tolower("Switzerland"),tolower(globe$Country)),2:ncol(globe)], 2, mean))
WIDEdat[WIDEdat$ID==122,26:43]<-as.numeric(apply(globe[grepl(tolower("Africa"),tolower(globe$Country)),2:ncol(globe)], 2, mean))


# and one from east germany:
WIDEdat[WIDEdat$ID==43,26:43]<-as.numeric(apply(globe[globe$Country=="Germany (EAST)",2:ncol(globe)], 2, mean))

# The following studies are from West Germany (this is value assigned to "Germany"): 
# 42, 43, 47, 63, 72, 73, 98 (researchers are from that area)


rm(list=setdiff(ls(), c("WIDEdat","WIDEdat_TPB","corrnames","varnames","missings","long2wide","createList")))

# methodological moderators
# each study is a matrix, that is printed on 12 rows
data<-read.table("input/meta analysis_study_2020_R_meta.txt", sep=";",header=F) 
colnames(data) <- c("ID","label","sample","Country","urban","N",
                    "response","sampling","collection","year","behavior")
# get rid of all punctuation
data$sample<-gsub("()/"," ", data$sample)

# recode sample description
i<-1
data$Dsample <- NA
for(i in seq(1, nrow(data))){
  
  sample<-gsub("/", " ",tolower(data$sample)[i])
  ls<-unlist(strsplit(sample," "))
  
  for(l in ls){
    # han 2019: travelers all from USA
    if(grepl(l,"consumers")==TRUE) data$Dsample[i] <- "consumers"
    if(grepl(l,"customers")==TRUE) data$Dsample[i] <- "consumers"
    if(grepl(l,"travelers")==TRUE) data$Dsample[i] <- "consumers"
    if(grepl(l,"tourists")==TRUE) data$Dsample[i] <- "consumers"
  }
  if(tolower(data$sample)[i] %in% c("car buyers", "car owners","car drivers","shoppers", 
                                    "museum visitors","recyclers and non-recyclers")) data$Dsample[i] <- "consumers"
  
  for(l in ls){
    if(grepl(l,"residents")==TRUE) data$Dsample[i] <- "residents"
    if(grepl(l,"tourists")==TRUE) data$Dsample[i] <- "residents"
    if(grepl(l,"households")==TRUE) data$Dsample[i] <- "residents"
  }
  if(tolower(data$sample)[i] %in% c("citizens","general population","generall population",
                                    "german-speaking swiss","panel","homebuyers","city","respondents",
                                    "houseowners","homeowners","parents","women","housewives")) data$Dsample[i] <- "residents" 
  
  
  for(l in ls){
    if(grepl(l,"employees")==TRUE) data$Dsample[i] <- "workforce"
    if(grepl(l,"managers")==TRUE) data$Dsample[i] <- "workforce"
    if(grepl(l,"workers")==TRUE) data$Dsample[i] <- "workforce"
  }
  if(tolower(data$sample)[i] %in% c("teachers","faculty at colelges universities","organizations",
                                    "stakeholders","oil palm planters","investors","entrepeneurs",
                                    "farmers","university members","decision makers in companies")) data$Dsample[i] <- "workforce"
  
  for(l in ls){
    if(grepl(l,"youth")==TRUE) data$Dsample[i] <- "youth"
    if(grepl(l,"young")==TRUE) data$Dsample[i] <- "youth"
    if(grepl(l,"pupils")==TRUE) data$Dsample[i] <- "youth"
    if(grepl(l,"students")==TRUE) data$Dsample[i] <- "youth"
  }
  if(tolower(data$sample)[i] %in% c("adolescents","millennials","Convenience (students)")) data$Dsample[i] <- "youth"
}
table(data$Dsample)
data$sample[is.na(data$Dsample)]

# recode sampling method
i<-1
data$Dsampling <- NA
data$sampling[144]<-"Judgement sampling"
for(i in seq(1, nrow(data))){
  
  if(is.na(data$sampling)[i]==TRUE) next
  
  sampling<-gsub("/", " ",tolower(data$sampling)[i])
  ls<-unlist(strsplit(sampling," "))
  
  for(l in ls){
    if(grepl(l,"quota")==TRUE) data$Dsampling[i] <-  "non-random"
    if(grepl(l,"snowball")==TRUE) data$Dsampling[i] <-  "non-random"
    if(grepl(l,"convenience")==TRUE) data$Dsampling[i] <-  "non-random"
    if(grepl(l,"convinience")==TRUE) data$Dsampling[i] <-  "non-random"
    if(grepl(l,"purposive")==TRUE) data$Dsampling[i] <-  "non-random"
  }
  if(tolower(data$sampling)[i] %in% c("judgement sampling","non-random","non-probability",
                                      "convencience","convenience?")) data$Dsampling[i] <- "non-random"
  
  for(l in ls){
    if(grepl(l,"representative")==TRUE) data$Dsampling[i] <- "random"
    if(grepl(l,"stratified")==TRUE) data$Dsampling[i] <- "random"
    if(grepl(l,"systematic")==TRUE) data$Dsampling[i] <- "random"
    if(grepl(l,"cluster")==TRUE) data$Dsampling[i] <- "random"
    if(grepl(l,"random")==TRUE) data$Dsampling[i] <- "random"
  }
  if(tolower(data$sampling)[i] %in% c("census sampling","yougov","stratified randomsation","random","random ",
                                      "random probability")) data$Dsampling[i] <- "random"
}

table(data$Dsampling)
table(is.na(data$sampling))
data$sampling[data$Dsampling=="non-random"]
data$Dsampling[is.na(data$Dsampling)]<-"non-random"

# recode collection method
table(data$collection)
data$collection[224] <- NA
data$Dcollection <- NA
for(i in seq(1, nrow(data))){
  
  if(is.na(data$collection)[i]==TRUE) next
  
  collection<-gsub("/", " ",tolower(data$collection)[i])
  ls<-unlist(strsplit(collection," "))
  
  for(l in ls)    if(grepl(l,"cawi")==TRUE) data$Dcollection[i] <-  "CAWI"
  for(l in ls)    if(grepl(l,"computer")==TRUE) data$Dcollection[i] <-  "CAWI"
  for(l in ls)    if(grepl(l,"papi")==TRUE) data$Dcollection[i] <-  "PAPI"
  for(l in ls)    if(grepl(l,"paper")==TRUE) data$Dcollection[i] <-  "PAPI"
  for(l in ls)    if(grepl(l,"f2f")==TRUE) data$Dcollection[i] <-  "F2F"
  for(l in ls)    if(grepl(l,"capi")==TRUE) data$Dcollection[i] <-  "F2F"
  for(l in ls)    if(grepl(l,"cati")==TRUE) data$Dcollection[i] <-  "F2F"
}
table(is.na(data$Dcollection))
table(is.na(data$collection))
data$Dcollection[is.na(data$Dcollection)]<-"CAWI"
table(data$Dcollection)

# recode behavior
table(data$behavior)
i<-1
data$Dbehavior <- NA
for(i in seq(1, nrow(data))){

  if(is.na(data$behavior)[i]==TRUE) next
  # `\(`  => look for `(` character. `\` is needed as `(` a special character. 
  # `|`  =>  OR condition 
  # `)` =   Look for `)`
  behavior<-gsub("\\(|)", " ",tolower(data$behavior)[i])
  behavior<-gsub("/", " ",behavior)
  ls<-unlist(strsplit(behavior," "))
  
  for(l in ls){
    if(grepl(l,"various")==TRUE) data$Dbehavior[i] <-  "general"
    if(grepl(l,"general")==TRUE) data$Dbehavior[i] <-  "general"
    if(grepl(l,"behavior")==TRUE) data$Dbehavior[i] <-  "general"
    if(grepl(l,"behaviour")==TRUE) data$Dbehavior[i] <-  "general"
  }  
  if(tolower(data$behavior)[i] %in% c("behavior","green intentions","intention")) data$Dbehavior[i] <-  "general"
  
  
  for(l in ls){
    if(grepl(l,"consumption")==TRUE) data$Dbehavior[i] <-  "consumption"
    if(grepl(l,"products")==TRUE) data$Dbehavior[i] <-  "consumption"
    if(grepl(l,"product")==TRUE) data$Dbehavior[i] <-  "consumption"
    if(grepl(l,"visit")==TRUE) data$Dbehavior[i] <-  "consumption"
    if(grepl(l,"buying")==TRUE) data$Dbehavior[i] <-  "consumption"
    if(grepl(l,"purchase")==TRUE) data$Dbehavior[i] <-  "consumption"
    if(grepl(l,"purchasing")==TRUE) data$Dbehavior[i] <-  "consumption"
  }
  if(tolower(data$behavior)[i] %in% c("green furniture","green hotel","urban green spaces","cruise",
                                      "tourism","Shared Autonomous Vehicles")) data$Dbehavior[i] <-  "consumption"
  
  for(l in ls){
    if(grepl(l,"car")==TRUE) data$Dbehavior[i] <-  "transportation"
    if(grepl(l,"commute")==TRUE) data$Dbehavior[i] <-  "transportation"
    if(grepl(l,"transport")==TRUE) data$Dbehavior[i] <-  "transportation"
    if(grepl(l,"transportation")==TRUE) data$Dbehavior[i] <-  "transportation"
    if(grepl(l,"traveling")==TRUE) data$Dbehavior[i] <-  "transportation"
    if(grepl(l,"behavior")==TRUE) data$Dbehavior[i] <-  "transportation"
    if(grepl(l,"commuting")==TRUE) data$Dbehavior[i] <-  "transportation"
  }
  if(tolower(data$behavior)[i] %in% c("bike sharing","bycle sharing","carpooling","intention to use transferium",
                                      "shared autonomous vehicles")) data$Dbehavior[i] <-  "transportation"
  
  for(l in ls){
    if(grepl(l,"electricity")==TRUE) data$Dbehavior[i] <-  "energy"
    if(grepl(l,"reduction")==TRUE) data$Dbehavior[i] <-  "energy"
    if(grepl(l,"energy")==TRUE) data$Dbehavior[i] <-  "energy"
    if(grepl(l,"bioenergy")==TRUE) data$Dbehavior[i] <-  "energy"
    if(grepl(l,"carbon")==TRUE) data$Dbehavior[i] <-  "energy"
    if(grepl(l,"low-carbon")==TRUE) data$Dbehavior[i] <-  "energy"
  }
  if(tolower(data$behavior)[i] %in% c("green housing","green it initiatives","green it use",
                                      "purchasing fuel-economic vehicles","Investing in GL","hybrid electronic vehicles",
                                      "intention to use wood pellet heating","e-waste disposal",
                                      "intention to move into energy-efficient homes ")) data$Dbehavior[i] <-  "energy"# GL stands for green logistics
  
  for(l in ls){
    if(grepl(l,"conservation")==TRUE) data$Dbehavior[i] <-  "conservation"
    if(grepl(l,"segregation")==TRUE) data$Dbehavior[i] <-  "conservation"
    if(grepl(l,"separation")==TRUE) data$Dbehavior[i] <-  "conservation"
    if(grepl(l,"pollution")==TRUE) data$Dbehavior[i] <-  "conservation"
    if(grepl(l,"preservation")==TRUE) data$Dbehavior[i] <-  "conservation"
    if(grepl(l,"recycle")==TRUE) data$Dbehavior[i] <-  "conservation"
    if(grepl(l,"recycling")==TRUE) data$Dbehavior[i] <-  "conservation"
    if(grepl(l,"nature")==TRUE) data$Dbehavior[i] <-  "conservation"
    if(grepl(l,"waste")==TRUE) data$Dbehavior[i] <-  "conservation"
  }
  if(tolower(data$behavior)[i] %in% c("farmland bird convervation","supply oil palm residues available at their plantations",
                                      "remanufactured auto parts","littering","kerbside resycling")) data$Dbehavior[i] <-  "conservation"
  
  # if food waste is mentioned: this will overrule the conservation assigned above!
  for(l in ls){
    if(grepl(l,"meat")==TRUE) data$Dbehavior[i] <-  "food"
    if(grepl(l,"food")==TRUE) data$Dbehavior[i] <-  "food"
    if(grepl(l,"fruit")==TRUE) data$Dbehavior[i] <-  "food"
    if(grepl(l,"organic")==TRUE) data$Dbehavior[i] <-  "food"
  }
  if(tolower(data$behavior)[i] %in% c("pesticides","green practices","agriculture")) data$Dbehavior[i] <-  "food" # green practices in restaurants
  
  for(l in ls){
    if(grepl(l,"activism")==TRUE) data$Dbehavior[i] <-  "activism"
    if(grepl(l,"complaint")==TRUE) data$Dbehavior[i] <-  "activism"
    if(grepl(l,"climate")==TRUE) data$Dbehavior[i] <-  "activism"
  }
  if(tolower(data$behavior)[i] %in% c("green investments","Haze mitigation (pro-environmental behavior)")) data$Dbehavior[i] <-  "activism"
}  
table(data$Dbehavior)
table(is.na(data$Dbehavior))
table(is.na(data$behavior))
data$Dbehavior[is.na(data$Dbehavior)]<-"general"

## attach to WIDEdat
WIDEdat$sample    <- NA
WIDEdat$sampling  <- NA
WIDEdat$collection<- NA
WIDEdat$behavior  <- NA
for(i in seq(1, nrow(WIDEdat))){
  WIDEdat[i,"sample"]<-data$Dsample[data$ID==WIDEdat$ID[i]]
  WIDEdat[i,"sampling"]<-data$Dsampling[data$ID==WIDEdat$ID[i]]
  WIDEdat[i,"collection"]<-data$Dcollection[data$ID==WIDEdat$ID[i]]
  WIDEdat[i,"behavior"]<-data$Dbehavior[data$ID==WIDEdat$ID[i]]
}
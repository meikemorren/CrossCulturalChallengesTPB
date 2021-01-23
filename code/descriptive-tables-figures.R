# first run preparation.R, moderators.R, list.R
####################################################################################
#############################  tables & figures ####################################
####################################################################################

#------------------------------ TABLE 1 --------------------------------------------#
## Extract the heterogeneity variance-covariance matrix
## NOTE: we use two-stage masem
library(metaSEM)
REM <- tssem1(dataList$Data, dataList$N, method = "REM", RE.type = "Diag") 
summary(coef(REM)[1:15])
summary(coef(REM)[16:30])
coeff<-round(coef(REM),3)

tab<-matrix(0,6,6)
colnames(tab)<-c("B","IB","ATT","PBC","PN","SN")
rownames(tab)<-c("B","IB","ATT","PBC","PN","SN")
tab[,1]<- c(1,coeff[1:5])
tab[,2]<- c(0,1,coeff[6:9])
tab[,3]<- c(0,0,1,coeff[10:12])
tab[,4]<- c(0,0,0,1,coeff[13:14])
tab[,5]<- c(0,0,0,0,1,coeff[15])
tab[,6]<- c(0,0,0,0,0,1)

# N
sequences <- list(5:9, 10:13, 14:16, 17:18, 19)
for(r in 1:5){
  N<-NULL
  for(i in sequences[[r]]) N <- c(N,sum(!is.na(WIDEdat[,i])))
  tab[r,(r+1):6] <- N
}

write.csv(tab, "output/tables/table1.csv")

#------------------------------ TABLE 2 --------------------------------------------#
table<-cbind(tapply(WIDEdat$N, 
             factor(WIDEdat$Country), sum),
      table(factor(WIDEdat$Country)))
rownames(table)<-paste(rownames(table)," (",table(factor(WIDEdat$Country)),")", sep = "")

values <- c("IC","inst_v","ingr_v","inst_p","ingr_p")
for(v in values){
  val<-tapply(as.numeric(WIDEdat[,v]), 
              factor(WIDEdat$Country), mean)
  print(val)
  table<-cbind(table, round(val,2))
}
colnames(table)[3:ncol(table)]<- values

write.csv(table, 'output/tables/table2.csv')

#------------------------------ TABLE 3 --------------------------------------------#

df<-merge(Hofstede,globe, by=intersect(names(Hofstede), names(globe)), all=TRUE)

# change names 
df[df$Country=="Germany (EAST)","Country"] <- "Germany"
df[df$Country=="French Switzerland","Country"] <- "Switzerland"
df[df$Country=="South Africa (Black Sample)","Country"] <- "South Africa"
df[df$Country=="South Africa (White Sample)","Country"] <- "South Africa"
df <- aggregate(df[,values],list(df$Country), mean, na.rm=T)
colnames(df)[1]<-"Country"

# exclude groups of countries
df <- df[!c(df$Country %in% c("West Africa", "Arabic countries","East Africa","Yugoslavia")),]

# included in our study
df$Included <-"no"
df[which(df$Country %in% WIDEdat$Country),"Included"]<- "yes"
cor.test(df$IC, df$ingr_p)
cor.test(df$IC, df$inst_p)

# correlations between values
values <- c("IC","inst_v","ingr_v","inst_p","ingr_p")

tab1<-matrix(0,5,5)
tab1[,1]<- c(cor(na.omit(df[,values]))[1:5])
tab1[,2]<- c(0,cor(na.omit(df[,values]))[7:10])
tab1[,3]<- c(0,0,cor(na.omit(df[,values]))[13:15])
tab1[,4]<- c(0,0,0,cor(na.omit(df[,values]))[19:20])
tab1[,5]<- c(0,0,0,0,1)
tab1

# included in study
tab<-matrix(0,5,5)
colnames(tab)<-c("IC","inst_v","ingr_v","inst_p","ingr_p")
tab[1,]<- c(cor(na.omit(df[df$Included=="yes",values]))[1:5])
tab[2,]<- c(tab1[2,1],cor(na.omit(df[df$Included=="yes",values]))[7:10])
tab[3,]<- c(tab1[3,1:2],cor(na.omit(df[df$Included=="yes",values]))[13:15])
tab[4,]<- c(tab1[4,1:3],cor(na.omit(df[df$Included=="yes",values]))[19:20])
tab[5,]<- c(tab1[5,1:4],1)
write.table(tab, "output/tables/table3.csv")

rm(list=setdiff(ls(), c("WIDEdat","WIDEdat_TPB","dataList","dataList_TPB")))

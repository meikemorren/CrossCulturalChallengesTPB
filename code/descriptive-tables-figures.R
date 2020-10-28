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
table2<-cbind(tapply(WIDEdat$N, 
             factor(WIDEdat$Country), sum),
      table(factor(WIDEdat$Country)))
rownames(table2)<-paste(rownames(table2)," (",table(factor(WIDEdat$Country)),")", sep = "")

values <- c("IC","inst_v","ingr_v","inst_p","ingr_p")
for(v in values){
  val<-tapply(as.numeric(WIDEdat[,v]), 
              factor(WIDEdat$Country), mean)
  print(val)
  table2<-cbind(table2, round(val,2))
}
colnames(table2)[3:ncol(table2)]<- values

write.csv(table2, 'output/tables/table2.csv')

#------------------------------ FIGURE 4 --------------------------------------------#
library(ggplot2)
library(grid)

# make sure to run moderators.R first!
df<-merge(Hofstede,globe, by=intersect(names(Hofstede), names(globe)), all=TRUE)

# correlations between values
values <- c("IC","inst_v","ingr_v","inst_p","ingr_p")
cor(na.omit(df[,values]))

# change names 
df[df$Country=="Germany (EAST)","Country"] <- "Germany"
df[df$Country=="French Switzerland","Country"] <- "Switzerland"
df[df$Country=="South Africa (Black Sample)","Country"] <- "South Africa"
df[df$Country=="South Africa (White Sample)","Country"] <- "South Africa"

# coloring whether in study
df$Included <-"no"
df[which(df$Country %in% WIDEdat$Country),"Included"]<- "yes"

# plots
tiff("output/figures/values-plots.tiff", width = 10, height = 10, 
     units = 'in', res = 300)

p<-ggplot(df,aes(as.numeric(ingr_v), IC, colour = Included))+
  ggtitle("Ingroup values") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(text=element_text(size=12,  family="serif"))+
  geom_point()
q<-ggplot(df,aes(as.numeric(ingr_p), IC, colour = factor(Included)))+
  ggtitle("Ingroup practices")  + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(text=element_text(size=12,  family="serif"))+
  geom_point()
r<-ggplot(df,aes(as.numeric(inst_v), IC, colour = factor(Included)))+
  ggtitle("Institutional values") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(text=element_text(size=12,  family="serif"))+
  geom_point()
s<-ggplot(df,aes(as.numeric(inst_p), IC, colour = factor(Included)))+
  ggtitle("Institutional practices") + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(text=element_text(size=12,  family="serif"))+
  geom_point()

# create grid
grid.arrange(p,q,r,s, ncol=2,nrow=2,
             widths=c(2.5,2.5),
             heights=c(2.4,2.4),
             left=textGrob("Collectivism - Individualism (Hofstede)", rot=90, 
                           gp=gpar(fontsize=16, fontfamily="serif")))
dev.off()

rm(list=setdiff(ls(), c("WIDEdat","WIDEdat_TPB","dataList","dataList_TPB")))

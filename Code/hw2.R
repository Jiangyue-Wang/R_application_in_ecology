
##hw2
##WANG Jiangyue 2021-10-27
library(ggplot2)
library(ggpubr)
library(cowplot)
library(xlsx)
##read file
woody<-read.csv("/Users/lilywang/Desktop/phD/Courses/R application in ecology/WoodyPlantDiversityinChina-part.csv")
##make column names more easy to understand
bio<-colnames(woody)
bio[20]<-"MTCQ"
bio[13]<-"tem_season"
colnames(woody)<-bio
'''
##to write a function that can generate regression plot arrays
hw2<-function(file,env_covs,spe_abun){ #file name, independent covs, dependent covs
  plist<-c() #generate a vector to storage plot names
  for(i in spe_abun){ #for every dependent cov (make the rows)
    for (j in env_covs){ #for every independent cov (make the columns)
      basicp<-ggplot(file,aes(get(j),get(i)))+geom_point()+stat_smooth(method=lm)+
        labs(x=NULL,y=NULL)+theme_classic() #make linear regression plot 
      finalp<-basicp #finalp will be modified on the axis title
      if(j==env_covs[1]){
        finalp<-finalp+labs(y=i)
      } #  if the plot is on the left column of the plot matrix, y-axis title should appear
      if(i==spe_abun[length(spe_abun)]){
        finalp<-finalp+labs(x=j)
      } #  if the plot is on the bottom row of the plot matrix, x-axis title should appear
      
      assign(paste("final",i,j,sep=""),finalp) #give every plot a name
      plist<-c(plist,paste("final",i,j,sep="")) #storage the names in a vector "plist"
      
    }
    
  }
###this is the part that i do not know how to make it simple and easy, as get() cannot
###deal with a vector; the aim of this part is to arrange a 4*5 plot matrix
  ggarrange(get(plist[1]),get(plist[2]),get(plist[3]),get(plist[4]),get(plist[5]),
            get(plist[6]),get(plist[7]),get(plist[8]),get(plist[9]),get(plist[10]),
            get(plist[11]),get(plist[12]),get(plist[13]),get(plist[14]),get(plist[15]),
            get(plist[16]),get(plist[17]),get(plist[18]),get(plist[19]),get(plist[20]),
            ncol=length(env_covs),nrow=length(spe_abun))
}

hw2(woody,c("MTCQ","AET","tem_season","Ele_RANGE","popu_den_gwp"),
    c("RS","RS_TREE","RS_SHRUB","RS_LIANA"))
'''


###All the code above is a function I attempted to generate plot matrix
###the logic is right but when you tmp<-ggplot(***i,***)
###when there is variables in ggplot(aes()),tmp will automatically update when you 
###change the variable i, and that will cause the function fail, leading to 20 same plots

###Next I will try another method to generate plots, dataframe will only include 3 columns
###x,y,and factor indicating which model are the corresponding xs and ys in

##extract variables
RS<-woody$RS
RS_TREE<-woody$RS_TREE
RS_SHRUB<-woody$RS_SHRUB
RS_LIANA<-woody$RS_LIANA
MTCQ<-woody$MTCQ
AET<-woody$AET
TSEA<-woody$tem_season
ELE<-woody$Ele_RANGE
POP<-woody$popu_den_gwp

##construct new dataframe
woody_mod<-data.frame(x=c(rep(c(MTCQ,AET,TSEA,ELE,POP),4)),
                      y=c(rep(RS,5),rep(RS_TREE,5),rep(RS_SHRUB,5),rep(RS_LIANA,5)),
                      rowtype=as.factor(rep(c("RS","RS_TREE","RS_SHRUB","RS_LIANA"),each=10000)),
                      coltype=as.factor(rep(rep(c("MTCQ","AET","TSEA","ELE","POP"),each=2000),4)))

summary(woody_mod)
ggplot(data=woody_mod,mapping=aes(x,y))+
  geom_point()+stat_smooth(method=lm)+
  facet_grid(rows = vars(rowtype),cols=vars(coltype),scales="free",switch="both")+
  theme_classic()









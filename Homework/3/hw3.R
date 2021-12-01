setwd("~/Desktop/phD/Courses/R application in ecology/3")
library(xlsx)
woody<-read.xlsx("WoodyPlantDiversityinChina-part.xlsx",sheetIndex = 1)
summary(woody)
woody[,6]<-as.numeric(woody[,6])
woody[,8]<-as.numeric(woody[,8])
R2<-function(model){
  Rsquared<-1-(model$deviance/model$null.deviance)
  return(Rsquared)
}
sig<-function(model){
  sigvalue<-summary(model)$coefficients[2,4]
  if(sigvalue<0.05&sigvalue>=0.01){
    star="*"
  }
  else if(sigvalue<0.01&sigvalue>=0.001){
    star="**"
  }
  else if(sigvalue<0.001&sigvalue>=0.0001){
    star="***"
  }
  else if(sigvalue<0.0001){
    star="****"
  }
  return(star)
}

exmat<-data.frame(row.names=colnames(woody[9:30]),RS=rep("NA",22),
                  RS_TREE=rep("NA",22),RS_SHRUB=rep("NA",22),
                  RS_LIANA=rep("NA",22))
for (i in c("RS","RS_TREE","RS_SHRUB","RS_LIANA")){
  for (j in colnames(woody)[9:30]){
    glmodel<-glm(get(i)~get(j),data=woody,family=poisson())
    glmodelr2<-R2(glmodel)
    glmodelsig<-sig(glmodel)
    finalr2<-glmodelr2
    finalstar<-glmodelsig
    exmat[j,i]=paste(round(finalr2,3)*100,finalstar,sep="")
  }
}
row.names(exmat)<-c("Ele_range","MAT","MDR","Isothermality","Temp_seasonality",
                   "Tmax7","Tmin1","ART","MeanT_WeQ","MeanT_DQ","MeanT_WQ","MeanT_CQ",
                   "AP","Pre_We","Pre_D","Pre_seasonality","Pre_WeQ",
                   "Pre_DQ","Pre_WQ","Pre_CQ","AET","popu_den_gwp")
write.csv(exmat,"hw3.csv")

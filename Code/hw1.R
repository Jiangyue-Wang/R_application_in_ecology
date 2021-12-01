install.packages("xlsx")
library(xlsx)

hw1<-function(v1,v2){
  ma<-matrix(nrow=v1,ncol=v2)
  for (i in 1:v1){
    ma[i,]<-floor(runif(v2,min=1,max=10))
  }
  write.xlsx(ma,"hw1.xlsx",col.names=FALSE,row.names=FALSE)
  return(ma)
}

setwd("~/Desktop")
hw1(100,300)

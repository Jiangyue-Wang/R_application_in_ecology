plist<-c() #generate a vector to storage plot names
for(i in c("RS","RS_TREE","RS_SHRUB","RS_LIANA")){ #for every dependent cov (make the rows)
  for (j in c("MTCQ","AET","tem_season","Ele_RANGE","popu_den_gwp")){ #for every independent cov (make the columns)
    basicp<-ggplot(woody,aes(get(j),get(i)))+geom_point()+stat_smooth(method=lm)+
      labs(x=NULL,y=NULL)+theme_classic() #make linear regression plot 
    finalp<-basicp #finalp will be modified on the axis title
    if(j==c("MTCQ","AET","tem_season","Ele_RANGE","popu_den_gwp")[1]){
      finalp<-finalp+labs(y=i)
    } #  if the plot is on the left column of the plot matrix, y-axis title should appear
    if(i==c("RS","RS_TREE","RS_SHRUB","RS_LIANA")[length(c("RS","RS_TREE","RS_SHRUB","RS_LIANA"))]){
      finalp<-finalp+labs(x=j)
    } #  if the plot is on the bottom row of the plot matrix, x-axis title should appear
    
    assign(paste("final",i,j,sep=""),finalp) #give every plot a name
    plist<-c(plist,paste("final",i,j,sep="")) #storage the names in a vector "plist"
  }
}  
  
  ###this is the part that i don't know how to make it simple and easy, as get() cannot
  ###deal with a vector; the aim of this part is to arrange a 4*5 plot matrix
  ggarrange(get(plist[1]),get(plist[2]),get(plist[3]),get(plist[4]),get(plist[5]),
            get(plist[6]),get(plist[7]),get(plist[8]),get(plist[9]),get(plist[10]),
            get(plist[11]),get(plist[12]),get(plist[13]),get(plist[14]),get(plist[15]),
            get(plist[16]),get(plist[17]),get(plist[18]),get(plist[19]),get(plist[20]),
            ncol=5,nrow=4)
  
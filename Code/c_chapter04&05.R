
###########################################################
## 1 t test
###########################################################
## example 1
## concentration of a chemical in water, mean = 4.5
x <- c(2.918, 2.730, 3.536, 3.879, 3.746, 1.921, 3.314, 2.593, 6.249, 
	1.747, 4.296, 1.315, 3.393, 5.381, 4.298, 5.329, 3.953, 4.309, 
	3.067, 2.749)
## histogram
hist(x)
## show the mean value as a vertical line
abline(v = 4.5, col="red", lty=2, lwd=2)
## t test
ex.t.test <- t.test(x = x, mu = 4.5)
## show the results of t test
attributes(ex.t.test)
ex.t.test$statistic
ex.t.test$p.value

## a t test step-by-step
se <- sd(x)/sqrt(length(x))
t.stat <- abs(mean(x) - 4.5)/se
(1 - pt(q=t.stat, df=19))*2


## example 2
## load the data
setwd("E:/My_Teaching/R_Group")
d.butt <- read.csv("01&02 R introduction/Butterflies.csv", header=T, stringsAsFactors=F)

## show the histogram of the data
par(mfrow=c(1,2), cex=1, cex.axis=1.2, cex.lab=1.6,cex.main=1.6)
hist(d.butt[d.butt[,1] == "North" ,2], col="lightgreen", border="black", 
	main="North", ylab="Frequency", xlab="species diversity", br=6)
abline(v=mean(d.butt[d.butt[,1] == "North" ,2]), col="red", lty=2, lwd=2)
hist(d.butt[d.butt[,1] == "South" ,2], col="lightgreen", border="black",
	main="South", ylab="Frequency", xlab="species diversity", br=6)
abline(v=mean(d.butt[d.butt[,1] == "South" ,2]), col="red", lty=2, lwd=2)

## t test: compare the species numbers on the north and south slopes
## a classic way
x <- d.butt[d.butt[,1] == "North" ,2]
y <- d.butt[d.butt[,1] == "South" ,2]
sp.comp <- t.test(x=x, y=y)
sp.comp
attributes(sp.comp)
sp.comp$statistic
sp.comp$p.value
sp.comp$estimate

## a more straightforward way
sp.comp <- t.test(Number_of_species~Slope, data=d.butt)
sp.comp
attributes(sp.comp)
sp.comp$statistic
sp.comp$p.value


## example 3
## one side
x <- d.butt[d.butt[,1] == "North" ,2]
y <- d.butt[d.butt[,1] == "South" ,2]
sp.comp <- t.test(x, y, alternative = "greater")
sp.comp



###########################################################
## 2 correlation analysis
###########################################################
## load the data: leaf margin states of Chinese woody plants
setwd("E:/my_teaching/r_group/")
d.leaf <- read.csv("03 Plots in R/data_for_plots.csv", header=T, stringsAsFactors=F)

## example 1
## evaluate which group of species determine the overall pattern in leaf margin state
y <- d.leaf$LM.dicot
x1 <- d.leaf$LM.tree
x2 <- d.leaf$LM.shrub
x3 <- d.leaf$LM.vine

## draw scatter plots between them
par(mfrow=c(1,3))
plot(y~x1, cex.lab=1.5,
	ylab="% of tooth species for all species", xlab="% of tooth species for trees")
plot(y~x2, cex.lab=1.5,
	ylab="% of tooth species for all species", xlab="% of tooth species for shrubs")
plot(y~x3, cex.lab=1.5,
	ylab="% of tooth species for all species", xlab="% of tooth species for vines")

## calcuate the correlation coefficients between two variables
r.result1 <- cor(x=x1, y=y, method="pearson")
r.result1

r.result2 <- cor(x=x2, y=y, method="pearson")
r.result2

r.result3 <- cor(x=x3, y=y, method="pearson")
r.result3


## example 2
## spearman correlation
par(mfrow=c(2,2))
hist(y, ylab="Frequency", xlab="% of toothed species", main="all species")
hist(x1, ylab="Frequency", xlab="% of toothed species", main="trees")
hist(x2, ylab="Frequency", xlab="% of toothed species", main="shrubs")
hist(x3, ylab="Frequency", xlab="% of toothed species", main="vines")

## calcuate the correlation coefficients between different variables
r.result1 <- cor(x=x1, y=y, method="spearman")
r.result1

r.result2 <- cor(x=x2, y=y, method="spearman")
r.result2

r.result3 <- cor(x=x3, y=y, method="spearman")
r.result3


## example 3
any(is.na(y))
y[10] <- NA
## calcuate the correlation coefficients between two variables
r.result1 <- cor(x=x1, y=y, method="pearson")
r.result1

r.result1 <- cor(x=x1, y=y, method="pearson", use = "complete.obs")
r.result1

r.result1 <- cor(x=x1, y=y, method="spearman")
r.result1

r.result1 <- cor(x=x1, y=y, method="spearman", use = "complete.obs")
r.result1


## example 4
## correlation between many variables
## define a character vector containing the variable names
y.list <- c("Bio1", "Bio11", "KMAT", "Bio12", 
	"LM.dicot", "LM.tree", "LM.shrub", "LM.vine")
mat.cor <- cor(d.leaf[y.list])
View(mat.cor)


## example 5: test the significance of correlations
## evaluate which group of species determine the overall pattern in leaf margin state
y <- d.leaf$LM.dicot
x1 <- d.leaf$LM.tree
x2 <- d.leaf$LM.shrub
x3 <- d.leaf$LM.vine

r.result1 <- cor.test(y,x1, method="pearson")
attributes(r.result1)
r.result1$estimate
r.result1$p.value

r.result2 <- cor.test(y,x2, method="pearson")
r.result2$p.value

r.result3 <- cor.test(y,x3, method="pearson")
r.result3$p.value

r.result1 <- cor.test(y,x1, method="spearman")
attributes(r.result1)
r.result1$estimate
r.result1$p.value



## example 6: test the significance of correlations between many variables
## define a character vector containing the variable names
y.list <- c("Bio1", "Bio11", "KMAT", "Bio12", 
	"LM.dicot", "LM.tree", "LM.shrub", "LM.vine")
N <- length(y.list)
mat.r <- mat.p <- matrix(NA, nrow=N, ncol=N)
for (i in 1:N) {
	x <- d.leaf[,y.list[i]]
	for (j in i:N) {
		y <- d.leaf[,y.list[j]]
		r.result <- cor.test(x,y, method="pearson")
		mat.r[i,j] <- r.result$estimate
		mat.p[i,j] <- r.result$p.value
		}
	}
View(mat.r)
View(mat.p)




###########################################################
## 3 make corplot
###########################################################

## install and load the package
install.packages("corrplot")
library(corrplot)

## calculate the correlation coefficients between every two variables
mat.cor <- cor(d.leaf[y.list])

## draw a figure showing the correlations between different variables
## using circles, where circle size is proportional to r
windows(); corrplot(corr = mat.cor, method = "circle")

## using squares, where square size is proportional to r
corrplot(corr = mat.cor, method = "square")

## using ellipses, where ellipse width is negetively proportional to r
corrplot(corr = mat.cor, method = "ellipse", type = "upper")

## show correlation coefficients
corrplot(corr = mat.cor, method = "number")

## show both correlation coefficients and circles 
corrplot.mixed(corr = mat.cor, lower="number", upper="circle")

## order the variables according a cluster analysis, so that variables with similar 
## correlations are next to each other
corrplot(corr = mat.cor, order = "hclust")




###########################################################
## 4 make simple linear regressions
###########################################################

## example 1
## relationship between woody plant diversity and climate across provinces in China

## load the data
d.woody.pro <- read.csv("04&05 Correlations and regressions/woody_prov.csv", header=T)

## make a scatter plot between the reponse variable and predictor
par(mar=c(6,6,3,1.5),                             ## margin
	cex=1,                                    ## size of the symbols
	cex.axis=1.2, cex.lab=1.5       ## size of the axis labels
	)
	
plot(Woody ~ Tmin, data=d.woody.pro,
	pch=19,     ## difine the symbol used to draw the scatter plot
	ylab="species richness", xlab="mean winter temperature"
	)
windows()
plot(LogWoody_Area ~ Tmin, data=d.woody.pro,
	pch=19,     ## difine the symbol used to draw the scatter plot
	ylab="corrected species richness", xlab="mean winter temperature"
	)

## conduct the simple linear analysis
m1 <- lm(LogWoody_Area ~ Tmin, data = d.woody.pro)

## summarize the regression results
s1 <- summary(m1)
attributes(s1)
s1$coefficients
s1$r.squared
s1$coefficients[2,4]

## draw a regression line
abline(m1, col="red", lwd=2)
abline(s1$coefficients[,1], col="red", lwd=2)

## write the R2 on the fiture
r2 <- s1$r.squared
r2 <- round(r2, digits=2)
text(x=-15, y=0.7, label = paste("R^2 =", r2))


## use a "subset" of data for the regression
## the relationship between diversity and Tmin in eastern China
m2 <- lm(LogWoody_Area~Tmin, data=d.woody.pro, subset = d.woody.pro$Long > 110)

## summarize the regression results
s2 <- summary(m2)
s2
s2$coefficients
s2$r.squared


## extract the fitted values and residuals of a regression model
fitted.v <- fitted(m1)
resid1 <- residuals(m1)
plot(resid1 ~ fitted.v, pch=19, ylab="residuals", xlab="fitted values")
abline(h=0, lty=2, lwd=2, col="red")



## example 2: 
## predict the outputs for given input values using a regression model
## define a dataframe containing a variable with the same variable name as the predictor of the model
a <- min(d.woody.pro$Tmin)
b <- max(d.woody.pro$Tmin)
newdata <- data.frame(Tmin = seq(from = a, to = b, length.out = 100))

## predict the outputs
y.predicted <- predict(object = m1, newdata = newdata)

## plot the predicted values on the figure
plot(LogWoody_Area ~ Tmin, data=d.woody.pro,
	pch=19,     ## difine the symbol used to draw the scatter plot
	ylab="corrected species richness", xlab="mean winter temperature"
	)
lines(y.predicted ~ newdata$Tmin, lwd=2, lty=1, col="red")



## example 3
## analyze the latitudinal gradients in both MAT and TSN in China

## load the data
d.clim <- read.csv(file="01&02 R introduction/ClimateChina.csv", header=T, stringsAsFactors=F)

windows(width=12, height=8)
par(mar=c(4.5, 4.5, 0.5, 4.5), tcl = -0.5, 
	mgp=c(2.5, 1, 0))
plot(Bio1~LAT, data = d.clim, 
	pch=19, col="darkgray", font=3, font.lab=2,                       ## change the symbol
	cex=1.2, cex.axis=1.2, cex.lab=1.7,                        ## modify the size of symbols and text
	xlab="latitude",                            ## include axes lables
	ylab="mean annual temperature",                          ## include axes lables
      ylim=c(-15,25), xlim=c(15, 60))                           ## set up the limits of the two axes

####
## draw a linear regression line
## linear regression model
m1 <- lm(Bio1~LAT, data = d.clim)

## generate a new dataframe
a <- min(d.clim$LAT, na.rm=T)
b <- max(d.clim$LAT, na.rm=T)
ba <- (b-a)*0.05
newdata <- data.frame(LAT = seq(from = a - ba, to = b + ba, length.out = 100))

## predict the outputs
y.predicted <- predict(object = m1, newdata = newdata)

## draw a line
lines(y.predicted~newdata$LAT, lwd=2, lty=1, col="black")           ## change color and weight of the line

####
## include another data set by points
plot.window(ylim=c(0,2000), xlim=c(15, 60))
col.rgb <- col2rgb(col = "lightgreen"); col.rgb                    ## set semi-transparent color
col.2 <- rgb(t(col.rgb)/255, alpha = 1)
points(Bio4~LAT, data = d.clim, 
      pch=1, col=col.2)

## include axis for the second dataset
axis(side = 4, font=3, font.lab=2,                                 ## change the symbol
	cex=1.2, cex.axis=1.2, cex.lab=1.7,                          ## modify the size of symbols and text
	col="lightgreen", col.axis="lightgreen")                     ## modify the the color of the new axis  
mtext(text="temperature seasonality", side=4, line=2.5, cex=1.7, font=2)

## draw a linear regression line
## linear regression model
m2 <- lm(Bio4 ~ LAT, data = d.clim)

## predict the outputs
y.predicted <- predict(object = m2, newdata = newdata)

## draw a line
lines(y.predicted~newdata$LAT, lwd=2, lty=2, col="blue")           ## change color and weight of the line

## include a legend
legend("bottomright",  
	legend=c("MAT", "TSN"), 
	pch=c(19,1), 
	col=c("gray", "green"), 
	lwd=c(2,2), pt.lwd=1)
legend("bottomright",  
	legend=c("MAT", "TSN"), 
	col=c("black", "blue"), 
	lwd=c(2,2), pt.lwd=1,
	merge=T)



## example 4: build a model without intercept
plot(GSP ~ MAP, data = d.woody.pro, ylim=c(0, 1200), xlim=c(0, 2300))

## build a model without intercept
m4 <- lm(GSP ~ -1 + MAP, data = d.woody.pro)
summary(m4)
## draw a regression line
abline(m4, col="red")

## build a normal model
m4 <- lm(GSP ~ MAP, data = d.woody.pro)
summary(m4)
## draw a regression line
abline(m4, col="black")



## example 5: deal with NA values using "na.action"
?lm
## replace one value of Tmin with NA
any(is.na(d.woody.pro[,"Tmin"]))
d.woody.pro[5,"Tmin"] <- NA

## built a few models with different way of na.action
m1 <- lm(LogWoody_Area ~ Tmin, data = d.woody.pro)    ## default: na.action = "na.fail"
m2 <- lm(LogWoody_Area ~ Tmin, data = d.woody.pro, na.action = "na.omit")
m3 <- lm(LogWoody_Area ~ Tmin, data = d.woody.pro, na.action = "na.exclude")

## extract the fitted values and residuals of a regression model
fitted.v <- fitted(m1)
resid1 <- residuals(m1)
cbind(d.woody.pro, fitted.values=fitted.v, resid=resid1)

fitted.v <- fitted(m2)
resid1 <- residuals(m2)
cbind(d.woody.pro, fitted.values=fitted.v, resid=resid1)

fitted.v <- fitted(m3)
resid1 <- residuals(m3)
cbind(d.woody.pro, fitted.values=fitted.v, resid=resid1)








###########################################################
## 5 make multiple linear regressions
###########################################################

## example 1
## explore the relationships between species diversity and climate, including Tmin, MAP, TOPO
## draw scatter plots between these variables
par(mfrow = c(1,3))
plot(LogWoody_Area ~ Tmin, data=d.woody.pro)
plot(LogWoody_Area ~ MAP, data=d.woody.pro)
plot(LogWoody_Area ~ TOPO, data=d.woody.pro)

## conduct a multiple linear regression
m3 <- lm(LogWoody_Area ~ Tmin + MAP + TOPO, data=d.woody.pro)
s3 <- summary(m3)
s3



## example 2
## explore the relationships between leaf margin precentage and climate
names(d.leaf)

## make a multiple regression
m4 <- lm(LM.tree ~ Bio1 + Bio11 + Bio12, data=d.leaf)
s4 <- summary(m4)
s4



## example 3: type III regression
m5 <- lm(LM.tree ~ Bio11 + Bio12, data=d.leaf)
m6 <- lm(Bio1 ~ Bio11 + Bio12, data=d.leaf)
y <- resid(m5)
x <- resid(m6)
m7 <- lm(y ~ x)
summary(m7)




## example 4: polynomial 
## make a scatter plot between the reponse variable and predictor
par(mar=c(6,6,3,1.5),                           ## margin
	cex=1,                                    ## size of the symbols
	cex.axis=1.2, cex.lab=1.5       ## size of the axis labels
	)
	
plot(Woody ~ PET, data=d.woody.pro,
	pch=19,     ## difine the symbol used to draw the scatter plot
	ylab="species richness", xlab="PET"
	)

## make a parabolic model
## Y = a + bX + cX^2
PET2 <- d.woody.pro$PET^2
m8 <- lm(Woody ~ PET + PET2, data=d.woody.pro)
summary(m8)






###########################################################
## 6 Variance partitioning
###########################################################

## example 1: partial regression
d.leaf <- read.csv("03 Plots in R/data_for_plots.csv", header=T, stringsAsFactors=F)
m1 <- lm(LM.tree ~ Bio1 + Bio12, data = d.leaf); s1 <- summary(m1)
m2 <- lm(LM.tree ~ Bio1, data = d.leaf); s2 <- summary(m2)
m3 <- lm(LM.tree ~ Bio12, data = d.leaf); s3 <- summary(m3)

## variance components
ab <- s2$r.squared
bc <- s3$r.squared
abc <- s1$r.squared
d <- 1 - abc
a <- abc - bc
c <- abc - ab
b <- abc - a - c

## make a plot showing the components 1
r2.var1 <- as.matrix(cbind(r2.var1 = c(ab, c, d), r2.var2 = c(0, 0, 0)))
r2.var2 <- as.matrix(cbind(r2.var1 = c(0, 0, 0), r2.var2 = c(a, bc, d)))
barplot(r2.var1, space=0, horiz = T, 
	col=c("gray", "transparent", "transparent"), border=NA, names.arg = c("", ""), axes = T)
barplot(r2.var2, space=0, horiz = T, add=T, 
	col=c("transparent", "gray", "transparent"), border=NA, names.arg = c("", ""), axes = F)
#axis(side=1, pos = 1)
abline(h = 1, lwd=2)

## make a plot showing the components 2
r2 <- matrix(c(a,b,c,d), ncol=1)
barplot(r2, space=0, horiz = T, 
	col=c("lightgreen", "gray", "lightyellow", "transparent"), names.arg = "")




## example 2: hierarchical partitioning
install.packages("relaimpo")
library(relaimpo)

## calculate the relative importance of Bio1 and Bio12
m4 <- calc.relimp(LM.tree ~ Bio1 + Bio12, data = d.leaf)
m4$var.y
m4$R2
m4$R2.decomp
m4$lmg
m4$lmg.rank



## example 3: hierarchical partitioning
install.packages("hier.part")
library(hier.part)

## calculate the relative importance of Bio1 and Bio12
m5 <- hier.part(y = d.leaf$LM.tree, xcan = d.leaf[c("Bio1", "Bio12")], gof = "Rsqu")
attributes(m5)
m5$IJ
m5$I.perc






###########################################################
## 7 Using factor variables in linear regressions
###########################################################

## example 1: the difference in species diversity between north and south slopes
d.butt <- read.csv("01 R introduction/Butterflies.csv", header=T, stringsAsFactors=T)
class(d.butt[,1])

## built a model in which the predictor is factor
m1 <- lm(Number_of_species ~ Slope, data = d.butt)
s1 <- summary(m1)
s1$coefficient
s1$r.squared

## the meaning of the slope and intercept
boxplot(Number_of_species ~ Slope, data = d.butt)
t.test(Number_of_species ~ Slope, data = d.butt)

## the calculation of R2
fitted.v <- fitted(m1)
rss <- sum((fitted.v - mean(fitted.v))^2); rss
tss <- sum((d.butt$Number_of_species - mean(d.butt$Number_of_species))^2); tss
r2 <- rss/tss
r2

## 
dummy.var1 <- as.numeric(d.butt$Slope != "North")
m1.1 <- lm(Number_of_species ~ dummy.var1, data = d.butt)
summary(m1.1)





## example 2: differences in plant height between families
d.woody <- read.csv("01 R introduction/Woody_SpList_Lifeform_Division.csv", header=T, stringsAsFactors=F)

## built a model with a factor predictor
m2 <- lm(Height2 ~ Family_E, data = d.woody, subset = d.woody$No <= 52)
s2 <- summary(m2)
s2

## meaning of the intercept and slopes
par(las = 3, mar=c(7, 5, 0.5, 0.5))
boxplot(Height2 ~ Family_E, data = d.woody, subset = d.woody$No <= 52)
abline(h=c(10, 20, 30, 40, 50), lty=2, col="lightgreen")

## a further test 1
d.woody.temp <- d.woody[d.woody$Family_E %in% c("Cyatheaceae", "Ephedraceae"), c("Family_E", "Height2")]
t.test(Height2 ~ Family_E, data = d.woody.temp)

## a further test 2
d.woody.temp <- d.woody[d.woody$No <= 52, c("Family_E", "Height2")]
dummy.var1 <- as.numeric(d.woody.temp$Family_E != "Cyatheaceae")
dummy.var2 <- as.numeric(!d.woody.temp$Family_E %in% c("Cyatheaceae", "Cycadaceae"))
dummy.var3 <- as.numeric(!d.woody.temp$Family_E %in% c("Cyatheaceae", "Cycadaceae", "Ephedraceae"))
cbind(dummy.var1, dummy.var2, dummy.var3)
m3 <- lm(Height2 ~ dummy.var1 + dummy.var2 + dummy.var3, data = d.woody.temp, na.action = "na.exclude")
summary(m3)





###########################################################
## 8 ANOVA
###########################################################

## example 1
m1 <- lm(Number_of_species ~ Slope, data = d.butt)
anova(m1)

## for comparison
rs.slope.anova <- aov(Number_of_species~Slope, data=d.butt)
summary(rs.slope.anova)


## example 2
## inport the herbivores.and.seed.output data table into R, and conduct an ANOVA
d.herbiv <- read.delim(file="01 R introduction/herbivores.and.seed.output.csv", sep=",", header=T, stringsAsFactors=F)
View(d.herbiv )
names(d.herbiv)

## ANOVA
seed.anova <- aov(Seeds.produced ~ Treatment, data=d.herbiv)
summary(seed.anova)


## example 3: compare two different models
PET2 <- d.woody.pro$PET^2
m1 <- lm(lm(Woody ~ PET, data=d.woody.pro))
m2 <- lm(lm(Woody ~ PET + PET2, data=d.woody.pro))
anova(m1, m2)
summary(m1)
summary(m2)


## example 4: compare two different models
PET2 <- d.woody.pro$PET^2
m1 <- lm(Woody ~ Tmin, data=d.woody.pro)
m2 <- lm(Woody ~ PET + PET2, data=d.woody.pro)
anova(m1, m2)
anova(m1)
anova(m2)


## example 5: sequences of the variables
m1 <- lm(LogWoody_Area ~ Tmin + MAP, data=d.woody.pro)
anova(m1)

m2 <- lm(LogWoody_Area ~ MAP + Tmin, data=d.woody.pro)
anova(m2)

## why????

## variance partitioning between Tmin and MAP
m1 <- lm(LogWoody_Area ~ Tmin + MAP, data = d.woody.pro); s1 <- summary(m1)
m2 <- lm(LogWoody_Area ~ Tmin, data = d.woody.pro); s2 <- summary(m2)
m3 <- lm(LogWoody_Area ~ MAP, data = d.woody.pro); s3 <- summary(m3)

## variance components
ab <- s2$r.squared
bc <- s3$r.squared
abc <- s1$r.squared
d <- 1 - abc
a <- abc - bc
c <- abc - ab
b <- abc - a - c

## make a plot showing the components 1
r2.var1 <- as.matrix(cbind(r2.var1 = c(ab, c, d), r2.var2 = c(0, 0, 0)))
r2.var2 <- as.matrix(cbind(r2.var1 = c(0, 0, 0), r2.var2 = c(a, bc, d)))
barplot(r2.var1, space=0, horiz = T, 
	col=c("gray", "transparent", "transparent"), border=NA, names.arg = c("", ""), axes = T)
text(x=0.5, y=0.5, label=paste("Tmin: R^2 = ", round(ab, digits=4), sep=""), cex=1.5)
barplot(r2.var2, space=0, horiz = T, add=T, 
	col=c("transparent", "gray", "transparent"), border=NA, names.arg = c("", ""), axes = F)
text(x=0.5, y=1.5, label=paste("MAP: R^2 = ", round(bc, digits=4), sep=""), cex=1.5)

#axis(side=1, pos = 1)
abline(h = 1, lwd=2)


## Type I regression using ANOVA
m1 <- lm(LogWoody_Area ~ Tmin + MAP, data=d.woody.pro)
anova(m1)
tss <- sum(anova(m1)[,2])
## r2 of Tmin
r2.tmin <- anova(m1)[1,2]/tss
## partial r2 of MAP
r2.map <- anova(m1)[2,2]/tss

m2 <- lm(LogWoody_Area ~ MAP + Tmin, data=d.woody.pro)
anova(m2)
tss <- sum(anova(m2)[,2])
## r2 of MAP
r2.map <- anova(m2)[1,2]/tss
## partial r2 of Tmin
r2.tmin <- anova(m2)[2,2]/tss






###########################################################
## 9 including interaction terms in general linear models
###########################################################
## example 1: including interaction terms
d.woody.pro$Biogeo <- as.factor(d.woody.pro$Biogeo)
plot(LogWoody_Area ~ Tmin, data = d.woody.pro, subset = d.woody.pro$Biogeo == 1)
points(LogWoody_Area ~ Tmin, data = d.woody.pro, pch = 19, subset = d.woody.pro$Biogeo == 2)

m1 <- lm(LogWoody_Area ~ Tmin, data = d.woody.pro, subset=d.woody.pro$Biogeo == 1)
abline(m1, col="red", lwd=2)
m2 <- lm(LogWoody_Area ~ Tmin, data = d.woody.pro, subset=d.woody.pro$Biogeo == 2)
abline(m2, col="cyan", lwd=2)



## to determine the significance of the difference between the two slopes
m3 <- lm(LogWoody_Area ~ Tmin + Biogeo + Tmin * Biogeo, data = d.woody.pro)
anova(m3)








###########################################################
## 10 none-linear regressions
###########################################################
## example 1
## make a parabolic model
## Y = a + bX + cX^2
plot(Woody ~ PET, data = d.woody.pro)
m1 <- nls(Woody ~ a + b*PET + c*PET^2, data = d.woody.pro, start = list(a=1, b=1, c=1))
summary(m1)

## using linear regression
PET2 <- d.woody.pro$PET^2
m2 <- lm(Woody ~ PET + PET2, data=d.woody.pro)
summary(m2)



## example 2
plot(LM.tree ~ Bio12, data = d.leaf, pch = 19, col="darkgray")
m1 <- lm(LM.tree ~ Bio12, data = d.leaf)
summary(m1)$r.squared
abline(m1, col="red", lwd=2)


## logistic regression
d.leaf$LM.tree[d.leaf$LM.tree == 0] <- 0.01
m2 <- nls(LM.tree ~ K/(1 + exp(a - r*Bio12)), data = d.leaf, start = list(K = 85, a = 5, r = 0.1))
summary(m2)

a <- seq(min(d.leaf$Bio12), max(d.leaf$Bio12), length.out = 100)
newdata <- list(Bio12 = a)
y.pred <- predict(m2, newdata=newdata, type = "response")
lines(y.pred ~ a, col="blue", lwd = 3)
summary(lm(d.leaf$LM.tree ~ fitted(m2)))$r.squared



## logistic regression
?selfStart
d.leaf$LM.tree[d.leaf$LM.tree == 0] <- 0.01
m3 <- nls(LM.tree ~ SSlogis(Bio12, Asym, xmid, scal), data = d.leaf)
summary(m3)

a <- seq(min(d.leaf$Bio12), max(d.leaf$Bio12), length.out = 100)
newdata <- list(Bio12 = a)
y.pred <- predict(m3, newdata=newdata, type = "response")
lines(y.pred ~ a, col="green", lwd = 3)






###########################################################
## 11 Principal component analysis
###########################################################

## example 1: PCA for temperature and precipitation variables 
## load the data
d.clim <- read.csv(file="01 R introduction/ClimateChina.csv", header=T, stringsAsFactors=F)
names(d.clim)

d.clim.t <- d.clim[,c(4:14)]
t.pca <- princomp(d.clim.t, cor = T)

summary(t.pca)
View(t.pca$scores)
t.pca$loadings

plot(t.pca$scores[,1] ~ t.pca$scores[,2], col="darkgray", ylim=c(-6, 6), xlim=c(-5,5))
abline(h=0, v=0)
r.mat <- cor(t.pca$scores, d.clim.t)
r.mat
arrows(x0=0, y0=0, x1 = 0.98*5, y1 = 0.15 * 6, col="red")
arrows(x0=0, y0=0, x1 = -0.77*5, y1 = 0.0077 * 6, col="blue")






###########################################################
## 12 structural equation model: SEM
###########################################################
install.packages("sem") 
require(sem)

d.XJ <- read.csv("04&05&06 Correlations and regressions/speciesRichness3ResolutionXinjiang.csv",header=T)
str(d.XJ); dim(d.XJ)

PET <- d.XJ$PET
PET2 <- PET^2
AET <- d.XJ$AET
plantRS <- d.XJ$plantRS
mammalRS <- d.XJ$mammalRS 
birdRS <- d.XJ$birdRS 


## PET level
PET.bp <- c(474, 476, 514) #c("plantRS", "birdRS", "mammalRS")
ene.level <- list()
for (i in 1:length(PET.bp)) ene.level[[i]] <- PET > PET.bp[i]

d <- cbind(PET, PET2, AET, plantRS, mammalRS)
cor.mat <- cor(d)


## build a SEM
model.0 <- specifyEquations()
mammalRS = beta11 * plantRS
mammalRS = gam12 * AET + gam13 * PET + gam14 * PET2
plantRS = gam22 * AET + gam23 * PET + gam24 * PET2
V(AET) = V[AET]
V(PET) = V[PET] 
V(PET2) = V[PET2]
C(PET, PET2) = aa

mm.mammal <- sem(model=model.0, S=cor.mat, N=nrow(d), data=d)
summary(mm.mammal, fit.indices=c("CFI", "NFI", "AIC"))#; coef(mm.mammal); df.residual(mm.mammal)
effects(mm.mammal)

dot <- pathDiagram(mm.mammal, file="t1", min.rank=c("AET", "PET", "PET2"), max.rank=c("mammalRS"), same.rank=c("plantRS"), output.type="graphics", graphics.fmt="pdf")
cat(paste(dot, collapse="\n"))
plot(mammalRS~PET2)
capture.output(pathDiagram(mm.mammal), file="t1.dot")



model.0 <- specifyEquations()
mammalRS = beta11 * plantRS
mammalRS = gam12 * AET 
mammalRS =  gam13 * PET 
mammalRS =  gam14 * PET2
plantRS = gam22 * AET 
plantRS = gam23 * PET 
plantRS = gam24 * PET2
V(AET) = V[AET]
V(PET) = V[PET] 
V(PET2) = V[PET2]
C(PET, PET2) = aa

mm.mammal <- sem(model=model.0, S=cor.mat, N=nrow(d), data=d)
summary(mm.mammal, fit.indices=c("CFI", "NFI", "AIC"))#; coef(mm.mammal); df.residual(mm.mammal)
effects(mm.mammal)



model.1 <- specifyModel()
plantRS -> mammalRS,   beta11     
AET -> mammalRS,       gam12      
PET -> mammalRS,       gam13      
PET2 -> mammalRS,      gam14      
AET -> plantRS,        gam22      
PET -> plantRS,        gam23      
PET2 -> plantRS,       gam24      
plantRS <-> plantRS,   V[plantRS] 
mammalRS <-> mammalRS, V[mammalRS]
PET <-> PET2,          aa
PET <-> PET,           V[PET] 
PET2 <-> PET2,         V[PET2] 
AET <-> AET,           V[AET]

mm.mammal <- sem(model=model.1, S=cor.mat, N=nrow(d), data=d)
summary(mm.mammal, fit.indices=c("CFI", "NFI", "AIC"))#; coef(mm.mammal); df.residual(mm.mammal)
effects(mm.mammal)




## different energy regions
d <- cbind(PET, PET2, AET, plantRS, mammalRS); d <- d[!ene.level[[1]], ]
cor.mat <- cor(d)#cor(cbind(PET, AET, plantRS, mammalRS))

model.1 <- specifyModel()
plantRS -> mammalRS,   beta11     
AET -> mammalRS,       gam12      
PET -> mammalRS,       gam13      
AET -> plantRS,        gam22      
PET -> plantRS,        gam23      
plantRS <-> plantRS,   V[plantRS] 
mammalRS <-> mammalRS, V[mammalRS]
PET <-> PET,          V[PET] 
AET <-> AET,          V[AET]


mm.mammal <- sem(model=model.1, S=cor.mat, N=nrow(d), data=d)
summary(mm.mammal, fit.indices=c("CFI", "NFI", "AIC"))
effects(mm.mammal)










###########################################################
## 13 linear mixed effect model: LME
###########################################################


## example 1: including interaction terms
d.woody.pro$Biogeo <- as.factor(d.woody.pro$Biogeo)
plot(LogWoody_Area ~ Tmin, data = d.woody.pro, subset = d.woody.pro$Biogeo == 1)
points(LogWoody_Area ~ Tmin, data = d.woody.pro, pch = 19, subset = d.woody.pro$Biogeo == 2)

m1 <- lm(LogWoody_Area ~ Tmin, data = d.woody.pro, subset=d.woody.pro$Biogeo == 1)
abline(m1, col="red", lwd=2)
m2 <- lm(LogWoody_Area ~ Tmin, data = d.woody.pro, subset=d.woody.pro$Biogeo == 2)
abline(m2, col="cyan", lwd=2)



## to determine the significance of the difference between the two slopes
m3 <- lm(LogWoody_Area ~ Tmin + Biogeo + Tmin * Biogeo, data = d.woody.pro)
anova(m3)



require(lme4)
m4 <- lmer(LogWoody_Area ~ Tmin, data = d.woody.pro, REML=FALSE)

m5 <- lmer(LogWoody_Area ~ Tmin + (1|Biogeo), data = d.woody.pro, REML=FALSE)
summary(m5)

m6 <- lmer(LogWoody_Area ~ Tmin + (1 + Tmin|Biogeo), data = d.woody.pro, REML=FALSE)
summary(m6)

m7 <- lmer(LogWoody_Area ~ 1 + (1|Biogeo), data = d.woody.pro, REML=FALSE)
anova(m7, m5)
summary(m7)

m8 <- lmer(LogWoody_Area ~ 1 + (1+Tmin|Biogeo), data = d.woody.pro, REML=FALSE)
summary(m8)
anova(m8, m6)

anova(m5, m6)










###########################################################
## 14 Generalized linear models: GLM
###########################################################
## example 1
## non-Gaussian distributions
par(mfrow =c(3,2))
hist(d.woody.pro$Woody)
m1 <- lm(Woody ~ Tmin, data=d.woody.pro)
summary(m1)
hist(m1$residuals)

## GLM with different distributions
?family

## for species richness
m2 <- glm(Woody ~ Tmin, data=d.woody.pro, family = poisson(link = "log"))
summary(m2)
hist(m2$residuals)

## R2 of LM and GLM
## LM
s1 <- summary(m1)
s1$r.square
## GLM
s2 <- summary(m2)
R2.glm <- 1 - s2$deviance / s2$null.deviance

## difference between the predictions of lm and glm
plot(Woody ~ Tmin, data=d.woody.pro)
abline(m1, col="blue", lwd=2)
a <- seq(from=min(d.woody.pro$Tmin), to = max(d.woody.pro$Tmin), length.out=100)
newdata <- data.frame(Tmin = a)
y.pred <- predict(m2, newdata=newdata, type="response")
lines(y.pred~a, lwd=2, col="red")

plot(m1$fitted ~ d.woody.pro$Woody)
abline(lm(m1$fitted ~ -1 + d.woody.pro$Woody))
plot(m2$fitted ~ d.woody.pro$Woody)
abline(lm(m2$fitted ~ -1 + d.woody.pro$Woody))



## example 3
plot(LM.tree ~ Bio12, data = d.leaf, pch = 19, col="darkgray")
m1 <- lm(LM.tree ~ Bio12, data = d.leaf)
summary(m1)$r.squared
abline(m1, col="red", lwd=2)

## logistic regression
d.leaf$LM.tree[d.leaf$LM.tree == 0] <- 0.01
m2 <- nls(LM.tree ~ K/(1 + exp(a - r*Bio12)), data = d.leaf, start = list(K = 85, a = 5, r = 0.1))
summary(m2)

a <- seq(min(d.leaf$Bio12), max(d.leaf$Bio12), length.out = 100)
newdata <- list(Bio12 = a)
y.pred <- predict(m2, newdata=newdata, type = "response")
lines(y.pred ~ a, col="blue", lwd = 3)
summary(lm(d.leaf$LM.tree ~ fitted(m2)))$r.squared

## GLM regression
d.leaf$LM.tree2 <- d.leaf$LM.tree/100
m3 <- glm(LM.tree2 ~ Bio12, data = d.leaf, family=binomial(link="logit"))
s3 <- summary(m3)
R2.glm <- 1 - s3$deviance / s3$null.deviance
a <- seq(min(d.leaf$Bio12), max(d.leaf$Bio12), length.out = 100)
newdata <- data.frame(Bio12 = a)
y.pred <- predict(m3, newdata=newdata, type="response")
lines(y.pred*100 ~ a, lwd=2, col="cyan")











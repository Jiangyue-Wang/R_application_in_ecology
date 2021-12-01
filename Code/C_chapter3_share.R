
## load the data
setwd("E:/My_Teaching/R_Group")

d.herbiv <- read.delim(file="01&02 R introduction/herbivores.and.seed.output.csv", sep=",", header=T, stringsAsFactors=F)
d.clim <- read.csv(file="01&02 R introduction/ClimateChina.csv", header=T, stringsAsFactors=F)


###########################################################
## 1 Simple scatter plot
###########################################################
x <- d.clim$Bio1           ## mean annual temperature
y <- d.clim$Bio12         ## mean annual precipitation
plot(x, y)
plot(y ~ x)
plot(Bio12 ~ Bio1, data = d.clim)



###########################################################
## 2 make the plot look better
###########################################################
## symbols
x <- 1:25
y <- rep(0, time=25)
plot(y ~ x, pch = x, cex = 3)
text(x=x, y=-0.1, labels=x)

plot(Bio12 ~ Bio1, data = d.clim, 
	pch=19)                                                   ## change the symbol

## colors
colors()
plot(Bio12~Bio1, data = d.clim, 
	pch=19, col="darkgray")                                   ## change the color

col.rgb <- col2rgb(col = "darkgray"); col.rgb                   ## set semi-transparent color
col <- rgb(red=col.rgb[1]/255, green=col.rgb[2]/255, 
		blue=col.rgb[3]/255, alpha = 0.2)
col <- rgb(t(col.rgb)/255, alpha = 0.2)
plot(Bio12~Bio1, data = d.clim, pch=19, col=col)                ## set semi-transparent color


## text: title, axis label, free text
## text: include the labels of x and y axes
plot(Bio12~Bio1, data = d.clim, 
	pch=19, col=col, 
	xlab="mean annual temperature",                           ## include axes lables
	ylab="mean annual precipitation")                         ## include axes lables

## text: include the title 1
plot(Bio12~Bio1, data = d.clim, 
	pch=19, col=col, 
	xlab="mean annual temperature",                           ## include axes lables
	ylab="mean annual precipitation",                         ## include axes lables
	main = "relationship between prec and temp")              ## include the title of the figure

## text: include the title 2
plot(Bio12~Bio1, data = d.clim, 
	pch=19, col=col, 
	xlab="mean annual temperature",                           ## include axes lables
	ylab="mean annual precipitation")                         ## include axes lables

title(main = "relationship between prec and temp")	          ## include the title of the figure


## text: include the title
plot(Bio12~Bio1, data = d.clim, 
	pch=19, col=col, 
	xlab="mean annual temperature",                           ## include axes lables
	ylab="mean annual precipitation",                         ## include axes lables
	main = "relationship between prec and temp")              ## include the title of the figure
text(x = -5, y = 3000, label="China, resolution: 50*50km")             ## add texts in the figure 
text(x = -5, y = 3000, label="China, resolution: 50*50km", adj=0)      ## add texts in the figure, adjust the position
text(x = -5, y = 3000, label="China, resolution: 50*50km", adj=1)      ## add texts in the figure, adjust the position 



## font
plot(Bio12~Bio1, data = d.clim, 
	pch=19, col=col, 
	xlab="mean annual temperature",                           ## include axes lables
	ylab="mean annual precipitation",                         ## include axes lables
	main = "relationship between prec and temp",              ## include the title of the figure
	font = 3, font.lab = 2)                                   ## change the font
text(x = -5, y = 3000, label="China, resolution: 50*50km", adj=0,      ## add texts in the figure, adjust the position
	font = 4)                                                        ## change the font


## symbol and font size
plot(Bio12~Bio1, data = d.clim, 
	pch=19, col=col, 
	xlab="mean annual temperature",                           ## include axes lables
	ylab="mean annual precipitation",                         ## include axes lables
	main = "relationship between prec and temp",              ## include the title of the figure
	font = 3, font.lab = 2,                                   ## change the font
	cex=0.9, cex.axis=1.2, cex.lab=1.4                        ## modify the size of symbols and text
	)

text(x = -5, y = 3000, label="China, resolution: 50*50km", adj=0,      ## add texts in the figure, adjust the position
	font = 4,                                                        ## change the font
	cex=1.2                                                          ## modify the size of symbols and text
	) 


## set up the limits of the two axes
plot(Bio12~Bio1, data = d.clim, 
	pch=19, col=col, 
	xlab="mean annual temperature",                           ## include axes lables
	ylab="mean annual precipitation",                         ## include axes lables
	main = "relationship between prec and temp",              ## include the title of the figure
	font = 3, font.lab = 2,                                   ## change the font
	cex=0.9, cex.axis=1.2, cex.lab=1.4,                       ## modify the size of symbols and text
	ylim=c(0, 4000), xlim=c(-15,25)                           ## set up the limits of the two axes
	)

text(x = -5, y = 3800, label="China, resolution: 50*50km", adj=0,      ## add texts in the figure, adjust the position
	font = 4,                                                        ## change the font
	cex=1.2                                                          ## modify the size of symbols and text
	) 


## emphasize the points with precipitation > 2500 mm
subset <- d.clim$Bio12 > 2500
points(Bio12~Bio1, data = d.clim, subset=subset, 
	cex=1.5, col="blue", pch=19)


## legends
legend("bottomright",  
	legend=c("normal", "extremes"), 
	pch=c(19, 19), 
	col=c(col, "blue")
	)




## compare: axes and margin
windows();
#windows(); par(mar=c(5,5,1,0.5), mgp=c(2.5,0.6,0))
windows(); par(mar=c(3,3,0.5,0.5), mgp=c(2,0.5,0), tck=-0.02)
windows(); par(mar=c(3,3,0.5,0.5), mgp=c(2,0.5,0), tck=0.02)
plot(Bio1~LAT, data=d.clim)

## compare: box
windows();par(mar=c(3,3,0.5,0.5), mgp=c(2,0.5,0), tck=-0.02)
plot(Bio1~LAT, data=d.clim, axes=F)
axis(side=1)
axis(side=2)
axis(side=3)
box()




###########################################################
## 3 make a scatter plot for publications
###########################################################
?par
windows(width=12, height=8)
par(mar=c(4.5, 4.5, 0.5, 4.5), tcl = -0.5, 
	mgp=c(2.5, 1, 0))
plot(Bio1~LAT, data = d.clim, 
	pch=19, col=col, font=3, font.lab=2,                       ## change the symbol
	cex=1.2, cex.axis=1.2, cex.lab=1.7,                        ## modify the size of symbols and text
	xlab="latitude",                            ## include axes lables
	ylab="mean annual temperature",                          ## include axes lables
      ylim=c(-15,25), xlim=c(15, 60))                           ## set up the limits of the two axes

## draw a linear regression line
## linear regression model
m <- lm(Bio1 ~ LAT, data = d.clim)
## extract the intercept and slope
a <- summary(m)$coefficient[1,1]; b <- summary(m)$coefficient[2,1]
abline(c(a,b), col="black", lwd=2)                                 ## change color and weight of the line

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
m <- lm(Bio4~LAT, data = d.clim)
## extract the intercept and slope
a <- summary(m)$coefficient[1,1]; b <- summary(m)$coefficient[2,1]
abline(c(a,b), col="blue", lwd=2)                                  ## change color and weight of the line

box()

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







###########################################################
## 4 make a bar plot
###########################################################
## example 1
r2 <- as.matrix(data.frame(sprich.tree = c(0.35, 0.45, 0.10, 0.10), 
	sprich.shrub = c(0.25, 0.30, 0.15, 0.30), 
	sprich.liana = c(0.40, 0.30, 0.10, 0.20)))
windows()
barplot(height=r2, 
	width = 1, space = 1, beside = F, 
	col = rainbow(4), ylab = "r-squared", 
	cex.lab = 1.7, cex.axis = 1.4, cex.names = 1.4)
box()


## example 2
d.butt <- read.csv("01&02 R introduction/Butterflies.csv", header=T, stringsAsFactors=F)
mean.num.sp <- tapply(X=d.butt[,2], INDEX=d.butt[,1], FUN=mean)
mean.num.sp

## first, define how the figure looks
par(mar=c(6,6,3,1.5), ## margin
	cex.main=3, cex.axis=1.5, cex.lab=3       ## size of the axis labels
	)
barplot.x <- barplot(mean.num.sp, space=0.5, col="skyblue", 
	names.arg=names(mean.num.sp),
	ylim=c(0, 16), xlim=c(0,3.5), 
	main = "butterfly", xlab = "slope", ylab = "number of species"
	)
box()


## add the error bars
std.sp <- tapply(X=d.butt[,2], INDEX=d.butt[,1], FUN=sd)
std.sp

## make a function to calculate standard error
std.error <- function(x, ...) {
	n <- sum(!is.na(x))
	sd(x, ...)/sqrt(n)
	}

ste.sp <- tapply(X=d.butt[,2], INDEX=d.butt[,1], FUN=std.error)
ste.sp

arrows(barplot.x, mean.num.sp + ste.sp, barplot.x, 
	mean.num.sp, angle=90, code=3, length=0.1)





###########################################################
## 5 make histograms
###########################################################
## open a new graphic window
windows(width = 8, height = 10, pointsize = 12)
## modify the default setting of the figure region
par(
        mfrow = c(2, 1),           ## number of panels
        mar = c(4,4,1,1),          ## margin
        tck = -0.02,                    ## tick length
        mgp = c(2.5, 0.8, 0)     ## distance between axes and axes labels
        )
hist(d.clim$Bio1, breaks=30,
      main="", xlab = "mean annual temperature", ylab = "frequency",          ## labels
      cex.axis = 1.2, cex.lab = 1.7, col="lightgreen", border="black")        ## size of labels and texts and color

hist(d.clim$Bio12, breaks=10,
	main="", xlab = "mean annual precipitation", ylab = "frequency",
	cex.axis = 1.2, cex.lab = 1.7, col="lightgreen", border="black")





###########################################################
## 6 make a box plot
###########################################################
## generate an example data frame, containing four columns
d1 <- data.frame(
	a = rnorm(100, sd = 1.8), 
	b = rnorm(100, mean=2.5, sd = 1), 
	c = rnorm(100, mean = 1.5, sd = 2), 
	d = rnorm(100, mean = 4, sd = 1))
## box plot for the example data 
boxplot(d1,  
	notch = T,                                         ## see boxplot.stats for the calcultion of notch
	col= "darkgray",
	ylab="values", xlab="variables", 	                  ## axis labels
	cex.axis=1.3, cex.lab=1.7, 	                  ## size for axis labels
	pch=19, cex=1.2)	                               ## symbol type and size for outliers



## box plot for the mean annual temperature of different 
## biogeographical regions in China
boxplot(Bio1~Biome, data = d.clim, 
	ylab = "mean annual temperature", 
	xlab = "Biogeographical regions", 
	notch = T, 
	col= "darkgray", pch=19, cex=1.2, cex.axis=1.3, cex.lab=1.7)
## draw a line showing the national mean value of MAT
abline(h=mean(d.clim$Bio1), col="red", lty=2, lwd=2)


## provide a new name for the group
biogeo.region <- c("Southeast China", "eastern Himalayas", "Tibetan Plateau", "North China",    
			"Mongolian Plateau", "Northeast China", "Northwest China")
par(mar=c(16, 5.5, 1, 1), mgp=c(12, 0.6, 0))
boxplot(Bio1~Biome, data = d.clim, 
	names = biogeo.region,                          ## names of the groups
	las = 3,                                        ## direction of the group names
	ylab = "mean annual temperature", 
	xlab = "Biogeographical regions", 
	notch = T, 
	varwidth = T,                                   ## the box width is scaled by the sample size
	outwex = T, 
	outline = T,                                    ## to show the outliers
	col= "darkgray", pch=19, cex=1.2, cex.axis=1.3, cex.lab=1.7)

mtext(text = "mean annual temperature", side=2, line=2.5, cex=1.7) ## another way to fadd the axis labels
## draw a line showing the national mean value of MAT
abline(h=mean(d.clim$Bio1), col="red", lty=2, lwd=2)






###########################################################
## 7 make composed figures
###########################################################
## example 1: simple
par(mfrow=c(2,2))
for (i in 1:4) {
	y <- rnorm(20); x <- rnorm(20)
	plot(y~x, axes=T)
	}


## example 2: better
if (Sys.info()["sysname"]=="Windows") {width <- 16; height <- 10; windows(width=width, height=height)}
if (Sys.info()["sysname"] == "Darwin") {width <- 10; height <- 6.25; quartz(width=width, height=height)}
n.col <- 2; n.row <- 2
mylayout <- layout(matrix(1:((2+n.col)*(2+n.row)), ncol=2+n.col, byrow=T), 
		width=c(0.9*width/(2+n.col), rep(width/(2+n.col),times = n.col), 0.1*width/(2+n.col)),
		height=c(0.4*height/(2+n.row), rep(height/(2+n.row),times=n.row), 0.6*height/(2+n.row)))
layout.show((2+n.col)*(2+n.row))


## example 2: codes
## Leaf margin proportion vs. temperature
## load the data
setwd("E:/my_teaching/r_group/03 Plots in R/")
data <- read.csv("data_for_plots.csv", header=T, stringsAsFactors=F)
names(data)

## X variables and corresponding labels
x.list <- c("Bio1", "Bio11", "KMAT", "Bio12")
x.lab <- c("MAT", "MTCQ", "1/KT", "MAP")
Nx <- length(x.list)

## y variables and corresponding labels
y.list <- c("LM.dicot", "LM.tree", "LM.shrub", "LM.vine")
y.lab <- c("% dicots", "% trees", "% shrubs", "% lianas")
Ny <- length(y.list)

## set a window for ploting and design the arrangement
## number of columnss: Nx; number of rows: Ny
if (Sys.info()["sysname"]=="Windows") {width <- 6; height <- 7; windows(width=width, height=height)}
if (Sys.info()["sysname"] == "Darwin") {width <- 7; height <- 7; quartz(width=width, height=height)}
NCOL <- Nx; NROW <- Ny
lom <- matrix(1:((2+NCOL)*(2+NROW)), ncol=2+NCOL, byrow=T)
mylayout <- layout(lom, 
		width=c(0.7*width/(2+NCOL), rep(width/(2+NCOL),times = NCOL), 0.1*width/(2+NCOL)),
		height=c(0.4*height/(2+NROW), rep(height/(2+NROW),times=NROW), 0.6*height/(2+NROW)))
layout.show((2+NCOL)*(2+NROW))
for (i in 1:length(lom)) {par(mar=c(0,0,0,0)); plot.new()}

for (ii.row in 1:NROW) {
	## get y variable
	y.var <- y.list[ii.row]
	y <- data[,y.var]
	
	## set the scale of Y axis
	ylim <- range(y, na.rm=T) + (max(y, na.rm=T) - min(y, na.rm=T))*c(-0.2,0.2)

	for (jj.col in 1:NCOL) {
		## get the x variable
		x.var <- x.list[jj.col]
		x <- data[,x.var]
		
		## set the scale of Y axis
		xlim <- range(x, na.rm=T) + (max(x, na.rm=T) - min(x, na.rm=T))*c(-0.2,0.2)
		
		## scatter plots: plot the data one by one
		par(mfg=c(ii.row+1, jj.col), 
			mar=c(0.9,0.9,0.1,0.1), mgp=c(2.4,0.3,0), 
			cex=0.8, cex.axis=1.1, cex.lab=1, tck=-0.03, new=F)
		plot(y~x, pch=19, 
			col=rgb(t(col2rgb("darkgray")/255), alpha=0.1), 
			axes=F, xlim=xlim)
		axis(side=1, label=F); axis(side=2, label=F); box()
		
		## regression model. optional
		m <- lm(y~x)		

		## draw the regression lines. optional
		abline(m, lwd=2, col="black")		
		
		## draw the axes and labels of the first column
		if (jj.col == 1) {
			axis(side=2, label=T); 
			mtext(side=2, text=y.lab[ii.row], line=2.1, cex=1.2, las=0); 
			}
		
		## draw the axes of the first and last rows
		#if (ii.row == 1) mtext(side=3, text=sp.list[j], line=0.3, cex=0.8, las=0)
		if (ii.row == NROW) {
			axis(side=1)
			mtext(side=1, text=x.lab[jj.col], line=1.8, cex=1.2, padj=0.7)
			}
		}
	}



###########################################################
## 8 draw polygons on figures
###########################################################
## set up the limits of the two axes
col.rgb <- col2rgb(col = "darkgray"); col.rgb                   ## set semi-transparent color
col <- rgb(red=col.rgb[1]/255, green=col.rgb[2]/255, 
		blue=col.rgb[3]/255, alpha = 0.2)
col <- rgb(t(col.rgb)/255, alpha = 0.2)

plot(Bio12~Bio1, data = d.clim, 
	pch=19, col=col, font=3, font.lab=2,
	cex=0.9, cex.axis=1.2, cex.lab=1.4,                       ## modify the size of symbols and text
	xlab="mean annual temperature",                           ## include axes lables
	ylab="mean annual precipitation",                         ## include axes lables
	ylim=c(0, 4000), xlim=c(-15,25))                          ## set up the limits of the two axes


## emphasize the points with precipitation > 2500 mm
subset <- d.clim$Bio12 > 2500
points(Bio12~Bio1, data = d.clim, subset=subset, cex=1.5, col="blue", pch=19)

## draw a polygon
polygon(x = c(10, 26, 26, 10, 10), 
        y = c(2200, 2200, 3800, 3800, 2200),
        col = "lightgreen")

## emphasize the points with precipitation > 2500 mm
subset <- d.clim$Bio12 > 2500
points(Bio12~Bio1, data = d.clim, subset=subset, cex=1.5, col="blue", pch=19)


## draw polygons
x<-seq(0,10,length.out = 1000)
y1<- dnorm(x, mean = 2,sd=1)
y2<- dnorm(x, mean = 6,sd=1)
plot(x,y1,type="l")
lines(x,y2)

y = pmin(y1, y2)

## draw a polydon showing the overlapped part
polygon(x = c(x, x[length(x)], x[1]), 
        y = c(y, 0, 0),
        col = "blue")










#########################################
## 9 using ggplot2
#########################################
install.packages("ggplot2")
install.packages("ggpubr")

library("ggplot2")
library("ggpubr")
setwd("e:/my_teaching/r_group/03 Plots in R/")
mydata <- read.csv("data_for_plots.csv", header=T, stringsAsFactors=F)
mycolor <- rainbow(3, alpha=0.3)

ggplot(data=mydata,
	aes(y=LM.dicot, x=Bio1))+ 
geom_point(pch=19,color=mycolor[3],size=2.8) 
	









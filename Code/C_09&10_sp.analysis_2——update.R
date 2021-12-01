

## libraries needed: sp, maptools, rgdal
library(sp); library(maptools); library(rgdal)
library(fields); library(rgeos); library(spdep)

## set the working directory
setwd("E:/My_Teaching/R_group/09&10 Spatial analysis with R/")



########################################################################################
## 6 Moran's I
#### a function by Zhiheng Wang, 2011
########################################################################################

moran.i <- function(x, d, n.dclass=NULL, method=c("equal.pairs", "equal.distance"))
	{
	if (!c("matrix") %in% class(d)) stop("distance is not in matrix format")
	n <- length(x)
	if (!all(dim(d) == c(n,n))) stop("the dimension of the distance matrix does not match the length of x")
	
	distclass <- function(d, n.dclass, method)
		{
		epsilon <- 0.0000000001
		if (method == "equal.distance") d.brks <- (0:n.dclass) * max(d, na.rm=TRUE)/n.dclass
		else if (method == "equal.pairs") d.brks <- as.numeric(quantile(d,prob=seq(0,1,length.out=n.dclass+1),na.rm=T))
		d.brks[n.dclass + 1] <- d.brks[n.dclass + 1] + epsilon

		dclass <- findInterval(d, d.brks, left.open = TRUE)
		dclass[dclass == 0] <- NA
		return(dclass)
		}

	method <- method[1]
	mu <- mean(x, na.rm=TRUE)
	variance <- var(x, na.rm=TRUE)

	if (is.null(n.dclass)) n.dclass <- round(1.5+3.3*log10(length(d)))
	morani <- numeric(length=n.dclass)
	dclass <- distclass(d=d, n.dclass=n.dclass, method=method)
	d.ave <- tapply(d, dclass, mean, na.rm=TRUE)
	n.pair <- tapply(d, dclass, length)
	
	x1 <- rep(x, times=n)
	x2 <- rep(x, each=n)
	for (i in 1:n.dclass)
		{
		gridpairs <- which(dclass==i)
		x1.1 <- x1[gridpairs]
		x2.1 <- x2[gridpairs]
		morani[i] <- sum((x1.1-mu)*(x2.1-mu),na.rm=T)/length(gridpairs)/variance
		}
	return(cbind(d.ave, morani, n.pair))
	}

SA <- read.csv("data_SouthAmerica_MacEco_Course.csv", header=T)

d <- spDists(as.matrix(SA[,c("Longitude", "Latitude")]), longlat=T)
head(d)
tail(d)
moran.SA <- moran.i(x=SA$Birds.richness, d=d, n.dclass=20, method="equal.pairs")

windows(); par(mfrow=c(3,1))
plot(moran.SA[,c(1:2)], pch=19, ylim=c(-1,1))
lines(moran.SA[,c(1:2)]); abline(h=0, lty=2)

moran.SA <- moran.i(x=SA$Birds.richness, d=d, n.dclass=20, method="equal.distance")
plot(moran.SA[,c(1:2)], pch=19, ylim=c(-1,1))
lines(moran.SA[,c(1:2)]); abline(h=0, lty=2)


#### by spdep package
## define distance interval, which was used to define neighbors
d.interval <- max(d)/20
## define the neighbors of each grid cell
nbs <- dnearneigh(x=as.matrix(SA[,c("Longitude", "Latitude")]), d1=d.interval*19, d2=d.interval*20, longlat=TRUE)
summary(nbs)
names(attributes(nbs))
card(nbs)
nbs[[313]]
which(d[313,]>d.interval*19 & d[313,]<=d.interval*20)
## transform the neighbor obj into a listw obj
nbs.lw <- nb2listw(nbs, style="B", zero.policy=TRUE) ## change style
nbs.lw$weights[card(nbs)>0]
sum(nbs.lw$weights[card(nbs)>0][[1]])
## caculate the Moran's I for the given distance interval 
moran.SA1 <- moran.test(x=SA$Birds.richness, listw=nbs.lw, zero.policy=TRUE)

## for all distances
moran.SA.spdep <- matrix(NA, nrow=20, ncol=4)
for (i in 1:20) {
	d1 <- d.interval*(i - 1)
	d2 <- d.interval*i
	moran.SA.spdep[i, 1:3] <- c(d1, d2, median(c(d1,d2)))
	## define the neighbors of each grid cell
	nbs <- dnearneigh(x=as.matrix(SA[,c("Longitude", "Latitude")]), d1=d1, d2=d2, longlat=TRUE)
	## transform the neighbor obj into a listw obj
	nbs.lw <- nb2listw(nbs, style="W", zero.policy=TRUE)
	## caculate the Moran's I for the given distance interval 	
	moran.SA1 <- moran.test(x=SA$Birds.richness, listw=nbs.lw, randomisation=F, zero.policy=T)
	moran.SA.spdep[i,4] <- moran.SA1$esti[1]
	}
plot(moran.SA.spdep[,c(3:4)], pch=19, ylim=c(-1,1))
lines(moran.SA.spdep[,c(3:4)]); abline(h=0, lty=2)




########################################################################################
## 7 spatial regression: spdep package
########################################################################################
install.packages("spatialreg")

library(spatialreg)
## simple linear regression: lm
plot(Birds.richness ~ AET, data=SA)
m0 <- lm(Birds.richness ~ AET, data=SA)
summary(m0)
d <- spDists(as.matrix(SA[,c("Longitude", "Latitude")]), longlat=T)
moran.resid <- moran.i(m0$residuals, d=d, n.dclass=20, "equal.pairs")

## moran's I of the data and the residuals
plot(moran.SA[,c(1:2)], pch=19, ylim=c(-1,1))
lines(moran.SA[,c(1:2)]); abline(h=0, lty=2)
points(moran.resid[,c(1:2)], pch=19, col="red")
lines(moran.resid[,c(1:2)], col="red")


## Error SAR
d2 <- max(d)/20
nbs <- dnearneigh(x=as.matrix(SA[,c("Longitude", "Latitude")]), d1=0, d2=d2, longlat=TRUE)
nbs.lw <- nb2listw(nbs, style="B")
moran.SA1 <- moran.test(x=SA$Birds.richness, listw=nbs.lw)

m1 <- errorsarlm(Birds.richness ~ AET, data = SA, listw=nbs.lw, tol.solve=1.0e-20)
summary(m1)
1 - summary(m1)$s2/var(SA$Birds.richness)
m1.1 <- lm(Birds.richness ~ m1$fitted, data=SA)
summary(m1.1)$r.square

moran.resid <- moran.i(x=m1$residuals, d=d, n.dclass=20, "equal.pairs")
points(moran.resid[,c(1:2)], pch=19, col="green"); lines(moran.resid[,c(1:2)], col="green")


## lag SAR
m2 <- lagsarlm(Birds.richness ~ AET, data = SA, listw = nbs.lw, tol.solve=1.0e-20)
summary(m2)
1 - summary(m2)$s2/var(SA$Birds.richness)
summary(lm(Birds.richness~m2$fitted, data=SA))$r.square

moran.resid <- moran.i(m2$residuals, d=d, n.dclass=20, "equal.pairs")
points(moran.resid[,c(1:2)], pch=19, col="blue"); lines(moran.resid[,c(1:2)], col="blue")


## NULL models of error SAR and lag SAR
m1.null <- errorsarlm(Birds.richness ~ 1, data = SA, listw = nbs.lw, tol.solve=1.0e-20)
summary(m1.null)
1 - summary(m1.null)$s2/var(SA$Birds.richness)
summary(lm(Birds.richness ~ m1.null$fitted, data=SA))$r.square
anova(m1.null, m1)
(1 - summary(m1)$s2)/var(SA$Birds.richness) - (1-summary(m1.null)$s2)/var(SA$Birds.richness)


m2.null <- lagsarlm(Birds.richness ~ 1, data=SA, listw=nbs.lw, tol.solve=1.0e-20)
summary(lm(Birds.richness~m2.null$fitted, data=SA))$r.square
summary(m2.null)
1 - summary(m2.null)$s2/var(SA$Birds.richness)
(1 - summary(m2)$s2)/var(SA$Birds.richness) - (1-summary(m2.null)$s2)/var(SA$Birds.richness)







########################################################################################
## 8 spline
########################################################################################
install.packages("fields")
install.packages("gstat")
library(fields)
require(fields)
library(gstat)
d.clim <- read.csv("~/Desktop/phD/Courses/R application in ecology/climate.csv", header=T)
names(d.clim)
dim(d.clim)
pts.clim <- d.clim
coordinates(pts.clim) <- ~ Longitude+Latitude
plot(pts.clim)

## krige
newdata <- read.csv("~/Desktop/phD/Courses/R application in ecology/China_grid_points_50km.csv")
newdata.pts <- newdata
coordinates(newdata.pts) <- ~LON+LAT
x <- krige(T01 ~ 1, locations=pts.clim, newdata=newdata.pts)
spplot(x["var1.pred"])



## thin plate spline
m <- Tps(x=as.matrix(d.clim[,c("Longitude", "Latitude", "Elevation")]), Y=d.clim$T01, m=3, lon.lat = T, miles = F)
X <- data.frame(Longitude = c(116, 116), Latitude=c(40,40), Elevation=c(500,800))
clim.pred <- predict(object = m, x = X)
X <- newdata[,2:4]; colnames(X) <- c("Longitude", "Latitude", "Elevation")
clim.pred <- predict(object = m, x = X)
newdata.pts@data$T01 <- clim.pred[,1]
spplot(newdata.pts["T01"])

m2 <- Tps(x=as.matrix(d.clim[,c("Longitude", "Latitude", "Elevation")]), Y=d.clim$R01, m=3, lon.lat = T, miles = F)
X <- data.frame(Longitude = c(116, 116), Latitude=c(40,40), Elevation=c(500,800))
clim.pred <- predict(object = m2, x = X)
X <- newdata[,2:4]; colnames(X) <- c("Longitude", "Latitude", "Elevation")
clim.pred <- predict(object = m2, x = X)
newdata.pts@data$R01 <- clim.pred[,1]
spplot(newdata.pts["R01"])












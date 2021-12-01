setwd("e:/my_teaching/r_group")



###########################################################
## 1 Principal component analysis
###########################################################

## example 1: PCA for temperature and precipitation variables 
## load the data
d.clim <- read.csv(file="01&02 R introduction/ClimateChina.csv", header=T, stringsAsFactors=F)
names(d.clim)

d.clim.t1 <- d.clim.t2 <- d.clim.t3 <- d.clim[,c(4:14)]

## not good
t.pca1 <- princomp(d.clim.t1, cor = F)
summary(t.pca1)
t.pca1$loadings[]
r.mat <- cor(d.clim.t1, t.pca1$scores)
r.mat
biplot(t.pca1, xlabs = rep("o", times=dim(d.clim.t1)[1]), col=c("gray", "red"))

## good
for (i in 1:ncol(d.clim.t2)) d.clim.t2[,i] <- (d.clim.t2[,i] - mean(d.clim.t2[,i]))/sd(d.clim.t2[,i])
t.pca2 <- princomp(d.clim.t2, cor = F)
summary(t.pca2)
t.pca2$loadings[]
r.mat <- cor(d.clim.t2, t.pca2$scores)
r.mat
biplot(t.pca2, xlabs = rep("o", times=dim(d.clim.t1)[1]), col=c("gray", "red"))


## good
t.pca3 <- princomp(d.clim.t3, cor = T)
summary(t.pca3)
t.pca3$loadings[]
r.mat <- cor(d.clim.t3, t.pca3$scores)
r.mat
biplot(t.pca2, xlabs = rep("o", times=dim(d.clim.t1)[1]), col=c("gray", "red"))
abline(h=0, v=0, lty=2)


## another way to plot the data
windows()
scores <- t.pca3$scores
loadings <- t.pca3$loadings[]
plot(scores[,2] ~ scores[,1], xlim=c(-6,6), ylim=c(-6,6),
	col="darkgray", pch=19,xlab="PC axis 1", ylab="PC axis 2")
abline(h=0, v=0, lty=2)
scale <- min(max(t.pca3$scores[,2]), max(t.pca3$scores[,1]))*3
textNudge <- 1.2
for (i in 1:nrow(r.mat)) {
	arrows(x0=0, y0=0, x1 = loadings[i,1]*scale, y1 = loadings[i,2]*scale, length=0.1, col="red")
	text(x = loadings[i,1]*scale*textNudge, y = loadings[i,2]*scale*textNudge, lab=rownames(loadings)[i], 
		col="red", adj=ifelse(sign(loadings[i,1])==-1, 1.5, -0.5))
	}




###########################################################
## 2 structural equation model: SEM
###########################################################
install.packages("sem") 
require(sem)

d.XJ <- read.csv("04&05 Correlations and regressions/speciesRichness3ResolutionXinjiang.csv",header=T)
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
mammalRS = beta11 * plantRS + gam12 * AET + gam13 * PET + gam14 * PET2
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
## 3 linear mixed effect model: LME
###########################################################

## load the data
d.woody.pro <- read.csv("04&05 Correlations and regressions/woody_prov.csv", header=T)

## including interaction terms
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

## different intercept for each biogeographical region
m5 <- lmer(LogWoody_Area ~ Tmin + (1|Biogeo), data = d.woody.pro, REML=FALSE)
summary(m5)

## different intercept and slope for each biogeographical region
m6 <- lmer(LogWoody_Area ~ Tmin + (1 + Tmin|Biogeo), data = d.woody.pro, REML=FALSE)
summary(m6)

## no fix effect, different intercept for each biogeographical region
m7 <- lmer(LogWoody_Area ~ 1 + (1|Biogeo), data = d.woody.pro, REML=FALSE)
anova(m7, m5)
summary(m7)

## no fix effect, different intercept and slope for each biogeographical region
m8 <- lmer(LogWoody_Area ~ 1 + (1+Tmin|Biogeo), data = d.woody.pro, REML=FALSE)
summary(m8)
anova(m8, m6)

anova(m5, m6)









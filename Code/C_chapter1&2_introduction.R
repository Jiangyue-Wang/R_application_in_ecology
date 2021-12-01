
###########################################################
## chapter 1: R environment and basic syntax
###########################################################


###########################################################
## 1 How to install packages
###########################################################
## manually: Packages -> set CRAN mirror; Packages -> Install package(s)
## OR: with the following code
install.packages("maptools") ## maptools is the name of a package


###########################################################
## 2 How to get help in R
###########################################################

## If you know the function names:
?read.table
?lm

## If you know a part of the function name
apropos(???cor????¡§???¡ì?¡ì)
apropos(???lm????¡§???¡ì?¡ì)

## If you don????¡ìo??¡ì?¡ìt know the functions
??read.table
??lm
??????¡§???¡ì?¡ìlinear regression????¡§???¡ì?¡ì
help.search(????¡§???¡ì?¡ìlinear regression????¡§???¡ì?¡ì)

## R manual
help.start()



###########################################################
## 3 How to define a variable and asign values
###########################################################
var1 <- 2.5
c(2.5, 3, 6) -> var2
var3 = 1:10
var1



###########################################################
## 4 Generate a numeric array
###########################################################

## generate a numeric array with 10 elements
x <- numeric(length=10)
x
## [1] 0 0 0 0 0 0 0 0 0 0

## set the 1st, 2nd, and 5th elements to be 2
x[c(1,2,5)] <- 2
x
## [1] 2 2 0 0 2 0 0 0 0 0

x <- c(1,2,3,4,5,6,7,8,9,10)
x
## [1]  1  2  3  4  5  6  7  8  9 10

x <- 1:10
x
## [1]  1  2  3  4  5  6  7  8  9 10

x <- seq(from = 1, to = 10, by = 2)
x
## [1] 1 3 5 7 9

## generate 10 random numbers from a normal distribution
x <- rnorm(10)
x

## name each element of the numeric array
names(x) <- 1:10
## extract or show the names 
names(x)

## view the data
View(x)

## extract some elements
x[3]
x["3"]

## calculate the length
length(x)



###########################################################
## 5 Generate a character array
###########################################################

## generate a numeric array with 10 elements
x <- character(length=10)
x
## [1] "" "" "" "" "" "" "" "" "" ""

## set the 3rd, 7th and 2nd elements to be "a"
x[c(3,7,2)] <- "a"
x
## [1] ""  "a" "a" ""  ""  ""  "a" ""  ""  "???

x <- c("ab","d","e")
x
## [1] "ab" "d"  "e" 

x <- letters
x
## [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
##[20] "t" "u" "v" "w" "x" "y" "z????¡§???¡ì?¡ì

x <- 1:10
x <- as.character(x)
x
## [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10????¡§???¡ì?¡ì

## name each element of the characterarray
names(x) <- 1:10
## extract or show the names 
names(x)

## view the data
View(x)

## extract some elements
x[3]
x["3"]

## calculate the length
length(x)




###########################################################
## 6 Generate a factor array
###########################################################

x <- factor(rep(1:2,times=5),levels=c(1,2))
x
## [1] 1 2 1 2 1 2 1 2 1 2
## Levels: 1 2

x <- factor(rep(1:2,times=5),levels=c(1,2),labels=c("control","treatment"))
x
## [1] control   treatment control   treatment control   treatment control  
## [8] treatment control   treatment
## Levels: control treatment

x <- factor(x = as.character(c(0,0,0,50,50,50,100,100,100)), 
		levels = c("0","50","100"), labels = c("control", "low N", "high N"))
x
## [1] control control control low N   low N   low N   high N  high N  high N 
## Levels: control low N high N

x <- 1:10
x <- as.factor(x)
x
## [1] 1  2  3  4  5  6  7  8  9  10
## Levels: 1 2 3 4 5 6 7 8 9 10


## name each element of the factor array
names(x) <- 1:10
## extract or show the names 
names(x)

## view the data
View(x)

## extract some elements
x[3]
x["3"]

## calculate the length
length(x)



###########################################################
## 7 Generate a logical array
###########################################################
x <- logical(length = 10)
x
## [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

x <- c(TRUE, FALSE)
x
## [1]  TRUE FALSE

x <- as.logical(rep(c(0,1), times=5))
x
## [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE

x <- as.logical(rep(c(1,2), times=5))
x
x <- as.logical(rep(c(-0.4,1.5), times=5))
x




###########################################################
## 8 Generate a matrix
###########################################################
x <- matrix(data=1:25, nrow=5, ncol=5, byrow=FALSE)
x
##     [,1] [,2] [,3] [,4] [,5]
##[1,]    1    6   11   16   21
##[2,]    2    7   12   17   22
##[3,]    3    8   13   18   23
##[4,]    4    9   14   19   24
##[5,]    5   10   15   20   25

## show/extract the dimension of the matrix
dim(x)
#[1] 5 5

x <- as.character(1:20)
dim(x) <- c(4,5)
x
##     [,1] [,2] [,3] [,4] [,5]
##[1,] "1"  "6"  "11" "16" "21"
##[2,] "2"  "7"  "12" "17" "22"
##[3,] "3"  "8"  "13" "18" "23"
##[4,] "4"  "9"  "14" "19" "24"
##[5,] "5"  "10" "15" "20" "25"

## column and row names of a matrix
x <- matrix(data=1:25,nrow=5,ncol=5,byrow=FALSE)
colnames(x) <- c("var1","var2","var3","var4","var5")
rownames(x) <- as.character(1:5)
x
##  var1 var2 var3 var4 var5
##1    1    6   11   16   21
##2    2    7   12   17   22
##3    3    8   13   18   23
##4    4    9   14   19   24
##5    5   10   15   20   25

##View/Extract data from a matrix
View(x)

names(x)
colnames(x)
##[1] "var1" "var2" "var3" "var4" "var5"
rownames(x)
##[1] "1" "2" "3" "4" "5"

## extract a value
x[2, 3]
#[1] 12

x[6,3]
#Error in x[6, 3] : subscript out of bounds

## extract a row
x[2, ]
##   var1 var2 var3 var4 var5 
##     2      7     12     17     22 

## extract a column
x[, 3]
##  1   2   3   4   5 
##  11 12 13 14 15 

x[, "var3"]
##  1   2   3   4   5 
##  11 12 13 14 15 

## Columns/row statistics of a matrix
colSums(x)
##[1]  15  40  65  90 115

rowSums(x)
##[1] 55 60 65 70 75




###########################################################
## 9 Generate a data frame
###########################################################
x <- data.frame(a = letters[1:5], 
			b = 1:5, 
			c = factor(c("T", "T", "T", "C", "C")),
	 		stringsAsFactors = FALSE)
x
#  a b c
#1 a 1 T
#2 b 2 T
#3 c 3 T
#4 d 4 C
#5 e 5 C

x <- data.frame(letters[1:5], 
			1:5, 
			factor(c("T", "T", "T", "C", "C")),
	 		stringsAsFactors = FALSE)
x

View(x)
colnames(x)
##[1] "a" "b" "c"

## extract all data of a column
x$a  ## character
## [1] "a" "b" "c" "d" "e"

x$b ## numeric
##[1] 1 2 3 4 5

x[1, 2] ## numeric
##[1] 1

b <- x[,"a", drop =F]

length(x) 
[1] 3 ## which is the number of columns; not 5, either 15

x <- matrix(data = 1:25, nrow=5, ncol=5, byrow = FALSE)
x <- as.data.frame(x)
x



###########################################################
## 10 Generate a list
###########################################################
x.temp <- matrix(data=1:25,nrow=5); colnames(x.temp) <- paste("v",1:5,sep="")
x <- list(a = letters[1:10], b = x.temp, c = factor(c("T", "T", "T", "C", "C")))
x

names(x)
#[1] "a" "b" "c"

length(x)
#[1] 3

structure(x)
#List of 3
# $ a: chr [1:10] "a" "b" "c" "d" ...
# $ b: int [1:5, 1:5] 1 2 3 4 5 6 7 8 9 10 ...
#  ..- attr(*, "dimnames")=List of 2
#  .. ..$ : NULL
#  .. ..$ : chr [1:5] "v1" "v2" "v3" "v4" ...
# $ c: Factor w/ 2 levels "C","T": 2 2 2 1 1






###########################################################
## 11 missing values
###########################################################
length( NA )
#[1] 1
 
length( NULL )
#[1] 0
 

mode( NULL )
#[1] "NULL"
 
class( NULL )
#[1] "NULL"

NULL == NULL
#logical(0)
 
NA == NA
#[1] NA

 
is.na( NA )
#[1] TRUE
 
is.na( NULL )
#logical(0)
#Warning message:
#In is.na(NULL) : is.na() applied to non-(list or vector) of type 'NULL'



## to find which elements have missing values
x <- c(1.2, 3, NA, 2.5, 3.2, NA)
is.na(x)
#[1] FALSE FALSE TRUE 
#[4] FALSE FALSE TRUE

which( is.na(x) )
#[1] 3 6



## to remove the missing values
x[ !is.na(x) ]
#[1] 1.2 3.0 2.5 3.2

x[ -which( is.na(x) ) ]
#[1] 1.2 3.0 2.5 3.2

x <- x[ !is.na(x) ]



###########################################################
## 12 data table input
###########################################################
## from text file
## set the working directory
setwd("E:/My_Teaching/R_group/")

d.clim <- read.table(file = "01 R introduction/ClimateChina.txt", header=T)

## from CSV file
d.clim <- read.csv(file = "01 R introduction/ClimateChina.csv", header=T)

## from dbf files
install.packages("foreign")
require("foreign")
d.grid50km <- read.dbf(file = "01 R introduction/grid50km.dbf")

## from clipboard, windows
d <- read.delim("clipboard", header=TRUE)

## view data
View(data)
names(data)
str(d)



###########################################################
## 13 write out data table
###########################################################
d.clim <- read.table(file = "01 R introduction/ClimateChina.txt", header=T)
## write the first 100 rows of the table into a text file
write.table(x=d.clim[1:100,],file = "ClimateChina_part1.txt",sep=" ")

## write as CSV file
write.csv(x=d.clim[1:100,],file = "ClimateChina_part1.csv", row.names=F)

## write as DBF file
write.dbf(dataframe=d.clim[1:100,],file = "ClimateChina_part1.dbf")




###########################################################
## 14 save and load a R object
###########################################################
d.clim <- read.table(file = "01 R introduction/ClimateChina.txt", header=T)
## save a variable as R object
save(d.clim, file="01 R introduction/ClimateChina.R")

## load a R object
load("01 R introduction/ClimateChina.R")

## load a R object
## If you don't remember the name of the variable, you could find it out from "a"
a <- load("01 R introduction/ClimateChina.R")



###########################################################
## 15 simple math: mean, standard deviation, standard error and variance 
##################################################################
d.butt <- read.csv("01 R introduction/Butterflies.csv", header=T, stringsAsFactors=F)
str(d.butt)

mean(d.butt[, 2])        ## mean
sd(d.butt[, 2])          ## standard deviation
var(d.butt[, 2])         ## variance
std.error(d.butt[, 2])   ## standard error
sd(d.butt[, 2])/sqrt(dim(d.butt)[1])    ## standard error by definition
hist(d.butt[, 2])


sqrt(d.butt[,2])
log(d.butt[,2])


###########################################################
## 16 Conditional statements
##################################################################

## ifelse: return only one value
a <- 1:10
x <- sample(x=a, size=1, replace=F)
b <- x %% 2
y <- ifelse (b == 0, yes="even", no="odd")
y <- ifelse (b == 0, yes=c("even", "random"), no="odd")
y

## if else: more general
a <- 1:10
x <- sample(x=a, size=1, replace=F)
b <- x %% 2
if (b == 0) y <- rnorm(n = 10, mean = 0, sd = 1) else y <- rnorm(n = 10, mean = 10, sd = 1) 
y

## Error: the else statement could not be run
if (b == 0) y <- rnorm(n = 10, mean = 0, sd = 1) 
else y <- rnorm(n = 10, mean = 10, sd = 1)}

## right: put the two lines in {} 
{
if (b == 0) 
	y <- rnorm(n = 10, mean = 0, sd = 1) 
else 
	y <- rnorm(n = 10, mean = 10, sd = 1)
}

## another example: 
a <- 1:10
x <- sample(x=a, size=1, replace=F)
b <- x %% 3
{
if (b == 1) y <- rnorm(n = 10, mean = 10, sd = 1) 
else if (b == 2) y <- rnorm(n = 10, mean = 20, sd = 1) 
else y <- rnorm(n = 10, mean = 0, sd = 1) 
}
y 

## another example: 
a <- 1:10
x <- sample(x=a, size=1, replace=F)
x
{
if (x <= 3) y <- rnorm(n = 10, mean = 0, sd = 1) 
else if (x <= 6) y <- rnorm(n = 10, mean = 10, sd = 1) 
else y <- rnorm(n = 10, mean = 20, sd = 1) 
}
y 




###########################################################
## 17 Write loops
##################################################################

## using "for"
for (i in 1:100) {
	m <- i*2
	print(m)
	}

for (i in c("a", "b", "c")) print(i)

## using "while"
i <- 1
j <- 10
while (i <= 10) {
	j <- i + 1
	print(j)
	i <- i + 1
	}

i <- 1
j <- 10
while (i <= 10) {
	j <- i + 1
	print(j)
	i <- i + 1
	if (j > 8) break()
	}

## bad script
i <- 1
j <- 10
while (i <= 10) {
	j <- i + 1
	print(j)
	}


## a more complicated case
## generate 100 numeric arrays from different distributions

a <- rnorm(n=100, mean=0, sd=1)
result <- list()
for (i in 1:100) {
	if (a[i] >= 0) result[[i]] <- rnorm(n=100, mean = 1, sd = 1)
	else result[[i]] <- rnorm(n=100, mean = -1, sd = 1)
	print(i)
	}
result 



###########################################################
## 18 Use apply functions
###########################################################
## use loop
result.mean <- result.sd <- numeric(length = 100)
for (i in 1:100) {
	result.mean[i] <- mean(result[[i]])
	result.sd[i] <- sd(result[[i]])
	}
cbind(mean = result.mean, sd = result.sd)

## use lapply
result.mean1 <- lapply(X = result, FUN = mean)
result.mean <- unlist(result.mean)
result.sd2 <- lapply(X = result, FUN = sd)
result.sd <- unlist(result.sd2)
cbind(mean = result.mean, sd = result.sd)


## use tapply
d.woody <- read.csv("01 R introduction/Woody_SpList_Lifeform_Division.csv", header=T, stringsAsFactors=F)
str(d.woody)
tmp <- tapply(X=d.woody$Species_E1, INDEX=d.woody$Family_E, FUN=length)
View(tmp)
tmp <- tapply(X=d.woody$Species_E1, INDEX=d.woody[c("Family_E","LifeForm1")], FUN=length)
View(tmp)

tmp <- tapply(X=d.woody$Height2, INDEX=d.woody[c("Family_E","LifeForm1")], FUN=mean, na.rm=TRUE)
View(tmp)


## use tapply: mean, standard deviation of species diversity on different slopes
d.butt <- read.csv("01 R introduction/Butterflies.csv", header=T, stringsAsFactors=F)
mean.num.sp <- tapply(X=d.butt[,2], INDEX=d.butt[,1], FUN=mean)
mean.num.sp

std.sp <- tapply(X=d.butt[,2], INDEX=d.butt[,1], FUN=sd)
std.sp


## use apply: conduct calculation for each column or row of a matrix
x <- matrix(rnorm(n=20), nrow=4, ncol=5)
## conduct calculation for each row
x.result <- apply(X=x, MARGIN=1, FUN=mean)
x.result
rowMeans(x)

## conduct calculation for each column
x.result <- apply(x, MARGIN=2, FUN=max)
x.result




###########################################################
## 19 Write your own functions
###########################################################
## example 1
Myfunction1 <- function(x = 1, y = 2)  {
	z <- x + y
	return(z)
	}
Myfunction1()
Myfunction1(x = 10, y = 2)


## example 2
Myfunction <- function(mean, sd) {
	x <- rnorm(n=10, mean=mean, sd=sd)
	return(x)
	}
Myfunction()
Myfunction(mean = 10, sd = 2)


## example 3
Myfunction <- function(x, ...) {
	x1 <- min(x, ...)
	x2 <- max(x, ...)
	x3 <- sd(x, ...)
	return(c(x1, x2, x3))
	}
Myfunction(d.woody$Height2)
Myfunction(d.woody$Height2, na.rm=T)

x <- matrix(rnorm(n=20), nrow=4, ncol=5)
x.result <- apply(x, MARGIN=1, FUN=Myfunction)
x.result





###########################################################
## 20 Write a script
###########################################################
## set up the working directory
setwd("E:/My_teaching/R_Group")

## read the data into R
d.clim <- read.csv(file = "01 R introduction/ClimateChina.csv", header=T)
str(d.clim)

## show the histograms of each variable
windows(width=8,height=10)
par(mar=c(3,3,1,0.5),mfrow=c(5,4), mgp=c(1.4,0.6,0))

## explore the correlations between climate variables
## define a matrix to put the correlation coefficients
n.var <- 19
cli.cor <- matrix(NA, nrow = n.var, ncol = n.var)
rownames(cli.cor) <- colnames(cli.cor) <- colnames(d.clim)[3+(1:n.var)]
for (i in 1:n.var)
	{
	x <- d.clim[,i+3]
	hist(x, ylab="", xlab="", main=colnames(d.clim)[i+3])
	for (j in i:n.var)
		{
		y <- d.clim[,j+3]
		xy.cor <- cor.test(x=x,y=y) ## return a list
		cli.cor[i,j] <- xy.cor$estimate
		}
	}

## export the result
write.csv(cli.cor,file="ClimateCorrelation.csv??¡§¡è)

















library(stringr)
setwd("E:/My_Teaching/R_group")




###########################################################
## 1 read texts
###########################################################
## read from a CSV file
d.lifeform <- read.csv("08 Strings analysis/Woody_SpList_Lifeform_Division.csv", 
	stringsAsFactors=F, header=T)
names(d.lifeform)
class(d.lifeform$Species_E1)

## read from a text file
d.splist <- readLines(con = "08 Strings analysis/Woody_SpList_Lifeform_Division_part1.txt")
d.splist <- readLines(con = "08 Strings analysis/Woody_SpList_Lifeform_Division_part2.txt")
class(d.splist)

## read from a text file
d.splist <- scan(file = "08 Strings analysis/Woody_SpList_Lifeform_Division_part2.txt", what="character", sep="\n")

## write to a file
writeLines(d.splist, con = "08 Strings analysis/text.txt")
cat(d.splist, file = "08 Strings analysis/text1.txt", sep="\n")



###########################################################
## 2 length of strings, lower case and upper case, sorting
###########################################################

## example 1: length of a string
x <- "apple"
nchar(x = x)

str_length(string = x)

y <- c("apple", "orange")
nchar(x = y)
str_length(string = y)

## for comparison
length(x)
length(y)


## example 2
y <- c("apple", "orange", "this is a banana", "I am Jon", "apply")
y1 <- toupper(y)
y1
tolower(y1)

str_to_title(y)


## example 3
sort(y)


###########################################################
## 3 manipulate spaces in strings
###########################################################
## example 1
## make all strings the same length by adding spaces
y <- c("apple", "orange", "this is a banana")
nchar(y)
y1 <- str_pad(string=y, width=12, side="both")
y1
nchar(y1)

str_trunc(y1, 12)


## example 2
## remove spaces from strings
str_trim(string = y1, side = "right")
str_trim("\n\nString with trailing and leading white space\n\n")

## remove repeated spaces
str_squish("  String with trailing,  middle, and leading white space\t")


## example 3
d.lifeform <- read.csv("08 Strings analysis/Woody_SpList_Lifeform_Division.csv", 
	stringsAsFactors=F, header=T)
names(d.lifeform)
which(d.lifeform$Species_E1 == "Alsophila andersonii")
d.lifeform$Species_E1[1]

which(d.lifeform$Species_E1 == "Alsophila austro-yunnanensis")
d.lifeform$Species_E1[2]

which(d.lifeform$Species_E1 == "Alsophila costularis")
d.lifeform$Species_E1[3]


d.lifeform$Species_E1 <- str_squish(d.lifeform$Species_E1)
#d.lifeform$Species_E1 <- str_trim(d.lifeform$Species_E1) ## does not remove spaces in between
which(d.lifeform$Species_E1 == "Alsophila andersonii")
d.lifeform$Species_E1[1]

which(d.lifeform$Species_E1 == "Alsophila austro-yunnanensis")
d.lifeform$Species_E1[2]

which(d.lifeform$Species_E1 == "Alsophila costularis")
d.lifeform$Species_E1[3]





###########################################################
## 4 search, extract or replace a part of a string
###########################################################
## length of a string
x <- "apple"
y <- c("apple", "orange", "banana")


## extract a part of from a string
substring(text = x, first = 2, last = 4)
str_sub(string = x, start=2, end=4)
str_sub(string = x, start=2, end=-2)

substring(y, first = 2, last = 4)
substring(y, c(2, 3, 4), c(4, 6, 6))
str_sub(string = y, start=c(2, 3, 3), end=c(4, 6, 5))


## replace 1
substring(x, first = 2, last = 4) <- "PPL6"
x
str_sub(string = x, start=2, end=4) <- "PPL"
x

## replace 2
x <- "MiXeD bAsE 123"
chartr(old="iXs", new="why", x)
chartr(old="a-cX", new="D-Fw", x)


## search and replace
grep(pattern = "PP", x=y, ignore.case=T)
grep(pattern = "or", x=y, ignore.case=F)
grepl(pattern = "or", x=y, ignore.case=F)

y1 <- c("apple", "ororange", "banana")
sub(pattern = "or", replacement="XX", x=y1, ignore.case=F)
gsub(pattern = "or", replacement="XX", x=y1, ignore.case=F)

tt <- regexpr(pattern="or", text = y1)
tt
gregexpr(pattern="or", text = y1)


## more complicated search
y <- c("apple", "orange", "banana", "ddd", "d2d8", "567", "DDD", "-", "pnas")

## look for any character
grep(pattern = "[0-9[:punct:]]", x=y, ignore.case=F)

## look for patterns
grep(pattern = "(pn)", x=y, ignore.case=F)
grep(pattern = "pn", x=y, ignore.case=F)
grep(pattern = "([pn][la])", x=y, ignore.case=F)


## look for special characters
## "\r", "\t", "\n"
## "("
## "["
## "\"
## "?", "*", ".", "+"
## "\<", "\>"
?regexp

y <- c("apple(1)", "orange[2]t", "banana\t")
grep(pattern = "(", x=y, ignore.case=F)
grep(pattern = "\(", x=y, ignore.case=F)
grep(pattern = "\\(", x=y, ignore.case=F)

grep(pattern = "\[", x=y, ignore.case=F)
grep(pattern = "\\[", x=y, ignore.case=F)
grep(pattern = "\\t", x=y, ignore.case=F)

grep(pattern = "[\\(\\[\t]", x=y, ignore.case=F)


## search strings starting with "a"
y <- c("apple", "orange", "banana\t")
grep(pattern = "a", x=y, ignore.case=F)
grep(pattern = "\\<t", x=y, ignore.case=F)
grep(pattern = "\\<[ab]", x=y, ignore.case=F)

## search strings ending with "e"
grep(pattern = "e\\>", x=y, ignore.case=F)
grep(pattern = "[ab]\\>", x=y, ignore.case=F)
grep(pattern = "t\\>", x=y, ignore.case=F)

## for comparison
y <- c("apple(1)", "orange[2]t", "banana\t")
grep(pattern = "\\<[ab]", x=y, ignore.case=F)
grep(pattern = "[ab]\\>", x=y, ignore.case=F)
grep(pattern = "t\\>", x=y, ignore.case=F)




## more complicated search
y <- c("apple(1)", "apple(15)", "orange((125),(467))", "banana(it is yellow)", "peach(a56)", "pear6", "apple(d)", "apple(abcd)")
## search a pattern
grep(pattern = "\\([0-9]\\)", x=y, ignore.case=F)
grep(pattern = "\\([0-9A-Za-z]\\)", x=y, ignore.case=F)
grep(pattern = "\\([a-z][0-9][0-9]\\)", x=y, ignore.case=F)

grep(pattern = "\\(*[0-9]\\)", x=y, ignore.case=F)
grep(pattern = "\\([0-9]*\\)", x=y, ignore.case=F)

grep(pattern = "\\([0-9a-z]*\\)", x=y, ignore.case=F) ## why not 4
grep(pattern = "\\([^0-9]*\\)", x=y, ignore.case=F)
grep(pattern = "\\([^0-9]\\)", x=y, ignore.case=F)
grep(pattern = "\\([ a-z]*\\)", x=y, ignore.case=F)  ## why not 5
grep(pattern = "\\([a-z]*\\)", x=y, ignore.case=F) ## why not 4 and 5

grep(pattern = "\\([0-9]*[ a-z]*\\)", x=y, ignore.case=F)
grep(pattern = "\\([a-z]+[0-9]+\\)", x=y, ignore.case=F)
grep(pattern = "\\(?[ a-z]\\)", x=y, ignore.case=F)



## search and replace
regexpr(pattern = "\\([ a-z]*[0-9]+\\)", y, ignore.case=F)
gsub(pattern = "(\\([ a-z]*[0-9]+)\\)", "", x=y, ignore.case=F)







###########################################################
## 5 search, extract or replace a part of a string
###########################################################

#install.packages("RCurl")
library(RCurl)
library(XML)
library(tools)

## check if a page exists
URL <- "http://db.kib.ac.cn/YNFLORA/VolumeBrowser.aspx?type=pid&id=Lauraceae"
omegahatExists <- url.exists(URL)

URL <- "http://db.kib.ac.cn/YNFLORA/VolumeBrowser.aspx?type=pid&id=Actinodaphne%20Nees"
omegahatExists <- url.exists(URL)


## download page: example 1
destfile <- "08 Strings analysis/Actinodaphne1.txt"
page.html <- scan(URL, what = "character", sep = "\n", quiet = TRUE, encoding = "Latin1")
writeLines(page.html, con = destfile)

destfile <- "08 Strings analysis/Actinodaphne2.txt"
tt <- download.file(url=URL, destfile=destfile, method="auto", quiet = T, 
		mode = "wb", cacheOK = TRUE)



## extract information from the page: example 1
X.txt <- readLines(destfile)
#Encoding(X.txt) <- "UTF-8"

X <- readHTMLTable(X.txt, head=F)
length(X)
class(X[[1]])
X <- as.matrix(X[[1]])
Encoding(colnames(X)) <- "UTF-8"
X

## write the data out
Encoding(X) <- "Latin1"
Encoding(colnames(X)) <- "Latin1"
write.csv(X, file="08 Strings analysis/test.txt")




## extract information from the page: example 2
URL <- "http://db.kib.ac.cn/YNFLORA/SearchResult.aspx?s=Actinodaphne%20confertiflora%20%20Meissn."
X.txt <- scan(URL, what = "character", sep = "\n", quiet = TRUE, encoding = "Latin1")
X.xml <- htmlTreeParse(X.txt)
attributes(X.xml)
attributes(X.xml$children)
attributes(X.xml$children$html$children)
#cat(X.txt, "08 Strings analysis/html.xml", quote="")
saveXML(X.xml$children$html, file="08 Strings analysis/html.xml")

X1 <- X.xml$children$html[["body"]][["form"]][[6]][[1]][[1]][[2]][[1]][[2]][["table"]][["tr"]][["td"]][["pre"]][["span"]][[1]]
X1 <- X1$value
Encoding(X1) <- "UTF-8"
X1
sp.text <- strsplit(X1, split="   ")[[1]]
Encoding(sp.text) <- "Latin1"
writeLines(sp.text, "08 Strings analysis/Actinodaphne confertiflora.txt")
Encoding(X1) <- "Latin1"
writeLines(X1, con="08 Strings analysis/Actinodaphne confertiflora_1.txt")








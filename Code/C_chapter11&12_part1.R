
install.packages("seqinr")
install.packages("BoSSA")
install.packages("XML")
install.packages("phytools")
install.packages("picante")
install.packages("bio3d")


library(ape)
library(seqinr)
library(BoSSA)
library(XML)
library(phytools)
library(picante)

library(vegan)

setwd("E:/My_Teaching/R_group/11&12 Phylogenetic analysis in R/")


########################################################################################
## 1 read phylogenetic trees
########################################################################################



## example 1
## newick format
text <- "(acer_alba,acer_sp);"

## read tree into the environment
tre1 <- read.tree(text = text)
class(tre1)

## plot the phylogenetic trees
plot(tre1)


## example 2
text <- "((a,b),c);"
tre2 <- read.tree(text=text)
plot(tre2)

text <- "((a:10,b:10)G1:30,c:70)F1;"
tre2 <- read.tree(text=text)
plot(tre2, show.node.label=T)


## example 3
text <- "((c:30,((d:8[dddd],e:8):7,f:15)G2:15)F1:20,(a:10,b:10,g:10)Acer:40)O1:50;"
tre3 <- read.tree(text=text)
windows()
plot(tre3, show.tip.label=T, show.node.label=T)
axisPhylo()

is.ultrametric(tre3)


## exatract the information of the phylogenetic trees
## ancestors and descendants
tre3$edge
## names of the tips
tre3$tip.label
## names of the label
tre3$node.label
## branch lengths
tre3$edge.length
## extract the branch lengths of the tips
pos <- which(tre3$edge[,2] <= 7)
sp.age <- tre3$edge.length[pos]
sp.age
names(sp.age) <- tre3$tip.label[tre3$edge[pos,2]]


## example 4: read the nexus tree
tre4 <- read.nexus(file="trees/example_tre3.nexus")
class(tre4)
plot(tre4)





########################################################################################
## 2 edit the tree
########################################################################################

## example 1: edit the tree
text <- "((c:30,((d:8[dddd],e:8):7,f:15)G2:15)F1:20,(a:10,b:10,g:10)Acer:40)O1:50;"
tre3 <- read.tree(text=text)
plot(tre3, show.tip.label=T, show.node.label=T)
axisPhylo()

tre4 <- drop.tip(phy=tre3, tip=c("b", "g", "c","d"))
windows()
plot(tre4)
axisPhylo()


## example 2
trans <- data.frame(tip.label=tre4$tip.label, 
	clade.label = c("g1", "g2", "g3"), 
	N = c(12,2,3), 
	depth = c(10, 3, 3))
tre4.backbone <- phylo.toBackbone(tre4, trans=trans)
plot(tre4.backbone)






########################################################################################
## 3 read and write sequence data
########################################################################################

## example 1: read and write fasta files using seqinr package
seq.data <- seqinr::read.fasta(file = "sequencedata/Quercus mongolica.fasta", 
       seqtype = "DNA", as.string = T, forceDNAtolower = TRUE,
       set.attributes = TRUE)
length(seq.data)
class(seq.data)
names(seq.data)
attributes(seq.data[[1]])$Annot
attr(seq.data[[1]], which = "Annot")

seq.data[[1]]

write.fasta(sequences = seq.data[[1]], names = "Quercus mongolica matK test", 
	file.out = "sequencedata/Quercus mongolica part1.fasta", open = "w", nbchar = 100, as.string = FALSE)


## example 2: read and write fasta files using ape package
seq.data <- read.FASTA(file = "sequencedata/Quercus mongolica.fasta", type="DNA")
names(seq.data)

seq.data <- ape::read.dna(file = "sequencedata/Quercus mongolica.fasta", format = "fasta", as.character = T)
names(seq.data)
write.dna(seq.data, file="sequencedata/Quercus mongolica dna.fasta", format = "fasta", nbcol = 1, colsep = " ", colw = 60, append = FALSE)


cat("3 40",
     "No305     NTTCGAAAAACACACCCACTACTAAAANTTATCAGTCACT",
     "No304     ATTCGAAAAACACACCCACTACTAAAAATTATCAACCACT",
     "No306     ATTCGAAAAACACACCCACTACTAAAAATTATCAATCACT",
     file = "sequencedata/exdna.txt", sep = "\n")

seq.data <- read.dna(file = "sequencedata/exdna.txt", format = "sequential", as.character = T)
names(seq.data)
write.dna(seq.data, file="sequencedata/exdna.fasta", format = "fasta", append = FALSE)


## example 3: read and write nexus files
seq.data <- read.nexus.data(file = "sequencedata/Quercus mongolica.nexus")
class(seq.data)
names(seq.data)
write.fasta(seq.data, file="sequencedata/Quercus mongolica_test3.fasta", names = names(seq.data))


#write.nexus.data


########################################################################################
## 4 download sequence data from GenBank
## https://www.ncbi.nlm.nih.gov/genbank/
########################################################################################

## example 1: directly download manually

## example 2: download sequence data from GenBank
## read.GenBank

seq1 <- read.GenBank(access.nb = "AB727877.1", as.character = T)
seq2 <- read.GenBank(access.nb = c("AB727877.1", "AB107629.1"), as.character = T)

## https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id=AB727877.1,AB107629.1&rettype=gb&retmode=text

#source("e:/r_cal/r_programs/myfunctions.R")
## example 3: download sequence data from GenBank
#source("c_functions.R")
#seq3 <- read.GenBank.new(access.nb = "AB727877.1", from = 15, to = 150)
#seq4 <- read.GenBank.new(access.nb = c("AB727877.1", "AB107629.1"), from = c(15,20), to = c(200, 500))



## example 4: download sequence data from GenBank using blast
## very useful
## very very slow
library(bio3d)
seq.data <- seqinr::read.fasta(file = "sequencedata/Quercus mongolica.fasta", 
       seqtype = "DNA", as.string = T, forceDNAtolower = TRUE,
       set.attributes = TRUE)

seq5 <- blast.pdb(as.character(seq.data[[1]]))





########################################################################################
## 5 sequence alignment and build a ML tree
########################################################################################

## example 1
## alignment
cmdstr <- paste("C:/mafft/mafft.bat ", 
		"--thread 2 --localpair --maxiterate 100 --adjustdirectionaccurately ", 
		"E:/My_Teaching/R_group/Rhododendron_matK_reduced.fasta", " > ", 
		"E:/My_Teaching/R_group/Rhododendron_matK_reduced_aligned.fasta", sep="")

date(); system(cmdstr); print(date())


## example 2
## build a ML tree
seq.align <- read.dna("E:/My_Teaching/R_group/Rhododendron_matK_reduced_aligned.fasta", format="fasta")
a <- 	attributes(seq.align)$dimnames[[1]]
a <- gsub(">", "", a)
a <- strsplit(a, split="\\|")
a <- unlist(lapply(a, "[", 1))
a <- gsub(" ", "_", a)
attributes(seq.align)$dimnames[[1]] <- a
write.dna(seq.align, file = "E:/My_Teaching/R_group/Rhododendron_matK_reduced_aligned.phylip", format = "sequential", colw = 1659)

## build a ML tree using RAxML
cmdstr <- paste("E:/My_UsefulSoft/raxmlHPC-PTHREADS.exe ", 
		"-T 4 -f a -p 12345 -m GTRGAMMA -k -x 12345 -N 10 -o ", 
		"Corema_album", 
		" -s ", "E:/My_Teaching/R_group/Rhododendron_matK_reduced_aligned.phylip", 
		" -n matK", sep="")
date(); system(cmdstr); date()


tre.rhod <- read.tree("RAxML_bestTree.matK")

plot(tre.rhod)
axisPhylo()



########################################################################################
## 6 find the ancestor of a species
########################################################################################

## example 1: find the root
text <- "((c:30,((d:8[dddd],e:8):7,f:15)G2:15)F1:20,(a:10,b:10,g:10)Acer:40)O1:10;"
tre3 <- read.tree(text=text)
plot(tre3, show.tip.label=T, show.node.label=T, root.edge = T)
axisPhylo()

## the root is the only node without father node
pos <- which(!tre3$edge[,1] %in% tre3$edge[,2])
tre3$edge[pos,1]
root.node <- unique(tre3$edge[pos,1])
root.node.label <- c(tre3$tip.label,tre3$node.label)[root.node]
root.node.label <- tre3$node.label[root.node - Ntip(tre3)]


## example 2: find the ancestor of a species
child <- which(tre3$tip.label == "e")

ancestor.edge <- which(tre3$edge[,2] == child)
ancestor.node <- tre3$edge[ancestor.edge,1]

## write a loop
ancestors <- numeric()
child <- tip <- which(tre3$tip.label == "e")
ancestor.node <- -1
while (ancestor.node != root.node) {
	ancestor.edge <- which(tre3$edge[,2] == child)
	ancestor.node <- tre3$edge[ancestor.edge,1]
	ancestors <- c(ancestors, ancestor.node)
	child <- ancestor.node
	}
tre3$node.label[ancestors - Ntip(tre3)]


## example 3: extract the branch lengths of the ancestors of  a species
## i.e. the root distance of a species
pos.edge <- which(tre3$edge[,2] == ancestors[1])
tre3$edge.length[pos.edge]
pos.edge <- which(tre3$edge[,2] == ancestors[2])
tre3$edge.length[pos.edge]
pos.edge <- which(tre3$edge[,2] == ancestors[3])
tre3$edge.length[pos.edge]
pos.edge <- which(tre3$edge[,2] == ancestors[4])
tre3$edge.length[pos.edge]

## write a loop
tip.ancestors <- c(tip, ancestors[-4])
branch.len <- numeric()
for(i in 1:length(tip.ancestors)) {
	pos.edge <- which(tre3$edge[,2] == tip.ancestors[i])
	branch.len[i] <- tre3$edge.length[pos.edge]
	}
sum(branch.len)

	

## example 4: most recent common ancestor of two species
ancestors <- numeric()
child <- tip <- which(tre3$tip.label == "e")
ancestor.node <- -1
while (ancestor.node != root.node) {
	ancestor.edge <- which(tre3$edge[,2] == child)
	ancestor.node <- tre3$edge[ancestor.edge,1]
	ancestors <- c(ancestors, ancestor.node)
	child <- ancestor.node
	}
ancestors.e <- c(tip, ancestors)

ancestors <- numeric()
child <- tip <- which(tre3$tip.label == "f")
ancestor.node <- -1
while (ancestor.node != root.node) {
	ancestor.edge <- which(tre3$edge[,2] == child)
	ancestor.node <- tre3$edge[ancestor.edge,1]
	ancestors <- c(ancestors, ancestor.node)
	child <- ancestor.node
	}
ancestors.f <- c(tip, ancestors)

ancestors.e
ancestors.f

## example 4: most recent common ancestor of two species
## use an ape function
mr.ancestor <- getMRCA(tre3, tip=c("f", "e"))
root.node.label <- tre3$node.label[mr.ancestor - Ntip(tre3)]

mr.ancestor <- getMRCA(tre3, tip=c("c", "e"))
root.node.label <- tre3$node.label[mr.ancestor - Ntip(tre3)]

mr.ancestor <- getMRCA(tre3, tip=c("c", "f", "e"))
root.node.label <- tre3$node.label[mr.ancestor - Ntip(tre3)]


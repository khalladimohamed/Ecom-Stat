##### Evaluation 02 : Les classifications : CAH et HCPC #####
library(FactoMineR)
library(factoextra)
library(cluster)

remplaceNAparMOY<-function(x) 
{ 
  return ( ifelse(is.na(x), mean(x,na.rm = TRUE), x)  ) 
} 

setwd("C:\\Users\\amine\\OneDrive\\Bureau\\EcomStat\\Labo\\Evaluation02\\datasets")




#Histoire d'eaux (minérales)
dataEaux <- read.table("Eaux2010.txt", sep="\t", header=TRUE, row.names=7)

summary(dataEaux)

dataEauxCAH <- dataEaux[, c("HCO3",	"SO4",	"Cl",	"Ca",	"Mg",	"Na")]

dataEauxCAH <- apply(dataEauxCAH, 2, remplaceNAparMOY)
dataEauxCAH

#CAH
daisy(dataEauxCAH)

classificationAverage <- agnes(scale(dataEauxCAH), method = "average")
par(mfrow = c(2,1))
plot(classificationAverage)

classificationAverage.h <- as.hclust(classificationAverage)
plot(rev(classificationAverage.h$height), type="h", ylab="hauteurs")

classificationWard <- agnes(scale(dataEauxCAH), method = "ward")
plot(classificationWard)
classificationWard.h <- as.hclust(classificationWard)
plot(rev(classificationWard.h$height), type="h", ylab="hauteurs")

classificationSingle <- agnes(scale(dataEauxCAH), method = "single")
plot(classificationSingle)
classificationSingle.h <- as.hclust(classificationSingle)
plot(rev(classificationSingle.h$height), type="h", ylab="hauteurs")

classificationComplete <- agnes(scale(dataEauxCAH), method = "complete")
plot(classificationComplete)
classificationComplete.h <- as.hclust(classificationComplete)
plot(rev(classificationComplete.h$height), type="h", ylab="hauteurs")

#rest a faire : la decoupe et order

#HCPC
classification.acp <- PCA(dataEauxCAH)

classification.hcpc <- HCPC(classification.acp, consol = F)

classification.hcpc$data.clust

classification.hcpc$desc.var

classification.hcpc$desc.ind

classification.hcpc$call$t$inert.gain

barplot(classification.hcpc$call$t$inert.gain)

classification.acp$eig




#Les vins italiens
dataVin <- read.table(file = "winedata.csv", header=TRUE, sep=";", row.names=1)

summary(dataVin)

#CAH
daisy(dataVin)

classification <- agnes(scale(dataVin), method = "ward")
plot(classification)
classification.h <- as.hclust(classification)
plot(rev(classification.h$height), type="h", ylab="hauteurs")

#rest a faire : la decoupe et order

#HCPC
classification.acp <- PCA(dataVin)

classification.hcpc <- HCPC(classification.acp, consol = F)

classification.hcpc$data.clust

classification.hcpc$desc.var

classification.hcpc$desc.ind

classification.hcpc$call$t$inert.gain

barplot(classification.hcpc$call$t$inert.gain)

classification.acp$eig
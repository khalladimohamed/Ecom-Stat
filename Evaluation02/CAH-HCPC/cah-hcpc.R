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
classificationAverage <- agnes(scale(dataEauxCAH), method = "average")
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

#Decoupage en 3 parties
classes <- cutree(classificationWard, k = 3)

nomEaux <- rownames(dataEauxCAH)

results3 <- cbind(nomEaux, classes)

results3<-results3[order(results3[,2]), ]
results3

#HCPC
classification.acp <- PCA(dataEauxCAH)

classification.hcpc <- HCPC(classification.acp, consol = F)

classification.hcpc$desc.var$quanti.var

classification.hcpc$desc.ind$para




#Les vins italiens
dataVin <- read.table(file = "winedata.csv", header=TRUE, sep=";", row.names=1)

summary(dataVin)

#CAH
classification <- agnes(scale(dataVin), method = "ward")
plot(classification)
classification.h <- as.hclust(classification)
plot(rev(classification.h$height), type="h", ylab="hauteurs")

#Decoupage en 3 parties
classes <- cutree(classification, k = 3)

idVins <- rownames(dataVin)

results3 <- cbind(idVins, classes)

results3<-results3[order(results3[,2]), ]
results3

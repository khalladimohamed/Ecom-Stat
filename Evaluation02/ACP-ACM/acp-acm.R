##### Evaluation 02 : ACP et ACM #####
library(FactoMineR)

setwd("C:\\Users\\amine\\OneDrive\\Bureau\\EcomStat\\Labo\\Evaluation02\\datasets")

#Eaux minérales
donnees <- read.table("Eaux1.txt", sep="\t", header=TRUE, row.names=7)

resultat_acp <- PCA(donnees)

resultat_acp$eig

plot(resultat_acp, choix="ind")

plot(resultat_acp, choix="var")

summary(resultat_acp)


#Etude de maïs


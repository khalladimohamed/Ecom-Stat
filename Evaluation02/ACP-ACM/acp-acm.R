##### Evaluation 02 : ACP et ACM #####
library(FactoMineR)
library(factoextra)

remplaceNAparMOY<-function(x) 
{ 
  return ( ifelse(is.na(x), mean(x,na.rm = TRUE), x)  ) 
} 

setwd("C:\\Users\\amine\\OneDrive\\Bureau\\EcomStat\\Labo\\Evaluation02\\datasets")




#Eaux minérales
donnees <- read.table("Eaux1.txt", sep="\t", header=TRUE, row.names=7)

summary(donnees)

resultat_acp <- PCA(donnees, graph = FALSE)

resultat_acp$eig

plot(resultat_acp, choix="ind")

plot(resultat_acp, choix="var")

dimdesc(resultat_acp)




#Etude de maïs

#ACP
dataMais <- read.table(file = "etude-agro-mais.csv", header=TRUE, sep=";", row.names=1)

summary(dataMais)

dataMais$Censure.droite <- as.factor(dataMais$Censure.droite)

dataMais <- dataMais[which(sapply(dataMais, is.numeric))]

summary(dataMais)

dataMais <- apply(dataMais, 2, remplaceNAparMOY)
dataMais

resultat_acp_mais <- PCA(dataMais, graph = FALSE)

resultat_acp_mais$eig

resultat_acp_mais$ind$cos2

resultat_acp_mais$var$cos2

resultat_acp_mais$var$contrib

plot(resultat_acp_mais, choix="ind")

plot(resultat_acp_mais, choix="var")

dimdesc(resultat_acp_mais)


#ACM
dataMais <- read.table(file = "etude-agro-mais.csv", header=TRUE, sep=";", row.names=1)

summary(dataMais)

dataMaisACM <- dataMais[, c("Couleur", "Germination.epi", "Enracinement", "Verse", "Attaque", "Parcelle", "Verse.Traitement")]

summary(dataMaisACM)

dataMaisACM <- na.omit(dataMaisACM)

resultat_acm <- MCA(dataMaisACM, graph = FALSE)

resultat_acm$eig

resultat_acm$var$cos2[,1:2]

plot(resultat_acm, choix="var")

fviz_mca_ind(resultat_acm)

fviz_mca_var(resultat_acm)




#Le retour du Titanic (ACM)
dataTitanic <- read.table(file = "titanic.csv", header=TRUE, sep=";", row.names=1)

summary(dataTitanic)

dataTitanic$CLASS <- factor(dataTitanic$CLASS, levels = c(0, 1, 2, 3), labels = c("Equipage", "Premiere Classe", "Seconde Classe", "Troisieme Classe"))
dataTitanic$AGE <- factor(dataTitanic$AGE, levels = c(0, 1), labels = c("Enfant", "Adulte"))
dataTitanic$SEX <- factor(dataTitanic$SEX, levels = c(0, 1), labels = c("Femme", "Homme"))
dataTitanic$SURV <- factor(dataTitanic$SURV, levels = c(0, 1), labels = c("Non", "Oui"))

summary(dataTitanic)

resultat_acm <- MCA(dataTitanic, graph = FALSE)

resultat_acm$eig

resultat_acm$var$cos2[,1:2]

plot(resultat_acm, choix="var")

fviz_mca_ind(resultat_acm)

fviz_mca_var(resultat_acm)




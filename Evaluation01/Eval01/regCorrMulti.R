######### Evaluation 01 : Regression et correlation multiple #########

setwd("C:\\Users\\amine\\OneDrive\\Bureau\\EcomStat\\Labo\\datasets")

accidents <- read.table("accidents2.csv", h=TRUE, sep=";", dec=",", row.names=1)
accidents
summary(accidents)

plot(accidents$sigs1, accidents$rate, xlab = "Nombre de signaux routiers par mile", ylab = "Taux d'accidents par million de véhicules", main = "Relation entre le taux d'accidents et le nombre de signaux routiers par mile")

plot(accidents$shld, accidents$rate, xlab = "Largeur de la bande d'urgence latérale (pieds)", ylab = "Taux d'accidents par million de véhicules", main = "Relation entre le taux d'accidents et la largeur de la bande d'urgence latérale")

modele1 <- lm(rate ~ sigs1 + shld, data = accidents)
modele1

summary(modele1)
sqrt(0.4329)

residus.studentises <- rstudent(modele1)

plot(residus.studentises, ylim = c(-3, 3), xlab = "Observations", ylab = "Résidus standardisés", main = "Graphique des résidus standardisés")

abline(h = c(-2, 0, 2), lty = c(2, 1, 2))



plot(accidents$acpt, accidents$rate, xlab = "Nombre d'entrées par mile", ylab = "Taux d'accidents par million de véhicules", main = "Relation entre le taux d'accidents et le nombre d'entrées par mile")

modele2 <- lm(rate ~ sigs1 + shld + acpt, data = accidents)
modele2

summary(modele2)
sqrt(0.609)

residus.studentises <- rstudent(modele2)

plot(residus.studentises, ylim = c(-3, 3), xlab = "Observations", ylab = "Résidus standardisés", main = "Graphique des résidus standardisés")

abline(h = c(-2, 0, 2), lty = c(2, 1, 2))

cooks.distance(modele2)

plot(cooks.distance(modele2))

step(modele2)

##### Evaluation 02 : ANOVA 2 #####

setwd("C:\\Users\\amine\\OneDrive\\Bureau\\EcomStat\\Labo\\Evaluation02\\datasets")

#Bière et petits maux
dataBiere <- read.csv("bieres_petits_maux.csv", h=TRUE, sep=";", fileEncoding="latin1")
dataBiere
dataBiere$maux <- as.factor(dataBiere$maux)
dataBiere$biere <- as.factor(dataBiere$biere)
summary(dataBiere)


with(dataBiere, interaction.plot(maux, biere, CBio007.69))
with(dataBiere, interaction.plot(biere, maux, CBio007.69))


modele_croise = lm(CBio007.69 ~ maux * biere, data = dataBiere)
modele_croise
anova(modele_croise)
summary(modele_croise)


modele_hierarchise = lm(CBio007.69 ~ maux + biere, data = dataBiere)
modele_hierarchise
anova(modele_hierarchise)
summary(modele_hierarchise)




#Les médicaments contre la GCE
dataMedicament <- data.frame(
  Amelioration = c(10, 12, 8, 10, 6, 13, 9, 10, 9, 8, 11, 18, 12, 15, 13, 8, 15, 16, 9, 13, 
                  7, 14, 10, 11, 9, 10, 11, 7, 9, 9, 8, 9, 10, 9, 11, 13, 7, 14, 15, 12,
                  12, 9, 11, 27, 7, 8, 13, 14, 10, 11, 7, 6, 10, 7, 7, 5, 6, 7, 9, 6),
  Molecule = rep(c("AlphaVictoire", "BetaTriomphe", "GammaSucces"), each = 20),
  Administration = rep(c("Oral", "Injection"), each = 10, times = 3)
)
dataMedicament
dataMedicament$Molecule <- as.factor(dataMedicament$Molecule)
dataMedicament$Administration <- as.factor(dataMedicament$Administration)
summary(dataMedicament)


with(dataMedicament, interaction.plot(Molecule, Administration, Amelioration))
with(dataMedicament, interaction.plot(Administration, Molecule, Amelioration))


modele_croise <- lm(Amelioration ~ Molecule * Administration, data = dataMedicament)
modele_croise
anova(modele_croise)
summary(modele_croise)


modele_hierarchise <- lm(Amelioration ~ Molecule + Administration, data = dataMedicament)
modele_hierarchise
anova(modele_hierarchise)
summary(modele_hierarchise)





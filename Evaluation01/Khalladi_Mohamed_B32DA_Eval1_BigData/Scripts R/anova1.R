######### Evaluation 01 : ANOVA 1 #########

setwd("C:\\Users\\amine\\OneDrive\\Bureau\\EcomStat\\Labo\\datasets")

civilisation <- read.table("civilisation.csv", header=TRUE, sep=";", dec=".")

summary(civilisation)

boxplot(civilisation$nbrRecipient~civilisation$civilisation)

model<-lm(civilisation$nbrRecipient~civilisation$civilisation)
model

anova(model)

summary(model)

oneway.test(civilisation$nbrRecipient~civilisation$civilisation, var.equal=FALSE)

pairwise.t.test(civilisation$nbrRecipient, civilisation$civilisation, p.adjust.method ="none", pool.sd=TRUE)

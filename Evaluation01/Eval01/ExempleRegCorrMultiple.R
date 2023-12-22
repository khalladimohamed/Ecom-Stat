setwd("C:\Users\amine\OneDrive\Bureau\Ecom - BigData\Labo\Eval01")
getwd()

demenagements<-read.table("demenagement.csv", header = T, dec = ".", sep = ";")
demenagements
summary(demenagements)

attach()
plot(volume, temps)
plot(nombre.de.grandes.pieces, temps)

modele<-lm(temps~volume+nombre.de.grandes.pieces)
modele
var<-summary(modele)
sqrt(0.9287)
sqrt(var$adj.r.squared)

cor.test(temps, volume+nombre.de.grandes.piece)
modelesimple<-lm(temps~volume)
modelesimple
summary(modelesimple)

#il faut un dataframe pour predicte

residus.studentises<-rstudent(modele)
plot(residus, studentises, ylim=c(-3, 3))
abline(h=c(-2,0,2), lty=c(2,1,2))

vol.pred<-1500
ngp.pred<-15
resultat150015<-dataframe(vol.pred,ngp.pred)
resultat150015

colnames(resultat150015)<-c("volume", "nombre.de.grandes.pieces")
resultat150015
predict(modele, resultat150015)
predict(modele, resultat150015, interval = "pred")
install.packages("DMwR")
library("DMwR")
head(algae)
summary(algae)

###Traiter les NA

#Identifier les lignes
algae[!complete.cases(algae),]
algae_na=na.omit(algae)
nrow(algae_na)
nrow(algae)

#Remplacer par la médiane de la variable si quantitative ou la valeur la plus fréquente si qualitative
algae2=centralImputation(algae)
nrow(algae[!complete.cases(algae),])
nrow(algae2[!complete.cases(algae2),])

#Remplacer les NA par la médiane des k valeurs les plus proches selon les autres variables
algae3 = knnImputation(algae, k =10, meth = "median")
nrow(algae[!complete.cases(algae),])
nrow(algae3[!complete.cases(algae3),])

###Exercice

#4.6.1
algae = knnImputation(algae, k =10, meth = "median")
lm.a1 <- lm(a1 ~ ., data = algae[, 1:12])
summary(lm.a1)
#a une variable indicatrice est créer pour chaque catégorie moins une etc..
#R2 ou R2-ajusté

#b season, nh4 et chla
anova(lm.a1)

#c  "size" "mxPH"   "mnO2" "Cl" "NO3" "NH4" "PO4"
final.lm=step(lm.a1)
summary(final.lm)

#4.6.2

#modele arbre de decision
algae = knnImputation(algae, k =10, meth = "median")
library(rpart)
rt.a1 = rpart(a1 ~ ., data = algae[, 1:12])
rt.a1

#afficher l'arbre de decision
par(lwd=2, col="red")
plot(rt.a1, compress=TRUE)
text(rt.a1, use.n=TRUE,col="blue")

#evaluer la qualite de la prevision
lm.predictions.a1 = predict(final.lm, algae)
rt.predictions.a1 = predict(rt.a1, algae)
regr.eval(algae[, "a1"], rt.predictions.a1, train.y = algae[,"a1"]) #rt meilleur prediction
regr.eval(algae[, "a1"], lm.predictions.a1, train.y = algae[,"a1"])

par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
plot(lm.predictions.a1, algae[, "a1"], main = "Linear Model",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a1, algae[, "a1"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)

#e
summary(test.algae)
test.algae=knnImputation(test.algae, k =10, meth = "median") #voir explication du td
lm.predictions.a1 = predict(final.lm, test.algae)
rt.predictions.a1 = predict(rt.a1, test.algae)

#f
regr.eval(algae.sols[, "a1"], rt.predictions.a1, train.y = algae[,"a1"])
regr.eval(algae.sols[, "a1"], lm.predictions.a1, train.y = algae[,"a1"])

par(mfrow = c(1, 2),col="navy")
plot(lm.predictions.a1, algae.sols[, "a1"], main = "Linear Model",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a1, algae.sols[, "a1"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
#### Elaboration modele de score de souscription a un produit epargne

rm(list=ls())
graphics.off()
library(readxl)

getwd()
base = read_excel("bank_data.xltx",sheet="bank_data")
base2 =  read_excel("bank_eval_etud.xltx",sheet="bank_eval_etud")


colnames(base)<-c("age","emploi","statut_marital", "etude","defaut","pret_immobilier","pret","contact","mois","jour_semaine","temps_appel","campagne_pub","jours_depuis_derniere_pub","nombre_appel_avant_campagne","etat_precedente_campagne","variation_taux_emploi","ipc","icc","euribor3m","nombre_employes","y")
colnames(base2)<-c("age","emploi","statut_marital", "etude","defaut","pret_immobilier","pret","contact","mois","jour_semaine","temps_appel","campagne_pub","jours_depuis_derniere_pub","nombre_appel_avant_campagne","etat_precedente_campagne","variation_taux_emploi","ipc","icc","euribor3m","nombre_employes")
base$y[(base$y)=="yes"]<-1
base$y[(base$y)=="no"]<-0

str(base2)
str(base)

base$emploi <- factor(base$emploi)
base$statut_marital <- factor(base$statut_marital)
base$etude <- factor(base$etude)
base$defaut <- factor(base$defaut)
base$pret_immobilier <- factor(base$pret_immobilier)
base$pret <- factor(base$pret)
base$contact <- factor(base$contact)
base$mois <- factor(base$mois)
base$jour_semaine <- factor(base$jour_semaine)
base$etat_precedente_campagne <- factor(base$etat_precedente_campagne)
base$y <- factor(base$y)

base2$emploi <- factor(base2$emploi)
base2$statut_marital <- factor(base2$statut_marital)
base2$etude <- factor(base2$etude)
base2$defaut <- factor(base2$defaut)
base2$pret_immobilier <- factor(base2$pret_immobilier)
base2$pret <- factor(base2$pret)
base2$contact <- factor(base2$contact)
base2$mois <- factor(base2$mois)
base2$jour_semaine <- factor(base2$jour_semaine)
base2$etat_precedente_campagne <- factor(base2$etat_precedente_campagne)

str(base2)
str(base)
library(funModeling)


summary(base)

library(gmodels)
library(ggplot2)
library(plotly)
#######################################################################################

############################## STATISTIQUES DESCRIPTIVES   ###########################

#######################################################################################


#Statistiques descriptives variables continues
g<-ggplot(base, aes(x=y, y=age, fill=y))+geom_boxplot()
g
ggplotly(g)

g<-ggplot(base, aes(x=y, y=temps_appel, fill=y))+geom_boxplot()
g
ggplotly(g)

g<-ggplot(base, aes(x=y, y=campagne_pub, fill=y))+geom_boxplot()
g
ggplotly(g)

g<-ggplot(base, aes(x=y, y=jours_depuis_derniere_pub, fill=y))+geom_boxplot()
g
ggplotly(g)

g<-ggplot(base, aes(x=y, y=nombre_appel_avant_campagne, fill=y))+geom_boxplot()
g
ggplotly(g)

g<-ggplot(base, aes(x=y, y=variation_taux_emploi, fill=y))+geom_boxplot()
g
ggplotly(g)

g<-ggplot(base, aes(x=y, y=ipc, fill=y))+geom_boxplot()
g
ggplotly(g)


g<-ggplot(base, aes(x=y, y=icc, fill=y))+geom_boxplot()
g
ggplotly(g)


g<-ggplot(base, aes(x=y, y=nombre_employes, fill=y))+geom_boxplot()
g
ggplotly(g)

#Statistiques descriptives variables categorielles

g<-ggplot(base, aes(y, fill=contact))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=emploi))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=statut_marital))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=etude))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=defaut))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=pret_immobilier))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=pret))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=mois))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=jour_semaine))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=etat_precedente_campagne))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=euribor3m))+geom_bar()
g
ggplotly(g)

g<-ggplot(base, aes(y, fill=y))+geom_bar()
g
ggplotly(g)

#######################################################################################

############################## DISCRETISATION   ######################################

#######################################################################################

base_discrete <- base
base_none <- base2

hist(base$nombre_appel_avant_campagne)
hist(base$variation_taux_emploi)


base_discrete$age <- cut(base_discrete$age,c(0,33,39,48,Inf),right=TRUE)
base_discrete$temps_appel <- cut(base_discrete$temps_appel,c(0,104,180,320,Inf),right=TRUE)
base_discrete$campagne_pub <- cut(base_discrete$campagne_pub,c(0,2,4,Inf),right=TRUE)
base_discrete$jours_depuis_derniere_pub <- cut(base_discrete$jours_depuis_derniere_pub,c(0,100,Inf),right=TRUE)
base_discrete$nombre_appel_avant_campagne <- cut(base_discrete$nombre_appel_avant_campagne,c(-Inf,1,Inf),right=TRUE)
base_discrete$variation_taux_emploi <- cut(base_discrete$variation_taux_emploi,c(-Inf,0.5,Inf),right=TRUE)
base_discrete$ipc <- cut(base_discrete$ipc,c(-Inf,93.20,93.80,94.03,Inf),right=TRUE)
base_discrete$icc <- cut(base_discrete$icc,c(-Inf,-42,-40.08,-36.1,Inf),right=TRUE)
base_discrete$euribor3m <- cut(base_discrete$euribor3m,c(-Inf,1.344,4.858,4.962,Inf),right=TRUE)
base_discrete$nombre_employes <- cut(base_discrete$nombre_employes,c(0,5176,5196,Inf),right=TRUE)


base_none$age <- cut(base_none$age,c(0,33,39,48,Inf),right=TRUE)
base_none$temps_appel <- cut(base_none$temps_appel,c(0,104,180,320,Inf),right=TRUE)
base_none$campagne_pub <- cut(base_none$campagne_pub,c(0,2,4,Inf),right=TRUE)
base_none$jours_depuis_derniere_pub <- cut(base_none$jours_depuis_derniere_pub,c(0,100,Inf),right=TRUE)
base_none$nombre_appel_avant_campagne <- cut(base_none$nombre_appel_avant_campagne,c(-Inf,1,Inf),right=TRUE)
base_none$variation_taux_emploi <- cut(base_none$variation_taux_emploi,c(-Inf,0.5,Inf),right=TRUE)
base_none$ipc <- cut(base_none$ipc,c(-Inf,93.20,93.80,94.03,Inf),right=TRUE)
base_none$icc <- cut(base_none$icc,c(-Inf,-42,-40.08,-36.1,Inf),right=TRUE)
base_none$euribor3m <- cut(base_none$euribor3m,c(-Inf,1.344,4.858,4.962,Inf),right=TRUE)
base_none$nombre_employes <- cut(base_none$nombre_employes,c(0,5176,5196,Inf),right=TRUE)




summary(base_none)
summary(base_discrete)

hist(base$nombre_appel_avant_campagne)
hist(base$variation_taux_emploi)

#######################################################################################

##############################     ACM   ##############################################

#######################################################################################

library(FactoMineR)

(modal = apply(base_discrete[,-21], 2, function(x) nlevels(as.factor(x))))
sum(modal) - ncol(base_discrete[,-21])
ACM <- MCA (base_discrete, ncp = 50, axes = c(1,2), graph = TRUE, quali.sup = 21)

barplot(ACM$eig[,2], names = paste("Dim",1:nrow(ACM$eig)))



#######################################################################################

##############################     STRATIFICATION   ###################################

#######################################################################################

summary(base)
library(sampling)
set.seed(123)
id <- strata(base_discrete, stratanames="y", size=c(sum(base_discrete$y==0)*2/3,sum(base_discrete$y==1)*2/3), method="srswor", description=T)$ID_unit
train  <- base_discrete[id,]
valid  <- base_discrete[-id,]
valid2 <- base_discrete[-id,]


summary(train$y)
summary(valid$y)

table(train$y)/nrow(train)*100 #nombre de 0 et 1 equilbre dans les 2 echantillons
table(valid$y)/nrow(valid)*100

#######################################################################################

##############################     VALEUR INFORMATION   ###############################

#######################################################################################

# Liaison des variables explicatives avec la variable a expliquer 

# calcul de la valeur d'information d'une variable (VI)
IV <- function(X,Y){
  tab <- table(X,Y)
  IV <- 100*sum(((tab[,1]/sum(tab[,1])) - (tab[,2]/sum(tab[,2]))) * log((tab[,1]/sum(tab[,1])) /(tab[,2]/sum(tab[,2]))))
  return(IV)
}
IV(base_discrete$temps_appel,base_discrete$y)


#######################################################################################

############################## V DE CRAMER ###########################################

#######################################################################################

dim(base$y)
base_discrete = as.data.frame(base_discrete)
base_none <- base_discrete[,c(-21)]
base_none = as.data.frame(base_none)

# V CRAMER : correlation entre les variables et la variables à expliquer 

# Calcul du V de Cramer
cramer  <- matrix(NA,ncol(base_none),4)
effectif <- dim(base_none)[1]
for (i in (1:ncol(base_none)))
    { cramer[i,1] <- names(base_none[i])
      cramer[i,2] <- sqrt(chisq.test(table(base_none[,i],base_discrete$y))$statistic/effectif)
      cramer[i,3] <- chisq.test(table(base_none[,i],base_discrete$y))$p.value  
      cramer[i,4] <- IV(base_none[,i],base_discrete$y)  
    }
colnames(cramer) <- c("variable", "V de Cramer", "p-value chi2","Valeur d'Information")

# Affichage des variables par VI décroissantes
vcramer <- cramer [order(cramer[,2], decreasing=T),]
vcramer
# Graphique des V de Cramer
old <- par(no.readonly = TRUE)
par(mar = c(8, 4, 4, 0))
barplot(as.numeric(vcramer[-1,2]),col=gray(0:nrow(vcramer)/nrow(vcramer)),
        names.arg=vcramer[-1,1], ylab='V de Cramer', ylim=c(0,0.35), cex.names = 0.8, las=3)
par(old)


# V de Cramer des paires de variables explicatives

library(questionr)
cramer  <- matrix(NA,ncol(base_discrete),ncol(base_discrete))
# Variante 1
for (i in (1:ncol(base_discrete)))
{     for (j in (1:ncol(base_discrete)))
{
  cramer[i,j] <- cramer.v(table(base_discrete[,i],base_discrete[,j]))
}
}

# Fin variantes
colnames(cramer) <- colnames(base_discrete)
rownames(cramer) <- colnames(base_discrete)
cramer
library(corrplot)
corrplot(cramer)
corrplot(cramer, method="shade", shade.col=NA, tl.col="black", tl.srt=45)
old <- par(no.readonly = TRUE)
par(omi=c(0.4,0.4,0.4,0.4))
corrplot(cramer, type="upper", tl.srt=60, tl.col="black", tl.cex=0.5, diag=F, addCoef.col="black", addCoefasPercent=T)
par(old)




# Nombre d'employes avec le taux d'euribor 3 mois 

exemple <- table(base_discrete$nombre_employes, base_discrete$euribor3m)
prop.table(exemple,1)



#######################################################################################

##################################### REGRESSION   ###################################

#######################################################################################



##################################### MODELE LOGIT  ###################################

#1er modele

logit <- glm(y ~., data=train, family=binomial(link="logit"))
summary(logit)
# AIC = 5953.4

#2eme modele

logit <- glm(y ~ age + emploi + etude + defaut + contact + mois + jour_semaine + temps_appel + campagne_pub + jours_depuis_derniere_pub + ipc + icc + euribor3m + nombre_employes, data=train, family=binomial(link="logit"))
summary(logit)
 # AIC = 5967.9

# Modele selection descendante

logit <- glm(y~., data=train, family=binomial(link = "logit"))
summary(logit)$coefficients
selection <- step(logit, direction="backward", trace=TRUE, k = 2)
selection

#Modele selection ascendante


logit1 <- glm(y~1, data=train, family=binomial(link = "logit"))
logit2 <- glm(y~., data=train, family=binomial(link = "logit"))

selection <- step(logit1, scope=list(lower=logit1, upper=logit2),
                  data=base_discrete, direction="forward")

#Modele Both retenu


logit <- glm(y~.,data=train,family=binomial(link="logit"))
summary(logit)

selection <- step(logit,direction = "both",data = base_discrete)

logit <- glm(y~age + emploi + contact + campagne_pub + etat_precedente_campagne + variation_taux_emploi + ipc + euribor3m ,data=base_discrete,family=binomial(link="logit"))
summary(logit)
# AIC = 11927
#Prediction

pred.logit <- predict(logit, newdata = valid,type="response")

head(pred.logit)

#Courbe ROC
library(pROC)
auc(valid$y,pred.logit)
auc(valid$y,pred.logit,quiet=TRUE)
roc <- plot.roc(valid$y ,pred.logit ,
                main ="",percent = TRUE ,
                ci = FALSE , quiet = TRUE)

#0.7765 pour le premier modele

valid <- cbind(pred.logit,valid)
head(valid)
tail(valid)

# Matrice de confusion
valid <- cbind(valid, pred.logit = factor(ifelse(valid$pred.logit >0.01, 1, 0)))
(m.confusion <- as.matrix(table(valid$pred.y, valid$y)))

head(valid)
summary(valid$pred.logit)

# Prediction sur l'echantillon test 

p <- predict(logit, newdata = base_none,type="response")
p <- data.frame(p)
colnames(p) <- "y"
summary(p)



##################################### RANDOM FOREST   ###################################

install.packages("randomForest")
library(randomForest)
set.seed(123)
model_randomforest <- randomForest(y~., data = train,na.action=na.exclude, ntree = 500, mtry =6)
# avec ntree = nombre d'arbres
# mtry : nombres de variables candidates 
print(model_randomforest$confusion)

prediction_foret<- predict(model_randomforest, valid)

model_randomforest <- randomForest(y~ ., data = train,na.action=na.exclude, ntree = 50, mtry =6)

print(model_randomforest$confusion)
prediction_foret<- predict(model_randomforest, valid)
CrossTable(print(table(valid$y, prediction_foret)))

nrow(train)
hist(model_randomforest$oob.times)


dim(base_discrete)


probabilite_random <- model_randomforest$votes
probabilite_random




# Prediction sur l'echantillon test 

p1 <- predict(model_randomforest,base_none, type = "prob")
head(p1)
p1 <- p1[,-1]
sum(p1==1)
summary(p1)
p1 <- data.frame(p1)
colnames(p1) <- "y"




################################## ARBRE DE DECISION   ###################################

install.packages("rpart")
library(rpart)

set.seed(123)
arbredecision <- rpart(y ~ age + emploi + contact + campagne_pub + etat_precedente_campagne + variation_taux_emploi + ipc + euribor3m, data = base_discrete, control=rpart.control(minsplit=20,cp=0))

# Affichage graphique de l'arbre
plot(arbredecision, branch=2, uniform=T, compress=T, margin=0.5)
text(arbredecision, fancy=T,use.n=T,pretty=0,all=T, cex=0.5)
sum(predict(arbredecision,type="class") != base_discrete$y)/nrow(base_discrete)

install.packages("rpart.plot")
library(rpart.plot)
prp(arbredecision,type=2,extra=4,split.box.col="lightgray")

prunearbredecision  <- prune(arbredecision, cp=0.035)

pred.arbredecision  <- predict(arbredecision, type="prob", base_none)
head(pred.arbredecision,5)



#######################################################################################

############################### PREDICTIONS SUR NOS DONNEES  ##########################

#######################################################################################

str(base2)
base2$age <- cut(base2$age,c(0,33,39,48,Inf),right=TRUE)
base2$temps_appel <- cut(base2$temps_appel,c(0,104,180,320,Inf),right=TRUE)
base2$campagne_pub <- cut(base2$campagne_pub,c(0,2,4,Inf),right=TRUE)
base2$jours_depuis_derniere_pub <- cut(base2$jours_depuis_derniere_pub,c(0,100,Inf),right=TRUE)
base2$nombre_appel_avant_campagne <- cut(base2$nombre_appel_avant_campagne,c(0,1,Inf),right=TRUE)
base2$variation_taux_emploi <- cut(base2$variation_taux_emploi,c(-Inf,0.5,Inf),right=TRUE)
base2$ipc <- cut(base2$ipc,c(-Inf,93.20,93.80,94.03,Inf),right=TRUE)
base2$icc <- cut(base2$icc,c(-Inf,-42,-40.08,-36.1,Inf),right=TRUE)
base2$euribor3m <- cut(base2$euribor3m,c(-Inf,1.344,4.858,4.962,Inf),right=TRUE)
base2$nombre_employes <- cut(base2$nombre_employes,c(0,5176,5196,Inf),right=TRUE)



################################## LOGIT   ###################################


plogit <- predict(logit, newdata = base2,type="response")
plogit <- data.frame(plogit)
colnames(plogit) <- "y"
summary(plogit)
write.table(plogit,"pred1.csv",sep=";")

################################## RANDOM FOREST  ###################################


prf <- predict(model_randomforest,base2, type = "prob")
head(prf)
prf <- prf[,-1]
sum(prf==1)
summary(prf)
prf <- data.frame(prf)
colnames(prf) <- "y"
write.table(prf,"pred2.csv",sep=";")


################################## ARBRE DE DECISION   ###################################

pad <- predict(arbredecision, type="prob", base2)
head(pad)
pad <- pad[,-1]
sum(pad==1)
summary(pad)
pad <- data.frame(pad)
colnames(pad) <- "y"

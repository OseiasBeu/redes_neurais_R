#######Regressão Logística#########



options(scipen=999) #Tira a notação cientifica##


##Importa a base##
reglog <-read.table("propensao.csv", header=TRUE, sep=";")
names(reglog)

##gera o modelo##
Modelo=glm(Doente ~ Proteina1+Proteina2+ Proteina3+ Proteina4+ Proteina5+ Proteina6,family=binomial(link='logit'), data=reglog)
summary(Modelo)


##stepwise##
step(Modelo, direction = 'both')
Modelo_step=glm(Doente ~ Proteina1+ Proteina4+ Proteina6,family=binomial(link='logit'), data=reglog)
summary(Modelo_step)

##Hosmer Lemeshow##
install.packages("ResourceSelection")
require(ResourceSelection)
hl=hoslem.test(reglog$Doente,fitted(Modelo_step),g=10)
hl

hl=hoslem.test(reglog$Doente,fitted(Modelo),g=10)
hl

##R-quadrado##

require(modEvA)

RsqGLM(Modelo_step)

##Curva ROC##
require(pROC)

roc1=plot.roc(reglog$Doente,fitted(Modelo_step))

plot(roc1,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)


##faz as previsoes##
prob=predict(Modelo_step,newdata=reglog,type="response")
classe_predita<-ifelse(prob > 0.5,1,0)
base_com_prob <-data.frame(reglog, prob=prob, classe_predita=classe_predita)
Confusao<-table(predito=classe_predita, Original=reglog$Doente)
acerto<- sum(diag(Confusao))/sum(Confusao)

Sensibilidade<- Confusao[1,1]/sum(Confusao[1,1], Confusao[1,2])
Especificidade<-Confusao[2,2]/sum(Confusao[2,1], Confusao[2,2])

#Fazendo com a base HATCO

#X1: Velocidade de Entrega
#X2: Preço
#X3 Flexibilidade de Negociação
#X4: Imagem da empresa
#X5:Nivel de Serviço
#X6:Força de Vendas
#X7:Qualidade do Produto
#X11:Avalia a compra pelo valor total (1) avalia a compra pela qualidade do produto(0)

hatco <-read.table("HATCO.csv", header=TRUE, sep=";")
summary(hatco)

##gera o modelo## x1;x2;x3;x4;x5;x6;x7;x11
Modelo=glm(x11 ~ x1+x2+x3+x4+x5+x6+x7,family=binomial(link='logit'), data=hatco)
summary(Modelo)


##stepwise##
step(Modelo, direction = 'both')
Modelo_step=glm(x11 ~ x1+x3+x6+x7,family=binomial(link='logit'), data=hatco)
summary(Modelo_step)

##Hosmer Lemeshow##
#install.packages("ResourceSelection")
#require(ResourceSelection)
hl=hoslem.test(hatco$x11,fitted(Modelo_step),g=10)
hl

hl=hoslem.test(hatco$x11,fitted(Modelo),g=10)
hl

##R-quadrado##
#require(modEvA)
RsqGLM(Modelo_step)

##Curva ROC##
require(pROC)

roc1=plot.roc(hatco$x11,fitted(Modelo_step))

plot(roc1,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)

##faz as previsoes##
prob=predict(Modelo_step,newdata=hatco,type="response")
classe_predita<-ifelse(prob > 0.5,1,0)
base_com_prob <-data.frame(hatco, prob=prob, classe_predita=classe_predita)
Confusao<-table(predito=classe_predita, Original=hatco$x11)
acerto<- sum(diag(Confusao))/sum(Confusao)

Sensibilidade<- Confusao[1,1]/sum(Confusao[1,1], Confusao[1,2])
Especificidade<-Confusao[2,2]/sum(Confusao[2,1], Confusao[2,2])


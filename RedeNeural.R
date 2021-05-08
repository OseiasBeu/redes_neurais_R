
#instalação das bibliotecas
install.packages('neuralnet')
require(neuralnet)

#importação da base
Hatco <-read.table("HATCO.csv", header=TRUE, sep=";")
summary(hatco)
View(hatco)

#constroi a rede
nn=neuralnet(x11~x1+x2+x3+x4+x5+x6+x7,
             data=Hatco, hidden=c(3,3), threshold=0.01)
plot(nn)
print(nn)

options(scipen=999)

nn$result.matrix 

nn$net.result

#Verifica acerto
classe_predita<-ifelse(nn$net.result > 0.5,1,0)

##faz as previsoes##
prob=predict(nn,newdata=Hatco,type="response")
classe_predita<-ifelse(prob > 0.5,1,0)
Confusao<-table(predito=classe_predita, Original=neural$x11)
list (Confusao)
acerto<- sum(diag(Confusao))/sum(Confusao)
Sensibilidade<- Confusao[1,1]/sum(Confusao[1,1], Confusao[1,2])
Especificidade<-Confusao[2,2]/sum(Confusao[2,1], Confusao[2,2])


#constroi a rede
Modelo_neural_Imovel=neuralnet(Valor~Area+Idade+Energia,
                               
                               data=Imovel, hidden=c(2), threshold=0.01)

plot(Modelo_neural_Imovel)

print (Modelo_neural_Imovel)


#importa a base

Prop_doenca<-read.table("C:\\R\\propensao.csv", header=TRUE, sep=";")





#constroi a rede com uma camada de dois neuronios

nn1=neuralnet(Doente ~ Proteina1+Proteina2+ Proteina3+ Proteina4+ Proteina5+ Proteina6,
              
              data=Hatco, hidden=c(2), threshold=0.01)

plot(nn1)



#constroi a rede com uma camada de três neuronios

nn2=neuralnet(Doente ~ Proteina1+Proteina2+ Proteina3+ Proteina4+ Proteina5+ Proteina6,
              
              data=Hatco, hidden=c(3), threshold=0.01)

plot(nn2)



#constroi a rede com uma camada de dois e uma três neuronios

nn3=neuralnet(Doente ~ Proteina1+Proteina2+ Proteina3+ Proteina4+ Proteina5+ Proteina6,
              
              data=Hatco, hidden=c(2,3), threshold=0.01)

plot(nn3)



#constroi a rede com uma camada de três e uma dois neuronios

nn4=neuralnet(Doente ~ Proteina1+Proteina2+ Proteina3+ Proteina4+ Proteina5+ Proteina6,
              
              data=Hatco, hidden=c(3,2), threshold=0.01)

plot(nn4)

16:06
#constroi a rede com uma camada de dois neuronios

nn1=neuralnet(Doente ~ Proteina1+Proteina2+ Proteina3+ Proteina4+ Proteina5+ Proteina6,
              
              data=Prop_doenca, hidden=c(2), threshold=0.01)

plot(nn1)



#constroi a rede com uma camada de três neuronios

nn2=neuralnet(Doente ~ Proteina1+Proteina2+ Proteina3+ Proteina4+ Proteina5+ Proteina6,
              
              data=Prop_doenca, hidden=c(3), threshold=0.01)

plot(nn2)



#constroi a rede com uma camada de dois e uma três neuronios

nn3=neuralnet(Doente ~ Proteina1+Proteina2+ Proteina3+ Proteina4+ Proteina5+ Proteina6,
              
              data=Prop_doenca, hidden=c(2,3), threshold=0.01)

plot(nn3)



#constroi a rede com uma camada de três e uma dois neuronios

nn4=neuralnet(Doente ~ Proteina1+Proteina2+ Proteina3+ Proteina4+ Proteina5+ Proteina6,
              
              data=Prop_doenca, hidden=c(3,2), threshold=0.01)

plot(nn4)

##faz as previsoes##

prob=predict(nn4,newdata=Prop_doenca,type="response")

classe_predita<-ifelse(prob > 0.5,1,0)





Confusao<-table(predito=classe_predita, Original=Prop_doenca$Doente)

list (Confusao)





acerto<- sum(diag(Confusao))/sum(Confusao)







Sensibilidade<- Confusao[1,1]/sum(Confusao[1,1], Confusao[1,2])

Especificidade<-Confusao[2,2]/sum(Confusao[2,1], Confusao[2,2])

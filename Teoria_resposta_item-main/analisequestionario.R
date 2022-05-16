setwd("C:/Users/Amábile/Documents/R") #acha caminho para o arquivo
getwd()
library(readxl) # chama pacote para ler arquivo excel
#install.packages('Deducer', dependencies = TRUE)
library(ltm)
library(irtoys)
library(mirt)
library(CTT)
library(tidyverse)
library(sfsmisc)


############################ ABRINDO ARQUIVO EXCEL CORRIGIDO ###########################
dados<-read_xlsx("Respostas.xlsx",sheet=2, col_names = TRUE)#Segunda aba
dados<- as.data.frame(dados)
head(dados)
itens<-dados[,2:14] #Separando apenas os itens
head(itens)
fatores<-dados[,15:17]# apenas fatores
head(fatores)

############################## ANÁLISE CLASSICA DOS ITENS #############################
# descrição dos itens
itens.desc<- descript(itens)
itens.desc

#interpretação TCT

desc <-itens.desc[[2]]
desc


#Gráfico
par( mfrow = c(1,1),oma = c(0,1,0,1.8))
plot(itens.desc,type="b",includeFirstLast=TRUE, pch=c(17,18,19,20,21,22,15,16,23), main='Gráfico dos escores vs proporção de respostas',col= 1:6)
legend("bottomright", legend = c('i1', 'i2','i3','i4', 'i5','i6', 'i7', 'i8', 'i9', 'i10', 'i11', 'i12', 'i13'), col = 1:6, pch = c(17,18,19,20,21,22,15,16,23), bty = "n", xpd = TRUE, inset = c(-0.1,-0.3))

#Interpretação ponto bisserial
conf.ctt<-reliability(itens, itemal=TRUE)
conf.ctt
bisserial<-conf.ctt$pBis# Coeficientes de correlação da base
bisserial


conf.ctt2<- itemAnalysis(itens)
conf.ctt2$itemReport


#retira itens 6 e 13 pb muito baixo

head(itens)

class(itens)
itens<-as.data.frame(itens)


######################## AJUSTE MODELO DE 3 PARAMETROS ####################
#####primeira calibração
M1<-mirt(itens, 1, itemtype = "3PL")
Cal1<-coef(M1, simplify = TRUE, IRTpars = TRUE)
Cal1

#graficos
plot(M1, type = 'trace', facet_items=FALSE, which.items = 1:13)
plot(M1, type = 'trace', which.items = 1:13)

itens1<-itens[,c(-7)] #retirando item problemático 7 abaixo de 0.6
head(itens1)

#####segunda calibração
M2<-mirt(itens1, 1, itemtype = "3PL")
Cal2<-coef(M2, simplify = TRUE, IRTpars = TRUE)
Cal2

plot(M2, type = 'trace', facet_items=FALSE, which.items = 1:12)
plot(M2, type = 'trace', which.items = 1:12)

itens2<-itens1[,c(-11)]
head(itens2)

### terceira calibração

M3<-mirt(itens2, 1, itemtype = '3PL')
Cal3<-coef(M3, simplify = TRUE, IRTpars = TRUE)
Cal3

plot(M3, type = 'trace', facet_items=FALSE, which.items = 1:11)
plot(M3, type = 'trace', which.items = 1:11)

itens3<-itens2[,-3]
head(itens3)

#### quarta calibração
M4<-mirt(itens3, 1, itemtype = '3PL')
Cal4<-coef(M4, simplify = TRUE, IRTpars = TRUE)
Cal4

plot(M4, type = 'trace', facet_items=FALSE, which.items = 1:10)
plot(M4, type = 'trace', which.items = 1:10)

itens4<-itens3[,-2]
head(itens4)

### quinta calibração

M5<-mirt(itens4, 1, itemtype = '3PL')
Cal5<-coef(M5, simplify = TRUE, IRTpars = TRUE)
Cal5

plot(M5, type = 'trace', facet_items=FALSE, which.items = 1:9)
plot(M5, type = 'trace', which.items = 1:9)

itens5<-itens4[,-7]
head(itens5)

### sexta calibração

M6<-mirt(itens5, 1, itemtype = '3PL')
Cal6<-coef(M6, simplify = TRUE, IRTpars = TRUE)
Cal6

plot(M6, type = 'trace', facet_items=FALSE, which.items = 1:8)
plot(M6, type = 'trace', which.items = 1:8)

itens6<-itens5[,-6]
head(itens6)

###setima calibração

M7<-mirt(itens6, 1, itemtype = '3PL')
Cal7<-coef(M7, simplify = TRUE, IRTpars = TRUE)
Cal7

plot(M7, type = 'trace', facet_items=FALSE, which.items = 1:7)
plot(M7, type = 'trace', which.items = 1:7)

itens7<-itens6[,-4]
head(itens7)

### oitava calibração

M8<-mirt(itens7, 1, itemtype = '3PL')
Cal8<-coef(M8, simplify = TRUE, IRTpars = TRUE)
Cal8

plot(M8, type = 'trace', facet_items=FALSE, which.items = 1:6)
plot(M8, type = 'trace', which.items = 1:6)

itens8<-itens7[,-4]
head(itens8)

### nona calibração

M9<-mirt(itens8, 1, itemtype = '3PL')
Cal9<-coef(M9, simplify = TRUE, IRTpars = TRUE)
Cal9

plot(M9, type = 'trace', facet_items=FALSE, which.items = 1:5)
plot(M9, type = 'trace', which.items = 1:5)


itens9<-itens8[,-3]
head(itens9)

### decima calibração
M10<-mirt(itens9, 1, itemtype = '3PL')
Cal10<-coef(M10, simplify = TRUE, IRTpars = TRUE)
Cal10

#itens2<- itens1[,-11] #retirando item 12 acima de 2.5

###### terceira calibração
#M3<-mirt(itens2, 1, itemtype = "3PL")
#Cal3<-coef(M3, simplify = TRUE, IRTpars = TRUE)
#Cal3

#plot(M3, type = 'trace', facet_items=FALSE, which.items = 1:11)
#plot(M3, type = 'trace', which.items = 1:11)

#itens3<- itens2[,-3] #retirando item 3 problematico maior que 2.5

##### quarta calibração
#M4<-mirt(itens3, 1, itemtype = "3PL")
#Cal4<-coef(M4, simplify = TRUE, IRTpars = TRUE)
#Cal4

#plot(M4, type = 'trace', facet_items=FALSE, which.items = 1:11)
#plot(M4, type = 'trace', which.items = 1:11)

#itens4<- itens3[,-7] # retirando item 9 acima de 3.4

#####quinta calibração
#M5<-mirt(itens4, 1, itemtype = "3PL")
#Cal5<-coef(M5, simplify = TRUE, IRTpars = TRUE)
#Cal5

#plot(M5, type = 'trace', facet_items=FALSE, which.items = 1:10)
#plot(M5, type = 'trace', which.items = 1:10)

#estimação do theta
scores <- fscores(M2)
scores
rank<-data.frame("Respondente" = 1:dim(scores)[1],
                 'theta'= scores[,1],
                 'posicao'=rank(scores[,1]),
                 'acertos'= rowSums(itens1))
rank
filter(rank, acertos == 9)
order(rank$theta,rank$theta,decreasing = TRUE )


################# Construção e interpretação da escala ###################






install.packages('tufte')
install.packages('xtable')












#########################################################################
####################### estudo de fatores associados ####################

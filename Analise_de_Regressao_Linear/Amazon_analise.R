## --------------- PACOTES --------------------------------

library(tidyverse)
#install.packages("DataExplorer")
library(DataExplorer)
#install.packages("esquisse")
library(car)

library(corrplot)
setwd("C:/Users/Am�bile/Documents/Estat�stica/Regress�o Linear/PI")
getwd()

### ----------------- CHAMANDO BASE DE DADOS -------------------------

base<-read.csv("Amazon.csv", header = TRUE, sep = ",", dec = ".")
str(base)




## --------------------- TRATAMENTO ---------------------------------------
### coluna com ano de lan�amento


# data<- as.Date(base$Data, "%d/%m/%Y")
# class(data)
# ano<- format(as.Date(base$Data, "%d/%m/%Y"), format= "%Y")
# ano<-as.numeric(ano)
# base[,"Ano"]<-ano
# base[, "Date"]<- data

base <-subset(base, select = -c(Dimensao, Data, Editora))## excluindo colunas que n�o ser�o usadas
str(base)


summary(base)

# plot_missing(base)


dados<- na.omit(base)
summary(dados)
str(dados)

## ------------------------------- AN�LISE EXPLORAT�RIA ---------------------------------------

### evolutivo de pre�o ao longo dos anos


ggplot(data = dados,
    aes(x = Date, y = Preco)) +
  geom_line()


### filtrar por data
dados %>% 
  filter(Date >= as.Date("2000-01-01")) %>% 
  ggplot(aes(x = Date, y = Preco)) +
  geom_line() 


dados %>% 
  filter(Date >= as.Date("2000-01-01")) %>% 
  ggplot(aes(x = Date, y = Preco)) +
  geom_line() +
  geom_smooth( method = "loess", se = FALSE)



#######---------------------------------------------------------------------------------------

### apenas com varivaeis n�mericas
### matriz de correla��o do modeo

dados1<- subset(dados, select = -c(TipoLivro, Idioma, Titulo))
cor.dados<- cor(dados1)
corrplot(cor.dados, method = "ellipse")

dadosdummy<- subset(dados, select = -c(Titulo))
du.dados<-dummify(dadosdummy)
cor.bados<-cor(du.dados)

plot_correlation(cor.bados)



## grafivo categoricas
##Livro
cate.livro<- table(dados$TipoLivro)

barplot(cate.livro,
        xlab = "Tipo de livro",
        ylab = "Frequ�ncia absoluta",
        col = c("pink", "blue"))

## Idioma
cate.idioma<- table(dados$Idioma)

barplot(cate.idioma,
        xlab = "Idioma",
        ylab = "Frequ�ncia absoluta",
        col = c("purple", "green"))

###

boxplot(Preco ~ TipoLivro, data = dados)

boxplot(Preco ~ Idioma, data = dados)

##----------------------------------------------------------------------------------------------

### Vari�vel resposta ser� pre�o, ent�o vamos ver o comportamento das 
####variaveis em rela��o a vari�vel resposta

tb2 <- dados1 %>%
  gather(key = "variable", value = "value", -Preco)

#### comportamento do conjunto de dados
ggplot(data = tb2,
       mapping = aes(x = value, y = Preco)) +
  facet_wrap(facets = ~variable, scale = "free_x") +
  scale_y_log10() +
  geom_point() 

#### ajuste da reta para cada uma das vari�veis
ggplot(data = tb2,
       mapping = aes(x = value, y = Preco)) +
  facet_wrap(facets = ~variable, scale = "free_x") +
  scale_y_log10() +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(data = tb2,
       mapping = aes(x = value, y = Preco)) +
  facet_wrap(facets = ~variable, scale = "free_x") +
  scale_y_log10() +
  geom_point() +
  geom_smooth()


### Transformado
transtb2 <- tb2 %>% 
  mutate(value = value^(1/3)) %>% 
  mutate(Preco = Preco^(1/3))


ggplot(data = transtb2,
       mapping = aes(x = value, y = Preco)) +
  facet_wrap(facets = ~variable, scale = "free_x") +
  scale_y_log10() +
  geom_point() +
  geom_smooth(method = "lm")




##### Removendo volume de Zeros

####### ---------------------------------------------------------------------------------
###### N�mero de Avalia��es

boxplot(dados$Preco)
boxplot(dados$nAvaliacao)


### Qual � a transforma��o?
m0 <- lm(Preco ~ nAvaliacao, data = dados)
summary(m0)


MASS::boxcox(m0)
abline(v = 1/3, col = 'red')



ggplot(data = filter(dados, nAvaliacao < 2000), ### menores de 2000
       mapping = aes(x = nAvaliacao, y = Preco)) +
  #scale_x_log10()+
  geom_point() +
  geom_smooth(method = "lm")


### transforma��o em raiz cubica
navtb <- dados %>% 
  mutate(nAvaliacao = nAvaliacao^(1/3)) %>% 
  mutate(Preco = Preco^(1/3))



ggplot(data =navtb,
       mapping = aes(x = nAvaliacao, y = Preco)) +
  geom_point()+
  geom_smooth(method = "lm")
  
  
ggplot(data = filter(navtb, nAvaliacao < 12, Preco < 10),
        mapping = aes(x = nAvaliacao, y = Preco)) +
   geom_point() +
  geom_smooth(method = "lm")

boxplot(navtb$nAvaliacao)

boxplot(navtb$Preco)

# filter(dados, nAvaliacao > 829) ### livros com mais de 2000 avalia��es
# filter(dados,  Preco > 1086) ### livros que custam mais de mil
# 
# tb <- filter(dados, nAvaliacao < 829, Preco < 1086 )

###-----------------------------------------------------------------------------------------------
#### Paginas




 # tb2 <- dados1 %>%
 #   gather(key = "variable", value = "value", -Preco)
# 
# #### comportamento do conjunto de dados
# ggplot(data = tb2,
#        mapping = aes(x = value, y = Preco)) +
#   facet_wrap(facets = ~variable, scale = "free_x") +
#   scale_y_log10() +
#   geom_point() 

m0 <- lm(Preco ~ Paginas, data = dados)
summary(m0)


MASS::boxcox(m0)
abline(v = 1/3, col = 'red')

?scale_x_sqrt

ggplot(data = dados, 
       mapping = aes(x = Paginas , y = Preco)) +
  geom_point() 


ggplot(data = dados, 
       mapping = aes(x = Paginas , y = Preco)) +
  geom_point() +
  geom_smooth()


ggplot(data = dados, 
       mapping = aes(x = Paginas , y = Preco)) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(data =filter(dados, Paginas < 1064),
       mapping = aes(x = Paginas, y = Preco)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(data =filter(dados, Preco < 900),
       mapping = aes(x = Paginas, y = Preco)) +
  geom_point()+
  geom_smooth(method = "lm")

# ggplot(data =filter(tb, Preco < 1000, Paginas < 1000),
#        mapping = aes(x = Paginas, y = Preco)) +
#   geom_point()+
#   geom_smooth(method = "lm")


Ptb <- dados %>% 
  mutate(Paginas = Paginas^(1/3)) %>% 
  mutate(Preco = Preco^(1/3))
  
ggplot(data = Ptb, 
       mapping = aes(x = Paginas , y = Preco)) +
  geom_point() 


ggplot(data = Ptb, 
       mapping = aes(x = Paginas , y = Preco)) +
  geom_point() +
  geom_smooth()


ggplot(data = Ptb, 
       mapping = aes(x = Paginas , y = Preco)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data =filter(Ptb, Preco >3),
       mapping = aes(x = Paginas, y = Preco)) +
  geom_point()+
  geom_smooth(method = "lm")


boxplot(Ptb$Paginas) ## pela box plot 

# filter(tb, Paginas > 1064) ### livros com mais de 2000 avalia��es
# filter(tb, Paginas < 80) ### livros que custam mais de mil
# 
# 
# dado3<- filter(tb, Paginas < 1064, Paginas > 80) ### Base com livros com menos de 1064 paginas e mais 80
# 
# ###### -----------------------------------------------------------------------------------------

################################# Variaveis categoricas ###########################################

#### --------------------------------------- Tipo de Livros --------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Paginas ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


######### SEM TRANSFORMA��O

## dispers�o dos dados
ggplot(data = dados,
       mapping = aes(x = Paginas, y = Preco, color = TipoLivro)) +
  geom_point()

## ajuste da reta de acordo com o tipo de livro

ggplot(data = dados,
       mapping = aes(x = Paginas, y = Preco, color = TipoLivro)) +
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
ggplot(data = dados,
       mapping = aes(x = Paginas, y = Preco, color = TipoLivro)) +
  facet_wrap(facets = ~TipoLivro, nrow = 3)+
  geom_point()+
  geom_smooth(method = "lm")


###### COM TRANSFORMA��O

Ptb2 <- dados %>% 
  mutate(Paginas = Paginas^(1/3)) %>% 
  mutate(Preco = Preco^(1/3))

?"%>%"
## dispers�o dos dados
ggplot(data = Ptb2,
       mapping = aes(x = Paginas, y = Preco, color = TipoLivro)) +
  geom_point()

## ajuste da reta de acordo com o tipo de livro

ggplot(data = Ptb2,
       mapping = aes(x = Paginas, y = Preco, color = TipoLivro)) +
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
ggplot(data = Ptb2,
       mapping = aes(x = Paginas, y = Preco, color = TipoLivro)) +
  facet_wrap(facets = ~TipoLivro, nrow = 3)+
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
# ggplot(data = dado3,
#        mapping = aes(x = Paginas, y = Preco)) +
#   facet_wrap(facets = ~TipoLivro, nrow = 3) +
#   geom_point() +
#   geom_smooth(method = "lm")


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ nAvaliacao ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### SEM TRANFORMACAO
## dispers�o dos dados
ggplot(data = dado3,
       mapping = aes(x = nAvaliacao, y = Preco, color = TipoLivro)) +
  geom_point()

## ajuste da reta de acordo com o tipo de livro

ggplot(data = dado3,
       mapping = aes(x = nAvaliacao, y = Preco, color = TipoLivro)) +
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
ggplot(data = dado3,
       mapping = aes(x = nAvaliacao, y = Preco, color = TipoLivro)) +
  facet_wrap(facets = ~TipoLivro, nrow = 3)+
  geom_point()+
  geom_smooth(method = "lm")


### COM TRANSFORMACAO

navtb2 <- dado3 %>% 
  mutate(nAvaliacao = nAvaliacao^(1/3)) %>% 
  mutate(Preco = Preco^(1/3))

## dispers�o dos dados

ggplot(data = navtb2,
       mapping = aes(x = nAvaliacao, y = Preco, color = TipoLivro)) +
  geom_point()

## ajuste da reta de acordo com o tipo de livro

ggplot(data = navtb2,
       mapping = aes(x = nAvaliacao, y = Preco, color = TipoLivro)) +
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
ggplot(data = navtb2,
       mapping = aes(x = nAvaliacao, y = Preco, color = TipoLivro)) +
  facet_wrap(facets = ~TipoLivro, nrow = 3)+
  geom_point()+
  geom_smooth(method = "lm")


#### ----------------------------------  IDIOMA ----------------------------------------------


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Paginas ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######### SEM TRANSFORMA��O

## dispers�o dos dados
ggplot(data = dados,
       mapping = aes(x = Paginas, y = Preco, color = Idioma)) +
  geom_point()

## ajuste da reta de acordo com o tipo de livro

ggplot(data = dados,
       mapping = aes(x = Paginas, y = Preco, color = Idioma)) +
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
ggplot(data = dados,
       mapping = aes(x = Paginas, y = Preco, color = Idioma)) +
  facet_wrap(facets = ~Idioma, nrow = 3)+
  geom_point()+
  geom_smooth(method = "lm")


###### COM TRANSFORMA��O

Ptb3 <- dados %>% 
  mutate(Paginas = Paginas^(1/3)) %>% 
  mutate(Preco = Preco^(1/3))


## dispers�o dos dados
ggplot(data = Ptb3,
       mapping = aes(x = Paginas, y = Preco, color = Idioma)) +
  geom_point()

## ajuste da reta de acordo com o tipo de livro

ggplot(data = Ptb3,
       mapping = aes(x = Paginas, y = Preco, color = Idioma)) +
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
ggplot(data = Ptb3,
       mapping = aes(x = Paginas, y = Preco, color = Idioma)) +
  facet_wrap(facets = ~Idioma, nrow = 3)+
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
# ggplot(data = dado3,
#        mapping = aes(x = Paginas, y = Preco)) +
#   facet_wrap(facets = ~TipoLivro, nrow = 3) +
#   geom_point() +
#   geom_smooth(method = "lm")


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ nAvaliacao ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### SEM TRANFORMACAO
## dispers�o dos dados
ggplot(data = dado3,
       mapping = aes(x = nAvaliacao, y = Preco, color = Idioma)) +
  geom_point()

## ajuste da reta de acordo com o tipo de livro

ggplot(data = dado3,
       mapping = aes(x = nAvaliacao, y = Preco, color = Idioma)) +
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
ggplot(data = dado3,
       mapping = aes(x = nAvaliacao, y = Preco, color = Idioma)) +
  facet_wrap(facets = ~Idioma, nrow = 3)+
  geom_point()+
  geom_smooth(method = "lm")


### COM TRANSFORMACAO

navtb3 <- dado3 %>% 
  mutate(nAvaliacao = nAvaliacao^(1/3)) %>% 
  mutate(Preco = Preco^(1/3))

## dispers�o dos dados

ggplot(data = navtb3,
       mapping = aes(x = nAvaliacao, y = Preco, color = Idioma)) +
  geom_point()

## ajuste da reta de acordo com o tipo de livro

ggplot(data = navtb3,
       mapping = aes(x = nAvaliacao, y = Preco, color = Idioma)) +
  geom_point()+
  geom_smooth(method = "lm")


## ajuste da reta com o livro separadamente
ggplot(data = navtb3,
       mapping = aes(x = nAvaliacao, y = Preco, color = Idioma)) +
  facet_wrap(facets = ~Idioma, nrow = 3)+
  geom_point()+
  geom_smooth(method = "lm")


################################### AN�LISE DE REGRESS�O LINEAR MULTIPLA ############################################


amazon<- dado3 %>% 
  mutate(Idioma = factor(Idioma),
         TipoLivro = factor(TipoLivro))
      
str(amazon)
dim(amazon)


######################################## VARIAVEIS CATEGORICAS #########################################


X<- as.data.frame(subset(amazon, select = c( Idioma)))
X
X1<-model.matrix(~X, contrasts.arg = list(X= "contr.SAS"))
dim(X1)


m4p



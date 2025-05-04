library(ggplot2)
library(lattice)
library(caret)
library(tidyverse)
library(ggplot2)
install.packages("gridExtra")
install.packages("ggridges")
install.packages("gtable")
install.packages("grid")
install.packages("egg")
install.packages("vip")
library(vip)
library(gridExtra)
library(ggridges)
library(gtable)
library(grid)
library(egg)

vinhosbrancos <- read.csv("D:\\Universidade Europeia - Pós Graduação\\Data Science for Business\\3. Machine Learning\\Trabalho Final - Machine Learning\\winequality-white.csv", sep = ";")

str(vinhosbrancos)
head(vinhosbrancos)


#Questão: Atingir um modelo preditivo de classificação da qualidade de vinhos brancos verdes
#baseado em dados psicoquímicos

set.seed(12345678)

colnames(vinhosbrancos)[1]="Acidez fixa"
colnames(vinhosbrancos)[2]="Acidez volátil"
colnames(vinhosbrancos)[3]="Ácido cítrico"
colnames(vinhosbrancos)[4]="Açúcar residual"
colnames(vinhosbrancos)[5]="Cloretos"
colnames(vinhosbrancos)[6]="Dióxido de enxofre livre"
colnames(vinhosbrancos)[7]="Dióxido de enxofre total"
colnames(vinhosbrancos)[8]="Densidade"
colnames(vinhosbrancos)[9]="pH"
colnames(vinhosbrancos)[10]="Sulfitos"
colnames(vinhosbrancos)[11]="Álcool"
colnames(vinhosbrancos)[12]="Qualidade"



#limpeza de dados
sum(is.na(vinhosbrancos))

vinhosbrancos$Qualidade <- as.numeric(vinhosbrancos$Qualidade, ordered = T)


qualidadevinhos<-rep("", 4898)
qualidadevinhos[which(vinhosbrancos$Qualidade<7)]<-"Mau vinho"
qualidadevinhos[which(vinhosbrancos$Qualidade>=7)]<-"Bom vinho"

vinhosbrancos$Classificacao <- qualidadevinhos

vinhosbrancos$Classificacao <- ordered(vinhosbrancos$Classificacao,
                       levels = c('Mau vinho', 'Bom vinho'))
str(vinhosbrancos)

vinhosbrancos1 <- as.data.frame(unclass(vinhosbrancos),                     
                        stringsAsFactors = TRUE)

summary(vinhosbrancos1)
ggplot(data = vinhosbrancos1) + 
  geom_bar(aes(Classificacao, fill = Classificacao)) +
  labs(title = "Prevalência da Classificação de Vinhos Brancos") +
  theme(legend.position = 'none')

#Objetivo do modelo é encontrar as variáveis mais relevantes estatisticamente na previsão da qualidade do vinho


install.packages("corrplot")
library(corrplot)
vinhosbrancos1 = select(vinhosbrancos1,- Qualidade)
corrplot(correlacao)

install.packages("Hmisc")
library(Hmisc)

 
#Variáveis mais relevantes ~ Açúcar residual, Acidez volátil, Álcool, Densidade para a Qualidade

pairs(vinhosbrancos1[, c("Açúcar.residual","Acidez.volátil","Álcool", "Densidade")], 
      col=rainbow(2)[vinhosbrancos1$Qualidade])

barplot(table(vinhosbrancos$Qualidade), col = "red",
     main = "Histograma Qualidade dos Vinhos Brancos",
     xlab = "Nota da Qualidade dos Vinhos Brancos", ylab = "Frequência")
barplot(table(vinhosbrancos$Álcool),col='blue',
        main='Histograma de Percentagem de Álcool em Vinhos Brancos', 
        xlab="% de Álcool",ylab='Frequência')
barplot(table(vinhosbrancos$`Açúcar residual`),col='green',
        main='Histograma de Quantidade de Açúcar Residual em Vinhos Brancos', 
        xlab="Quantidade de açúcar redidual em g/dm3",ylab='Frequência')

 
prop.table(table(qualidadevinhos))
table(qualidadevinhos)

#Fazer uma lista de linhas do dataset que vão ser divididas em treino (80%) e teste (20%).

set.seed(127382781)

indextreino<-createDataPartition(y=vinhosbrancos$Classificacao, p=.8,
                                 list=FALSE)
indextreino.knn <- createDataPartition(y=vinhosbrancos1$Classificacao, p = .8,
                                  list = FALSE)

#Colocar as linhas de treino numa nova tabela de dados
treino.knn <- vinhosbrancos[ indextreino,]

#Colocar as linhas de teste numa nova tabela de dados
teste.knn  <- vinhosbrancos[-indextreino,]

#Verificar o número de linhas de treino
nrow(treino.knn)

#Verificar o número de linhas de teste
nrow(teste.knn)

#treinar um algoritmo de knn existente dentro do agregador caret
vinhos.knn <- train(Classificacao ~ ., data = treino.knn,
                 method = "knn",tuneLength = 15)

#verificar o algoritmo de knn criado
vinhos.knn
plot(vinhos.knn)
#Verificar as previsões do modelo
previsao.knn <- predict(vinhos.knn, teste.knn)
confusionMatrix(previsao.knn, teste.knn$Classificacao)

vip(vinhos.knn , num_features = 13, bar = FALSE)

table(vinhosbrancos1$Classificacao)
prop.table(table(vinhosbrancos1$Classificacao))


barplot(table(qualidadevinhos),col=c("purple","blue"),
        main='Histograma da Categorização da Qualidade de Vinhos Brancos', 
        xlab="Categoria", ylab='Frequência')
ggplot(data = novo_vinhosbrancos) + 
  geom_bar(aes(Classificacao, fill = Classificacao)) +
  labs(title = "Prevalência da Classificação de Vinhos Brancos") +
  theme(legend.position = 'none')

vip(rf.vinhos, num_features = 13, bar = FALSE)

#valores desproporcionais de resultados podem levar a uma eficacia enganadoramente alta
novo_vinhosbrancos <- downSample(x = vinhosbrancos1[, -12], y = vinhosbrancos1[, 12], 
                                 yname = "Classificacao")

summary(novo_vinhosbrancos)
prop.table(table(novo_vinhosbrancos$Classificacao))
table(novo_vinhosbrancos$Classificacao)
indextreino.knn <- createDataPartition(y=novo_vinhosbrancos$Classificacao, p = .8,
                                  list = FALSE)
#Colocar as linhas de treino numa nova tabela de dados
treino.knn1 <- novo_vinhosbrancos[ indextreino.knn,]

#Colocar as linhas de teste numa nova tabela de dados
teste.knn1  <- novo_vinhosbrancos[-indextreino.knn,]

#Verificar o número de linhas de treino
nrow(treino.knn1)

#Verificar o número de linhas de teste
nrow(teste.knn1)

#treinar um algoritmo de knn existente dentro do agregador caret
vinhos.knn1 <- train(Classificacao ~ ., data = treino.knn1,
                 method = "knn", tuneLength = 15)

#verificar o algoritmo de knn criado
vinhos.knn1
plot(vinhos.knn1)

#Verificar as previsões do modelo
previsao.knn1 <- predict(vinhos.knn1, teste.knn1)
confusionMatrixKNN<-confusionMatrix(previsao.knn1, teste.knn1$Classificacao)
confusionMatrixKNN
#######################################
#########. KMEANS #####################
#######################################

str(novo_vinhosbrancos)
novo_vinhosbrancos.km<-novo_vinhosbrancos[,c("Densidade","Álcool")]
head(novo_vinhosbrancos.km)
km_novo_vinhosbrancos <- kmeans(novo_vinhosbrancos.km,centers = 2,nstart=25,iter.max = 20)
#Fazer um gráfico dos grupos criados usando o kmeans
plot(novo_vinhosbrancos.km$Densidade, novo_vinhosbrancos.km$Álcool,
     xlab = "Densidade",
     ylab = "Álcool",
     col = km_novo_vinhosbrancos$cluster)

novo_vinhosbrancos.km1<-novo_vinhosbrancos[,c("Densidade","Açúcar.residual")]
head(novo_vinhosbrancos.km1)
km_novo_vinhosbrancos1 <- kmeans(novo_vinhosbrancos.km1,centers = 2,nstart=25,iter.max = 20)
#Fazer um gráfico dos grupos criados usando o kmeans
plot(novo_vinhosbrancos.km$Densidade, novo_vinhosbrancos.km$Álcool,
     xlab = "Densidade",
     ylab = "Açúcar residual",
     col = km_novo_vinhosbrancos1$cluster)
     legend("bottomright", legend = paste("Group", 1:3), col = 1:3, pch = 19, bty = "n")

#######################################
######### Decision Trees #####################
#######################################
set.seed(123456781)
indextreino.tree <- createDataPartition(y=simp_vinhos$Classificacao, p = .8,
                                             list = FALSE)   
#Colocar as linhas de treino numa nova tabela de dados
treino.tree <- novo_vinhosbrancos[ indextreino.tree,]

#Colocar as linhas de teste numa nova tabela de dados
teste.tree  <- novo_vinhosbrancos[-indextreino.tree,]

#Verificar o número de linhas de treino
nrow(treino.tree)

#Verificar o número de linhas de teste
nrow(teste.tree)
     
#treinar uma árvore de decisão usando o caret
dtree_vinhos<-train(Classificacao ~., data = treino.tree, method = "rpart", 
                           parms = list(split = "Classificacao"), tuneLength=10)

dtree_vinhos
dtree_vinhos$finalModel
vip(dtree_vinhos, num_features = 12, bar = FALSE)
library(rpart.plot)
prp(dtree_vinhos$finalModel, box.palette="Blues", tweak=0.8)


names(novo_vinhosbrancos)
simp_vinhos<-novo_vinhosbrancos[,c('Acidez.fixa','Acidez.volátil','Ácido.cítrico','Açúcar.residual','Cloretos','Dióxido.de.enxofre.livre','Dióxido.de.enxofre.total','Densidade','pH','Sulfitos','Álcool', 'Classificacao')]

indextreino.dtree <- createDataPartition(y= simp_vinhos$Classificacao, p = .8,
                                  list = FALSE)
#Colocar as linhas de treino numa nova tabela de dados
treino.tree1 <- simp_vinhos[ indextreino.dtree,]

#Colocar as linhas de teste numa nova tabela de dados
teste.tree1  <- simp_vinhos[ -indextreino.dtree,]

#Verificar o número de linhas de treino
nrow(treino.tree1)

#Verificar o número de linhas de teste
nrow(teste.tree1)

#treinar uma árvore de decisão usando o caret
dtree_vinhos1<-train(Classificacao ~., data = treino.tree1, method = "rpart", 
                    parms = list(split = "Classificacao"), tuneLength=15)

dtree_vinhos1
plot(dtree_vinhos1)
dtree_vinhos1$finalModel
vip(dtree_vinhos, num_features = 12, bar = FALSE, positive = "Bom vinho")
prp(dtree_vinhos1$finalModel, box.palette="Blues", tweak=1, positive="Bom vinho")

predictions.tree <- predict(dtree_vinhos1, teste.tree1)
confusionMatrix(predictions.tree, teste.tree1$Classificacao)
confusionMatrix(predictions.tree, teste.tree1$Classificacao, positive = "Bom vinho")

#agora usar uma random forest para treinar
rf.vinhos <- train(Classificacao ~ ., data = treino.tree1,
                method = "rf", tuneLength=10)

#verificar o método treinado
rf.vinhos
#agora ver a qualidade das previsões
predictions <- predict(rf.vinhos, teste.tree1)
confusionMatrix(predictions, teste.tree1$Classificacao, positive = "Bom vinho")
plot(rf.vinhos)
rf.vinhos$finalModel
vip(rf.vinhos, num_features = 12, bar = FALSE)

vip(rf.vinhos, num_features = 13, bar = FALSE)
vip(rf.vinhos, num_features = 13, bar = FALSE, all_permutations = TRUE, jitter = TRUE)
vip(dtree_vinhos , num_features = 13, bar = FALSE)
vip(vinhos.knn2 , num_features = 13, bar = FALSE)

install.packages("rpart.plot")
library(rpart.plot)

rf_accuracy <- mean(predictions == teste.tree1$Classificacao)
rf_accuracy

(728 + 68) / nrow(teste.knn)

#######################################
######### SVM #####################
#######################################

indextreino.svm <- createDataPartition(y=simp_vinhos$Classificacao, p = .8,
                                        list = FALSE)   
#Colocar as linhas de treino numa nova tabela de dados
treino.svm <- simp_vinhos[ indextreino.svm,]

#Colocar as linhas de teste numa nova tabela de dados
teste.svm  <- simp_vinhos[-indextreino.svm,]

#Verificar o número de linhas de treino
nrow(treino.svm)

#Verificar o número de linhas de teste
nrow(teste.svm)

install.packages("kernlab")
library(kernlab)


svm_vinhos<-train(Classificacao ~., data = treino.svm, method = "svmLinear2", tuneLength=10)
svm_vinhos
plot(svm_vinhos)
previsao.svm <- predict(svm_vinhos, teste.svm)
confusionMatrix(previsao.svm, teste.svm$Classificacao, positive = "Bom vinho")

confusionMatrix(predictions, teste.tree1$Classificacao, positive = "Bom vinho")
svm_vinhos1<-train(Classificacao ~., data = treino.svm, preProcess = c("center", "scale"), 
                  method = "svmRadial",tuneGrid = data.frame(.C = c(.25, .5, 1, 2, 4, 8, 16, 32, 64, 128),
                                                             .sigma = .05))
svm_vinhos1
plot(svm_vinhos1)
previsao.svm1 <- predict(svm_vinhos1, teste.svm)
confusionMatrix(previsao.svm1, teste.svm$Classificacao)

set.seed(12345)
indextreino.svm2 <- createDataPartition(y=simp_vinhos2$Classificacao, p = .8,
                                       list = FALSE)   
#Colocar as linhas de treino numa nova tabela de dados
treino.svm2 <- simp_vinhos2[ indextreino.svm2,]

#Colocar as linhas de teste numa nova tabela de dados
teste.svm2  <- simp_vinhos2[-indextreino.svm2,]

#Verificar o número de linhas de treino
nrow(treino.svm2)

#Verificar o número de linhas de teste
nrow(teste.svm2)
svm_vinhos2<-train(Classificacao ~., data = treino.svm2, method = "svmLinear2", tuneLength=10)
svm_vinhos2
plot(svm_vinhos2)
previsao.svm2 <- predict(svm_vinhos2, teste.svm2)
confusionMatrix(previsao.svm2, teste.svm2$Classificacao)


#########################
     ## MODELS ##
#########################

## SPLIT DATA ##

set.seed(1971)


# Gerando substes para RF Model.
ctrainRFM <- ctrain
ctesteRFM <- cteste


# Verificando a estrutura dos subset treino.
str(ctrainRFM)


# Verificando a estrutura dos subset teste.
str(ctesteRFM)


################################
##### 3 # RANDOM FOREST # ######
################################


# Análises para o algoritmo Random Forest (RFM)

# Random Forest é um algoritmo de predição com um método de aprendizado conjunto para classificação, regressão 
# e outras tarefas, que opera construindo árvores de decisão, de maneira aleatória, onde cada árvore será utilizada na escolha do resultado final.


# Treinando o modelo inicial de RF.
churnRFM <- randomForest(Churn ~., 
                         data = ctrainRFM,
                         ntree=200, 
                         type="classification")


# Exibindo o resultado do modelo sumarizado.
print(churnRFM)


# Aplicando o modelo de predição ao subset de teste.
RFM_Pred <- predict(churnRFM, 
                   ctesteRFM)


# Verificando a precisão da classificação  
mean(RFM_Pred == ctesteRFM$Churn)  


# Verificando o resultado da Matriz de Confusão. 
caret::confusionMatrix(RFM_Pred, 
                       ctesteRFM$Churn)




# Exibindo resultado da Matriz de Confusão.
print("Confusion Matrix Para Random Forest"); table(Actual = ctesteRFM$Churn, 
                                                    Predicted = RFM_Pred)

# Visualizando graficamente a redução da taxa de erros para o modelo inicial.
plot(churnRFM)

# Pouco ganho além de ~200 trees, aparentemente estagnação após 300 trees


# Exibir a importância da variável do modelo RF.
varImpPlot(churnRFM, 
           sort=T, 
           n.var = 10, 
           main = 'Top 10 Atributos em Importâcia')
# MonthlyCharges | Contract | Tenure | PaymentMethod | InternetService


# Demonstrando a importância.
importance(churnRFM)

#---

# Afinando Modelo2 mtry com tuneRF 
churnRFM3 <- tuneRF(x = subset(ctrainRFM, select = -Churn), 
                    y = ctrainRFM$Churn, 
                    ntreeTry = 100, 
                    doBest = TRUE)

# O OOB Error é um método de medição do erro de predição para Random Forest.
# É uma estimativa para subamostras de dados não tratadas no treinamento.

# for ntreeTry = 100 When mtry = 4, OOB decreases from 20.63%
# for ntreeTry = 100 When mtry = 2, OOB decreases from 20.04% 


# Exibindo o resultado sumarizado. 
print(churnRFM3)


# Performando a predição com o subset de teste. 
RFM_Pred3 <- predict(churnRFM3, 
                    ctesteRFM)


# Gerando a Matriz de Confusão.
caret::confusionMatrix(RFM_Pred3, 
                       ctesteRFM$Churn)


# Exibindo resultado da Matriz de Confusão.
print("Confusion Matrix Para Random Forest"); table(cteste$Churn, RFM_Pred3)


# O desempenho é um pouco semelhante ao modelo de árvore de decisão.
# A taxa de falsos negativos é baixa (1445 corretos versus 103 incorretos), mas a taxa de falsos positivos é bastante alta (272 corretos versus 288 incorretos). E a precisão geral?


# Gerando o valor de Acurácia. 
RFM_binomial <- tibble("target" = ctesteRFM$Churn, 
                        "prediction" = RFM_Pred3)


RFM_basic_table <- table(RFM_binomial)


# Carregando valor de Acurácia. 
RFM_acc = (RFM_basic_table[1,1] + RFM_basic_table[2,2])/sum(RFM_basic_table)


# Demonstrando o valor de Acurácia.
print(paste("Acurácia Final", RFM_acc))

## [1] "Acurácia Final 0.790322580645161"

      

# Semelhante à Decision Tree, este modelo de Random Forest identificou o status do contrato e a duração da posse como preditores importantes para o churn. O status do serviço de Internet não aparece tão importante neste modelo, e a variável total de cobranças agora é altamente enfatizada.

# O modelo Random Forest é um pouco mais preciso que o modelo de Decision Tree, sendo capaz de prever corretamente o status de churn de um cliente no subconjunto de teste com 79,03% de precisão.


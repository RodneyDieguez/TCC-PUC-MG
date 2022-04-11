#########################
      ## MODELS ##
#########################

## SPLIT DATA ##

   
set.seed(1971)   
 

# Gerando substes para LR Model.
ctrainLRM <- ctrain
ctesteLRM <- cteste


# Verificando a estrutura dos subset treino.
str(ctrainLRM)


# Verificando a estrutura dos subset teste.
str(ctesteLRM)


#################################
### 1 # LOGISTIC REGRESSION # ###

# Análises para modelo Logistic Regression (LRM)

# Logistic Regression envolve a regressão de variáveis preditoras em um resultado binário usando uma função de ligação binomial.
# O ajuste do modelo será dado usando a função de modelagem linear geral [glm]. 


# Treinando o modelo LR.
churnLRM <- glm(Churn ~., 
                data = ctrainLRM,
                family=binomial(link='logit'))


# Exibindo o resultado sumarizado.
print(summary(churnLRM))


# Segundo o autor do livro Estatistica aplicada a experimentacao animal, a função pR2 realiza análises de regressão 
# com tratamentos quantitativos, variância, teste t para coeficientes, realizando análises para falta de ajustes.
pR2(churnLRM)


# Segundo os autores do livro Numerical Ecology with R o VIF é uma medida da proporção em que 
# a variância de um coeficiente de regressão é inflacionado pela presença de outra variável explicativa. 
# Calcular os fatores de variação de inflação de todos os preditores em modelos de regressão.
vif(churnLRM)


# Ao examinar os valores de significância, os valores p mais baixos e podem ser identificados como os melhores preditores de rotatividade de clientes.


# Análise de Variância - ANOVA
# Um dos objetivos da aplicação da função ANOVA é realizar teste estatístico para verificar se há diferença entre a distribuição de uma medida entre vários grupos.
anova(churnLRM, test="Chisq")


# As variáveis com valor p < 0,001 denotam maior significância.

model_2<- stepAIC(churnLRM, direction="both")


summary(model_2)
# Verificar as variaveis mais importantes para o modelo podemos ver a queda no desvio ao adicionar cada variável. 
# Ao adicionar Partner, InternetService, OnlineSecurity, Contract e TenureYear reduz significativamente o desvio residual. 



# Aplicando o modelo de predição ao subset de teste.
LRM_prob1 <- predict(churnLRM, 
                     ctesteLRM, 
                     type="response")


LRM_pred1 <- ifelse(LRM_prob1 > 0.5,
                    "Yes",
                    "No")


# Carregando valor de Acurácia.
misClassficError <- mean(LRM_pred1 != ctesteLRM$Churn)


# Demonstrando o valor de Acurácia.
print(paste("Accuracy", 
            1-misClassficError))

# [1] "Accuracy 0.804079696394687"
# [1] "Accuracy 0.799810246679317"

# A taxa de precisão do modelo de regressão logística é de 79,98%.


# Verificando o resultado da Matriz de Confusão. 
# Examinar a matriz de confusão com base no modelo de regressão logística.
table(Predicted = LRM_pred1, 
      Actual = ctesteLRM$Churn)

#          Actual
# Predicted   No  Yes
# No  1385  250
# Yes  163  310

# Efetuando nova rodada de predição.
LRM_prob2 <- predict(churnLRM, 
                     ctrainLRM, 
                     type="response")


LRM_pred2 <- ifelse(LRM_prob2 > 0.5,
                    "Yes",
                    "No")


LRM_tab1 <- table(Predicted = LRM_pred2, 
                  Actual = ctrainLRM$Churn)


LRM_tab2 <- table(Predicted = LRM_pred1, 
                  Actual = ctesteLRM$Churn)



# Gerando Matriz de Confusão para cada subset.

# Train
caret::confusionMatrix(as.factor(LRM_pred2),  
                       as.factor(ctrainLRM$Churn),  
                       positive = "Yes" )


# Test
caret::confusionMatrix(as.factor(LRM_pred1),  
                       as.factor(ctesteLRM$Churn),  
                       positive = "Yes" )


# As entradas diagonais fornecem nossas previsões corretas, com o canto superior esquerdo sendo TN e o inferior direito sendo TP. 
# O canto superior direito fornece o FN enquanto o canto inferior esquerdo fornece o FP. 


# Carregando valor de Acurácia.
LRM_acc <- sum(diag(LRM_tab2))/sum(LRM_tab2)


# Demonstrando o valor de Acurácia.
print(paste("Acurácia", 
            LRM_acc))


# [1] "Acurácia 0.804079696394687"
# [1] 0.7998102
# [1] 0.8026565



# Efetuando verificação da performance da predição.
library(ROCR)
LRM_pred1 <- predict(churnLRM, 
                     ctesteLRM, 
                     type="response")

LRM_pred2 <- prediction(LRM_pred1, 
                        ctesteLRM$Churn)

LRM_perf <- performance(LRM_pred2, 
                        measure = "tpr", 
                        x.measure = "fpr")


# Exibindo resultado graficamente.
plot(LRM_perf, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)


# Gerando valor de Acurácia.
LRM_auc <- performance(LRM_pred2, 
                   measure = "auc")


# Carregando valor de Acurácia.
LRM_auc <- LRM_auc@y.values[[1]]

 
# Exibindo valor de Acurácia (AUC).
print(paste("Acurácia Final", LRM_auc))

# [1] "Acurácia Final 0.84341546696198"

# A taxa de precisão do modelo é de 84,34%.

# Em demais testes, fazendo a redução de variáveis, mesmo as de menor significância do teste ANOVA, 
# o valor final da acurácia não foi melhor do que os 84,33% obtidos com essa linha de execução.

# O resultado anterior mais próximo foi auc = 0.8384575, com a retirada dos atributos:
# Gender, PhoneService, MultipleLines, StreamingTV, StreamingMovies e MonthlyCharges.

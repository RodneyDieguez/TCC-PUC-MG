#########################
     ## MODELS ##
#########################


## SPLIT DATA ##

set.seed(1971)


# Gerando substes para DT Model.
ctrainDTM <- ctrain
ctesteDTM <- cteste


# Verificando a estrutura dos subset treino.
str(ctrainDTM)


# Verificando a estrutura dos subset teste.
str(ctesteDTM)


################################
##### 2 # DECISION TREE # ######


# # Análises para modelo Decision Tree (DTM).


# Decision tree é um método de classificação que usa modelos de decisões em forma de árvore, e seus possíveis resultados, usando métodos de particionamento recursivos. 
# Este método exploratório identifica as variáveis mais importantes relacionadas ao Churn em um formato hierárquico.


# Treinando o modelo DT.
churnDTM <- rpart(Churn ~., 
                  data = ctrainDTM, 
                  method="class")


# Exibindo o resultado detalhado graficamente.
fancyRpartPlot(churnDTM,
               main = 'Gráfico churnDTM')


# Gráfico com nível de detalhamento nos percentuais para Churn mais relevantes para o modelo.


# Exibindo o resultado resumido graficamente .
prp(churnDTM, 
    type = 4, 
    extra = 6,   
    box.palette = "auto",  
    faclen = 0,
    main = "Gráfico churnDTM")

# Gráfico com nível de detalhamento nos atributos para Churn mais relevantes para o modelo.




# A partir desta árvore de decisão, podemos interpretar o seguinte:
#   
# A variável Contract com 1 ano ou 2 anos tem menos Taxa de Churn no modelo. Clientes com contratos mensais são mais propensos ao churn.
# Os clientes sem o serviço de Internet ou com serviço DSL são menos propensos ao churn. Clientes com serviço de Fibra Ótica são mais propensos ao churn.
# Os clientes com valor total de gastos igual ou superior a $1556 são menos propensos a churn. Clientes com valor total de gastos inferior a $1556 são mais propensos ao churn.



# Aplicando o modelo de predição ao subset de teste.
DTM_pred1 <- predict(churnDTM, 
                     newdata = ctesteDTM, 
                     type = "class")
 

# Gerando a Matriz de Confusão.
DTM_confMat <- table(ctesteDTM$Churn, 
                     DTM_pred1)

# Verificando o resultado da Matriz de Confusão. 
DTM_confMat
# 
#      DTM_pred1
#       No  Yes
# No  1439  109
# Yes  352  208


table(Predicted = DTM_pred1, 
      Actual = ctesteDTM$Churn)

#            Actual
# Predicted   No  Yes
#       No  1439  352
#      Yes   109  208


# As entradas diagonais fornecem nossas previsões corretas, com o canto superior esquerdo sendo TN e o inferior direito sendo TP. 
# O canto superior direito fornece o FN enquanto o canto inferior esquerdo fornece o FP. 


# A partir dessa matriz de confusão, podemos ver que o modelo tem um bom desempenho na previsão de clientes que não desistem 
# (1.439 corretos versus 109 incorretos), mas não tem um desempenho tão bom na previsão de clientes que desistem (352 incorretos versus 208 corretos).



# Efetuando nova rodada de predição para Matriz de Confusão e Acurácia.
DTM_prob2 <- predict(churnDTM, 
                     data = ctrainDTM)

DTM_pred2 <- ifelse(DTM_prob2[,2] > 0.5,
                    "Yes",
                    "No")

DTM_tab1 <- table(Predicted = DTM_pred2, 
                  Actual = ctrainDTM$Churn)

DTM_tab2 <- table(Predicted = DTM_pred1, 
                  Actual = ctesteDTM$Churn)


# Gerando Matriz de Confusão para cada subset.

# Train
caret::confusionMatrix(as.factor(DTM_pred2), 
                       as.factor(ctrainDTM$Churn), 
                       positive = "Yes")



# Teste
caret::confusionMatrix(
  as.factor(DTM_pred1),
  as.factor(ctesteDTM$Churn),
  positive = "Yes" 
)
 

# Carregando valor de Acurácia.
DTM_acc <- sum(diag(DTM_tab2))/sum(DTM_tab2)


# Demonstrando o valor de Acurácia.
print(paste("Acurácia Final", DTM_acc))

# [1] "Acurácia Final 0.7813092979127133"

## [1] 0.7813092. 

# A taxa de precisão do modelo é de 78,13%.

 

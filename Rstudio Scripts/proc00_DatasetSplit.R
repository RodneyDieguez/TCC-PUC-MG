#########################
##### SPLIT DATASET ##### 
#########################

## CONSULTAS DATASET ##
 
# Dataset padrão [churn_clean]. 

# Verificação da estrutura do novo dataset, tratado e limpo.
str(churn_clean)


# Dimensão do conjunto de dados.
dim(churn_clean)

 
# Confirmando  as variáveis em tipo factor.
churn_clean <- as.data.frame(unclass(churn_clean), 
                             stringsAsFactors = TRUE) 


# Conferindo as classes das variáveis.
sapply(churn_clean, class)


# Usando função glimpse para demonstrar os tipos de dados e das variáveis, e uma amostra dos 5 primeiros valores (editado manualmente).
glimpse(churn_clean)


# Conferência de existência de algum valor NA na base após as alterações. 
any(is.na(churn_clean))


# Conferência de existência valores estranhos, zerados, nulos, após as alterações. 
# Essa validação também demonstra os tipos de cada variável e quantos níveis de valores existem em cada fator.
df_status(churn_clean)



# Remoção de variável pelo nome: X
#churn_clean$X  <- NULL



## SPLIT ##

# Dataset Split seed de aleatoriedade. 
set.seed(1971)


# Split em ctrain (treino) e cteste (produtivo). 
data_split  <- createDataPartition(churn_clean$Churn, p=0.7,list=FALSE)

ctrain <- churn_clean[data_split, ]
cteste <- churn_clean[-data_split, ]
 

# Verificando as estruturas. 
str(ctrain)

str(cteste)


# Visualização da dimensão de cada variável em cada split.
qtt_ctrain <- df_status(ctrain)
qtt_ctrain[ , -c(2:8)] 	


qtt_cteste <- df_status(cteste)
qtt_cteste[ , -c(2:8)] 	


# Verificando o quantitativo da variável alvo nos subsets.
table(ctrain$Churn)
table(cteste$Churn)


# Detalhando o mapa dos subsets.
#ctrain %>% map(table)
#cteste %>% map(table)


# Verificando que a proporção de valores da variável alvo manteve-se após split
prop.table(table(ctrain$Churn))
prop.table(table(cteste$Churn))



# Checagem das proporções cross com o quantitativo de cada subset, apenas para Churn. 
CrossTable(ctrain$Churn, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
CrossTable(cteste$Churn, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)


# Plotagem do quantitativo da variável Churn para o subset de treino.
ggplot(ctrain, aes(x = Churn)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            position=position_stack(vjust=0.5),
            size = 5) +
  labs(title = "Churn Train", 
       x = "Churn", 
       y = "Count")


# Plotagem do quantitativo da variável Churn para o subset de teste.
ggplot(cteste, aes(x = Churn)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            position=position_stack(vjust=0.5),
            size = 5) +
  labs(title = "Churn Test", 
       x = "Churn", 
       y = "Count")

 
## TRATAMENTOS ##

# listando os nomes das colunas e suas posições.
colnames(ctrain)
colnames(cteste)


# Remoção de variáveis pelo nome da variável.
ctrain$X  <- NULL
cteste$X  <- NULL


# Removendo as colunas pela posição da variável.
ctrain <- ctrain[ , -c(18, 19)]
cteste <- cteste[ , -c(18, 19)] 
 

# Gerando substes para LR Model.
ctrainLRM <- ctrain
ctesteLRM <- cteste

 
# Verificando estrutura.
str(ctrainLRM)
str(ctesteLRM)


# Gerando substes para RF Model.
ctrainRFM <- ctrain
ctesteRFM <- cteste


# Verificando estrutura.
str(ctrainRFM)
str(ctesteRFM)

 
# Gerando substes para DT Model.
ctrainDTM <- ctrain
ctesteDTM <- cteste


# Verificando estrutura.
str(ctrainDTM)
str(ctrainDTM)
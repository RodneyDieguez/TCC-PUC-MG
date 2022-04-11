# Importando dataset agrupado original.
churn <- read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
  
 
# Criando um dataset tipo raw para os tratamentos.
churn_raw <- churn
 

# Visualização de todo o dataset.
View(churn_raw)


# Dimensão do conjunto de dados.
dim(churn_raw)


# 5 linhas demonstrativas das 21 variáveis.
head(churn_raw)


# Listando as variáveis e a estrutura dos dados.  
str(churn_raw)


# Usando outra função para demonstrar os tipos de dados das 21 variáveis.
churn_raw %>% glimpse()


# Sumário com as informaçõs descritivas de cada variável. 
summary(churn_raw)

#---Destaque para a identificação de 11 valores NA (null/em branco) na coluna TotalCharges


# Obtendo tabela com indicadores para as variáveis numéricas.
profiling_num(churn_raw)


df_status(churn_raw)


Hmisc::describe(churn_raw)


# Gráficos para visualização das variáveis
churn_raw %>%  select ("gender", "SeniorCitizen", "Partner", "Dependents", "PhoneService", "MultipleLines", 
                         "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport",
                         "StreamingTV", "StreamingMovies", "Contract", "tenure" ,"PaperlessBilling", "PaymentMethod", "Churn")  %>% plot_bar()



##########------- Ajustes estéticos.

# Conferindo os nomes atuais.
colnames(churn_raw)


# Alteração das clunas customerID, gender e tenure.
names(churn_raw)[names(churn_raw) == 'customerID'] <- 'CustomerID'
names(churn_raw)[names(churn_raw) == 'gender'] <- 'Gender'
names(churn_raw)[names(churn_raw) == 'tenure'] <- 'Tenure'


# Reordenação da coluna Tenure da 6ª posição para 18ª posição.
churn_raw <- churn_raw %>% relocate(Tenure, .before = MonthlyCharges)


# Verificação se os nomes e a ordenação ficaram em conformidade.
colnames(churn_raw)

# Estrutura após alterações.
str(churn_raw)

# Sumarização após alterações.
summary(churn_raw)


####------------ Recoding and Cleaning



####------------ Missing Values

which(is.na(churn_raw$TotalCharges)) # showing indexes with NAs for TotalCharges

churn_raw[c(489,754,937,1083,1341,3332,3827,4381,5219,6671,6755),]

churn_raw  %>% subset(is.na(churn_raw[,"TotalCharges"]))

###---

# Existe algum valor NA na base? 
any(is.na(churn_raw))


# Listagem horizontal com somatório da quantidade
sapply(churn_raw, function(x) sum(is.na(x)))


# Variação para busca de missing values (listagem vertical de todas as colunas)
map(churn_raw$TotalCharges, ~sum (is.na(.)))


# Sabendo em qual coluna se encontram os valores NA (contagem simples)
sum(is.na(churn_raw$TotalCharges)) 


# Outra forma, gráfica, para identificar missing values 
plot_missing(churn_raw)
missmap(churn_raw,col=c("yellow","gray"))


# Também reconhece os valores NA e retornam sua proporção
# É forma variante para identificar se há colunas com valores zerados/NA
df_status(churn_raw)


# Confirmando a proporção de 0,16% para os 11 registros, com missing values
sum(is.na(churn_raw$TotalCharges))/nrow(churn_raw)


# Listando os 11 registros de TotalCharges com valor = NULL.
churn_raw[is.na(churn_raw$TotalCharges),]


# Buscar os 11 registros com dados das primeiras 5 colunas do conjunto e 
# colocando ao final as colunas Tenure TotalCharges e Churn (verificando se são churnners).
churn_raw[is.na(churn_raw$TotalCharges),c(1:5, 18, 20, 21)]


# Gerado dataset [churn_raw_new] sem nenhum missing value.
churn_raw_new <- na.omit(churn_raw)


# Existe algum valor NA na base? 
any(is.na(churn_raw_new ))


# Identificando se ainda constam colunas com valores NA para TotalCharges.
df_status(churn_raw_new)

# Verificando existência de Missing Values
missmap(churn_raw_new, main = "Missing values vs observed")



####------------ Recoding variáveis categóricas

# Coluna SeniorCitizen 

# Visualização simples da frequência dos dados antes do recondicionamento

# Disposição tabular horizontal, e identificação da variável
apply(churn_raw_new[c("SeniorCitizen")], 2, table)


# prop.table - aqui a verificação é com basse no percentual dos dados entre eles.
prop.table(table(churn_raw_new$SeniorCitizen))
prop.table(table(churn_raw_new$SeniorCitizen))*100


# troca dos valores
churn_raw_new$SeniorCitizen <- as.character(mapvalues(churn_raw_new$SeniorCitizen,
                                                      from=c("0","1"),
                                                      to=c("No", "Yes")))


# Visualização da frequência dos dados após recondicionamento
apply(churn_raw_new[c("SeniorCitizen")], 2, table)


# Detalhamento das frequências dos valores da coluna SeniorCitizen após recodificação, com apenas os valores Yes|No.
tab1(churn_raw_new$SeniorCitizen, sort.group = "decreasing", cum.percent = TRUE)


# Coluna Phone Services - MultipleLines

# Fazendo uma análise da frequência dos dados de cada variável
apply(churn_raw_new[c("PhoneService", "MultipleLines")], 2, table)


# Uma análise cruzada entre os dados das duas variáveis
CrossTable(churn_raw_new$PhoneService, 
           churn_raw_new$MultipleLines, 
           prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)


# Recodificando a resposta 'No phone service' para 'No'.
churn_raw_new$PhoneService <- as.character(mapvalues(churn_raw_new$PhoneService, 
                                                      from=c("No phone service"),
                                                      to=c("No")))

churn_raw_new$MultipleLines <- as.character(mapvalues(churn_raw_new$MultipleLines, 
                                                      from=c("No phone service"),
                                                      to=c("No")))

# Fazendo nova análise cruzada da frequência após recondicionamento.
apply(churn_raw_new[c("PhoneService", "MultipleLines")], 2, table)


# Detalhamento das frequências dos valores da coluna MultipleLines após recodificação, com apenas os valores Yes|No.
tab1(churn_raw_new$MultipleLines, sort.group = "decreasing", cum.percent = TRUE)



####------------ colunas Internet Services


# Verificando a frequência dos dados de serviço de internet.
CrossTable(churn_raw_new$InternetService, 
           prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

#--- Existem 1520 clientes que não possuem serviço de internet.


# Comparação das variáveis de serviço de internet para verificação de dependência
table(churn_raw_new[, c("InternetService", "OnlineSecurity")])

table(churn_raw_new[, c("InternetService", "OnlineBackup")])

table(churn_raw_new[, c("InternetService", "DeviceProtection")])

table(churn_raw_new[, c("InternetService", "TechSupport")])

table(churn_raw_new[, c("InternetService", "StreamingTV")])

table(churn_raw_new[, c("InternetService", "StreamingMovies")])



# As frequências ANTES da alteração, com 3 valores, as frequências DEPOIS com apenas 2 valores.
CrossTable(churn_raw_new$InternetService, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
CrossTable(churn_raw_new$OnlineSecurity, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
CrossTable(churn_raw_new$OnlineBackup, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
CrossTable(churn_raw_new$DeviceProtection, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
CrossTable(churn_raw_new$TechSupport, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
CrossTable(churn_raw_new$StreamingTV, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
CrossTable(churn_raw_new$StreamingMovies, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)


# Alterando todas as variáveis dependendtes trocando o valor 'No internet service' por 'No'.
churn_raw_new$OnlineSecurity[churn_raw_new$OnlineSecurity=="No internet service"] <- "No"
churn_raw_new$OnlineBackup[churn_raw_new$OnlineBackup=="No internet service"] <- "No"
churn_raw_new$DeviceProtection[churn_raw_new$DeviceProtection=="No internet service"] <- "No"
churn_raw_new$TechSupport[churn_raw_new$TechSupport=="No internet service"] <- "No"
churn_raw_new$StreamingTV[churn_raw_new$StreamingTV=="No internet service"] <- "No"
churn_raw_new$StreamingMovies[churn_raw_new$StreamingMovies=="No internet service"] <- "No"


# As frequências DEPOIS da alteração, com 2 valores.
CrossTable(churn_raw_new$InternetService, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

CrossTable(churn_raw_new$OnlineSecurity, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

CrossTable(churn_raw_new$OnlineBackup, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

CrossTable(churn_raw_new$DeviceProtection, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

CrossTable(churn_raw_new$TechSupport, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

CrossTable(churn_raw_new$StreamingTV, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

CrossTable(churn_raw_new$StreamingMovies, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)


# Visualização da frequência cruzada dos valores em tablução simples. 
table(churn_raw_new[, c("InternetService", "OnlineSecurity")])

table(churn_raw_new[, c("InternetService", "OnlineBackup")])

table(churn_raw_new[, c("InternetService", "DeviceProtection")])

table(churn_raw_new[, c("InternetService", "TechSupport")])

table(churn_raw_new[, c("InternetService", "StreamingTV")])

table(churn_raw_new[, c("InternetService", "StreamingMovies")])


# Verificando a estrutura do dataset após as alterações.
str(churn_raw_new)

# Após o recoding, todas as variáveis Demográficas e de Serviços ficaram com valores Yes|No para seus registros.
# Para algumas variáveis, a categorização não foi alterada, casos de:
# Gender (Male|Female) e InternetService (DSL|Fiber optic|No)
glimpse(churn_raw_new)


# Consulta simples se há algum valor NA em todo o dataset.
any(is.na(churn_raw_new))

 
####------------ Análises antes do recoding das Variáveis Numéricas

# Verificação de Outliers

#---
t <- hist(churn_raw_new$Tenure, 
          xlim=c(0,80),
          breaks=10, 
          main='Histograma Tenure', 
          xlab='Tenure',
          ylab='Frequência', 
          col = "#56B4E9")

text(t$mids, t$counts, labels=t$counts, adj=c(0.5, 0.99))

#---

mc <- hist(churn_raw_new$MonthlyCharges,
           xlim=c(0,120),
           breaks=5, 
           main='Histograma Monthly Charges',
           xlab='Monthly Charges',
           ylab='Frequência',
           col = "#56B4E9")

text(mc$mids, mc$counts, labels=mc$counts, adj=c(0.5, 1))

#---

tc <- hist(churn_raw_new$TotalCharges,
           xlim=c(0,10000),
           breaks=8,
           main='Histograma Total Charges', 
           xlab='Total Charges',
           ylab='Frequência', 
           col = "#56B4E9")

text(tc$mids, tc$counts, labels=tc$counts, adj=c(0.5,0.8))

#---

str(churn_raw_new)

# A correlação visa mensurar a relação entre as variáveis numéricas contínuas 
# do conjunto, a fim de evitar uma multicolinearidade na modelagem de regressão.

# Correlação Tenure X Monthly Charges X Total Charges

ggcorrplot(round(cor(churn_raw_new[,c(18, 19, 20)]),2), 
           title = "Matriz de Correlação", 
           hc.order=TRUE, 
           lab=TRUE, lab_size = 5,
           type = "upper") + 
  theme(plot.title=element_text(hjust = 0.5, size = 14), 
        axis.text.y = element_text(size = 11), 
        axis.text.x = element_text(size = 11), 
        legend.text = element_text(size = 9))


#---

# Gráficos de Barras com sobreposição dos valores YES | NO para Churn.

# Tenure X Churn.
ggplot(data = churn_raw_new, 
       aes(x = Tenure, 
           fill = Churn)) + 
  geom_histogram(colour = 'white',
                 position = 'stack') +
  scale_x_continuous(
    breaks = seq(0, 72, 12),
    limits=c(0, 72)) +
  ggtitle("Gráfico Tenure X Churn (sobreposição)") +
  labs(y = "Proportion")


# MonthlyCharges X Churn.
ggplot(data = churn_raw_new, 
       aes(x = MonthlyCharges, 
           fill = Churn)) + 
  geom_histogram(colour = 'white', 
                 position = 'stack') +
  scale_x_continuous(
    breaks = seq(0, 120, 15),
    limits=c(0, 120)) +
  ggtitle("Gráfico Monthly Charges X Churn (sobreposição)") +
  labs(y = "Proportion")

# TotalCharges X Churn.
ggplot(data = churn_raw_new, 
       aes(x = TotalCharges, 
           fill = Churn)) + 
  geom_histogram(colour = 'white', 
                 position = 'stack') +
                 scale_x_continuous(
                   breaks = seq(0, 8800, 1100),
                   limits=c(0, 8800)) +
  ggtitle("Gráfico Total Charges X Churn (sobreposição)") +
  labs(y = "Proportion")

#----
colnames(churn_raw_new)


####------------ Recoding Variáveis Numéricas

# Conferindo a estrutura do dataset após estratificação.
str(churn_raw_new)


# MonthlyCharges x TotalCharges

# Correlação numérica entre os valores TotalCharges e MonthlyCharges.
CORR_TotalXMonthly <- cor(churn_raw_new[c('MonthlyCharges','TotalCharges')], use= 'complete')
#---
CORR_TotalXMonthly.melted <- melt(CORR_TotalXMonthly)
#---
CORR_TotalXMonthly



# Gráfico da correlação.
churn_raw_new %>%
  dplyr::select (TotalCharges, MonthlyCharges) %>%
  cor() %>%
  corrplot.mixed(upper = "circle", 
                 tl.col = "black", 
                 number.cex = 0.7)


# Sumário da variável TotalCharges e MonthlyCharges.
summary(churn_raw_new$TotalCharges)

summary(churn_raw_new$MonthlyCharges)


#--- Verificando que há uma maior amplitude entre os valores totais do que entre os valores mensais.


# Verificando a estrutura do dataset após essas tratativas.
str(churn_raw_new)



####------------ Cleaning 

# Alteração do dataset para finalização.
churn_clean <- churn_raw_new


# Tenure

# Sumário da variável Tenure antes da estratificação.
summary(churn_clean$Tenure)


churn_clean %>%
  mutate(TenureYear = case_when(Tenure <= 12 ~ "0-1 ano",
                                Tenure > 12 & Tenure <= 24 ~ "1-2 anos",
                                Tenure > 24 & Tenure <= 36 ~ "2-3 anos",
                                Tenure > 36 & Tenure <= 48 ~ "3-4 anos",
                                Tenure > 48 & Tenure <= 60 ~ "4-5 anos",
                                Tenure > 60 & Tenure <= 72 ~ "5-6 anos")) -> churn_clean


# Verificando a estratificação.
# 1 - table.
table(churn_clean$TenureYear)


# 2 - CrossTable.
CrossTable(churn_clean$TenureYear, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)


# Alterando a posição da coluna TenureYear que ficou no final do dataset para a 18ª posição.
# E alterando Tenure para o final.
churn_clean <- churn_clean %>% relocate(TenureYear , .before = MonthlyCharges)
churn_clean <- churn_clean %>% relocate(Tenure, .after = Churn)

#---churn_raw_new <- churn_raw_new %>% relocate(TenureYear, .after = Nome_Coluna)


## CLEANING ##
# Remoção das colunas tratadas e que não serão mais necessárias para a modelagem. 

# Remoção da coluna CustomerID.
churn_clean$CustomerID <- NULL


# Remoção da coluna TotalCharges.
churn_clean$TotalCharges <- NULL


# Remoção da coluna Tenure.
churn_clean$Tenure <-NULL


# Dimensão do conjunto de dados após cleaning.
dim(churn_clean)


# Também reconhece os valores NA e retornam sua proporção
df_status(churn_clean)

Hmisc::describe(churn_clean)

profiling_num(churn_clean)
 

# Verificação da estrutura do novo dataset, tratado e limpo.
str(churn_clean)


#############-------------------

# FIM DO PRE PROCESSING

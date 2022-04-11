str(train)
 
# Convert as Numeric (No to 0 and Yes to 1)
train$Gender = as.numeric(as.factor( train$Gender)) - 1
train$SeniorCitizen = as.numeric(as.factor( train$SeniorCitizen)) - 1
train$Partner = as.numeric(as.factor( train$Partner)) - 1
train$Dependents = as.numeric(as.factor( train$Dependents)) - 1
train$PhoneService = as.numeric(as.factor( train$PhoneService)) - 1
train$MultipleLines = as.numeric(as.factor( train$MultipleLines)) - 1
train$OnlineSecurity = as.numeric(as.factor( train$OnlineSecurity)) - 1
train$OnlineBackup = as.numeric(as.factor( train$OnlineBackup)) - 1
train$DeviceProtection = as.numeric(as.factor( train$DeviceProtection)) - 1
train$TechSupport = as.numeric(as.factor( train$TechSupport)) - 1
train$StreamingTV = as.numeric(as.factor( train$StreamingTV)) - 1
train$StreamingMovies = as.numeric(as.factor( train$StreamingMovies)) - 1
train$PaperlessBilling = as.numeric(as.factor( train$PaperlessBilling)) - 1
train$Churn = as.numeric(as.factor( train$Churn)) - 1
 
 
# Convert as Factor
train$InternetService = as.factor( train$InternetService)
train$Contract = as.factor( train$Contract)
train$PaymentMethod = as.factor( train$PaymentMethod)
train$TenureYear = as.factor( train$TenureYear)


# Remoção de variáveis pelo nome da variável.
ctrain$MonthlyCharges  <- NULL
cteste$MonthlyCharges  <- NULL


# Removendo as colunas pela posição da variável.
ctrain <- ctrain[ , -c(18, 19)]
cteste <- cteste[ , -c(18, 19)] 

################

str(test)


# Convert character(No|Yes) == as.numeric (O|1) 
test$Gender = as.numeric(as.factor( test$Gender)) - 1
test$SeniorCitizen = as.numeric(as.factor( test$SeniorCitizen)) - 1
test$Partner = as.numeric(as.factor( test$Partner)) - 1
test$Dependents = as.numeric(as.factor( test$Dependents)) - 1
test$PhoneService = as.numeric(as.factor( test$PhoneService)) - 1
test$MultipleLines = as.numeric(as.factor( test$MultipleLines)) - 1
test$OnlineSecurity = as.numeric(as.factor( test$OnlineSecurity)) - 1
test$OnlineBackup = as.numeric(as.factor( test$OnlineBackup)) - 1
test$DeviceProtection = as.numeric(as.factor( test$DeviceProtection)) - 1
test$TechSupport = as.numeric(as.factor( test$TechSupport)) - 1
test$StreamingTV = as.numeric(as.factor( test$StreamingTV)) - 1
test$StreamingMovies = as.numeric(as.factor( test$StreamingMovies)) - 1
test$PaperlessBilling = as.numeric(as.factor( test$PaperlessBilling)) - 1
test$Churn = as.numeric(as.factor( test$Churn)) - 1

test$InternetService = as.factor( test$InternetService)
test$Contract = as.factor( test$Contract)
test$PaymentMethod = as.factor( test$PaymentMethod)
test$TenureYear = as.factor( test$TenureYear)


# Convert numeric (O|1) == as.character(YES|NO)

train$Churn <- as.character(train$Churn)
train$Churn[train$Churn=="0"] <- "No"
train$Churn[train$Churn=="1"] <- "Yes"

#---

test$Churn <- as.character(test$Churn)
test$Churn[test$Churn=="0"] <- "No"
test$Churn[test$Churn=="1"] <- "Yes"

 

## FATORIZAÇÃO ##
 
# 1 #  Fatorização de todas as variáveis

# Transformando os tipos de variáveis (numéricos e categóricos) em fatores (factor).
str(train)

# Criando um vetor com os nomes do dataframe.
train[] <- lapply(train, factor) 
col_names <- names(train)

# Aplicando em todo subset.
train[col_names] <- lapply(train[col_names], factor)

# Estrutura após 'fatorização'.
str(train)


# 2 # Fatorização apenas as com tipo = character (numeric == numeric as well)
sapply(churn, class)

churn <- as.data.frame(unclass(churn), 
                       stringsAsFactors = TRUE)

sapply(churn, class)


#---
churn1[sapply(churn1, is.character)] <- lapply(churn1[sapply(churn1, is.character)], 
                                               as.factor)






# https://datascienceplus.com/predict-customer-churn-logistic-regression-decision-tree-and-random-forest/
  
  
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}


churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))



###########################################################
# Binarized all variables

customer_churn_binarized_tbl <- churn_clean %>%
  mutate(MonthlyCharges) %>%
  binarize(n_bins = 6, 
           thresh_infreq = 0.01, 
           name_infreq = "OTHER", 
           one_hot = TRUE)

glimpse(customer_churn_binarized_tbl)


customer_churn_corr_tbl <- customer_churn_binarized_tbl %>%
  correlate(Churn__Yes)

customer_churn_corr_tbl

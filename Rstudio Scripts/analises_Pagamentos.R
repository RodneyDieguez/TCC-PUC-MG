## CONTRATOS X PAGAMENTOS 


ctrain
colnames(ctrain)
Contract
PaymentMethod
Churn


str(churn_clean)
colnames(churn_clean)


# "Gender"           "SeniorCitizen"    "Partner"          "Dependents"      
# "PhoneService"     "MultipleLines"    "InternetService"  "OnlineSecurity"  
# "OnlineBackup"     "DeviceProtection" "TechSupport"      "StreamingTV"     
# "StreamingMovies"  "Contract"         "PaperlessBilling" "PaymentMethod"   
# "Tenure"           "MonthlyCharges"   "TotalCharges"     "Churn"    


#----------

display.brewer.all()


###############################################

# Plotagem com representação de churners para o contratos x PaymentMethod.
ggplot(churn_clean, aes(x = Contract, y = PaymentMethod, color = Churn)) +
  geom_jitter(width = .2) +
  facet_wrap(~ Churn)


# Plotagem com representação de churners para o contratos x PaperlessBilling.
ggplot(churn_clean, aes(x = Contract, y = PaperlessBilling, color = Churn)) +
  geom_jitter(width = .2) +
  facet_wrap(~ Churn)


# Plotagem com representação de churners para PaymentMethod x PaperlessBilling.
ggplot(churn_clean, aes(x = PaymentMethod, y = PaperlessBilling, color = Churn)) +
  geom_jitter(width = .2) +
  facet_wrap(~ Churn)

 

# CROSS TABLE

tab1(churn_clean$Contract, 
     sort.group = "decreasing", 
     #brewer.pal(2, 'Pastel1'),
     cum.percent = TRUE)


tab1(churn_clean$PaymentMethod, 
     sort.group = "decreasing", 
     #brewer.pal(2, 'Pastel1'),
     cum.percent = TRUE)

 
tab1(churn_clean$PaperlessBilling, 
     sort.group = "decreasing", 
     #brewer.pal(2, 'Pastel1'),
     cum.percent = TRUE)




# Uma análise cruzada entre os dados das duas variáveis
CrossTable(churn_clean$Churn, 
           churn_clean$PaymentMethod, 
           prop.t=TRUE, prop.r=FALSE, prop.c=FALSE)


#---


# Valores agregados por Soma do TotalCharges e Churn X SeniorCitizen.
aggregate(Churn ~ PaymentMethod + PaperlessBilling, 
          data = churn_clean, 
          FUN = length)


#---

# sumarização dos valores deadicionais.
summary(filter(churn_clean, PaymentMethod == "Electronic check" & Churn == "Yes"))

summary(filter(churn_clean, OnlineSecurity == "Yes" & Churn == "Yes"))


summary(filter(churn_clean, TechSupport == "Yes" & OnlineSecurity == "Yes" & Churn == "Yes"))

summary(filter(churn_clean, TechSupport == "No" & OnlineSecurity == "No" & Churn == "Yes"))

#---


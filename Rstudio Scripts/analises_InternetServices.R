## INTERNET SERVICES  

churn_clean
colnames(churn_clean)
Contract
PaperlessBilling
PaymentMethod
InternetService  
MonthlyCharges
TotalCharges

Churn


display.brewer.all()


###############################################



#---

# Demonstração agregada de churners para os serviços de Internet DSL X Fibra Óptica 
# confrontando comm dados de contrato, forma de pagamento, média e máximo de gastos e média de tempo de fidelização.

summary(filter(churn_clean, InternetService == "DSL" & Churn == "Yes"))

summary(filter(churn_clean, InternetService == "Fiber optic" & Churn == "Yes"))


#---


# Plotagem com representação de churners para o InternetService.

Internet_churners <- ggplot(churn_clean, aes(x = InternetService, 
                                             y = Churn, 
                                             color = Churn)) + 
  geom_jitter(width = .25) 


ggarrange(Internet_churners, widths = c(1,1))


 
# Plotagem com representação de churners para o InternetService X PhoneService.
ggplot(churn_clean, aes(x = PhoneService, y = InternetService, color = Churn)) +
  geom_jitter(width = .2) +
  facet_wrap(~ Churn)

#---

# Uma análise cruzada entre os dados das duas variáveis
CrossTable(churn_clean$PhoneService, 
           churn_clean$InternetService,
           prop.t=FALSE, prop.r=FALSE, prop.c=FALSE)



# Uma análise cruzada entre os dados das duas variáveis
CrossTable(#churn_clean$Churn, 
  churn_clean$InternetService,
  churn_clean$Contract,
  prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)


#---

# Valores agregados por Média de TotalCharges e Churn  X InternetService.
aggregate(TotalCharges ~ Churn + InternetService, 
          data = churn_clean, 
          FUN = mean)

# Valores agregados por Soma de TotalCharges e Churn  X InternetService.
aggregate(TotalCharges ~ Churn + InternetService, 
          data = churn_clean, 
          FUN = sum)


# Valores agregados por Soma de TotalCharges e Contrato X InternetService.
aggregate(TotalCharges ~ Contract + InternetService, 
          data = churn_clean, 
          FUN = sum)


#---

# Valores agregados por Média de MonthlyCharges e Churn  X InternetService.
aggregate(MonthlyCharges ~ Churn + InternetService,  
          data = churn_clean, 
          FUN = mean)

# Valores agregados por Soma de MonthlyCharges e Churn X InternetService.
aggregate(MonthlyCharges ~ Churn + InternetService,  
          data = churn_clean, 
          FUN = sum)

# Valores agregados por Soma de MonthlyCharges e Contrato X InternetService.
aggregate(MonthlyCharges ~ Contract + InternetService, 
          data = churn_clean, 
          FUN = sum)

 
#---


# Valores agregados por quantidade de Contrato e Churn X InternetService.
aggregate(Contract ~ Churn + InternetService,  
          data = churn_clean, 
          FUN = length)

# Valores agregados por quantidade de Contrato e PaymentMethod X InternetService.
aggregate(PaymentMethod ~ Churn + InternetService,  
          data = churn_clean, 
          FUN = length)

# Valores agregados por quantidade de Contrato e PaperlessBilling X InternetService.
aggregate(PaperlessBilling ~ Churn + InternetService, 
          data = churn_clean, 
          FUN = length)

# Valores agregados por quantidade de Contrato e PhoneService X InternetService.
aggregate(Churn ~ PhoneService + InternetService, 
          data = churn_clean, 
          FUN = length)




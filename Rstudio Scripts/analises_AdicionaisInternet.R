## INTERNET SERVICES ADICIONAIS 

churn_clean
colnames(churn_clean)
"InternetService"  
"OnlineSecurity"   
"OnlineBackup"     
"DeviceProtection"
"TechSupport"     
"StreamingTV"      
"StreamingMovies" 
Churn

  
display.brewer.all()


###############################################


# Plotagem com representação de churners para o adicionais de Internet.
p1 <- ggplot(churn_clean, aes(x = OnlineSecurity, y = Churn, color = Churn)) +
  geom_jitter(width = .25) 
p2 <- ggplot(churn_clean, aes(x = OnlineBackup, y = Churn, color = Churn)) +
  geom_jitter(width = .25)
p3 <- ggplot(churn_clean, aes(x = TechSupport, y = Churn, color = Churn)) +
  geom_jitter(width = .25)
p4 <- ggplot(churn_clean, aes(x = DeviceProtection, y = Churn, color = Churn)) +
  geom_jitter(width = .25)

ggarrange(p1, p2, p3, p4, widths = c(4,4))


#---

tab1(churn_clean$OnlineSecurity,  
     sort.group = "decreasing", 
     #brewer.pal(2, 'Pastel1'),
     cum.percent = TRUE)

tab1(churn_clean$TechSupport,  
     sort.group = "decreasing", 
     #brewer.pal(2, 'Pastel1'),
     cum.percent = TRUE)

#---

# sumarização dos valores adicionais.
summary(filter(churn_clean, TechSupport == "Yes" & Churn == "Yes"))

summary(filter(churn_clean, OnlineSecurity == "Yes" & Churn == "Yes"))


summary(filter(churn_clean, TechSupport == "Yes" & OnlineSecurity == "Yes" & Churn == "Yes"))

summary(filter(churn_clean, TechSupport == "No" & OnlineSecurity == "No" & Churn == "Yes"))

#---




# Valores agregados por Churn X todos os atributos.
aggregate(.~ TechSupport, 
          data = churn_clean, 
          FUN = sum)


# Valores agregados por Churn X todos os atributos.
aggregate(.~ OnlineSecurity, 
          data = churn_clean, 
          FUN = sum)

#---


# Valores agregados por Soma do TotalCharges e Churn X SeniorCitizen.
aggregate(Churn ~ TechSupport + OnlineSecurity, 
          data = churn_clean, 
          FUN = length)
 
 

# Valores agregados por Média de TotalCharges e TechSupport  X OnlineSecurity.
aggregate(TotalCharges ~ Churn + TechSupport + OnlineSecurity, 
          data = churn_clean, 
          FUN = mean)


# Valores agregados por Média de Tenure e TechSupport  X OnlineSecurity.
aggregate(Tenure ~ Churn + TechSupport + OnlineSecurity, 
          data = churn_clean, 
          FUN = mean)


#---





# Uma análise cruzada entre os dados das duas variáveis
CrossTable(churn_clean$Churn, 
           churn_clean$TechSupport,
           prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)



# Uma análise cruzada entre os dados das duas variáveis
CrossTable(churn_clean$Churn, 
           churn_clean$OnlineSecurity,
           prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)




####################################################################
# CONCLUSAO PHONE SERVICE ANALISES

colnames(ctrain)

ctrain
colnames(ctrain)
Tenure
MonthlyCharges
PhoneService
MultipleLines
Churn
 
display.brewer.all()


# Uma análise cruzada entre os dados das duas variáveis
CrossTable(ctrain$Churn, 
           ctrain$PhoneService, 
           prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)



# Plotagem com representação de churners para o PhoneService X MultipleLines usando apenas como referência o subset de treino.

p1 <- ggplot(ctrain, aes(x = PhoneService, y = Churn, color = Churn)) +
   geom_jitter(width = .25) 

p2 <- ggplot(ctrain, aes(x = MultipleLines, y = Churn, color = Churn)) +
   geom_jitter(width = .25)

ggarrange(p1, p2, widths = c(2,2)) 


#---

#---
# Valores agregados pela Média do tempo de fidelização.
aggregate(Tenure ~ MultipleLines + PhoneService, 
          data = churn_clean, 
          FUN = mean)
>
  MultipleLines PhoneService   Tenure
1            No           No 31.83088
2            No          Yes 24.17046
3           Yes          Yes 41.97101

# A média de fidelização é maior para clientes possuidores de dmais de uma linha.
# Demonstrando que proporcionalmente há menos churners quando o cliente tem mais de uma linha.


#---

# Valores agregados pela Média do tempo de fidelização associada ao tipo de contrato.

# Para PhoneService.
aggregate(Tenure ~ PhoneService + Contract, 
          data = churn_clean, 
          FUN = mean)
>
  PhoneService       Contract   Tenure
1           No Month-to-month 18.65426
2          Yes Month-to-month 17.97028
3           No       One year 39.67586
4          Yes       One year 42.33534
5           No       Two year 55.83648
6          Yes       Two year 57.20052

# Clientes com contratos mensais tem redução de cerca de 43% na média de fidelização em comparação aos contratos de 1 ano (linhas 2 e 4)
# e entre os contratos de 2 anos, a redução da média é da ordem de 70% (linhas 2 e 6). 

# Para MultipleLines.
aggregate(Tenure ~ MultipleLines + Contract, 
          data = churn_clean, 
          FUN = mean)
>
  MultipleLines       Contract   Tenure
1            No Month-to-month 13.06853
2           Yes Month-to-month 26.05870
3            No       One year 35.63668
4           Yes       One year 51.01786
5            No       Two year 51.08333
6           Yes       Two year 62.69505

# Clientes com contratos mensais tem redução de cerca de 50% na média de fidelização em comparação aos contratos de 1 ano (linhas 2 e 4)
# e entre os contratos de 2 anos, a redução da média é de 58% (linhas 2 e 6). 


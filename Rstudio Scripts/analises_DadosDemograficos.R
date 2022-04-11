
####################################################################
# CONCLUSAO DADOS DEMOGRAFICOS
 

# Valores agregados por Churn X todos os atributos.
aggregate(.~ Churn , 
          data = churn_clean, 
          FUN = sum)


#---


# Soma do total de encargos pagos agrupados por Churn.
aggregate(TotalCharges ~ Churn + Churn, 
          data = churn_clean, 
          FUN = sum)
>
  Churn TotalCharges
1    No     13193242
2   Yes      2862927

# Editado e utilizado o dataset [churn_clean] para obter o total de registros e sem valores nulos. 


#---


# Valores agregados por Soma do TotalCharges e Churn X SeniorCitizen.
aggregate(TotalCharges ~ Churn + SeniorCitizen, 
          data = churn_clean, 
          FUN = sum)
>
  Churn SeniorCitizen TotalCharges
1    No            No   10866095.7
2   Yes            No    1980521.8
3    No           Yes    2327146.1
4   Yes           Yes     882405.2

# Indivíduos não idosos são mais propensos a churn, e com tiveram gasto total com cobranças quase 4 vezes maior do que clientes idosos. 
# O segmento de clientes churners não idosos tem uma participação de 12,33% da composição dos valores totais.

#---

# Valores agregados por Soma do TotalCharges e Churn X Partner.
aggregate(TotalCharges ~ Churn + Partner, 
          data = churn_clean, 
          FUN = sum)
>
  Churn Partner TotalCharges
1    No      No      4460895
2   Yes      No      1306776
3    No     Yes      8732347
4   Yes     Yes      1556151

# Indivíduos sem parceiros são mais propensos a churn, e tiveram um gasto total com cobranças quase 2 vezes menor do que clientes com parceiros.
# O segmento de clientes churners sem parceiros tem uma participação de 8,14% da composição de valores totais gastos.

#---

# Valores agregados por Soma do TotalCharges e Churn X Dependents.
aggregate(TotalCharges ~ Churn + Dependents, 
          data = churn_clean, 
          FUN = sum)
>
  Churn Dependents TotalCharges
1    No         No    8530129.8
2   Yes         No    2261840.0
3    No        Yes    4663112.0
4   Yes        Yes     601086.9

# Indivíduos sem dependentes são mais propensos a churn, e tiveram um gasto total com cobranças 2 vezes maior do que clientes com parceiros.
# O segmento de clientes churners sem dependentes tem uma participação de 14,09% da composição de valores totais gastos.



#---

summary(filter(churn_clean, Churn == "Yes"))

summary(filter(churn_clean, Churn == "No"))



#---

# Valores agregados por Soma de TotalCharges X SeniorCitizen.
aggregate(TotalCharges ~ SeniorCitizen,
          data = churn_clean, 
          FUN = sum)


# Valores agregados por Soma de TotalCharges X Partner.
aggregate(TotalCharges ~ Partner,
          data = churn_clean, 
          FUN = sum)


# Valores agregados por Soma de TotalCharges X Dependents.
aggregate(TotalCharges ~ Dependents,
          data = churn_clean, 
          FUN = sum)



#---

# Valores agregados por Soma de TotalCharges e Churn  X SeniorCitizen.
aggregate(TotalCharges ~ Churn + SeniorCitizen, 
          data = churn_clean, 
          FUN = sum)


# Valores agregados por Soma de TotalCharges e Churn  X Partner.
aggregate(TotalCharges ~ Churn + Partner, 
          data = churn_clean, 
          FUN = sum)


# Valores agregados por Soma de TotalCharges e Churn  X Dependents.
aggregate(TotalCharges ~ Churn + Dependents, 
          data = churn_clean, 
          FUN = sum)



#---


# Valores agregados por Média de TotalCharges e Churn  X SeniorCitizen.
aggregate(TotalCharges ~ Churn + SeniorCitizen, 
          data = churn_clean, 
          FUN = mean)


# Valores agregados por Média de TotalCharges e Churn  X Partner.
aggregate(TotalCharges ~ Partner + Churn, 
          data = churn_clean, 
          FUN = mean)


# Valores agregados por Média de TotalCharges e Churn  X Dependents.
aggregate(TotalCharges ~ Dependents + Churn, 
          data = churn_clean, 
          FUN = mean)

#---

# Todos as tuplas de possibilidades de cenários demográficos X somatório de TotalCharges
aggregate(TotalCharges ~ Dependents + Partner + SeniorCitizen + Churn, 
          data = churn_clean, 
          FUN = sum)







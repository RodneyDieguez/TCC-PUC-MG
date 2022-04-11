# Churn

# A coluna Churn é categórica com valores Yes|No e para alguma modelagem será conveniente que os dados estejam 
# em formato binomial de 0|1. A fim de gerar condições para essa situação, adicionada uma coluna ChurnBin com esses valores.


# Criando uma nova coluna atribuindo valores 0|1 para No|Yes
churn_clean$ChurnBin = 0
churn_clean$ChurnBin[churn_clean$Churn == "Yes"] = 1
churn_clean$ChurnBin[churn_clean$Churn == "No"] = 0


# CrossTable entre Churn e ChurnBin.
CrossTable(churn_clean$Churn, prop.t=FALSE, prop.r=FALSE, prop.c=FALSE)
CrossTable(churn_clean$ChurnBin, prop.t=FALSE, prop.r=FALSE, prop.c=FALSE)

 
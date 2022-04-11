### EDA POS CLEANING

   
# Antes da modelagem dos dados, um exame sobre as estatísticas descritivas dos dados após cleaning.

# Simples conferência do dataset. 
plot_intro(churn_clean)


# Aplicando describe aos dados do dataset. 
describe(churn_clean)


# Informações do dataset.
churn_clean %>% glimpse()

churn_clean %>% map(table)

churn_clean %>% str()


# Dimensão do dataset. 
dim(churn_clean)


# Sumário com as informações descritivas de cada variável.
churn_clean %>% summary()


# Simples contagem de conferência entre as variáveis. 
# count(churn_clean,c('Churn', 'outra_coluna'))

################---------------------

# ANALISE GRAFICOS

# Sobre a variável alvo: Churn.

# Plotagem com uma visualização simples da composição dos valores (YES | NO). 
ggplot(churn_clean, aes(x = Churn)) +
  geom_bar(aes(fill = Churn)) +
  labs(title = "Customer Churn", 
       x = "Churn", 
       y = "Count")


# Plotagem dos dados quantitativos dos clientes churners e não-churners.
ggplot(churn_clean, aes(x = Churn)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            position=position_stack(vjust=0.5)) +
  labs(title = "Customer Churn Quantitative", 
       x = "Churn", 
       y = "Count")


# Plotagem dos dados percentuais dos clientes churners e não-churners.
ggplot(churn_clean, aes(x = Churn)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title = "Customer Churn Proportion", 
       x = "Churn", 
       y = "Frequência")

###---

# Compartivo entre os dados das variáveis de interesse X Churn.

###---

# Estabelecer visualização sobre clientes churners para cada variável.

# DADOS DEMOGRÁFICOS

# Gênero X Churn
ggplot(churn_clean,aes(x = Gender, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.8), 
            size = 4) +
  labs(title = "Customer Gender Churn Status", 
       x = "What is the gender of the customers?", 
       y = "Count")


# Idoso X Churn
ggplot(churn_clean,aes(x = SeniorCitizen, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.8), 
            size = 4) +
  labs(title = "Customer Senior Churn Status", 
       x = "Is the customer senior?", 
       y = "Count")


# Parceiro X Churn
ggplot(churn_clean,aes(x = Partner, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.8), 
            size = 4) +
  labs(title = "Customer Partner Churn Status", 
       x = "Does the Customer have a Partner?", 
       y = "Count")


# Dependentes X Churn
ggplot(churn_clean, aes(Dependents, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.8), 
            size = 4) +
  labs(title = "Customer Dependents Churn Status", 
       x = "Does the Customer have Dependents?", 
       y = "Count")



# DADOS SERVIÇOS #

# Compartivo entre dados serviços telefônicos e Churn.


# Telefonia X Churn
ggplot(churn_clean, aes(PhoneService, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.99), 
            size = 4) +
  labs(title = "Customer PhoneService Churn Status", 
       x = "Does the Customer have Phone Service?", 
       y = "Count")


# Mais de 1 Linha  X Churn
ggplot(churn_clean, aes(MultipleLines, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.8), 
            size = 4) +
  labs(title = "Customer MultipleLines Churn Status", 
       x = "Does the Customer have Multiple Phone Lines?", 
       y = "Count")


# Compartivo entre dados serviços internet e Churn.


# Serviço de Internet X Churn
ggplot(churn_clean, aes(InternetService, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.99), 
            size = 4) +
  labs(title = "Customer InternetService Churn Status", 
       x = "What type of internet does the customer have?", 
       y = "Count")


# Serviços adicionais de internet X Churn
ggplot(churn_clean, aes(OnlineSecurity, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.99), 
            size = 4) +
  labs(title = "Customer OnlineSecurity Churn Status", 
       x = "Does the customer have this additional service?", 
       y = "Count")



# Serviços adicionais de internet X Churn
ggplot(churn_clean, aes(OnlineBackup, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.99), 
            size = 4) +
  labs(title = "Customer OnlineBackup Churn Status", 
       x = "Does the customer have this additional service?",
       y = "Count")


# Serviços adicionais de internet X Churn
ggplot(churn_clean, aes(TechSupport, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.99), 
            size = 4) +
  labs(title = "Customer TechSupport Churn Status", 
       x = "Does the customer have this additional service?",
       y = "Count")



# Serviços adicionais de internet X Churn
ggplot(churn_clean, aes(StreamingTV, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.99), 
            size = 4) +
  labs(title = "Customer StreamingTV Churn Status", 
       x = "Does the customer have this additional service?",
       y = "Count")



# Serviços adicionais de internet X Churn
ggplot(churn_clean, aes(StreamingMovies, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.99), 
            size = 4) +
  labs(title = "Customer StreamingMovies Churn Status", 
       x = "Does the customer have this additional service?",
       y = "Count")

###################################

# Serviços contratuais X Churn
ggplot(churn_clean, aes(Contract, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.99), 
            size = 4) +
  labs(title = "Customer Contract Churn Status", 
       x = "What type of contract does the customer have?",
       y = "Count")

# Serviços conta online X Churn
ggplot(churn_clean, aes(PaperlessBilling, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(.99), 
            size = 4) +
  labs(title = "Customer PaperlessBilling Churn Status", 
       x = "What type of billing service the customer have?",
       y = "Count")


# Serviços tipo de pagamento X Churn
ggplot(churn_clean, aes(PaymentMethod, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position = position_stack(1), 
            size = 4) +
  labs(title = "Customer PaymentMethod Churn Status", 
       x = "What type of payment method the customer have?",
       y = "Count")

# DADOS FIDELIZAÇÃO

# Distribuição da frequência de clientes entre os tempos de contrato estratificados
# Grafico com as frequencias por ano.
ggplot(churn_clean, aes(churn_clean$TenureYear)) +
  geom_bar(fill = "#56B4E9") +
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.1) +
  theme_classic() +
  labs(title = "Customer Tenure Status", 
       x = "TenureYear", 
       y = "Count")


# Compartivo percentual entre Tenure X Churn.

# Plotagem para percentual de clientes por Tenure Year.
ggplot(churn_clean, aes(x = TenureYear)) +
  geom_bar(aes(fill = Churn), colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 4) +
  labs(title = "Customer Tenure Status X Churn", 
       x = "TenureYear", 
       y = "Count")


# Compartivo quantitativo de churners para cada ano.

# Plotagem do total de churners por Tenure Year.
ggplot(churn_clean, aes(TenureYear, fill = Churn)) +
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(label = ..count..),
            stat = "count",
            position=position_stack(vjust=0.5),
            size = 4) +
  labs(title = "Customer Tenure Status X Churn", 
       x = "TenureYear", 
       y = "Proportion")

# Plotagem do percentual de clientes por ano X Churn.
ggplot(churn_clean,aes(x = TenureYear, fill = Churn)) + 
  geom_bar(position = "stack", colour = 'white') +
  geom_text(aes(y = ..count.. -200, 
            label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count',
            position=position_stack(vjust=0.99), 
            size = 4) +
  labs(title = "Customer Tenure Status X Churn", 
       x = "TenureYear", 
       y = "Proportion")

####--- GASTOS MENSAIS

# Compartivo quantitativo de churners pelos gastos mensais.

# MonthlyCharges distributions separated by categorical features.
ggplot(data = churn_clean, 
       aes(x = MonthlyCharges, 
           fill = Churn)) +
  scale_x_continuous(
    breaks = seq(0, 120, 10),
    limits=c(0, 120)) +  
  geom_histogram(colour = 'white') + 
labs(title = "Customer Charges X Churn", 
     x = "MonthlyCharges", 
     y = "Proportion")


# Verificando informçãoes estatísticas sobre a variável.
profiling_num(churn_clean)


# Média de MonthlyCharges por tipo de Churn.
data_means <- aggregate(churn_clean$MonthlyCharges,                       # Means by group
                        list(churn_clean$Churn),
                        mean)
data_means


# Boxplot para média de TotalCharges.
options(repr.plot.width = 6, repr.plot.height = 2)

ggplot(churn_clean, aes(x="Churn", y=MonthlyCharges)) +
  geom_boxplot(aes(fill=Churn)) +
  scale_fill_brewer(palette="Set1") +
  labs(title = "Monthly Charges Average", 
       x = "MonthlyCharges", 
       y = "Proportion")

 
## FIM ##



 




churn_clean1 <- churn_clean


ctrainLRM
ctesteLRM


str(churn_clean1)

colnames(churn_clean1)

glimpse(ctrain)
 

####################################################################


(head(churn_clean1[,1:20],3))


display.brewer.all()


colnames(ctrainLRM)

display.brewer.all()

brewer.pal(2, 'Paired')
brewer.pal(2, 'Pastel1')
brewer.pal(2, 'Spectral')

####################################################################


str(churn_clean1)
colnames(churn_clean1)

#----------

# colnames(churn_clean1)
# "Gender"           "SeniorCitizen"    "Partner"          "Dependents"      
# "PhoneService"     "MultipleLines"    "InternetService"  "OnlineSecurity"  
# "OnlineBackup"     "DeviceProtection" "TechSupport"      "StreamingTV"     
# "StreamingMovies"  "Contract"         "PaperlessBilling" "PaymentMethod"   
# "Tenure"           "MonthlyCharges"   "TotalCharges"     "Churn"    


#----------


####################################################################

# CROSS TABLE

prop.table(table(ctrain$MultipleLines))

tab1(ctrain$MultipleLines, 
     sort.group = "decreasing", 
     cum.percent = TRUE)


# Uma análise cruzada entre os dados das duas variáveis
CrossTable(ctrain$Churn, 
           ctrain$PhoneService, 
           prop.t=TRUE, prop.r=FALSE, prop.c=FALSE)


# Uma análise cruzada entre os dados das duas variáveis
CrossTable(ctrain$Churn, 
           ctrain$MultipleLines, 
           prop.t=TRUE, prop.r=FALSE, prop.c=FALSE)

# Comparação das variáveis de serviço de internet para verificação de dependência
table(churn_clean[, c("Churn", "PhoneService")])

table(churn_clean[, c("Churn", "MultipleLines")])


tab1(churn_clean$PhoneService, 
     sort.group = "decreasing", 
     cum.percent = TRUE)


tab1(churn_clean$MultipleLines, 
     sort.group = "decreasing", 
     cum.percent = TRUE)



####################################################################
# PLOT TABLE


plot(table(churn_clean$Contract, churn_clean$Churn), 
     col=c('gray','red'), 
     border = 'white', 
     main = "Customer Churn")



plot(table(ctrain$PhoneService, ctrain$MultipleLines), 
     col=brewer.pal(2, 'Paired'), 
     border = 'white', main = "Customer Churn")

plot(table(ctrain$TechSupport, ctrain$Contract), 
     col=c('gray','orange'), 
     border = 'white', main = "Customer Churn")


plot(table(churn$Contract, churn$Churn), 
     col=c('gray','orange'), 
     border = 'white', main = "Customer Churn")


plot(table(churn$TechSupport, churn$Contract, churn$Churn), 
     col=c('gray','orange', 'red'), 
     border = 'white', main = "Customer Churn")


plot(table(ctrain$PhoneService, ctrain$MultipleLines), 
     col=brewer.pal(2, 'Paired'), 
     border = 'white', main = "Customer Churn")


plot(table(ctrain$Churn, ctrain$MultipleLines), 
     col=brewer.pal(2, 'Set1'), 
     border = 'white', 
     main = "Customer Churn")



plot(table(churn_clean$Contract, churn_clean$Churn), 
     col=c('gray','red'), 
     border = 'white', 
     main = "Customer Churn")
#--

####################################################################
# SURVIVE PLOT 

colnames(churn_clean1)

churn_clean1$Churn <- ifelse(churn_clean1$Churn=='Yes',1,0 )

data_surv <- Surv(churn_clean1$Tenure, churn_clean1$Churn)

fit <- survfit(data_surv ~ InternetService, 
               data = churn_clean1)

ggsurvplot(fit,
           data = churn_clean1,
           size = 1,                             # change line size
           palette = brewer.pal(3, "Paired"), # custom color palettes
           conf.int = TRUE,                      # Add confidence interval
           pval = TRUE,                          # Add p-value
           risk.table = TRUE,                    # Add risk table
           risk.table.col = "strata",            # Risk table color by groups
           legend.labs = c("No", "DSL", "Fiber"),            # Change legend labels
           risk.table.height = 0.25,             # Useful to change when you have multiple groups
           ggtheme = theme_bw()                  # Change ggplot2 theme
)




churn_clean1$Churn <- ifelse(churn_clean1$Churn=='Yes', 1, 0)

data_surv <- Surv(churn_clean1$Tenure, churn_clean1$Churn)


fit <- survfit(data_surv ~ InternetService, 
               data = churn_clean1)

ggsurvplot(fit,
           data = churn_clean1,
           size = 1,                             # change line size
           palette = brewer.pal(3, 'Spectral'),
           #palette = c("#E7B800", "#2E9FDF", '#6C8EBF'), # custom color palettes
           conf.int = TRUE,                      # Add confidence interval
           pval = TRUE,                          # Add p-value
           risk.table = TRUE,                    # Add risk table
           risk.table.col = "strata",            # Risk table color by groups
           legend.labs = c("No", "DSL", "Fiber"),            # Change legend labels
           risk.table.height = 0.25,             # Useful to change when you have multiple groups
           ggtheme = theme_bw() ,                 # Change ggplot2 theme
           check.names = TRUE
)


fit <- survfit(data_surv ~ Gender   , 
               data = churn_clean1)

ggsurvplot(fit, 
           data = churn_clean1, 
           pval = TRUE, 
           conf.int = TRUE)

#------



####################################################################
# GGPLOT GEOM HEX

ctrainLRM%>%
  ggplot(aes(Tenure, MonthlyCharges )) +
  geom_hex(aes(fill = Churn),
           binwidth = c(1, 1)) +
  coord_fixed() +
  labs(title='Churn de clientes com Internet por tipo de Contrato',
       subtitle = 'Custo mensal para Internet Service por Contrato',
       x='Números de meses que o cliente permaneceu na empresa',
       y='Valor MonthlyCharges')+
  theme_bw()+
  facet_wrap(~ MultipleLines)


ctrainLRM%>%
  ggplot(aes(Tenure, MonthlyCharges )) +
  geom_hex(aes(fill = Churn),
           binwidth = c(1, 1)) +
  coord_fixed() +
  labs(title='Churn de clientes com Internet por tipo de Contrato',
       subtitle = 'Custo mensal para Internet Service por Contrato',
       x='Números de meses que o cliente permaneceu na empresa',
       y='Valor MonthlyCharges')+
  theme_bw()+
  facet_wrap(~ PhoneService)


ctrain%>%
  ggplot(aes(SeniorCitizen, TotalCharges))+
  geom_hex(aes(fill = Churn), bins=75)+
  labs(title='Diferenças entre Internet Service e a manutenção dos clientes',
       subtitle = 'Custo com Fibra Ótica entre clientes',
       x='Números de meses que o cliente permaneceu na empresa',
       y='MultipleLines')+
  theme_bw()+
  facet_wrap(~ Churn)


colnames(ctrainLRM)


ctrain%>%
  ggplot(aes(Tenure, TotalCharges))+
  geom_hex(aes(fill = Churn), bins=50)+
  labs(title='Diferenças entre Internet Service e a manutenção dos clientes',
       subtitle = 'Custo com Fibra Ótica entre clientes',
       x='Números de meses que o cliente permaneceu na empresa',
       y='Total Charges')+
  theme_bw()+
  facet_wrap(~InternetService)


ctrainLRM%>%
  ggplot(aes(Tenure, TotalCharges)) +
  geom_hex(aes(fill = Churn), bins=50) +
  labs(title='Churn de clientes por tipo de contrato e custo total',
       subtitle = 'Custo total entre tipos de contratos',
       x='Números de meses que o cliente permaneceu na empresa',
       y='Valor Total Charges')+
  theme_bw()+
  facet_wrap(~Contract)


ctrainLRM%>%
  ggplot(aes(Tenure, SeniorCitizen))+
  geom_hex(aes(fill = Churn), 
           bins=70)+
  labs(title='Churn de clientes idosos por tipo de Contrato',
       subtitle = '',
       x='Números de meses que o cliente permaneceu na empresa',
       y='SeniorCitizen')+
  theme_bw()+
  facet_wrap(~Contract)


ctrainLRM%>%
  ggplot(aes(Tenure, MonthlyCharges )) +
  geom_hex(aes(fill = Churn),
           binwidth = c(1, 1)) +
  coord_fixed() +
  labs(title='Churn de clientes com Internet por tipo de Contrato',
       subtitle = 'Custo mensal para Internet Service por Contrato',
       x='Números de meses que o cliente permaneceu na empresa',
       y='Valor MonthlyCharges')+
  theme_bw()+
  facet_wrap(~ InternetService)



ctrainLRM%>%
  ggplot(aes(Tenure, MonthlyCharges)) +
  geom_hex(aes(fill = Churn), bins=75) +
  labs(title='Churn de clientes por tipo de contrato e custo total',
       subtitle = 'Custo total entre tipos de contratos',
       x='Números de meses que o cliente permaneceu na empresa',
       y='Valor Total Charges')+
  theme_bw()+
  facet_wrap(~PaymentMethod)



ctrainLRM%>%
  ggplot(aes(Tenure, MultipleLines)) +
  geom_hex(aes(fill = Churn), bins=50) +
  labs(title='Churn de clientes por tipo de contrato e custo total',
       subtitle = 'Custo total entre tipos de contratos',
       x='Números de meses que o cliente permaneceu na empresa',
       y='Valor Total Charges')+
  theme_bw()+
  facet_wrap(~Contract)


ctrainLRM%>%
  ggplot(aes(Tenure, PhoneService)) +
  geom_hex(aes(fill = Churn), bins=50) +
  labs(title='Churn de clientes por tipo de contrato e custo total',
       subtitle = 'Custo total entre tipos de contratos',
       x='Números de meses que o cliente permaneceu na empresa',
       y='Valor Total Charges')+
  theme_bw()+
  facet_wrap(~Contract)


####################################################################
# GEOM MOSAIC

ggplot(data = ctrain) +
  geom_mosaic(aes(x = product(Churn, PhoneService), fill = PhoneService)) 


ggplot(data = ctrain) +
  geom_mosaic(aes(x = product(Churn, MultipleLines), fill = MultipleLines)) 



####################################################################
# GGPLOT GEOM BOXPLOT

options(repr.plot.width = 12, repr.plot.height = 8)

ggplot(churn_clean, aes(y= Tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

ggplot(churn_clean, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

ggplot(churn_clean, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")


churn_clean

options(repr.plot.width = 12, repr.plot.height = 8)

ggplot(churn_clean, aes(y= Tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

ggplot(churn_clean, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

ggplot(churn_clean, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

####################################################################
# GGPLOT GEOM POINT

ggplot(churn_clean, aes(x=Tenure, y=TotalCharges, color=Churn)) +
  geom_point()+
  facet_wrap(~SeniorCitizen)


ggplot(ctrainLRM, aes(x = Tenure,
                      y = TotalCharges,
                      color = Gender)) + 
  geom_point(color='grey') +
  stat_smooth(method='lm',
              aes(color=Gender))



ggplot(ctrainLRM) + 
  geom_point(mapping=aes(x = Tenure,
                         y = TotalCharges,
                         color = Churn))



ggplot(ctrainLRM, aes(x = Tenure,
                      y = TotalCharges,
                      color = InternetService)) + 
  geom_point(color='grey') +
  stat_smooth(method='lm',
              aes(color=InternetService))



ggplot(ctrainLRM, aes(x = Tenure,
                      y = TotalCharges,
                      color = PaperlessBilling)) + 
  geom_point(color='grey') +
  stat_smooth(method='lm',
              aes(color=PaperlessBilling))



ggplot(ctrainLRM, aes(x = Tenure,
                      y = TotalCharges,
                      color = PaymentMethod)) + 
  geom_point(color='grey') +
  stat_smooth(method='lm',
              aes(color=PaymentMethod))



ggplot(ctrainLRM, aes(x = Tenure,
                      y = TotalCharges,
                      color = Contract)) + 
  geom_point(color='grey') +
  stat_smooth(method='lm',
              aes(color=Contract))


####################################################################

## PHONE SERVICES 


# Compartivo entre dados serviços telefônicos e Churn. 

# GGARRANGE

# Plotagem para Phone Service.
fig1 <- category(churn_clean,'PhoneService')
st1 <- plotfunction(fig,"PhoneService")

# Plotagem para Multiple Lines.
fig2 <- category(churn_clean,'MultipleLines')
st2  <- plotfunction(fig,"MultipleLines")


# Geração da plotagem dos dados de serviços telefônicos. 
ggarrange(st1, st2, ncol=2, nrow=1)



####################################################################
# PLOT X TABS2 

PlotXTabs2(
  data = ctrain,
  x = PhoneService,
  y = Churn, 
  plottype = "mosaic", 
  data.label = "both", 
  mosaic.alpha = .9, 
  bf.display = "support", 
  title = "Motorcars Mosaic Plot VS by AM"
)


PlotXTabs2(
  data = ctrain,
  x = MultipleLines,
  y = Churn, 
  plottype = "mosaic", 
  data.label = "both", 
  mosaic.alpha = .9, 
  bf.display = "support", 
  title = "Motorcars Mosaic Plot VS by AM"
)




####################################################################

## IDADOS DEMOGRÁFICOS

















####################################################################
## DESCARTES 

churn_tech = churn_clean1 %>% 
  mutate(grp = as.factor(ifelse(TechSupport=='Yes' & Contract != 'Month-to-month','1','0')))

plot(churn_tech)


## Formulas, one ~ one, one ~ many, many ~ one, and many ~ many:
aggregate(weight ~ feed, data = chickwts, mean)
aggregate(breaks ~ wool + tension, data = warpbreaks, mean)
aggregate(cbind(Ozone, Temp) ~ Month, data = airquality, mean)
aggregate(cbind(ncases, ncontrols) ~ alcgp + tobgp, data = esoph, sum)

## Dot notation:
aggregate(. ~ Species, data = iris, mean)
aggregate(len ~ ., data = ToothGrowth, mean)

## Often followed by xtabs():
ag <- aggregate(len ~ ., data = ToothGrowth, mean)
xtabs(len ~ ., data = ag)


## Compute the average annual approval ratings for American presidents.
aggregate(presidents, nfrequency = 1, FUN = mean)
## Give the summer less weight.
aggregate(presidents, nfrequency = 1,
          FUN = weighted.mean, w = c(1, 1, 0.5, 1))










# 
# log.mod <- glm(Churn ~ PaymentMethod, 
#                binomial(link = "logit"), 
#                data=ctrain
# )
# 
# summary(log.mod)
# 
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -1.689569   0.082275 -20.536   <2e-16 ***
#   PaymentMethodCredit card (automatic)  0.005851   0.117948   0.050   0.9604    
# PaymentMethodElectronic check         1.533725   0.095990  15.978   <2e-16 ***
#   PaymentMethodMailed check             0.245186   0.112617   2.177   0.0295 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Payment Method: Electronic check can level up the retention rate
# Bank transfer will reduce the retention rate
# Mail check can increase the retention rate, but not obvious
# 
# 
# log.mod <- glm(Churn ~ InternetService, 
#                binomial(link = "logit"), 
#                data=ctrain
# )
# 
# summary(log.mod)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                -1.46634    0.06251 -23.459   <2e-16 ***
#   InternetServiceFiber optic  1.14660    0.07604  15.080   <2e-16 ***
#   InternetServiceNo          -1.13279    0.13647  -8.301   <2e-16 ***
#   
#   Internet Service: DSL, No will reduce the retention rate overall
# Fiber optic will increase the retention rate overall
# 
# 
# 
# log.mod <- glm(Churn ~ PaymentMethod + InternetService, 
#                binomial(link = "logit"), 
#                data=ctrain
# )
# 
# summary(log.mod)
# 
# Payment Method: Mailed check becomes significant when adding 'InternetService'
# 
# 
# 
# 
# 
# 
# 
# 
# log.mod7<-glm(Churn ~ Tenure+TotalCharges, 
#               binomial(link = "logit"), 
#               data=ctrain)
# 
# summary(log.mod7)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   3.126e-01  5.474e-02    5.71 1.13e-08 ***
#   Tenure       -1.194e-01  5.979e-03  -19.97  < 2e-16 ***
#   TotalCharges  8.808e-04  5.716e-05   15.41  < 2e-16 ***
#   
#   If the tenure is static, more total Charges will increase the retention rate  
# 
# 
# 
# log.mod8<-glm(Churn ~ TotalCharges*InternetService, 
#               binomial(link = "logit"), 
#               data=ctrain)
# 
# summary(log.mod8)
# 
# 
# Fiber optic will increase the total charge, and let the customer stay
# 
# 
# log.mod8<-glm(Churn ~ Contract*Tenure + TotalCharges*Tenure, 
#               binomial(link = "logit"), 
#               data=ctrain)
# 
# summary(log.mod8)
# 
# If others are static and if tenure and Total Charges both increase, the retention rate will decrease
# It shows that if the tenure and the Contract are static, more total Charges will increase the retention rate
# 


mod<-glm(Churn ~ InternetService + PaperlessBilling +
           SeniorCitizen*InternetService +
           SeniorCitizen*MonthlyCharges +
           Contract*Tenure + 
           TotalCharges*Tenure*InternetService +
           PaymentMethod, 
         family=binomial(link='logit'),
         data=ctrain)

anova(mod, test="Chisq")

confusionMatrix(
  as.factor(ifelse(predict(mod)>=0,1,0)),
  ctrain$Churn)


str(churn_clean1)
colnames(churn_clean1)

churn_clean1$Churn <- ifelse(churn_clean1$Churn=='Yes',1,0 )

data_surv <- Surv(churn_clean1$Tenure, churn_clean1$Churn)

#----

fit <- survfit(data_surv ~ SeniorCitizen   , 
               data = churn_clean1)

ggsurvplot(fit, 
           data = churn_clean1, 
           pval = TRUE, 
           conf.int = TRUE)

# --

fit <- survfit(data_surv ~ InternetService   , 
               data = churn_clean1)

ggsurvplot(fit, 
           data = churn_clean1, 
           pval = TRUE, 
           conf.int = TRUE)

# ---
fit <- survfit(data_surv ~ OnlineSecurity   , 
               data = churn_clean1)

ggsurvplot(fit, 
           data = churn_clean1, 
           pval = TRUE, 
           conf.int = TRUE)

# --
fit <- survfit(data_surv ~ OnlineBackup   , 
               data = churn_clean1)

ggsurvplot(fit, 
           data = churn_clean1, 
           pval = TRUE, 
           conf.int = TRUE)

#---
fit <- survfit(data_surv ~ TechSupport   , 
               data = churn_clean1)

ggsurvplot(fit, 
           data = churn_clean1, 
           pval = TRUE, 
           conf.int = TRUE)

# --
fit <- survfit(data_surv ~ Contract   , 
               data = churn_clean1)

ggsurvplot(fit, 
           data = churn_clean1, 
           pval = TRUE, 
           conf.int = TRUE)

# --
fit <- survfit(data_surv ~ PaperlessBilling   , 
               data = churn_clean1)

ggsurvplot(fit, 
           data = churn_clean1, 
           pval = TRUE, 
           conf.int = TRUE)

# --
fit <- survfit(data_surv ~ PaymentMethod   , 
               data = churn_clean1)

p1 <- ggsurvplot(fit, 
                 data = churn_clean1, 
                 pval = TRUE, 
                 conf.int = TRUE,
                 xlab="Tempo [meses]"
)

p1$plot <- p1$plot + 
  geom_vline(xintercept = 12) + 
  geom_vline(xintercept = 18) +
  geom_vline(xintercept = 36) + 
  geom_hline(yintercept = 0.8)
p1

# --
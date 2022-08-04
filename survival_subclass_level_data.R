###### Set working directory #####
setwd("/Users/Z0056WQ/Documents/Target/Data Mining Conference")

###### Packages #####
library(data.table)
library(dplyr)
library(survival)
library(survminer)
library(plotly)
library(ggplot2)

##### Read the data #####
surv.data.sbcl <- fread("code/subclass_level_surv_data.csv", data.table = F)
View(surv.data.sbcl)

# inv_fin is the censored variable here
### Kaplan-Meier
# using censored variable and time
km.fit2 <- survfit(Surv(surv_time_days, inv_fin) ~ 1, conf.type = "log-log",
                   data = surv.data.sbcl,
                   type = "kaplan-meier")
print(km.fit2)
summary(km.fit2)
ggsurvplot(km.fit2, data = surv.data.sbcl, 
           risk.table = TRUE,
           conf.int = TRUE,
           ggtheme = theme_minimal())

# inv_fin is the censored variable here
### Kaplan-Meier
# using censored variable and time and with brand as a categprical variable
km.fit3 <- survfit(Surv(surv_time_days, inv_fin) ~ brand, conf.type = "log-log",
                   data = surv.data.sbcl,
                   type = "kaplan-meier")
print(km.fit3)
summary(km.fit3)
ggsurvplot(km.fit3, data = surv.data.sbcl, 
           risk.table = TRUE,
           conf.int = TRUE,
           pval = TRUE, 
           pval.method = TRUE,
           ggtheme = theme_minimal())

# log-rank test
survdiff(Surv(surv_time_days, inv_fin)~ brand,
         data = surv.data.sbcl)

# Estimate Cox Proportional Hazard model
cox_reg1 <- coxph(Surv(surv_time_days, inv_fin) ~ avg_boh + avg_price_before_clr + brand,
                  data = surv.data.sbcl)
summary(cox_reg1)

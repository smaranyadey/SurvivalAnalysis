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
qty.data <- fread("code/aggTrainingData.csv", data.table = F)

unique(qty.data$mdse_item_i)
dim(qty.data)

qty.data$qty <- qty.data$qty + rnorm(n = dim(qty.data)[1], 
                                     mean = 0, sd = 1)

colnames(qty.data)

###### tcin mdse_item mapping ######
tcin.mdse.mapping <- fread("code/tcin_mdse_mapping_file.csv", data.table = F)

###### join qty.data with the above mapping ######
qty.data <- qty.data %>% 
  inner_join(tcin.mdse.mapping, by = c("mdse_item_i" = "mdse_item_i"))

###### read the item hierarchy data ######
itm.hier <- fread("code/item_hierarchy_mdse.csv", data.table = F)

###### join with the above data ######
qty.data <- qty.data %>% 
  inner_join(itm.hier, by = c("tcin" = "tcin"))


##### Only the relevant columns #####
survival.data <- qty.data %>% 
  dplyr::select(c(colnames(qty.data)[37:48], 'wkofyr','clr_pgm_strt_d', 'clr_pgm_end_d',
                  'wk_begin_date', 'wk_from_strt_date_q', 'cluster',
                  'qty', 'boh_q', 'wt_mkdn_p', 'pre_mkdn_retl_a',
                  'priorQty', 'priorPrice'))

##### Let us consider cluster as stores #####
##### Aggregate to get tcin level data #####
surv.data.tcin <- survival.data %>% 
  select(c(colnames(survival.data)[1:17], colnames(survival.data)[19:24])) %>% 
  group_by_at(colnames(survival.data)[1:17]) %>% 
  summarise(qty = mean(qty),
         boh_q = mean(boh_q),
         wt_mkdn_p = median(wt_mkdn_p),
         pre_mkdn_retl_a = median(pre_mkdn_retl_a),
         priorQty = mean(priorQty),
         priorPrice = median(priorPrice)
         )

##### Creating censored variable #####
# clr_ind would be our right censored variable
# focal event is clearance started for the products
surv.data.tcin <- surv.data.tcin %>% 
  mutate(clr_ind = if_else(wt_mkdn_p == 0, 0, 1))

# transforming the wk_from_start_date_q
surv.data.tcin <- surv.data.tcin %>% 
  mutate(study_week = wk_from_strt_date_q + 13)


##### Plotting the qty variables #####
# at mdse_item_i-level
df_16628342 <- surv.data.tcin %>%
  dplyr::filter(tcin == 16628342) %>% 
  dplyr::select(qty, wk_from_strt_date_q)

plotly.data <- data.frame(wk_from_strt_date_q = df_16628342$wk_from_strt_date_q, 
                          qty = df_16628342$qty) 

fig_16628342 <- plot_ly(plotly.data, x = ~wk_from_strt_date_q, y = ~qty, 
               type = 'scatter', mode = 'lines')
fig_16628342

##### Kaplan-Meir Model #####
# using qty, clr_ind 
km.fit1 <- survfit(Surv(qty, clr_ind) ~ 1,
                   data = surv.data.tcin,
                   type = "kaplan-meier")
summary(km.fit1)
print(km.fit1)
ggsurvplot(km.fit1, data = surv.data.tcin)
 
# using censored variable and time
km.fit2 <- survfit(Surv(wk_from_strt_date_q, clr_ind) ~ 1,
                   data = surv.data.tcin,
                   type = "kaplan-meier", conf.type = "log-log")
summary(km.fit2)
ggsurvplot(km.fit2, data = surv.data.tcin)

# using censored variable and time with qty
km.fit3 <- survfit(Surv(wk_from_strt_date_q, qty, clr_ind) ~ 1,
                   data = surv.data.tcin,
                   type = "kaplan-meier")
print(km.fit3)
summary(km.fit3)
ggsurvplot(km.fit3, data = surv.data.tcin)

# life table






survival.data <- survival.data %>% 
  group_by(tcin) %>% 
  mutate(sample(c(0,1), replace = TRUE, size = 21))

simulated_no <- floor(runif(160, min = 10000000, max = 99999999))

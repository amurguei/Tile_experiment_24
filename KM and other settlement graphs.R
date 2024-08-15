#Code for Kaplan Meier curves settlement experiment March 2024
#Libraries 

library(dplyr)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(rstatix)
library(survival)
library(survminer)
library(purrr)
library(tidyr)
library(tidyverse)
library(ggsci)
library(splitstackshape)
library(MuMIn)

setwd("E:/Users/amurgueitio/Documents/Tiles_bacteria")

Bacteria_set_status_ <- read_csv("Bacteria_set_status .csv", 
                                 col_types = cols(date = col_date(format = "%d/%m/%Y")))
Sum_bacteria <- Bacteria_set_status_ %>% 
  group_by(treatment, age_days, status_set %>% 
             summarise_each(funs(sum), count)
View(Sum_bacteria)
           
Sum_bacteria <- Bacteria_set_status_ %>%
group_by(treatment, age_days, status_set) %>%
summarise(count_sum = sum(count, na.rm = TRUE))
           
Expanded_bacteria <- expandRows(Bacteria_set_status_, "count")
           
Expanded_bacteria$treatment = factor(Expanded_bacteria$treatment, levels = c("PAhh","v3","f7","f8","all","none"))
K$treatment = factor(K$treatment, levels = c("SS","SD","DS","DD"))
           
           
#AAAAAH what a bloody pain, I need to include 3 more fucking treatments on the dataset, do after figuring out code
           
# Settlement
fitExpanded_bacteria = survfit(Surv(age_days, status_set)~ treatment, data = Expanded_bacteria)
summary(fitExpanded_bacteria)


setplot_bacteria

setplot_bacteria <- ggsurvplot(fitExpanded_bacteria, size = 1,
                               pval = TRUE, pval.method = TRUE,
                               pval.coord = c(0.4, 0.4),
                               pval.method.coord = c(0.4, 0.48),
                               fun = "event",
                               ggtheme = theme_classic() +
                                 theme(axis.ticks = element_line(size = 1), 
                                       axis.text = element_text(colour = "black", size = 10),
                                       axis.line = element_line(size = 1), 
                                       axis.title = element_text(size = 12), 
                                       legend.background = element_blank(), 
                                       legend.text = element_text(size = 10)),
                               palette = c("paleturquoise", "hotpink1", "darkmagenta",  "darkseagreen", "steelblue", "gold1"),
                               legend = c(0.15, 0.9), legend.title = "", 
                               xlab = "Days", ylab = "Settlement probability", 
                               legend.labs = c("PAhh", "v3", "f7", "f8", "all", "none"))

setplot_bacteria

Pairwise comparisons using Log-Rank test 

# Settlement - Pairwise comparison between treatments, fixed effects only, p value adj. method BH
resbacteria <- pairwise_survdiff(Surv(age_days, status_set) ~ treatment,
                         data= Bacteria_set_status_)
resbacteria   

Pairwise comparisons using Log-Rank test 

#data:  Bacteria_set_status_ and treatment 

#all  f7   f8   none PAhh
#f7   0.95 -    -    -    -   
#  f8   0.95 0.95 -    -    -   
#  none 0.95 0.95 0.95 -    -   
#  PAhh 0.95 1.00 0.95 0.95 -   
#  v3   0.95 0.95 0.95 0.95 0.95

#P value adjustment method: BH 


# I'm trying cox proportional hazards and a GLM with a binomial distribution to include the proportion of settled larvae.
# Creating a'proportion of settled larvae' Bacteria_set_status_`
Bacteria_set_status_$settled_proportion <- Bacteria_set_status_$count / Bacteria_set_status_$n_larvae

# View the updated dataset to ensure the new column is correct
View(Bacteria_set_status_)

# Fit the GLM model with a binomial family
glm_model <- glm(cbind(count, n_larvae - count) ~ treatment + age_days, 
                 family = binomial, data = Bacteria_set_status_)

#summary(glm_model)

#Call:
 # glm(formula = cbind(count, n_larvae - count) ~ treatment + age_days, 
      family = binomial, data = Bacteria_set_status_)

#Deviance Residuals: 
 # Min       1Q   Median       3Q      Max  
#-3.0895  -1.3695  -0.1943   0.9246   3.7126  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -2.241767   0.165026 -13.584  < 2e-16 ***
#  treatmentf7    0.674067   0.167788   4.017 5.89e-05 ***
#  treatmentf8   -0.193803   0.179943  -1.077    0.281    
#treatmentnone -0.193803   0.179942  -1.077    0.281    
#treatmentPAhh  0.222018   0.172298   1.289    0.198    
#treatmentv3    1.322115   0.167130   7.911 2.56e-15 ***
#  age_days       0.115950   0.009908  11.703  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 679.96  on 215  degrees of freedom
#Residual deviance: 408.60  on 209  degrees of freedom
#AIC: 870.94

#Number of Fisher Scoring iterations: 4


# Fit the Cox proportional hazards model
cox_model <- coxph(Surv(age_days, status_set) ~ treatment + settled_proportion, data = Bacteria_set_status_)

# View the summary of the Cox model
summary(cox_model)

#Call:
 # coxph(formula = Surv(age_days, status_set) ~ treatment + settled_proportion, 
 #       data = Bacteria_set_status_)

#n= 216, number of events= 177 

#coef exp(coef) se(coef)      z Pr(>|z|)  
#treatmentf7         0.05123   1.05256  0.26634  0.192   0.8475  
#treatmentf8        -0.10284   0.90228  0.26073 -0.394   0.6933  
#treatmentnone      -0.21512   0.80645  0.26689 -0.806   0.4202  
#treatmentPAhh      -0.01985   0.98035  0.25996 -0.076   0.9391  
#treatmentv3         0.29487   1.34296  0.26703  1.104   0.2695  
#settled_proportion -0.81635   0.44204  0.43972 -1.857   0.0634 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#exp(coef) exp(-coef) lower .95 upper .95
#treatmentf7           1.0526     0.9501    0.6245     1.774
#treatmentf8           0.9023     1.1083    0.5413     1.504
#treatmentnone         0.8064     1.2400    0.4780     1.361
#treatmentPAhh         0.9803     1.0200    0.5890     1.632
#treatmentv3           1.3430     0.7446    0.7957     2.267
#settled_proportion    0.4420     2.2622    0.1867     1.047


#Concordance= 0.597  (se = 0.029 )
#Likelihood ratio test= 4.78  on 6 df,   p=0.6
#Wald test            = 4.93  on 6 df,   p=0.6
#Score (logrank) test = 4.95  on 6 df,   p=0.5

# For Cox model visualization
ggsurvplot(survfit(cox_model), data = Bacteria_set_status_, 
           ggtheme = theme_classic(), xlab = "Days", ylab = "Survival Probability")

# For GLM predictions (e.g., using `predict` to generate probabilities)
predicted <- predict(glm_model, type = "response")

# Generate predictions for the GLM model
Bacteria_set_status_$predicted_prob <- predict(glm_model, type = "response")

library(ggplot2)

# Plot predicted probabilities
ggplot(Bacteria_set_status_, aes(x = age_days, y = predicted_prob, color = treatment)) +
  geom_line(size = 1) +
  geom_point(aes(y = settled_proportion), size = 2, shape = 21, fill = "white") +
  theme_classic() +
  labs(x = "Days", y = "Predicted Settlement Probability",
       title = "Predicted Settlement Probability Over Time",
       color = "Treatment") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


library(ggplot2)

# Calculate proportion of settled larvae
Bacteria_set_status_$prop_settled <- Bacteria_set_status_$count / Bacteria_set_status_$n_larvae

# Plot
ggplot(Bacteria_set_status_, aes(x = age_days, y = prop_settled, color = treatment, group = well_id)) +
  geom_line(aes(linetype = treatment), size = 1) +  # Line plot for each well
  theme_classic() +
  labs(x = "Days", y = "Proportion of Settled Larvae", 
       title = "Proportion of Settled Larvae Over Time by Treatment",
       color = "Treatment") +
  scale_color_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen", "steelblue", "gold1"))



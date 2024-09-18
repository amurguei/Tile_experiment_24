#Code for Kaplan Meier curves settlement experiment March 2024
install.packages("cmprsk")


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
library(cmprsk)
library(car)
library(nlme)



setwd("E:/Users/amurgueitio/Documents/Tiles_bacteria")

Bacteria_set_status_ <- read_csv("Bacteria_set_status .csv", 
                                 col_types = cols(date = col_date(format = "%d/%m/%Y")))
           
Sum_bacteria <- Bacteria_set_status_ %>%
group_by(treatment, age_days, status_set) %>%
summarise(count_sum = sum(count, na.rm = TRUE))
           
Expanded_bacteria <- expandRows(Bacteria_set_status_, "count")
           
Expanded_bacteria$treatment = factor(Expanded_bacteria$treatment, levels = c("PAhh","v3","f7","f8","all","none"))
           

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

#Pairwise comparisons using Log-Rank test 

# Settlement - Pairwise comparison between treatments, fixed effects only, p value adj. method BH
resbacteria <- pairwise_survdiff(Surv(age_days, status_set) ~ treatment,
                         data= Bacteria_set_status_)
resbacteria   


#Pairwise comparisons using Log-Rank test 

#data:  Bacteria_set_status_ and treatment 

#all  f7   f8   none PAhh
#f7   0.95 -    -    -    -   
#  f8   0.95 0.95 -    -    -   
#  none 0.95 0.95 0.95 -    -   
#  PAhh 0.95 1.00 0.95 0.95 -   
#  v3   0.95 0.95 0.95 0.95 0.95

#P value adjustment method: BH 

#This results were a little boring (testing differences on settlement assuming some settlement... So I´m making a 30% threshold and testing that)
## Create a new column indicating if 30% or more larvae settled (event == 1) or not (event == 0)
Bacteria_set_status_ <- Bacteria_set_status_ %>%
  mutate(event_30 = ifelse(prop_settled >= 0.3, 1, 0))

# Summarize the data (optional, if needed for exploratory purposes)
Sum_bacteria <- Bacteria_set_status_ %>%
  group_by(treatment, age_days, event_30) %>%
  summarise(count_sum = sum(count, na.rm = TRUE))

# Expand the dataset to have a row per larva
Expanded_bacteria <- expandRows(Bacteria_set_status_, "count")

# Recalculate the binary event for the expanded data
Expanded_bacteria <- Expanded_bacteria %>%
  mutate(event_30 = ifelse(prop_settled >= 0.3, 1, 0))

# Set factor levels for treatments
Expanded_bacteria$treatment = factor(Expanded_bacteria$treatment, levels = c("PAhh","v3","f7","f8","all","none"))

# Kaplan-Meier survival fit with event_30 as the event
fitExpanded_bacteria_30 = survfit(Surv(age_days, event_30) ~ treatment, data = Expanded_bacteria)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_bacteria_30)

# Plot the Kaplan-Meier curve
setplot_bacteria_30 <- ggsurvplot(fitExpanded_bacteria_30, size = 1,
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
                                  xlab = "Days", ylab = "Probability of reaching 30% settlement", 
                                  legend.labs = c("PAhh", "v3", "f7", "f8", "all", "none"))

# Plot the results
setplot_bacteria_30

# Pairwise comparison using the event_30 (30% settlement) column
resbacteria_30 <- pairwise_survdiff(Surv(age_days, event_30) ~ treatment,
                                    data = Expanded_bacteria,
                                    p.adjust.method = "BH")

#Pairwise comparisons using Log-Rank test 

#data:  Expanded_bacteria and treatment 

#PAhh    v3      f7      f8      all    
#v3   0.28250 -       -       -       -      
#  f7   0.58004 0.58487 -       -       -      
#  f8   0.00950 0.00051 0.00178 -       -      
#  all  0.01703 0.00058 0.00290 0.82026 -      
#  none 0.09190 0.00471 0.01703 0.48438 0.58487

#P value adjustment method: BH 
# Print the results
resbacteria_30

#Testing 40%

# Create a new column indicating if 40% or more larvae settled (event == 1) or not (event == 0)
Bacteria_set_status_ <- Bacteria_set_status_ %>%
  mutate(event_40 = ifelse(prop_settled >= 0.4, 1, 0))

# Summarize the data (optional, if needed for exploratory purposes)
Sum_bacteria <- Bacteria_set_status_ %>%
  group_by(treatment, age_days, event_40) %>%
  summarise(count_sum = sum(count, na.rm = TRUE))

# Expand the dataset to have a row per larva
Expanded_bacteria <- expandRows(Bacteria_set_status_, "count")

# Recalculate the binary event for the expanded data
Expanded_bacteria <- Expanded_bacteria %>%
  mutate(event_40 = ifelse(prop_settled >= 0.4, 1, 0))

# Set factor levels for treatments
Expanded_bacteria$treatment = factor(Expanded_bacteria$treatment, levels = c("PAhh","v3","f7","f8","all","none"))

# Kaplan-Meier survival fit with event_40 as the event
fitExpanded_bacteria_40 = survfit(Surv(age_days, event_40) ~ treatment, data = Expanded_bacteria)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_bacteria_40)

# Plot the Kaplan-Meier curve for 40% settlement
setplot_bacteria_40 <- ggsurvplot(fitExpanded_bacteria_40, size = 1,
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
                                  xlab = "Days", ylab = "Probability of reaching 40% settlement", 
                                  legend.labs = c("PAhh", "v3", "f7", "f8", "all", "none"))

# Plot the results
setplot_bacteria_40

# Pairwise comparison using the event_40 (40% settlement) column
resbacteria_40 <- pairwise_survdiff(Surv(age_days, event_40) ~ treatment,
                                    data = Expanded_bacteria,
                                    p.adjust.method = "BH")

# Print the pairwise comparison results
resbacteria_40


#Pairwise comparisons using Log-Rank test 

#data:  Expanded_bacteria and treatment 

#PAhh    v3      f7      f8     all   
#v3   0.1816  -       -       -      -     
#  f7   0.4124  0.5325  -       -      -     
#  f8   9.7e-05 1.1e-06 4.6e-06 -      -     
#  all  0.0959  0.0038  0.0154  0.0435 -     
#  none 0.4124  0.0474  0.1379  0.0038 0.4124

#P value adjustment method: BH 

# Create a new column indicating if 50% or more larvae settled (event == 1) or not (event == 0)
Bacteria_set_status_ <- Bacteria_set_status_ %>%
  mutate(event_50 = ifelse(prop_settled >= 0.5, 1, 0))

# Expand the dataset to have a row per larva
Expanded_bacteria <- expandRows(Bacteria_set_status_, "count")

# Recalculate the binary event for the expanded data
Expanded_bacteria <- Expanded_bacteria %>%
  mutate(event_50 = ifelse(prop_settled >= 0.5, 1, 0))

# Kaplan-Meier survival fit with event_50 as the event
fitExpanded_bacteria_50 = survfit(Surv(age_days, event_50) ~ treatment, data = Expanded_bacteria)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_bacteria_50)

# Plot the Kaplan-Meier curve for 50% settlement
setplot_bacteria_50 <- ggsurvplot(fitExpanded_bacteria_50, size = 1,
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
                                  xlab = "Days", ylab = "Probability of reaching 50% settlement", 
                                  legend.labs = c("PAhh", "v3", "f7", "f8", "all", "none"))

# Plot the results
setplot_bacteria_50

# Pairwise comparison using the event_50 (50% settlement) column
resbacteria_50 <- pairwise_survdiff(Surv(age_days, event_50) ~ treatment,
                                    data = Expanded_bacteria,
                                    p.adjust.method = "BH")

# Print the pairwise comparison results
resbacteria_50

 

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
glm(formula = cbind(count, n_larvae - count) ~ treatment + age_days, 
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
#  Signif. codes:  0 â€˜***â€™ 0.001 â€˜**â€™ 0.01 â€˜*â€™ 0.05 â€˜.â€™ 0.1 â€˜ â€™ 1

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
#  Signif. codes:  0 â€˜***â€™ 0.001 â€˜**â€™ 0.01 â€˜*â€™ 0.05 â€˜.â€™ 0.1 â€˜ â€™ 1

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

#Another attempt but with confidence interval

# Summarize data by treatment and age_days
Bacteria_summary <- Bacteria_set_status_ %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_prop = mean(prop_settled),
    sd_prop = sd(prop_settled),
    n = n(),  # number of replicates
    se_prop = sd_prop / sqrt(n)  # standard error of the mean
  )

# Plot with mean and confidence intervals
ggplot(Bacteria_summary, aes(x = age_days, y = mean_prop, color = treatment)) +
  geom_line(aes(linetype = treatment), size = 1) +  # Mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_prop - se_prop, ymax = mean_prop + se_prop, fill = treatment), 
              alpha = 0.2) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Settled Larvae", 
       title = "Mean Proportion of Settled Larvae Over Time by Treatment",
       color = "Treatment") +
  scale_color_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen", "steelblue", "gold1")) +
  scale_fill_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen", "steelblue", "gold1"))




# Plot with mean and confidence intervals, continuous lines, and a single legend
ggplot(Bacteria_summary, aes(x = age_days, y = mean_prop, color = treatment)) +
  geom_line(size = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_prop - se_prop, ymax = mean_prop + se_prop, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Settled Larvae", 
       title = "Mean Proportion of Settled Larvae Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen", "steelblue", "gold1")) +
  scale_fill_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen", "steelblue", "gold1"))

#Testing with colours assigned as in KM 

# Ensure the treatment factor is ordered correctly
Bacteria_summary$treatment <- factor(Bacteria_summary$treatment, 
                                     levels = c("PAhh", "v3", "f7", "f8", "all", "none"))

# Plot with mean and confidence intervals
ggplot(Bacteria_summary, aes(x = age_days, y = mean_prop, color = treatment)) +
  geom_line(size = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_prop - se_prop, ymax = mean_prop + se_prop, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Settled Larvae", 
       title = "Mean Proportion of Settled Larvae Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen", "steelblue", "gold1")) +
  scale_fill_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen", "steelblue", "gold1"))


#Proportional logistic regression:
# Fit a GLM model for proportions with a binomial family and logit link
data_logit_glm <- Bacteria_set_status_ %>%
  select(t, age_days, treatment, n_larvae, prop_settled) %>%
  mutate(event_time = age_days)  # Time variable (age of larvae)

glm_model <- glm(prop_settled ~ event_time * treatment, 
                 family = quasibinomial(link = "logit"),  # Use quasibinomial to handle overdispersion
                 data = data_logit_glm)

# Check model summary
summary(glm_model)

Call:
  glm(formula = prop_settled ~ event_time * treatment, family = quasibinomial(link = "logit"), 
      data = data_logit_glm)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.92843  -0.40480  -0.06458   0.29861   1.16198  

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              -2.233755   0.402014  -5.556 8.54e-08 ***
#  event_time                0.115217   0.033505   3.439 0.000708 ***
 # treatmentf7               0.760530   0.521997   1.457 0.146664    
#treatmentf8              -0.545929   0.610850  -0.894 0.372525    
#treatmentnone            -0.357523   0.597784  -0.598 0.550450    
#treatmentPAhh             0.173437   0.553074   0.314 0.754155    
#treatmentv3               1.521746   0.502866   3.026 0.002796 ** 
#  event_time:treatmentf7   -0.008611   0.044672  -0.193 0.847338    
#event_time:treatmentf8    0.031097   0.049842   0.624 0.533381    
#event_time:treatmentnone  0.014598   0.049135   0.297 0.766694    
#event_time:treatmentPAhh  0.004518   0.046429   0.097 0.922581    
#event_time:treatmentv3   -0.021663   0.044009  -0.492 0.623077    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for quasibinomial family taken to be 0.1767424)

#Null deviance: 67.996  on 215  degrees of freedom
#Residual deviance: 40.582  on 204  degrees of freedom
#AIC: NA

#Number of Fisher Scoring iterations: 5

# Ensure treatment is a factor
data_logit_glm$treatment <- factor(data_logit_glm$treatment)

# Reorder the levels, setting 'control' as the baseline
data_logit_glm$treatment <- relevel(data_logit_glm$treatment, ref = "none")

# Fit the model again
glm_model <- glm(prop_settled ~ event_time * treatment, 
                 family = quasibinomial(link = "logit"), 
                 data = data_logit_glm)

# Summary of the updated model
summary(glm_model)
#Call:
#  glm(formula = prop_settled ~ event_time * treatment, family = quasibinomial(link = "logit"), 
#      data = data_logit_glm)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.92843  -0.40480  -0.06458   0.29861   1.16198  

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              -2.59128    0.44241  -5.857 1.86e-08 ***
 # event_time                0.12982    0.03594   3.612 0.000382 ***
  #treatmentall              0.35752    0.59778   0.598 0.550450    
#treatmentf7               1.11805    0.55371   2.019 0.044775 *  
 # treatmentf8              -0.18841    0.63816  -0.295 0.768118    
#treatmentPAhh             0.53096    0.58310   0.911 0.363591    
#treatmentv3               1.87927    0.53571   3.508 0.000555 ***
#  event_time:treatmentall  -0.01460    0.04914  -0.297 0.766694    
#event_time:treatmentf7   -0.02321    0.04653  -0.499 0.618428    
#event_time:treatmentf8    0.01650    0.05151   0.320 0.749061    
#event_time:treatmentPAhh -0.01008    0.04821  -0.209 0.834603    
#event_time:treatmentv3   -0.03626    0.04589  -0.790 0.430335    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for quasibinomial family taken to be 0.1767424)

#Null deviance: 67.996  on 215  degrees of freedom
#Residual deviance: 40.582  on 204  degrees of freedom
#AIC: NA

#Number of Fisher Scoring iterations: 5


# Create a data frame with all combinations of event_time and treatment
time_points <- expand.grid(
  event_time = seq(min(data_logit_glm$event_time), max(data_logit_glm$event_time), by = 0.5),
  treatment = unique(data_logit_glm$treatment)
)
# Ensure the treatment variable is a factor (if it’s not already)
time_points$treatment <- factor(time_points$treatment, levels = unique(data_logit_glm$treatment))

# Make predictions
predictions <- predict(glm_model, newdata = time_points, type = "response")

# Add predictions to the time_points data frame
time_points$predicted_prop_settled <- predictions

# Plot the predicted settlement proportions over time
# Plot the predicted settlement proportions over time
ggplot(time_points, aes(x = event_time, y = predicted_prop_settled, color = treatment)) +
  geom_line() +
  labs(x = "Time (days)", y = "Predicted Proportion of Settlement",
       title = "Predicted Proportion of Larvae Settlement Over Time by Treatment")

#Rm ANOVA

#Check assumptions

# Ensure the time variable is numeric or factor and well_id is a factor
Bacteria_set_status_$age_days <- as.numeric(Bacteria_set_status_$age_days)
Bacteria_set_status_$well_id <- as.factor(Bacteria_set_status_$well_id)

# Fit the repeated measures ANOVA model
model <- aov(prop_settled ~ treatment * age_days + Error(well_id/age_days), data = Bacteria_set_status_)

simple_model <- aov(prop_settled ~ treatment + age_days + Error(well_id/age_days), data = Bacteria_set_status_)

summary(simple_model)


# Fit a simpler linear model for checking out the residuals since it wasn´t working with RM anova
simple_lm <- lm(prop_settled ~ treatment * age_days + factor(well_id), data = Bacteria_set_status_)

# Extract fitted values and residuals
fitted_values <- fitted(simple_lm)
residuals <- residuals(simple_lm)

# Check if the extraction was successful
head(fitted_values)
head(residuals)

library(nlme)

# Fit the model with nlme
model_nlme <- lme(prop_settled ~ treatment * age_days, random = ~1 | well_id, data = Bacteria_set_status_)

# Extract residuals
residuals_nlme <- residuals(model_nlme)

# Check residuals
head(residuals_nlme)
#PAhh1       PAhh1       PAhh1       PAhh1       PAhh1       PAhh1 
#-0.07699731 -0.10073312 -0.12446893 -0.14820474 -0.07194055  0.10432364 

# Q-Q plot and Shapiro-Wilk test for normality
qqnorm(residuals_nlme)
qqline(residuals_nlme, col = "red")
shapiro.test(residuals_nlme)
#Shapiro-Wilk normality test

#data:  residuals_nlme
#W = 0.98663, p-value = 0.04006

# Check model structure
summary(model)
str(model)

# Extract residuals from RM ANOVA model
residuals_rm <- residuals(model)

# Q-Q Plot
qqnorm(residuals_rm)
qqline(residuals_rm, col = "red")

# Shapiro-Wilk Test
shapiro.test(residuals_rm)


library(afex)  # For Mauchly's Test
install.packages("afex")
# Fit model using afex::aov_ez for easier sphericity checks
library(afex)
model_afex <- aov_ez("well_id", "prop_settled", Bacteria_set_status_, within = c("age_days"), between = c("treatment"))

# Mauchly's Test
mauchly_test <- mauchly.test(model_afex)
print(mauchly_test)

# Check correction factors if sphericity is violated
library(emmeans)
emm <- emmeans(model_afex, ~ treatment * age_days)
summary(emm, infer = TRUE)


#Rm anova is not working, moving on to something else



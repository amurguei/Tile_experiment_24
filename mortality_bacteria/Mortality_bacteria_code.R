#Death Graphs for bacteria experiment
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

#Read in dataset
library(readr)
Bacteria_mort <- read_delim("Bacteria_set_status .csv", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)
#Excluding one treatment based on lack of background info

Bacteria_mort <- Bacteria_mort %>%
  filter(treatment != "puA16")

#Graphs for proportion of mortality by treatment through the days of the experiment.

# Calculate proportion of settled larvae
Bacteria_mort$prop_dead <- Bacteria_mort$dead / Bacteria_mort$n_larvae

#Get mean and summary stats for plots
Bacteria_summary <- Bacteria_mort %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_dead = mean(prop_dead),
    sd_prop = sd(prop_dead),
    n = n(),  # number of replicates
    se_prop = sd_prop / sqrt(n)  # standard error of the mean
  )
Bacteria_summary$treatment <- factor(Bacteria_summary$treatment, 
                                     levels = c("PAhh", "v3", "f7", "f23","f24","f8", "all", "none"))

# Plot with mean and confidence intervals
mean_mortality <- ggplot(Bacteria_summary, aes(x = age_days, y = mean_dead, color = treatment)) +
  geom_line(size = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_dead - se_prop, ymax = mean_dead + se_prop, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Dead Larvae", 
       title = "Mean Proportion of Dead Larvae Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen","midnightblue","black", "steelblue", "gold1")) +
  scale_fill_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen","midnightblue","black", "steelblue", "gold1"))

plot(mean_mortality)

#Alternative, survival by treatment

#calculate survival 1-mort

Bacteria_mort$prop_alive <- 1 - Bacteria_mort$prop_dead

#Summary surv

Bacteria_summary_surv <- Bacteria_mort %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_alive = mean(prop_alive),
    sd_alive = sd(prop_alive),
    n = n(),  # number of replicates
    se_prop = sd_alive / sqrt(n)  # standard error of the mean
  )
Bacteria_summary_surv$treatment <- factor(Bacteria_summary$treatment, 
                                     levels = c("PAhh", "v3", "f7", "f23","f24","f8", "all", "none"))
# Plot with mean and confidence intervals
mean_survival <- ggplot(Bacteria_summary_surv, aes(x = age_days, y = mean_alive, color = treatment)) +
  geom_line(size = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_alive - se_prop, ymax = mean_alive + se_prop, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Dead Larvae", 
       title = "Mean Proportion of Dead Larvae Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen","midnightblue","black", "steelblue", "gold1")) +
  scale_fill_manual(values = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen","midnightblue","black", "steelblue", "gold1"))

plot(mean_survival)

#GLM for testing differences with mortality

data_logit_glm <- Bacteria_mort %>%
     select(t, age_days, treatment, n_larvae, prop_dead) %>%
     mutate(event_time = age_days)  # Time variable (age of larvae)

# Ensure treatment is a factor
data_logit_glm$treatment <- factor(data_logit_glm$treatment)

# Reorder the levels, setting 'control' as the baseline
data_logit_glm$treatment <- relevel(data_logit_glm$treatment, ref = "none")

# Fit the model again
glm_model <- glm(prop_dead ~ event_time * treatment, 
                 family = quasibinomial(link = "logit"), 
                 data = data_logit_glm)
levels(data_logit_glm$treatment)

#summary(glm_model)
#Call:
#  glm(formula = prop_dead ~ event_time * treatment, family = quasibinomial(link = "logit"), 
#      data = data_logit_glm)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.82456  -0.22679  -0.11280  -0.00818   0.72568  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              -9.86691    2.84929  -3.463  0.00062 ***
#  event_time                0.41394    0.17674   2.342  0.01990 *  
#  treatmentall              3.73885    3.07834   1.215  0.22558    
#treatmentf23              5.81324    2.90425   2.002  0.04632 *  
#  treatmentf24              2.29443    3.14071   0.731  0.46569    
#treatmentf7              -2.00926    4.98981  -0.403  0.68751    
#treatmentf8               4.07323    2.97306   1.370  0.17180    
#treatmentPAhh             4.72729    2.98230   1.585  0.11410    
#treatmentv3               4.50609    2.93691   1.534  0.12612    
#event_time:treatmentall  -0.20691    0.19426  -1.065  0.28776    
#event_time:treatmentf23  -0.28477    0.18183  -1.566  0.11849    
#event_time:treatmentf24  -0.06564    0.19619  -0.335  0.73820    
#event_time:treatmentf7    0.10944    0.30396   0.360  0.71910    
#event_time:treatmentf8   -0.16147    0.18598  -0.868  0.38606    
#event_time:treatmentPAhh -0.26488    0.18815  -1.408  0.16032    
#event_time:treatmentv3   -0.16635    0.18349  -0.907  0.36543    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for quasibinomial family taken to be 0.08695683)

#Null deviance: 40.845  on 287  degrees of freedom
#Residual deviance: 23.958  on 272  degrees of freedom
#AIC: NA

#Number of Fisher Scoring iterations: 9

# Fit a GLM model for proportions with a quasibinomial family and logit link
data_logit_glm <- Bacteria_mort %>%
  select(t, age_days, treatment, n_larvae, prop_dead) %>%
  mutate(event_time = age_days)  # Time variable (age of larvae)

# Fit the GLM model
glm_model_mort <- glm(prop_dead ~ event_time * treatment, 
                      family = quasibinomial(link = "logit"),  # Use quasibinomial to handle overdispersion
                      data = data_logit_glm)

# Check model summary
summary(glm_model_mort)
# Ensure 'treatment' is a factor
data_logit_glm <- data_logit_glm %>%
  mutate(treatment = factor(treatment))

# Find out which treatment level was used as the baseline
levels(data_logit_glm$treatment)  # This shows the factor levels, with the first being the baseline

#GLM for testing differences in survival

#GLM for testing differences with mortality

data_logit_glm_surv <- Bacteria_mort %>%
  select(t, age_days, treatment, n_larvae, prop_alive) %>%
  mutate(event_time = age_days)  # Time variable (age of larvae)

# Ensure treatment is a factor
data_logit_glm_surv$treatment <- factor(data_logit_glm_surv$treatment)

# Reorder the levels, setting 'control' as the baseline
data_logit_glm_surv$treatment <- relevel(data_logit_glm_surv$treatment, ref = "none")

# Fit the model again
glm_model_surv <- glm(prop_alive ~ event_time * treatment, 
                 family = quasibinomial(link = "logit"), 
                 data = data_logit_glm_surv)
levels(data_logit_glm_surv$treatment)

summary(glm_model_surv)
#Call:


#Call:
#  glm(formula = prop_alive ~ event_time * treatment, family = quasibinomial(link = "logit"), 
      data = data_logit_glm_surv)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.72568   0.00818   0.11280   0.22679   0.82456  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)               9.86691    2.84929   3.463  0.00062 ***
#  event_time               -0.41394    0.17674  -2.342  0.01990 *  
#  treatmentall             -3.73885    3.07834  -1.215  0.22558    
#treatmentf23             -5.81324    2.90425  -2.002  0.04632 *  
#  treatmentf24             -2.29443    3.14071  -0.731  0.46569    
#treatmentf7               2.00926    4.98981   0.403  0.68751    
#treatmentf8              -4.07323    2.97306  -1.370  0.17180    
#treatmentPAhh            -4.72729    2.98230  -1.585  0.11410    
#treatmentv3              -4.50609    2.93691  -1.534  0.12612    
#event_time:treatmentall   0.20691    0.19426   1.065  0.28776    
#event_time:treatmentf23   0.28477    0.18183   1.566  0.11849    
#event_time:treatmentf24   0.06564    0.19619   0.335  0.73820    
#event_time:treatmentf7   -0.10944    0.30396  -0.360  0.71910    
#event_time:treatmentf8    0.16147    0.18598   0.868  0.38606    
#event_time:treatmentPAhh  0.26488    0.18815   1.408  0.16032    
#event_time:treatmentv3    0.16635    0.18349   0.907  0.36543    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for quasibinomial family taken to be 0.08695683)

#Null deviance: 40.845  on 287  degrees of freedom
#Residual deviance: 23.958  on 272  degrees of freedom
#AIC: NA

#Number of Fisher Scoring iterations: 9


#Trying RM-Anova again

# Fit a linear model for prop_dead
model <- lm(prop_dead ~ treatment * t, data = Bacteria_mort)

# Check residuals normality with Shapiro-Wilk test
shapiro.test(residuals(model))

# shapiro.test(residuals(model))

#Shapiro-Wilk normality test

#data:  residuals(model)
#W = 0.88179, p-value = 3.907e-14

# Q-Q plot for visual check
qqnorm(residuals(model))
qqline(residuals(model))

library(car)
# Mauchly’s test for sphericity
mauchly.test(model)

library(car)
# Levene’s test for homogeneity of variances
leveneTest(prop_dead ~ treatment, data = Bacteria_mort)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value   Pr(>F)   
#group   7  3.3482 0.001917 **
      280                    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Boxplot to identify potential outliers
boxplot(prop_dead ~ treatment, data = Bacteria_mort)
# Load RColorBrewer package for the color palette
library(RColorBrewer)

# Set the color palette to "Set3"
colors <- brewer.pal(3, "Set3")

# Create the boxplot with the selected colors
boxplot(prop_dead ~ treatment, data = Bacteria_mort, 
        col = colors, 
        main = "Boxplot of Prop Dead by Treatment",
        ylab = "Proportion Dead",
        xlab = "Treatment")



# Cook’s distance for influential points
cooks_distances <- cooks.distance(model)
plot(cooks_distances)

#Running Kruskall Wallis 

# Filter data to keep only the final time point (t17)
final_data <- Bacteria_mort[Bacteria_mort$t == 't17', ]

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(final_data$prop_dead)

Display the results
print(shapiro_test)

#shapiro_test <- shapiro.test(final_data$prop_dead)
# print(shapiro_test)

#Shapiro-Wilk normality test

#data:  final_data$prop_dead
#W = 0.77103, p-value = 0.001152

#Display the results
#print(shapiro_test)

# Perform the Kruskal-Wallis test
kruskal_test <- kruskal.test(prop_dead ~ treatment, data = final_data)

# Display the results
#print(kruskal_test)
#Kruskal-Wallis rank sum test

#data:  prop_dead by treatment
#Kruskal-Wallis chi-squared = 0.9359, df = 7, p-value = 0.9958
#No significant differences in mortality by the end of the treatment
# print(kruskal_test)

#Kruskal-Wallis rank sum test

#data:  prop_dead by treatment
#Kruskal-Wallis chi-squared = 0.9359, df = 7, p-value = 0.9958


# Q-Q plot for normality check
qqnorm(final_data$prop_dead)
qqline(final_data$prop_dead, col = "red")

# Q-Q plot for normality check
qqnorm(final_data$prop_alive)
qqline(final_data$prop_alive, col = "red")

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(final_data$prop_alive)
print(shapiro_test)

#Shapiro-Wilk normality test

#data:  final_data$prop_alive
#W = 0.77103, p-value = 0.001152

# Perform the Kruskal-Wallis test
kruskal_test <- kruskal.test(prop_alive ~ treatment, data = final_data)

# Display the results
print(kruskal_test)
#Kruskal-Wallis rank sum test

#data: 	Kruskal-Wallis rank sum test

#data:  prop_alive by treatment
#Kruskal-Wallis chi-squared = 0.9359, df = 7, p-value = 0.9958

Sum_bacteria_KM <-  Bacteria_mort%>%
  group_by(treatment, age_days, status_dead) %>%
  summarise(count_sum = sum(dead, na.rm = TRUE))

Expanded_mort <- expandRows(Bacteria_mort, "dead")

Expanded_mort$treatment = factor(Expanded_mort$treatment, levels = c("PAhh", "v3", "f7", "f23","f24","f8", "all", "none"))

# Mortality
fitExpanded_mort = survfit(Surv(age_days, status_dead)~ treatment, data = Expanded_mort)
summary(fitExpanded_mort)


setplot_mortality <- ggsurvplot(fitExpanded_mort, size = 1,
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
                               palette = c("paleturquoise", "hotpink1", "darkmagenta", "darkseagreen","midnightblue","black", "steelblue", "gold1"),
                               legend = c(0.15, 0.9), legend.title = "", 
                               xlab = "Days", ylab = "Settlement probability", 
                               legend.labs = c("PAhh", "v3", "f7", "f23","f24","f8", "all", "none"))

setplot_mortality

#Pairwise comparisons using Log-Rank test 

# Settlement - Pairwise comparison between treatments, fixed effects only, p value adj. method BH
resbacteria <- pairwise_survdiff(Surv(age_days, status_dead) ~ treatment,
                                 data= Bacteria_mort)
resbacteria   
#	Pairwise comparisons using Log-Rank test 

#data:  Bacteria_mort and treatment 

#all  f23  f24  f7   f8   none PAhh
#f23  0.78 -    -    -    -    -    -   
#  f24  0.78 0.45 -    -    -    -    -   
#  f7   0.45 0.45 0.78 -    -    -    -   
#  f8   1.00 0.78 0.78 0.45 -    -    -   
#  none 0.56 0.45 0.87 0.87 0.56 -    -   
#  PAhh 0.82 0.89 0.52 0.45 0.82 0.45 -   
#  v3   0.82 0.89 0.52 0.45 0.82 0.45 1.00

#P value adjustment method: BH 

#Testing a 0.1 threshold

library(dplyr)
library(survival)
library(survminer)

library(dplyr)
library(survival)
library(survminer)







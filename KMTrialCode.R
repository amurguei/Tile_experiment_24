#install.packages("survival", dependencies = TRUE)   # only if not yet installed
#install.packages("survminer", dependencies = TRUE)  # only if not yet installed

library(ggplot2)
library(survival)
library(survminer)

ebola <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter21/chap21e1EbolaMalaria.csv"))
ebolaSurv <- Surv(time = ebola$time, event = ebola$outcome)
ebolaKM <- survfit(ebolaSurv ~ 1, data = ebola, type="kaplan-meier") 

ggsurvplot(ebolaKM, conf.int = TRUE, pval = FALSE, risk.table = FALSE, 
           legend = "none", censor.shape = "|", censor.size = 4, palette = c("firebrick"), 
           ylab = "Proportion surviving", xlab = "Time (days since admission)")
summary(ebolaKM, censored = FALSE)

tumors <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter21/chap21e2BreastCancer.csv"))
head(tumors)

tumorSurv <- Surv(time = tumors$OverallSurvivalMonths, event = tumors$status)
tumorKM <- survfit(tumorSurv ~ Subtype, data = tumors, type="kaplan-meier") 

ggsurvplot(tumorKM, conf.int = TRUE, pval = FALSE, risk.table = FALSE,
           legend.labs=c("Basal", "LuminalA"), 
           legend = c(0.15, 0.15), break.time.by = 25,
           legend.title = "Subtype",
           censor.shape = "|", censor.size = 4,
           palette=c("firebrick", "goldenrod1"), 
           xlab = "Time since diagnosis (months)",
           ylab = "Proportion surviving")

tumorDiff <- survdiff(tumorSurv ~ Subtype, data = tumors)
print(tumorDiff, digits = 4)

D1 <- tumorDiff$obs[1]
D2 <- tumorDiff$obs[2]
E1 <- tumorDiff$exp[1]
E2 <- tumorDiff$exp[2]
HR <- (D1/D2)/(E1/E2)
HR

SE_lnHR = sqrt(1/E1 + 1/E2)
SE_lnHR

L = log(HR)
lower <- exp(L - 1.96*SE_lnHR)
upper <- exp(L + 1.96*SE_lnHR)
ci95 <- c(lower=lower, upper=upper)
ci95







setwd("E:/Users/amurgueitio/Documents/Tiles_bacteria")

k = read.csv("KMinsituAll_jitter.csv")

Sum_all <- k %>% 
  group_by(treatment, age_days, dead_status) %>% 
  summarise_each(funs(sum), count)
View(Sum_all)

K = expandRows(k, "count")

K$treatment = factor(K$treatment, levels = c("SS","SD","DS","DD"))

### Fit KM curves ###

# Settlement
fitK = survfit(Surv(age_days, set_status)~ treatment, data = K)
summary(fitK)


names(fitK$strata) <- gsub("treatment=", "", names(fitK$strata))

setplot <- ggsurvplot(fitK, size = 1,
                      pval = TRUE, pval.method = TRUE,
                      pval.coord = c(0.4, 0.4),
                      pval.method.coord = c(0.4, 0.48),
                      fun = "event",
                      ggtheme = theme_classic()+
                        theme(axis.ticks = element_line(size = 1), axis.text = element_text(colour = "black", size = 10),
                              axis.line = element_line(size = 1), axis.title = element_text(size = 12), legend.background = element_blank(), legend.text = element_text(size = 10)),
                      palette = c("#f0f921", "#FCA510", "#87CEDD",  "#1035AC"),
                      legend = c(0.15, 0.9), legend.title = "", xlab="Days", ylab = "Settlement probability", legend.labs = c("SS","SD","DS","DD"))

setplot

# Settlement - Pairwise comparison between treatments, fixed effects only, p value adj. method BH
res <- pairwise_survdiff(Surv(age_days, set_status) ~ treatment,
                         data= K)
res   

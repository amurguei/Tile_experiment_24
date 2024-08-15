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
fitK = survfit(Surv(age_days, set_status)~ treatment, data = K)
summary(fitK)

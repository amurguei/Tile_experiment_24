#Code for Kaplan Meier curves settlement experiment March 2024
#install.packages("cmprsk")

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
library(readr)

setwd("E:/Users/amurgueitio/Documents/Tiles_bacteria/Tiles")

#Import dataset

tile_exp_long <- read_csv("tile_exp_long.csv")

#Make proportion of settled larvae, grouping settled_tile and settled_plastic

tile_exp_long$prop_settled <- (tile_exp_long$settled_plastic + tile_exp_long$settled_tile) / tile_exp_long$n_larvae

# Summarize data by treatment and age_days
Tile_summary <- tile_exp_long %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_prop = mean(prop_settled),
    sd_prop = sd(prop_settled),
    n = n(),  # number of replicates
    se_prop = sd_prop / sqrt(n)  # standard error of the mean
  )

# Ensure the treatment factor is ordered correctly
Tile_summary$treatment <- factor(Tile_summary$treatment, 
                                     levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot with mean and confidence intervals, and x-axis ranging from 1 to 15
mean_proportion_settled_larvae <- ggplot(Tile_summary, aes(x = age_days, y = mean_prop, color = treatment)) +
  geom_line(linewidth = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_prop - se_prop, ymax = mean_prop + se_prop, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Settled Larvae", 
       title = "Mean Proportion of Settled Larvae Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_fill_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_x_continuous(breaks = seq(1, 18, 1))  # Set x-axis to go from 1 to 18

plot(mean_proportion_settled_larvae)
# Save the plot
ggsave("mean_proportion_settled_larvae.png", 
       plot = last_plot(),  # Uses the most recent plot generated
       width = 8, height = 6,  # Dimensions in inches
       dpi = 300)  # Resolution in dots per inch (good for high-quality output)


#Proportion of larvae settled on plastic 


tile_exp_long$prop_setplastic <- (tile_exp_long$settled_plastic) / tile_exp_long$n_larvae

# Summarize data by treatment and age_days
Tile_summary_plastic <- tile_exp_long %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_plastic = mean(prop_setplastic),
    sd_plastic = sd(prop_setplastic),
    n = n(),  # number of replicates
    se_plastic = sd_plastic / sqrt(n)  # standard error of the mean
  )

# Ensure the treatment factor is ordered correctly
Tile_summary_plastic$treatment <- factor(Tile_summary_plastic$treatment, 
                                 levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot with mean and confidence intervals, and x-axis ranging from 1 to 15
mean_proportion_settled_plastic <- ggplot(Tile_summary_plastic, aes(x = age_days, y = mean_plastic, color = treatment)) +
  geom_line(linewidth = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_plastic - se_plastic, ymax = mean_plastic + se_plastic, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Larvae Settled on Well Plastic", 
       title = "Larvae Settled on Well Plastic Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_fill_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_x_continuous(breaks = seq(1, 18, 1))  # Set x-axis to go from 1 to 18

plot(mean_proportion_settled_plastic)
# Save the plot
ggsave("mean_proportion_settled_plastic.png", 
       plot = last_plot(),  # Uses the most recent plot generated
       width = 8, height = 6,  # Dimensions in inches
       dpi = 300)  # Resolution in dots per inch (good for high-quality output)

#Proportion of larvae settled in tiles

tile_exp_long$prop_settile <- (tile_exp_long$settled_tile) / tile_exp_long$n_larvae

# Summarize data by treatment and age_days
Tile_summary_tile <- tile_exp_long %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_tile = mean(prop_settile),
    sd_tile = sd(prop_settile),
    n = n(),  # number of replicates
    se_tile = sd_tile / sqrt(n)  # standard error of the mean
  )

# Ensure the treatment factor is ordered correctly
Tile_summary_tile$treatment <- factor(Tile_summary_tile$treatment, 
                                      levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot with mean and confidence intervals, and x-axis ranging from 1 to 15
mean_proportion_settled_tile <- ggplot(Tile_summary_tile, aes(x = age_days, y = mean_tile, color = treatment)) +
  geom_line(linewidth = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_tile - se_tile, ymax = mean_tile + se_tile, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Larvae Settled on Well tile", 
       title = "Larvae Settled on Well tile Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_fill_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_x_continuous(breaks = seq(1, 18, 1))  # Set x-axis to go from 1 to 18

plot(mean_proportion_settled_tile)


#Larvae settled on tile count


# Summarize data by treatment and age_days
Tile_summary_tile <- tile_exp_long %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_set_tile = mean(settled_tile),
    sd_set_tile = sd(settled_tile),
    n = n(),  # number of replicates
    se_set_tile = sd_set_tile / sqrt(n)  # standard error of the mean
  )

# Ensure the treatment factor is ordered correctly
Tile_summary_tile$treatment <- factor(Tile_summary_tile$treatment, 
                                      levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot with mean and confidence intervals, and x-axis ranging from 1 to 15
mean_count_tile <- ggplot(Tile_summary_tile, aes(x = age_days, y = mean_set_tile, color = treatment)) +
  geom_line(linewidth = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_set_tile - se_set_tile, ymax = mean_set_tile + se_set_tile, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Count of Larvae Settled on Tiles", 
       title = "Mean Count of Larvae Settled on Tiles Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_fill_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_x_continuous(breaks = seq(1, 18, 1))  # Set x-axis to go from 1 to 18

plot(mean_count_tile)
# Save the plot
ggsave("mean_count_settled_tile.png", 
       plot = last_plot(),  # Uses the most recent plot generated
       width = 8, height = 6,  # Dimensions in inches
       dpi = 300)  # Resolution in dots per inch (good for high-quality output)



#Plot summary of swimming larvae

#Swimming larvae proportion

Tile_summary_swim <- tile_exp_long %>%
     group_by(treatment, age_days) %>%
     summarize(
         mean_swim = mean(swim),
         sd_swim = sd(swim),
         n = n(),  # number of replicates
         se_swim = sd_swim / sqrt(n)  # standard error of the mean
       )

Tile_summary_swim$treatment <- factor(Tile_summary_swim$treatment, 
                                 levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot with mean and confidence intervals, and x-axis ranging from 1 to 15
mean_swimming_larvae <- ggplot(Tile_summary_swim, aes(x = age_days, y = mean_swim, color = treatment)) +
  geom_line(linewidth = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_swim - se_swim, ymax = mean_swim + se_swim, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Swimming Larvae", 
       title = "Mean Count of Swimming Larvae Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_fill_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_x_continuous(breaks = seq(1, 18, 1))  # Set x-axis to go from 1 to 18

# Save the plot
plot(mean_swimming_larvae)

ggsave("mean_swimming_larvae.png", 
       plot = last_plot(),  # Uses the most recent plot generated
       width = 8, height = 6,  # Dimensions in inches
       dpi = 300)  # Resolution in dots per inch (good for high-quality output)


#Mean proportion of swimming larvae
#Calculate proportion of swimming larvae
tile_exp_long$prop_swim <- tile_exp_long$swim / tile_exp_long$n_larvae

#Calculate summary
Tile_summary_prop_swim <- tile_exp_long %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_prop_swim = mean(prop_swim),
    sd_prop_swim = sd(prop_swim),
    n = n(),  # number of replicates
    se_prop_swim = sd_prop_swim / sqrt(n)  # standard error of the mean
  )

Tile_summary_prop_swim$treatment <- factor(Tile_summary_prop_swim$treatment, 
                                      levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot with mean and confidence intervals, and x-axis ranging from 1 to 18
mean_prop_swimming_larvae <- ggplot(Tile_summary_prop_swim, aes(x = age_days, y = mean_prop_swim, color = treatment)) +
  geom_line(linewidth = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_prop_swim - se_prop_swim, ymax = mean_prop_swim + se_prop_swim, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Stage I Larvae", 
       title = "Mean Proportion of Stage I Larvae Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_fill_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_x_continuous(breaks = seq(1, 18, 1))  # Set x-axis to go from 1 to 18

# Save the plot
plot(mean_prop_swimming_larvae)

ggsave("mean_prop_swimming_larvae.png", 
       plot = last_plot(),  # Uses the most recent plot generated
       width = 8, height = 6,  # Dimensions in inches
       dpi = 300)  # Resolution in dots per inch (good for high-quality output)


#Mean proportion of round larvae
#Calculate proportion of round larvae
tile_exp_long$prop_round <- tile_exp_long$round / tile_exp_long$n_larvae

#Calculate summary
Tile_summary_prop_round <- tile_exp_long %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_prop_round = mean(prop_round),
    sd_prop_round = sd(prop_round),
    n = n(),  # number of replicates
    se_prop_round = sd_prop_round / sqrt(n)  # standard error of the mean
  )

Tile_summary_prop_round$treatment <- factor(Tile_summary_prop_round$treatment, 
                                            levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot with mean and confidence intervals, and x-axis ranging from 1 to 18
mean_prop_round_larvae <- ggplot(Tile_summary_prop_round, aes(x = age_days, y = mean_prop_round, color = treatment)) +
  geom_line(linewidth = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_prop_round - se_prop_round, ymax = mean_prop_round + se_prop_round, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Stage II Larvae", 
       title = "Mean Proportion of Stage II Larvae Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_fill_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_x_continuous(breaks = seq(1, 18, 1))  # Set x-axis to go from 1 to 18

# Save the plot
plot(mean_prop_round_larvae)

ggsave("mean_prop_StageII_larvae.png", 
       plot = last_plot(),  # Uses the most recent plot generated
       width = 8, height = 6,  # Dimensions in inches
       dpi = 300)  # Resolution in dots per inch (good for high-quality output)

#Mean proportion of detached larvae
#Calculate proportion of detached larvae
tile_exp_long$prop_detached <- tile_exp_long$detached / tile_exp_long$n_larvae

#Calculate summary
Tile_summary_prop_detached <- tile_exp_long %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_prop_detached = mean(prop_detached),
    sd_prop_detached = sd(prop_detached),
    n = n(),  # number of replicates
    se_prop_detached = sd_prop_detached / sqrt(n)  # standard error of the mean
  )

Tile_summary_prop_detached$treatment <- factor(Tile_summary_prop_detached$treatment, 
                                               levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot with mean and confidence intervals, and x-axis ranging from 1 to 18
mean_prop_detached_spat <- ggplot(Tile_summary_prop_detached, aes(x = age_days, y = mean_prop_detached, color = treatment)) +
  geom_line(linewidth = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_prop_detached - se_prop_detached, ymax = mean_prop_detached + se_prop_detached, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of  Detached Spat", 
       title = "Mean Proportion of Detached Spat Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_fill_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_x_continuous(breaks = seq(1, 18, 1))  # Set x-axis to go from 1 to 18

# Save the plot
plot(mean_prop_detached_spat)

ggsave("mean_prop_detached_spat.png", 
       plot = last_plot(),  # Uses the most recent plot generated
       width = 8, height = 6,  # Dimensions in inches
       dpi = 300)  # Resolution in dots per inch (good for high-quality output)


#Mortality

#Mean proportion of dead larvae
#Calculate proportion of dead larvae
tile_exp_long$prop_dead <- tile_exp_long$dead / tile_exp_long$n_larvae

#Calculate summary
Tile_summary_prop_dead <- tile_exp_long %>%
  group_by(treatment, age_days) %>%
  summarize(
    mean_prop_dead = mean(prop_dead),
    sd_prop_dead = sd(prop_dead),
    n = n(),  # number of replicates
    se_prop_dead = sd_prop_dead / sqrt(n)  # standard error of the mean
  )

Tile_summary_prop_dead$treatment <- factor(Tile_summary_prop_dead$treatment, 
                                           levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot with mean and confidence intervals, and x-axis ranging from 1 to 18
mean_prop_dead_larvae <- ggplot(Tile_summary_prop_dead, aes(x = age_days, y = mean_prop_dead, color = treatment)) +
  geom_line(linewidth = 1) +  # Continuous mean line plot for each treatment
  geom_ribbon(aes(ymin = mean_prop_dead - se_prop_dead, ymax = mean_prop_dead + se_prop_dead, fill = treatment), 
              alpha = 0.2, color = NA) +  # Confidence intervals
  theme_classic() +
  labs(x = "Days", y = "Mean Proportion of Dead Larvae", 
       title = "Mean Proportion of Dead Larvae Over Time by Treatment",
       color = "Treatment", fill = "Treatment") +
  scale_color_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_fill_manual(values = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")) +
  scale_x_continuous(breaks = seq(1, 18, 1))  # Set x-axis to go from 1 to 18

# Save the plot
plot(mean_prop_dead_larvae)

ggsave("mean_prop_dead_larvae.png", 
       plot = last_plot(),  # Uses the most recent plot generated
       width = 8, height = 6,  # Dimensions in inches
       dpi = 300)  # Resolution in dots per inch (good for high-quality output)

#Attempt at NMDS, this one is basic, I'll try a cooler one
Tiles_wide <- read_csv("tile_exp24_somemods - Sheet1 (1).csv")

# Load the vegan package
library(vegan)

# Select the columns 4 to 110 for calculating the dissimilarity matrix
data_matrix <- Tiles_wide[, 4:110]

# Perform NMDS using the Bray-Curtis dissimilarity
nmds_result <- metaMDS(data_matrix, distance = "bray", k = 2, trymax = 100)

# Plot NMDS and color by treatment
treatment_col <- Tiles_wide$treatment  # Define colors based on treatment

# Plot NMDS
plot(nmds_result$points, 
     col = as.factor(treatment_col), 
     pch = 19, 
     xlab = "NMDS1", 
     ylab = "NMDS2",
     main = "NMDS of Time Series by Treatment")

# Add legend
legend("topright", legend = levels(as.factor(treatment_col)), col = 1:length(unique(treatment_col)), pch = 19)


#Here's a nicer version with shapes 
# Load the vegan package
library(vegan)

# Select the columns 4 to 110 for calculating the dissimilarity matrix
data_matrix <- Tiles_wide[, 4:110]

# Perform NMDS using the Bray-Curtis dissimilarity
nmds_result <- metaMDS(data_matrix, distance = "bray", k = 2, trymax = 100)

# Define the treatments, colors, and shapes
treatment_levels <- c("seawater", "REF1", "REF2", "20%", "35%", "50%")
treatment_col <- Tiles_wide$treatment  # Extract treatment column

# Define custom colors and shapes for each treatment
color_palette <- c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")
shape_palette <- c(15, 16, 17, 18, 19, 8)  # Different shapes for each treatment

# Map treatments to colors and shapes
color_vector <- color_palette[match(treatment_col, treatment_levels)]
shape_vector <- shape_palette[match(treatment_col, treatment_levels)]

# Plot NMDS with custom colors and shapes
plot(nmds_result$points, 
     col = color_vector, 
     pch = shape_vector, 
     xlab = "NMDS1", 
     ylab = "NMDS2",
     main = "NMDS of Time Series by Treatment")

# Add legend with specified colors, shapes, and treatments
legend("topright", legend = treatment_levels, col = color_palette, pch = shape_palette)


#Nice! Now with convex hulls

# Load required libraries
library(vegan)
library(ggplot2)
library(patchwork)  # For geom_mark_hull

# Select the columns 4 to 110 for calculating the dissimilarity matrix
data_matrix <- Tiles_wide[, 4:110]

# Perform NMDS using the Bray-Curtis dissimilarity
nmds_result <- metaMDS(data_matrix, distance = "bray", k = 2, trymax = 100)

# Extract NMDS coordinates
nmds_points <- as.data.frame(nmds_result$points)
nmds_points$treatment <- Tiles_wide$treatment  # Add treatment information

# Define the treatments, colors, and shapes
treatment_levels <- c("seawater", "REF1", "REF2", "20%", "35%", "50%")
color_palette <- c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1")
shape_palette <- c(15, 16, 17, 18, 19, 8)  # Different shapes for each treatment

# Map treatments to colors and shapes
nmds_points$treatment <- factor(nmds_points$treatment, levels = treatment_levels)
color_vector <- color_palette[as.numeric(nmds_points$treatment)]
shape_vector <- shape_palette[as.numeric(nmds_points$treatment)]

# Create the NMDS plot with convex hulls
ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = color_palette) +
  scale_shape_manual(values = shape_palette) +
  geom_mark_hull(aes(fill = treatment), alpha = 0.2, show.legend = FALSE) +  # Add convex hulls
  labs(title = "NMDS of Time Series by Treatment") +
  theme_minimal() +
  theme(legend.position = "right")

#This is not working

# Load the vegan package
library(vegan)
library(cluster)
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

# Select the columns 4 to 110 for calculating the dissimilarity matrix
data_matrix <- Tiles_wide[, 4:110]

# Calculate the Bray-Curtis dissimilarity matrix
diss_matrix <- vegdist(data_matrix, method = "bray")

# Extract the treatment column
treatment <- Tiles_wide$treatment

# Perform PERMANOVA using adonis2
adonis2_result <- adonis2(diss_matrix ~ treatment, permutations = 9999)

# Print the results
print(adonis2_result)
#Permutation test for adonis under reduced model
#Terms added sequentially (first to last)
#Permutation: free
#Number of permutations: 9999

#adonis2(formula = diss_matrix ~ treatment, permutations = 9999)
#Df SumOfSqs      R2      F Pr(>F)    
#treatment  5   2.6133 0.18952 3.9283  1e-04 ***
#  Residual  84  11.1763 0.81048                  
#Total     89  13.7896 1.00000                  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

pairwise.adonis(diss_matrix, treatment, sim.method="bray")
#pairs Df  SumsOfSqs    F.Model         R2 p.value p.adjusted sig
#1      REF1 vs REF2  1 0.05903498  0.3939234 0.01387351   0.730      1.000    
#2       REF1 vs 20%  1 0.34452760  2.3566842 0.07763312   0.098      1.000    
#3       REF1 vs 35%  1 0.40556584  2.8986106 0.09381039   0.046      0.690    
#4       REF1 vs 50%  1 0.14303807  0.9626519 0.03323770   0.343      1.000    
#5  REF1 vs seawater  1 1.52613580 12.8140750 0.31396216   0.001      0.015   .
#6       REF2 vs 20%  1 0.26207040  1.8224283 0.06110932   0.147      1.000    
#7       REF2 vs 35%  1 0.25554630  1.8581332 0.06223206   0.154      1.000    
#8       REF2 vs 50%  1 0.14805453  1.0126938 0.03490520   0.351      1.000    
#9  REF2 vs seawater  1 1.26377778 10.8283977 0.27887830   0.001      0.015   .
#10       20% vs 35%  1 0.08625343  0.6443745 0.02249567   0.604      1.000    
#11       20% vs 50%  1 0.13249069  0.9295879 0.03213277   0.397      1.000    
#12  20% vs seawater  1 0.93150009  8.2406559 0.22738705   0.002      0.030   .
#13       35% vs 50%  1 0.25933333  1.9033372 0.06364966   0.131      1.000    
#14  35% vs seawater  1 0.56789300  5.3192036 0.15964378   0.007      0.105    
#15  50% vs seawater  1 1.45482716 12.6032192 0.31039951   0.001      0.015   .

#Checking for batch effects (plate_id)

# Load the vegan package
library(vegan)

# Select the columns 4 to 110 for calculating the dissimilarity matrix
data_matrix <- Tiles_wide[, 4:110]

# Perform NMDS using the Bray-Curtis dissimilarity
nmds_result <- metaMDS(data_matrix, distance = "bray", k = 2, trymax = 100)

# Extract NMDS coordinates
nmds_points <- as.data.frame(nmds_result$points)
nmds_points$plate_id <- Tiles_wide$plate_id  # Add plate_id information

# Define custom colors and shapes for each plate_id
plate_levels <- unique(nmds_points$plate_id)
color_palette <- rainbow(length(plate_levels))  # Generate a color palette
shape_palette <- 15:(14 + length(plate_levels))  # Different shapes for each plate_id

# Map plate_id to colors and shapes
color_vector <- color_palette[as.numeric(factor(nmds_points$plate_id, levels = plate_levels))]
shape_vector <- shape_palette[as.numeric(factor(nmds_points$plate_id, levels = plate_levels))]

# Plot NMDS with custom colors and shapes
plot(nmds_result$points, 
     col = color_vector, 
     pch = shape_vector, 
     xlab = "NMDS1", 
     ylab = "NMDS2",
     main = "NMDS of Time Series by Plate ID")

# Add legend with specified colors, shapes, and plate_id
legend("topright", 
       legend = plate_levels, 
       col = color_palette, 
       pch = shape_palette, 
       title = "Plate ID")

# Load the vegan package
library(vegan)

# Select the columns 4 to 110 for calculating the dissimilarity matrix
data_matrix <- Tiles_wide[, 4:110]

# Perform NMDS using the Bray-Curtis dissimilarity
nmds_result <- metaMDS(data_matrix, distance = "bray", k = 2, trymax = 100)

# Extract NMDS coordinates
nmds_points <- as.data.frame(nmds_result$points)
nmds_points$plate_id <- Tiles_wide$plate_id  # Add plate_id information

# Define custom colors and shapes for each plate_id
plate_levels <- unique(nmds_points$plate_id)
color_palette <- rainbow(length(plate_levels))  # Generate a color palette

# Define a valid set of shapes
shape_palette <- 15:(14 + length(plate_levels))  # Shapes from 15 onwards
valid_shapes <- c(15, 16, 17, 18, 19, 20, 21, 22)  # Valid shapes in R

# Ensure we don't use more shapes than available
shape_palette <- shape_palette[shape_palette %in% valid_shapes]

# Map plate_id to colors and shapes
color_vector <- color_palette[as.numeric(factor(nmds_points$plate_id, levels = plate_levels))]
shape_vector <- shape_palette[as.numeric(factor(nmds_points$plate_id, levels = plate_levels))]

# Plot NMDS with custom colors and shapes
plot(nmds_result$points, 
     col = color_vector, 
     pch = shape_vector, 
     xlab = "NMDS1", 
     ylab = "NMDS2",
     main = "NMDS of Time Series by Plate ID")

# Add legend with specified colors, shapes, and plate_id
legend("topright", 
       legend = plate_levels, 
       col = color_palette, 
       pch = shape_palette, 
       title = "Plate ID")
#It's coming out w/ mistake, check later


#Proportional logistic regression:
# Fit a GLM model for proportions with a binomial family and logit link
data_logit_glm_settlement <- tile_exp_long %>%
  select(t, age_days, treatment, n_larvae, prop_settled) %>%
  mutate(event_time = age_days)  # Time variable (age of larvae)

# Ensure treatment is a factor
data_logit_glm_settlement$treatment <- factor(data_logit_glm_settlement$treatment)

# Reorder the levels, setting 'control' as the baseline
data_logit_glm_settlement$treatment <- relevel(data_logit_glm_settlement$treatment, ref = "seawater")

# Fit the model again
glm_model <- glm(prop_settled ~ event_time * treatment, 
                 family = quasibinomial(link = "logit"), 
                 data = data_logit_glm_settlement)
#Call:
glm(formula = prop_settled ~ event_time * treatment, family = quasibinomial(link = "logit"), 
    data = data_logit_glm_settlement)

# Summary of the updated model
summary(glm_model)


#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5165  -0.4152  -0.3015   0.2113   1.3893  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              -2.06405    0.15042 -13.722  < 2e-16 ***
#  event_time                0.15740    0.01306  12.054  < 2e-16 ***
#  treatment20%             -0.76730    0.27069  -2.835 0.004645 ** 
#  treatment35%             -0.63977    0.24804  -2.579 0.009989 ** 
#  treatment50%             -1.04117    0.30869  -3.373 0.000761 ***
#  treatmentREF1            -0.56292    0.25897  -2.174 0.029872 *  
#  treatmentREF2            -0.85670    0.27465  -3.119 0.001845 ** 
#  event_time:treatment20%  -0.09388    0.02290  -4.100 4.34e-05 ***
#  event_time:treatment35%  -0.05537    0.02067  -2.679 0.007450 ** 
#  event_time:treatment50%  -0.12049    0.02671  -4.512 6.90e-06 ***
#  event_time:treatmentREF1 -0.10265    0.02214  -4.636 3.84e-06 ***
#  event_time:treatmentREF2 -0.08645    0.02306  -3.749 0.000184 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for quasibinomial family taken to be 0.2204225)

#Null deviance: 537.76  on 1619  degrees of freedom
#Residual deviance: 362.02  on 1608  degrees of freedom
#AIC: NA

#Number of Fisher Scoring iterations: 57

#Count of settled larvae and assumptions for ANOVA in final point

tile_exp_long$count_settled <- tile_exp_long$settled_tile + tile_exp_long$settled_plastic

t17_data <- subset(tile_exp_long, t == "t17") #Make subset for final point
shapiro_test <- shapiro.test(t17_data$count_settled)
shapiro_test

# Histogram
hist(t17_data$count_settled, main = "Histogram of count_settled at t17", xlab = "Count Settled", col = "lightblue")

# Q-Q plot
qqnorm(t17_data$count_settled)
qqline(t17_data$count_settled, col = "red")

# Adding a small constant and log transformation
t17_data$count_settled_log <- log(t17_data$count_settled + 0.1)

# Checking normality after log transformation
shapiro_test_log <- shapiro.test(t17_data$count_settled_log)
print(shapiro_test_log)
#
#Shapiro-Wilk normality test

#data:  t17_data$count_settled_log
#W = 0.8228, p-value = 5.189e-09

# Or square root transformation
t17_data$count_settled_sqrt <- sqrt(t17_data$count_settled + 0.1)
## Checking normality after square root transformation
shapiro_test_sqrt <- shapiro.test(t17_data$count_settled_sqrt)
print(shapiro_test_sqrt)
#	Shapiro-Wilk normality test

#data:  t17_data$count_settled_sqrt
#W = 0.88392, p-value = 8.275e-07

# Kruskal-Wallis test for count_settled
kruskal_test <- kruskal.test(count_settled ~ treatment, data = t17_data)
print(kruskal_test)

#
#Kruskal-Wallis rank sum test

#data:  count_settled by treatment
#Kruskal-Wallis chi-squared = 27.257, df = 5, p-value = 5.084e-05

install.packages("FSA")

library(FSA)

# Post-hoc Dunn test with Bonferroni
dunn_test <- dunnTest(count_settled ~ treatment, data = t17_data, method = "bonferroni")
print(dunn_test)

#Comparison          Z      P.unadj        P.adj
#1        20% - 35% -1.0161698 3.095485e-01 1.000000e+00
#2        20% - 50%  1.0018070 3.164368e-01 1.000000e+00
#3        35% - 50%  2.0179767 4.359368e-02 6.539053e-01
#4       20% - REF1 -0.1400375 8.886303e-01 1.000000e+00
#5       35% - REF1  0.8761323 3.809581e-01 1.000000e+00
#6       50% - REF1 -1.1418445 2.535187e-01 1.000000e+00
#7       20% - REF2 -0.3590706 7.195423e-01 1.000000e+00
#8       35% - REF2  0.6570992 5.111171e-01 1.000000e+00
#9       50% - REF2 -1.3608776 1.735524e-01 1.000000e+00
#10     REF1 - REF2 -0.2190331 8.266243e-01 1.000000e+00
#11  20% - seawater -3.8169204 1.351277e-04 2.026916e-03
#12  35% - seawater -2.8007506 5.098390e-03 7.647585e-02
#13  50% - seawater -4.8187274 1.444768e-06 2.167152e-05
#14 REF1 - seawater -3.6768829 2.361014e-04 3.541520e-03
#15 REF2 - seawater -3.4578498 5.445048e-04 8.167572e-03

# Post-hoc Dunn test with bh
dunn_test <- dunnTest(count_settled ~ treatment, data = t17_data, method = "bh")
print(dunn_test)
#print(dunn_test)
#Dunn (1964) Kruskal-Wallis multiple comparison
#p-values adjusted with the Benjamini-Hochberg method.

#Comparison          Z      P.unadj        P.adj
#1        20% - 35% -1.0161698 3.095485e-01 5.159142e-01
#2        20% - 50%  1.0018070 3.164368e-01 4.746553e-01
#3        35% - 50%  2.0179767 4.359368e-02 1.089842e-01
#4       20% - REF1 -0.1400375 8.886303e-01 8.886303e-01
#5       35% - REF1  0.8761323 3.809581e-01 5.194884e-01
#6       50% - REF1 -1.1418445 2.535187e-01 4.753475e-01
#7       20% - REF2 -0.3590706 7.195423e-01 8.302411e-01
#8       35% - REF2  0.6570992 5.111171e-01 6.388964e-01
#9       50% - REF2 -1.3608776 1.735524e-01 3.718980e-01
#10     REF1 - REF2 -0.2190331 8.266243e-01 8.856689e-01
#11  20% - seawater -3.8169204 1.351277e-04 1.013458e-03
#12  35% - seawater -2.8007506 5.098390e-03 1.529517e-02
#13  50% - seawater -4.8187274 1.444768e-06 2.167152e-05
#14 REF1 - seawater -3.6768829 2.361014e-04 1.180507e-03
#15 REF2 - seawater -3.4578498 5.445048e-04 2.041893e-03
 

#20% - seawater (p.adj = 0.0010)
#35% - seawater (p.adj = 0.0153)
#50% - seawater (p.adj = 0.00002)
#REF1 - seawater (p.adj = 0.0012)
#REF2 - seawater (p.adj = 0.0020)

#Boxplot for last point counts
# Define the color palette
color_palette <- c("seawater" = "darkturquoise", 
                   "REF1" = "springgreen4", 
                   "REF2" = "lightgreen", 
                   "20%" = "lightpink", 
                   "35%" = "palevioletred", 
                   "50%" = "deeppink1")

# Create the boxplot with means plotted as circles in red
ggplot(t17_data, aes(x = treatment, y = count_settled, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = color_palette) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3, shape = 16) +
  labs(title = "Settlement Counts at the End of the Experiment",
       x = "Treatment",
       y = "Settlement Count") +
  theme_minimal()

# Define the color palette
color_palette <- c("seawater" = "darkturquoise", 
                   "REF1" = "springgreen4", 
                   "REF2" = "lightgreen", 
                   "20%" = "lightpink", 
                   "35%" = "palevioletred", 
                   "50%" = "deeppink1")

# Create the boxplot with red and black circles for the means
ggplot(t17_data, aes(x = treatment, y = count_settled, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = color_palette) +
  # Red dot (slightly larger)
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 16) +
  # Black circle (slightly smaller)
  stat_summary(fun = mean, geom = "point", color = "red", size = 2, shape = 16) +
  labs(title = "Settlement Counts at the End of the Experiment",
       x = "Treatment",
       y = "Settlement Count") +
  theme_minimal()


#Proportional logistic regression:
# Fit a GLM model for proportions with a binomial family and logit link
data_logit_glm_dead <- tile_exp_long %>%
  select(t, age_days, treatment, n_larvae, prop_dead) %>%
  mutate(event_time = age_days)  # Time variable (age of larvae)

# Ensure treatment is a factor
data_logit_glm_dead$treatment <- factor(data_logit_glm_dead$treatment)

# Reorder the levels, setting 'control' as the baseline
data_logit_glm_dead$treatment <- relevel(data_logit_glm_dead$treatment, ref = "seawater")

# Fit the model again
glm_model_dead <- glm(prop_dead ~ event_time * treatment, 
                 family = quasibinomial(link = "logit"), 
                 data = data_logit_glm_dead)
#Call:
glm(formula = prop_dead~ event_time * treatment, family = quasibinomial(link = "logit"), 
    data = data_logit_glm_dead)

# Summary of the updated model
summary(glm_model_dead)
#Call:
#glm(formula = prop_dead ~ event_time * treatment, family = quasibinomial(link = "logit"), 
#    data = data_logit_glm_dead)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.2158  -0.5250  -0.2233   0.2086   1.8827  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              -4.954784   0.660518  -7.501 1.04e-13 ***
#  event_time                0.115963   0.050213   2.309  0.02105 *  
#  treatment20%              1.960173   0.713950   2.746  0.00611 ** 
#  treatment35%              1.493135   0.732917   2.037  0.04179 *  
#  treatment50%              2.982427   0.690137   4.321 1.64e-05 ***
#  treatmentREF1             3.265133   0.685251   4.765 2.06e-06 ***
#  treatmentREF2             3.190550   0.686690   4.646 3.66e-06 ***
#  event_time:treatment20%   0.004312   0.054629   0.079  0.93710    
#event_time:treatment35%   0.018086   0.055883   0.324  0.74625    
#event_time:treatment50%  -0.029250   0.053010  -0.552  0.58117    
#event_time:treatmentREF1 -0.017105   0.052647  -0.325  0.74530    
#event_time:treatmentREF2 -0.026815   0.052751  -0.508  0.61129    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for quasibinomial family taken to be 0.3574965)

#Null deviance: 789.52  on 1619  degrees of freedom
#Residual deviance: 590.75  on 1608  degrees of freedom
#AIC: NA

#Number of Fisher Scoring iterations: 7
#Call:

#Shapiro for normality of dead counts

shapiro_test <- shapiro.test(t17_data$dead)
shapiro_test
#Shapiro-Wilk normality test

#data:  t17_data$dead
#W = 0.83806, p-value = 1.639e-08

shapiro_test <- shapiro.test(t17_data$dead)
shapiro_test

# Histogram
hist(t17_data$dead, main = "Histogram of dead at t17", xlab = "Count Settled", col = "lightblue")

# Q-Q plot
qqnorm(t17_data$dead)
qqline(t17_data$dead, col = "red")

# Adding a small constant and log transformation
t17_data$dead_log <- log(t17_data$dead + 0.1)

# Checking normality after log transformation
shapiro_test_log_dead <- shapiro.test(t17_data$dead_log)
print(shapiro_test_log_dead)
#Shapiro-Wilk normality test

#data:  t17_data$dead_log
#W = 0.84219, p-value = 2.264e-08

# Adding a small constant and log transformation
t17_data$dead_log <- log(t17_data$dead + 0.1)
#	Shapiro-Wilk normality test

#data:  t17_data$dead_log
#W = 0.84219, p-value = 2.264e-08

# Checking normality after log transformation
shapiro_test_log_dead <- shapiro.test(t17_data$dead_log)
print(shapiro_test_log_dead)

# Or square root transformation
t17_data$dead_sqrt <- sqrt(t17_data$dead + 0.1)
## Checking normality after square root transformation
shapiro_test_sqrt <- shapiro.test(t17_data$dead_sqrt)
print(shapiro_test_sqrt)
#	Shapiro-Wilk normality test

#data:  t17_data$dead_sqrt
#W = 0.91241, p-value = 1.521e-05
#	Shapiro-Wilk normality test

#data:  t17_data$dead_sqrt
#W = 0.88392, p-value = 8.275e-07

# Kruskal-Wallis test for dead
kruskal_test <- kruskal.test(dead ~ treatment, data = t17_data)
print(kruskal_test)
#Kruskal-Wallis rank sum test

#data:  dead by treatment
#Kruskal-Wallis chi-squared = 18.815, df = 5, p-value = 0.002081

# Post-hoc Dunn test with Bonferroni
dunn_test <- dunnTest(dead ~ treatment, data = t17_data, method = "bonferroni")
print(dunn_test)
#Comparison            Z      P.unadj       P.adj
#1        20% - 35% -0.305106313 0.7602851834 1.000000000
#2        20% - 50% -1.312666694 0.1892952852 1.000000000
#3        35% - 50% -1.007560382 0.3136655566 1.000000000
#4       20% - REF1 -1.223972999 0.2209624136 1.000000000
#5       35% - REF1 -0.918866686 0.3581653068 1.000000000
#6       50% - REF1  0.088693696 0.9293253430 1.000000000
#7       20% - REF2 -1.231068494 0.2182972478 1.000000000
#8       35% - REF2 -0.925962182 0.3544656343 1.000000000
#9       50% - REF2  0.081598200 0.9349662328 1.000000000
#10     REF1 - REF2 -0.007095496 0.9943386611 1.000000000
#11  20% - seawater  2.263463111 0.0236071562 0.354107342
#12  35% - seawater  2.568569423 0.0102119255 0.153178883
#13  50% - seawater  3.576129805 0.0003487185 0.005230778
#14 REF1 - seawater  3.487436109 0.0004876754 0.007315131
#15 REF2 - seawater  3.494531605 0.0004748944 0.007123416

# Post-hoc Dunn test with bh
dunn_test <- dunnTest(dead ~ treatment, data = t17_data, method = "bh")
print(dunn_test)
#       Comparison            Z      P.unadj       P.adj
#1        20% - 35% -0.305106313 0.7602851834 0.950356479
#2        20% - 50% -1.312666694 0.1892952852 0.473238213
#3        35% - 50% -1.007560382 0.3136655566 0.522775928
#4       20% - REF1 -1.223972999 0.2209624136 0.414304526
#5       35% - REF1 -0.918866686 0.3581653068 0.488407237
#6       50% - REF1  0.088693696 0.9293253430 1.000000000
#7       20% - REF2 -1.231068494 0.2182972478 0.467779817
#8       35% - REF2 -0.925962182 0.3544656343 0.531698451
#9       50% - REF2  0.081598200 0.9349662328 1.000000000
#10     REF1 - REF2 -0.007095496 0.9943386611 0.994338661
#11  20% - seawater  2.263463111 0.0236071562 0.070821468
#12  35% - seawater  2.568569423 0.0102119255 0.038294721
#13  50% - seawater  3.576129805 0.0003487185 0.005230778
#14 REF1 - seawater  3.487436109 0.0004876754 0.002438377
#15 REF2 - seawater  3.494531605 0.0004748944 0.003561708

# Define the color palette
color_palette <- c("seawater" = "darkturquoise", 
                   "REF1" = "springgreen4", 
                   "REF2" = "lightgreen", 
                   "20%" = "lightpink", 
                   "35%" = "palevioletred", 
                   "50%" = "deeppink1")

# Create the boxplot with red and black circles for the means
ggplot(t17_data, aes(x = treatment, y = dead, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = color_palette) +
  # Red dot (slightly larger)
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 16) +
  # Black circle (slightly smaller)
  stat_summary(fun = mean, geom = "point", color = "red", size = 2, shape = 16) +
  labs(title = "Mortality Counts at the End of the Experiment",
       x = "Treatment",
       y = "Mortality Count") +
  theme_minimal()

# KM settlement

tile_exp_long <- tile_exp_long %>%
  mutate(set_status = ifelse(prop_settled >= 0.1, 1, 0))

# Summarize the data
Sum_tiles <- tile_exp_long %>%
  group_by(treatment, age_days, set_status) %>%
  summarise(count_sum = sum(count_settled, na.rm = TRUE))

# Expand the dataset to have a row per larva
Expanded_tiles <- expandRows(tile_exp_long, "count_settled")

# Recalculate the binary event for the expanded data
Expanded_tiles <- Expanded_tiles %>%
  mutate(set_status = ifelse(prop_settled >= 0.1, 1, 0))

# Set factor levels for treatments
Expanded_tiles$treatment = factor(Expanded_tiles$treatment, levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Kaplan-Meier survival fit with some settlement as the event
fitExpanded_tiles = survfit(Surv(age_days, set_status) ~ treatment, data = Expanded_tiles)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_tiles)

# Plot the Kaplan-Meier curve
setplot_tiles_settlement <- ggsurvplot(fitExpanded_tiles, size = 1,
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
                                  palette = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1"),
                                  legend = c(0.15, 0.9), legend.title = "", 
                                  xlab = "Days", ylab = "Probability of seeing some settlement", 
                                  legend.labs = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot the results
setplot_tiles_settlement

# Pairwise comparison using the set_status (probability of some settlement) column
settlement_comparison <- pairwise_survdiff(Surv(age_days, set_status) ~ treatment,
                                    data = Expanded_tiles,
                                    p.adjust.method = "BH")

settlement_comparison
#
#Pairwise comparisons using Log-Rank test 

#data:  Expanded_tiles and treatment 

#seawater REF1 REF2 20%  35% 
 # REF1 0.25     -    -    -    -   
#  REF2 0.48     0.74 -    -    -   
#  20%  0.35     0.80 0.84 -    -   
#  35%  0.80     0.35 0.65 0.60 -   
#  50%  0.25     0.80 0.65 0.74 0.35

#P value adjustment method: BH 


# Create the binary event_30 for larvae with >=30% settlement
tile_exp_long <- tile_exp_long %>%
  mutate(event_30 = ifelse(prop_settled >= 0.3, 1, 0))

# Summarize the data
Sum_tiles_event_30 <- tile_exp_long %>%
  group_by(treatment, age_days, event_30) %>%
  summarise(count_sum = sum(count_settled, na.rm = TRUE))

# Expand the dataset to have a row per larva
Expanded_tiles_event_30 <- expandRows(tile_exp_long, "count_settled")

# Recalculate the binary event for the expanded data
Expanded_tiles_event_30 <- Expanded_tiles_event_30 %>%
  mutate(event_30 = ifelse(prop_settled >= 0.3, 1, 0))

# Set factor levels for treatments
Expanded_tiles_event_30$treatment = factor(Expanded_tiles_event_30$treatment, levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Kaplan-Meier survival fit with 30% settlement as the event
fitExpanded_tiles_event_30 = survfit(Surv(age_days, event_30) ~ treatment, data = Expanded_tiles_event_30)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_tiles_event_30)

# Plot the Kaplan-Meier curve
setplot_tiles_event_30 <- ggsurvplot(fitExpanded_tiles_event_30, size = 1,
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
                                     palette = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1"),
                                     legend = c(0.15, 0.9), legend.title = "", 
                                     xlab = "Days", ylab = "Probability of seeing 30% settlement", 
                                     legend.labs = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot the results
setplot_tiles_event_30

# Pairwise comparison using the event_30 (probability of 30% settlement) column
settlement_comparison_event_30 <- pairwise_survdiff(Surv(age_days, event_30) ~ treatment,
                                                    data = Expanded_tiles_event_30,
                                                    p.adjust.method = "BH")

settlement_comparison_event_30

#Pairwise comparisons using Log-Rank test 

#data:  Expanded_tiles_event_30 and treatment 

#seawater REF1    REF2    20%     35%    
#  REF1 1.3e-10  -       -       -       -      
#  REF2 2.8e-15  0.14170 -       -       -      
#  20%  4.3e-09  0.70539 0.07579 -       -      
#  35%  0.00015  0.00232 5.5e-06 0.00915 -      
#  50%  0.01644  0.02269 0.00055 0.05949 0.92986

#P value adjustment method: BH 

# Create the binary event_50 for larvae with >=50% settlement
tile_exp_long <- tile_exp_long %>%
  mutate(event_50 = ifelse(prop_settled >= 0.5, 1, 0))

# Summarize the data
Sum_tiles_event_50 <- tile_exp_long %>%
  group_by(treatment, age_days, event_50) %>%
  summarise(count_sum = sum(count_settled, na.rm = TRUE))

# Expand the dataset to have a row per larva
Expanded_tiles_event_50 <- expandRows(tile_exp_long, "count_settled")

# Recalculate the binary event for the expanded data
Expanded_tiles_event_50 <- Expanded_tiles_event_50 %>%
  mutate(event_50 = ifelse(prop_settled >= 0.5, 1, 0))

# Set factor levels for treatments
Expanded_tiles_event_50$treatment = factor(Expanded_tiles_event_50$treatment, levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Kaplan-Meier survival fit with 50% settlement as the event
fitExpanded_tiles_event_50 = survfit(Surv(age_days, event_50) ~ treatment, data = Expanded_tiles_event_50)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_tiles_event_50)

# Plot the Kaplan-Meier curve
setplot_tiles_event_50 <- ggsurvplot(fitExpanded_tiles_event_50, size = 1,
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
                                     palette = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1"),
                                     legend = c(0.15, 0.9), legend.title = "", 
                                     xlab = "Days", ylab = "Probability of seeing 50% settlement", 
                                     legend.labs = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot the results
setplot_tiles_event_50

# Pairwise comparison using the event_50 (probability of 50% settlement) column
settlement_comparison_event_50 <- pairwise_survdiff(Surv(age_days, event_50) ~ treatment,
                                                    data = Expanded_tiles_event_50,
                                                    p.adjust.method = "BH")

settlement_comparison_event_50
#Pairwise comparisons using Log-Rank test 

#data:  Expanded_tiles_event_50 and treatment 

#seawater REF1    REF2    20%     35%    
#  REF1 < 2e-16  -       -       -       -      
#  REF2 < 2e-16  6.3e-08 -       -       -      
#  20%  < 2e-16  0.00570 0.00096 -       -      
#  35%  < 2e-16  1.5e-11 < 2e-16 < 2e-16 -      
#  50%  < 2e-16  4.3e-05 1.00000 0.01350 2.1e-14

#P value adjustment method: BH 

#Event 60%

# Create the binary event_60 for larvae with >=60% settlement
tile_exp_long <- tile_exp_long %>%
  mutate(event_60 = ifelse(prop_settled >= 0.6, 1, 0))

# Summarize the data
Sum_tiles_event_60 <- tile_exp_long %>%
  group_by(treatment, age_days, event_60) %>%
  summarise(count_sum = sum(count_settled, na.rm = TRUE))

# Expand the dataset to have a row per larva
Expanded_tiles_event_60 <- expandRows(tile_exp_long, "count_settled")

# Recalculate the binary event for the expanded data
Expanded_tiles_event_60 <- Expanded_tiles_event_60 %>%
  mutate(event_60 = ifelse(prop_settled >= 0.6, 1, 0))

# Set factor levels for treatments
Expanded_tiles_event_60$treatment = factor(Expanded_tiles_event_60$treatment, levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Kaplan-Meier survival fit with 60% settlement as the event
fitExpanded_tiles_event_60 = survfit(Surv(age_days, event_60) ~ treatment, data = Expanded_tiles_event_60)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_tiles_event_60)

# Plot the Kaplan-Meier curve
setplot_tiles_event_60 <- ggsurvplot(fitExpanded_tiles_event_60, size = 1,
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
                                     palette = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1"),
                                     legend = c(0.15, 0.9), legend.title = "", 
                                     xlab = "Days", ylab = "Probability of seeing 60% settlement", 
                                     legend.labs = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot the results
setplot_tiles_event_60

# Pairwise comparison using the event_60 (probability of 60% settlement) column
settlement_comparison_event_60 <- pairwise_survdiff(Surv(age_days, event_60) ~ treatment,
                                                    data = Expanded_tiles_event_60,
                                                    p.adjust.method = "BH")

settlement_comparison_event_60
#	Pairwise comparisons using Log-Rank test 

#data:  Expanded_tiles_event_60 and treatment 

#seawater REF1  REF2  20%   35%  
#  REF1 <2e-16   -     -     -     -    
#  REF2 <2e-16   0.027 -     -     -    
#  20%  <2e-16   0.927 0.029 -     -    
#  35%  <2e-16   0.446 0.100 0.419 -    
#  50%  <2e-16   0.093 1.000 0.100 0.229

#P value adjustment method: BH 


#

# Create the binary event_40 for larvae with >=40% settlement
tile_exp_long <- tile_exp_long %>%
  mutate(event_40 = ifelse(prop_settled >= 0.4, 1, 0))

# Summarize the data
Sum_tiles_event_40 <- tile_exp_long %>%
  group_by(treatment, age_days, event_40) %>%
  summarise(count_sum = sum(count_settled, na.rm = TRUE))

# Expand the dataset to have a row per larva
Expanded_tiles_event_40 <- expandRows(tile_exp_long, "count_settled")

# Recalculate the binary event for the expanded data
Expanded_tiles_event_40 <- Expanded_tiles_event_40 %>%
  mutate(event_40 = ifelse(prop_settled >= 0.4, 1, 0))

# Set factor levels for treatments
Expanded_tiles_event_40$treatment = factor(Expanded_tiles_event_40$treatment, levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Kaplan-Meier survival fit with 40% settlement as the event
fitExpanded_tiles_event_40 = survfit(Surv(age_days, event_40) ~ treatment, data = Expanded_tiles_event_40)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_tiles_event_40)

# Plot the Kaplan-Meier curve
setplot_tiles_event_40 <- ggsurvplot(fitExpanded_tiles_event_40, size = 1,
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
                                     palette = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1"),
                                     legend = c(0.15, 0.9), legend.title = "", 
                                     xlab = "Days", ylab = "Probability of seeing 40% settlement", 
                                     legend.labs = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot the results
setplot_tiles_event_40

# Pairwise comparison using the event_40 (probability of 40% settlement) column
settlement_comparison_event_40 <- pairwise_survdiff(Surv(age_days, event_40) ~ treatment,
                                                    data = Expanded_tiles_event_40,
                                                    p.adjust.method = "BH")

settlement_comparison_event_40


#probability of some deaths

# Create the binary event_40 for larvae with >=40% settlement
tile_exp_long <- tile_exp_long %>%
  mutate(event_40 = ifelse(prop_settled >= 0.4, 1, 0))

# Summarize the data
Sum_tiles_event_40 <- tile_exp_long %>%
  group_by(treatment, age_days, event_40) %>%
  summarise(count_sum = sum(count_settled, na.rm = TRUE))

# Expand the dataset to have a row per larva
Expanded_tiles_event_40 <- expandRows(tile_exp_long, "count_settled")

# Recalculate the binary event for the expanded data
Expanded_tiles_event_40 <- Expanded_tiles_event_40 %>%
  mutate(event_40 = ifelse(prop_settled >= 0.4, 1, 0))

# Set factor levels for treatments
Expanded_tiles_event_40$treatment = factor(Expanded_tiles_event_40$treatment, levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Kaplan-Meier survival fit with 40% settlement as the event
fitExpanded_tiles_event_40 = survfit(Surv(age_days, event_40) ~ treatment, data = Expanded_tiles_event_40)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_tiles_event_40)

# Plot the Kaplan-Meier curve
setplot_tiles_event_40 <- ggsurvplot(fitExpanded_tiles_event_40, size = 1,
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
                                  palette = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1"),
                                  legend = c(0.15, 0.9), legend.title = "", 
                                  xlab = "Days", ylab = "Probability of seeing 40% settlement", 
                                  legend.labs = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot the results
setplot_tiles_event_40

# Pairwise comparison using the event_40 (probability of 40% settlement) column
settlement_comparison_event_40 <- pairwise_survdiff(Surv(age_days, event_40) ~ treatment,
                                    data = Expanded_tiles_event_40,
                                    p.adjust.method = "BH")

settlement_comparison_event_40

#Pairwise comparisons using Log-Rank test 

#data:  Expanded_tiles_event_40 and treatment 

#seawater REF1    REF2    20%     35%   
#  REF1 5.0e-16  -       -       -       -     
#  REF2 < 2e-16  0.0357  -       -       -     
#  20%  < 2e-16  0.5846  0.1524  -       -     
#  35%  1.5e-05  1.3e-05 6.3e-11 7.2e-07 -     
#  50%  6.7e-09  0.7187  0.0346  0.4663  0.0016

#P value adjustment method: BH 

#probability of some deaths

tile_exp_long <- tile_exp_long %>%
  mutate(dead_status = ifelse(dead >= 1, 1, 0))

# Summarize the data
Sum_tiles <- tile_exp_long %>%
  group_by(treatment, age_days, dead_status) %>%
  summarise(count_sum = sum(dead, na.rm = TRUE))

# Expand the dataset to have a row per larva
Expanded_tiles_dead <- expandRows(tile_exp_long, "dead")

# Set factor levels for treatments
Expanded_tiles_dead$treatment = factor(Expanded_tiles_dead$treatment, levels = c("seawater","REF1","REF2","20%","35%","50%"))

# Kaplan-Meier survival fit with some deaths as the event
fitExpanded_tiles_dead = survfit(Surv(age_days, dead_status) ~ treatment, data = Expanded_tiles_dead)

# Summary of the Kaplan-Meier fit
summary(fitExpanded_tiles_dead)

# Plot the Kaplan-Meier curve
setplot_tiles_dead <- ggsurvplot(fitExpanded_tiles_dead, size = 1,
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
                                 palette = c("darkturquoise", "springgreen4", "lightgreen", "lightpink", "palevioletred", "deeppink1"),
                                 legend = c(0.15, 0.9), legend.title = "", 
                                 xlab = "Days", ylab = "Probability of seeing some deaths in wells", 
                                 legend.labs = c("seawater","REF1","REF2","20%","35%","50%"))

# Plot the results
setplot_tiles_dead

# Pairwise comparison using the set_status (probability of some deaths) column
dead_comparison <- pairwise_survdiff(Surv(age_days, dead_status) ~ treatment,
                                     data = Expanded_tiles,
                                     p.adjust.method = "BH")

dead_comparison

#	Pairwise comparisons using Log-Rank test 

#data:  Expanded_tiles and treatment 

#20%     35%     50%     REF1    REF2   
#35%      0.17153 -       -       -       -      
#  50%      0.09895 0.00344 -       -       -      
#  REF1     0.05242 0.00095 0.87033 -       -      
#  REF2     0.04695 0.00095 0.76682 0.89081 -      
#  seawater 0.49746 0.98628 0.13356 0.09895 0.09895

#P value adjustment method: BH 


#Histogram comparing settled in tiles vs. settled in plastic

library(dplyr)

tile_exp_long <- tile_exp_long %>%
  mutate(total_settled = settled_tile + settled_plastic)

library(ggplot2)
library(tidyr)
library(RColorBrewer)

# Define the color palette
colors <- brewer.pal(9, "PiYG")[c(1, 2, 3)]  # Select a subset of the PiYG palette

# Pivot the data to long format for ggplot
tile_long <- tile_exp_long %>%
  select(well_id, settled_tile, settled_plastic, total_settled) %>%
  pivot_longer(cols = c(settled_tile, settled_plastic, total_settled), 
               names_to = "variable", values_to = "value")

# Rename the variables for plotting
tile_long$variable <- factor(tile_long$variable, 
                             levels = c("settled_tile", "settled_plastic", "total_settled"),
                             labels = c("Settlement in tiles", "Settlement in well plastic", "Total settlement"))

# Plot histograms
ggplot(tile_long, aes(x = value, fill = variable)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Count", y = "Frequency", title = "Histograms of Settlement Data") +
  scale_fill_manual(values = colors) +
  theme_classic() +
  theme(legend.position = "none")

library(ggplot2)
library(tidyr)


library(ggplot2)
library(tidyr)
library(dplyr)

library(ggplot2)
library(tidyr)
library(dplyr)


#Chi squared for proportion settled in tiles vs. plastic

# Create contingency table for settled vs. not settled by treatment
tile_exp_long$total_settled <- tile_exp_long$settled_tile + tile_exp_long$settled_plastic

# Create a column indicating whether larvae settled or not
tile_exp_long$settled <- ifelse(tile_exp_long$total_settled > 0, "Settled", "Not Settled")

# Create contingency table for chi-squared test
contingency_table <- table(tile_exp_long$treatment, tile_exp_long$settled)

# Perform the chi-squared test
chisq_test <- chisq.test(contingency_table)

# Display the results
print(contingency_table)
print(chisq_test)

#          Not Settled Settled
#20%              132     138
#35%              117     153
#50%              194      76
#REF1             126     144
#REF2             122     148
#seawater          72     198
#print(chisq_test)

#Pearson's Chi-squared test

#data:  contingency_table
#X-squared = 113.94, df = 5, p-value < 2.2e-16

# Create a new dataset with the relevant columns
# Melt the data so that "settled_tile" and "settled_plastic" are in one column
library(reshape2)
tile_exp_long_melt <- melt(tile_exp_long, 
                           id.vars = c("treatment", "n_larvae"), 
                           measure.vars = c("settled_tile", "settled_plastic"),
                           variable.name = "surface", 
                           value.name = "n_settled")

# Create a binary column indicating whether the larvae settled or not
tile_exp_long_melt$settled <- ifelse(tile_exp_long_melt$n_settled > 0, "Settled", "Not Settled")

# Create a contingency table for the surface (tile vs plastic) and whether larvae settled
contingency_table <- table(tile_exp_long_melt$surface, tile_exp_long_melt$settled)

# Perform the chi-squared test
chisq_test <- chisq.test(contingency_table)

# Display the results
#print(contingency_table)
#print(chisq_test)
#Not Settled Settled
#settled_tile           1585      35
#settled_plastic         771     849
#> print(chisq_test)

#Pearson's Chi-squared test with Yates' continuity correction

#data:  contingency_table
#X-squared = 1028.2, df = 1, p-value < 2.2e-16

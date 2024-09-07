# Set Working Directory
setwd("INSERT PATH")

# Load Libraries
library(tidyverse)
library(dplyr)

# Load Data
library("readxl")
spillover <- read_excel("structured_spillover_ranking_weighted.xlsx")

# Label the factors as green (not spillover-dependent) or red (spillover-dependent)
green <- c("Host plasticity - No. of species - Weighted Score",	
           "Host plasticity - No. of orders - Weighted Score",
           "Geography of the host(s) - Weighted Score",
           "Number of primary high-risk disease transmission interfaces where the virus has been detected - Weighted Score",	
           "Genetic relatedness between the host species and humans - Weighted Score",	
           "Land use in host ecosystem - Weighted Score",
           "Livestock density in host ecosystem - Weighted Score",	
           "Human population density in host ecosystem - Weighted Score",	
           "Deforestation in host ecosystem - Weighted Score",	
           "Urbanization in host ecosystem - Weighted Score",	
           "Agricultural system change in host ecosystem - Weighted Score",	
           "Genome classification of the virus - Weighted Score",
           "Envelope status of the virus - Weighted Score",	
           "Viral genome segmentation - Weighted Score",	
           "Virus species infectivity in terrestrial mammals (excluding humans) - Weighted Score",	
           "Proportion of virus species known to infect terrestrial mammals (excluding humans) in the viral family - Weighted Score",	
           "Proportion of viruses within a viral family that are known to infect more than 1 host species - Weighted Score",	
           "Geography of the virus in animals - Weighted Score",	
           "Transmission mode of the virus - Weighted Score",	
           "Frequency of interaction between domestic animals and humans in the host ecosystem - Weighted Score",	
           "Intimacy of interaction between domestic animals and humans in the host ecosystem - Weighted Score",
           "Frequency of interaction between wild animals and humans in the host ecosystem - Weighted Score",
           "Intimacy of interaction between wild animals and humans in the host ecosystem - Weighted Score")
red <- c("Virus species infectivity in humans - Weighted Score",
              "Proportion of virus species known to infect humans in the viral family - Weighted Score",	
              "Epidemicity of the virus species - Weighted Score",
              "Animal to human transmission - Weighted Score",	
              "Human to human transmission - Weighted Score",	
              "Duration of virus species infection in humans - Weighted Score",	
              "Pandemic virus - Weighted Score",
              "Proportion of known human pathogens in the viral family - Weighted Score")

# Calculate original scores (sum of all columns labeled red and green)
original_scores <- rowSums(spillover[, c(red, green)])

# Calculate adjusted scores (sum of green columns)
adjusted_scores <- rowSums(spillover[, c(green)])

# Normalize the scores to the highest scoring virus
normalized_original_scores <- original_scores / max(original_scores)
normalized_adjusted_scores <- adjusted_scores / max(adjusted_scores)

# Rank the scores (descending order)
original_ranks <- rank(-original_scores)
adjusted_ranks <- rank(-adjusted_scores)

# Create a data frame for plotting
plot_spillover <- data.frame(
  VirusName = spillover$`Virus Name`,
  VirusSpecies = spillover$`Virus Species`,
  VirusFamily = spillover$`Virus Family`,
  VirusGenus = spillover$`Virus Genus`,	
  VirusDistribution = spillover$`Virus Distribution`,
  HostDistribution= spillover$`Host Distribution`,
  HostPlasticity_Species= spillover$`Host Plasticity (Species)`,
  HostPlasticity_Order= spillover$`Host Plasticity (Order)`,
  HumanVirus = spillover$`Human Virus?`,
  ZoonoticVirus = spillover$`Zoonotic Virus?`,
  HumanTransmission = spillover$`Human Transmission?`,
  RiskLevels = spillover$`Risk Levels`,
  DataAvailability = spillover$`Data Availability`,
  NormalizedOriginalScores = normalized_original_scores,
  NormalizedAdjustedScores = normalized_adjusted_scores,
  OriginalRanks = original_ranks,
  AdjustedRanks = adjusted_ranks
)

# Identify the top ten highest and lowest original scores
top_10_original <- plot_spillover %>% arrange(desc(NormalizedOriginalScores)) %>% head(10)
bottom_10_original <- plot_spillover %>% arrange(NormalizedOriginalScores) %>% head(10)

# Identify the top ten highest and lowest adjusted scores
top_10_adjusted <- plot_spillover %>% arrange(desc(NormalizedAdjustedScores)) %>% head(10)
bottom_10_adjusted <- plot_spillover %>% arrange(NormalizedAdjustedScores) %>% head(10)

# Load fonts for graphing
library(extrafont)
font_import()
fonts()  

### FIGURE 1. Normalized Original vs. Adjusted Scores by Virus Genus and Family

# Remove rows with missing values for 'Genus' and 'Family'
plot_spillover_clean <- plot_spillover %>%
  filter(!is.na(NormalizedOriginalScores), !is.na(NormalizedAdjustedScores), !is.na(VirusGenus), !is.na(VirusFamily))

# Create custom shapes
custom_shapes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)

# Plot the Normalized Original vs. Adjusted Scores by Viral Genus and Family
ggplot(plot_spillover_clean, aes(x = NormalizedOriginalScores, y = NormalizedAdjustedScores, color = VirusFamily, shape = VirusFamily)) +
  geom_point(size = 3) +
  scale_shape_manual(values = custom_shapes) +
  labs(title = "Normalized Original vs. Adjusted Scores by Viral Family",
       x = "Normalized Original Scores",
       y = "Normalized Adjusted Scores",
       color = "Viral Family",
       shape = "Viral Family") +
  theme_minimal()+
  theme(text=element_text(family="Times New Roman", face="bold"))

### FIGURE 2.A. Violin Plots for Normalized Original and Adjusted Scores by Human Virus Classification
  # The "Human Virus?" factor is designated in the SpillOver: Viral Risk Ranking online tool 

plot_spillover_long <- plot_spillover %>%
  pivot_longer(cols = starts_with("Normalized"), names_to = "ScoreType", values_to = "Score") %>%
  mutate(ScoreType = ifelse(ScoreType == "NormalizedOriginalScores", "Normalized Original Scores", "Normalized Adjusted Scores"))

# Violin Plots for Adjusted & Original Scores (side-by-side)

# Load Library
library(gridExtra)

ggplot(plot_spillover_long, aes(x = HumanVirus, y = Score,  color=HumanVirus)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width = 0.2) +
  facet_grid(. ~ ScoreType) +
  labs(title = "Normalized Scores by Human Virus Classification",
       x = "Human Virus",
       y = "Scores",
       color = "Human Virus Classification") +
  theme_minimal()+
  theme(text=element_text(family="Times New Roman", face="bold"))

### FIGURE 2.B. ROC Curve for "Human Virus?" Factor
  # The "Human Virus?" factor is designated in the SpillOver: Viral Risk Ranking online tool

# Load library
library(pROC)

# Create a binary outcome (0 or 1) for "Human Virus?" factor
plot_spillover$HumanVirusBinary <- ifelse(plot_spillover$HumanVirus == "Yes", 1, 0)

# Extract the normalized scores and "Human Virus?" column
scores_data <- plot_spillover %>%
  select(HumanVirusBinary, NormalizedOriginalScores, NormalizedAdjustedScores)

# Change Font
par(family = "serif", font = 2)

# Plot ROC for Normalized Original Scores
roc_original <- roc(scores_data$HumanVirusBinary, scores_data$NormalizedOriginalScores, 
                    plot = TRUE, 
                    col = "lightgreen", 
                    main = "ROC Curves for Normalized Original and Adjusted Scores by Human Virus Classification",
                    cex.main = 1)

# Add ROC Plot for Normalized Original Scores
roc_adjusted <- roc(scores_data$HumanVirusBinary, scores_data$NormalizedAdjustedScores, 
                    plot = TRUE, 
                    add = TRUE, 
                    col = "orange")

# Add legend
legend("bottomright", legend = c("Original Scores", "Adjusted Scores"), col = c("lightgreen", "orange"), lwd = 2)

# Compare AUC values
auc_original <- auc(roc_original)
auc_adjusted <- auc(roc_adjusted)

print(paste("AUC for Original Scores: ", round(auc_original, 2)))
print(paste("AUC for Adjusted Scores: ", round(auc_adjusted, 2)))

### FIGURE 3. Mean Weighted Scores and SDs for the human and non-human viruses for all factors 
  # the human and non-human viruses were designated by the "Human Virus?" column in the SpillOver: Viral Risk Ranking online tool 

# Combine green and red factors
all_factors <- c(green, red)

# Label each factor as either 'Spillover Dependent Factors' or 'Non-Spillover Dependent Factors'
factor_labels <- data.frame(
  Variable = c(green, red),
  Type = c(rep("Non-Spillover Dependent Factors", length(green)), rep("Spillover Dependent Factors", length(red)))
)

# Calculate the mean and SD for each factor grouped by "Human Virus?" 
mean_sd_all <- spillover %>%
  select(all_of(all_factors), HumanVirus = `Human Virus?`) %>%
  pivot_longer(cols = -HumanVirus, names_to = "Variable", values_to = "Score") %>%
  group_by(Variable, HumanVirus) %>%
  summarize(
    Mean = mean(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(factor_labels, by = "Variable")  # Merge with factor 

# Plot the means and SDs, color coded by "Human Virus?" and shape by Factor Type
 # Factors are either 'Spillover Dependent Factors' or 'Non-Spillover Dependent Factors'
ggplot(mean_sd_all, aes(x = Variable, y = Mean, color = HumanVirus, shape = Type)) +
  geom_point(position = position_dodge(width = 0.8), size = 4) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(title = "Comparing Human and Non-Human Viruses Across Non-Spillover & Spillover Dependent Factors",
       x = "Factors",
       y = "Mean Weighted Score & Standard Deviation",
       color = "Human Virus",
       shape = "Factor Type") +
  theme_minimal()+
  theme(text=element_text(family="Times New Roman", face="bold"), axis.text.y = element_text(size = 9.5), legend.position = "bottom")


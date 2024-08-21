# TITLE: Spillover-Evaluation
# DESCRIPTION: The following workflow is provided to take users through a three part process of maintaining, organizing, and analyzing the "Spillover Rankings" as provided by the SpillOver: Viral Risk Ranking tool. 

# STEP 1: Download CSV File from Spillover Global Website
# Visit: https://spillover.global/ranking-comparison/
# Click on "Download Results" to download the CSV file
# The download should result in a file named SpilloverRankings.csv

### STEP 1 is required to obtain the original data as provided by the Spillover: Viral Risk Ranking tool. 

# STEP 2: Open the Jupyter Notebook (Spillover_Organization.ipynb)
## This Jupyter Notebook was edited and run in Google Colab.
# Upload the SpilloverRankings.csv as directed in the first code cell
# Run the second code cell to organize the data

### STEP 2 is required to organize the column entitled 'Risk Levels' into a usable format for data analysis. In the original csv this column contains the "Risk Name", "Risk Score", "Impact Score", "Weighted Score", and "Corresponding level/levels" for all factors evaluated for each virus (row). 

# STEP 3: Open the R Script (Spillover_Evaluation_Analysis_FINAL.R)
## This R Script was edited and run in R Studio
# Set working directory
# Load Data
# Run code to reproduce Figures 1, 2a, 2b, and 3




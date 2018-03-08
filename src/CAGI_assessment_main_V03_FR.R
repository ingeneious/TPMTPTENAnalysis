#!/usr/bin/env/ Rscript
#clear workspace
setwd("/Users/mabelfurutsuki/Desktop/TPMTPTENTest/data")
rm(list = ls())

# ROCR 
library(ROCR) # required by function StatisticalValidation
library(plotrix)

  
# set working directory
setwd("../")

# load assessment functions
source("src/Check_prediction_format_V06_FR.R")
source("src/Scatterplot_pred_obs_V01_FR.R")
source("src/Calculate_statistics_V01_FR.R")

#Import data
#Import real values
exp.val <- read.delim('./data/experimental_value/real_proliferation.txt', sep='\t', header = TRUE) #file with experimental data

#Import submission template
sub.template <- read.table('./data/template/template.txt', sep='\t', header = TRUE) # submission template

#template.path='./data/submission/p16.Submission.1.txt'
submission.files <- list.files(path='./data/submissions', pattern="prediction", full.names = TRUE)

# check submission format
ValidateSubmission(files.path = submission.files,
                     number.of.rows = nrow(sub.template), number.of.columns = ncol(sub.template) , 
                     correct.header = colnames(sub.template), correct.column.1 =  sub.template[, 1])

# Create experimental-observed scatterplots
ExperimentalVsPredicted(path.prediction = submission.files, real.data = exp.val)

#Compute performance measures and make heatmap
Auc.threshold=c(0.65, 0.75, 0.9) # set list of threshold to be used by AUC, if empty AUC will not be calculated
StatisticalValidation(Experimental.data = exp.val, path.prediction=submission.files, AUC.list=Auc.threshold)

# make Submission predictions Correlation plot

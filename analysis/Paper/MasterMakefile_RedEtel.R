####################################################################
### MAKEFILE CHULWALHAR    
### Bill Kerneckel, Nathan Mowat, Chris Woodard, Jessica Wheeler
### Created July 23, 2016
### Updated July 23, 2016
#####################################################################

#Set the working directory
setwd("/Users/wkerneck/GitHub/CaseStudy2")

#File to load the data:
source("/Users/wkerneck/GitHub/CaseStudy2/analysis/data/LoadingDataChulwalhar.R")

#File to clean the data:
source("/Users/wkerneck/GitHub/CaseStudy2/analysis/data/CleaningDataChulwalhar.R")

#Analysis of Basic Data
source("/Users/wkerneck/GitHub/CaseStudy2/analysis/data/AnalysisBasicChulwalhar.R")

#Module3- Indicator Correlations
source("/Users/wkerneck/GitHub/CaseStudy2/analysis/data/Module3Chulwalhar.R")

#Modules 4 - 7- Models and Forecasting and Smoothing
source("/Users/wkerneck/GitHub/CaseStudy2/analysis/data/ForecastingModelsR.R")

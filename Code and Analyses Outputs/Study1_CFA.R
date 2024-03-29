#Title: "Consumer biases in the perception of Organizational Greed"
#Arango, L., Singaraju, S., Niininen, O. & D'Souza, C.

#In this script, we do CFA to establish validity and reliability of our questionnaire. we employ cfa function from the lavaan package with Satorra-Bentler chi-square and robust se.

#This code was created using R version 4.1 and it's likely to break if previous versions are used to run it.


#Packages installation and loading

if(!require(semPlot)){install.packages("semPlot")}
if(!require(tidySEM)){install.packages("tidySEM")}
if(!require(MVN)){install.packages("MVN")}
if(!require(semTools)){install.packages("semTools")}
if(!require(lavaan)){install.packages("lavaan")}
if(!require(stringi)){install.packages("stringi")}
if(!require(Rtools)){install.packages("Rtools")}
if(!require(Rcpp)){install.packages("Rcpp")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}


library(rlang)
library(semPlot)
library(tidySEM)
library(MVN)
library(semTools)
library(lavaan)
library(stringi)
library(Rcpp)
library(ggplot2)
library(dplyr)


#data upload
cfa_data <- read.csv("Main_Data_Study1.csv")

#1. Normality assumption SEM
#Normality endogenous variable - Multivariate indicator normality

#Data frame transform
n_data <- cfa_data[c("Dir1", "Dir2", "Dir3", "Dir4", "Ind1", "Ind2", "Ind3", "Ind4")]

#a. Multivariate normality test - Default: Henze-Zirkler's test  
mvn(n_data)

#b. Visualization

#qq Plot
mvn(n_data, 
    multivariatePlot = "qq")

#Histogram
mvn(n_data, 
    univariatePlot = "histogram")

#Box Plot #preferably, clear all plots before running the following line of code
mvn(n_data, 
    univariatePlot = "box")

#2. CFA

#a. Measurement model specification for cfa lavaan function
m_model <- '
           #Define latent variables and indicators
           Greed =~ Dir1 + Dir2 + Dir3 + Dir4 + Ind1 + Ind2 + Ind3 + Ind4
           Equality =~ Eq1 + Eq2
           Need =~ N1 + N2
           Equity =~ Des1 + Des2
           Depriv =~ Dep1 + Dep2 
           Blame =~ Bla1 + Bla2 + Bla3
        '
#b. CFA analysis - Satorra Bentler chi Square and robust standard errors
fit1 = cfa(m_model, 
           data = cfa_data, 
           estimator = "MLM", 
           se = "robust")

#c. Model summary

#Summary including fit measures, standardized values and rquare.
summary(fit1, 
        fit.measures = TRUE, 
        standardize = TRUE,
        rsquare= TRUE)

#reliability values
reliability(fit1)

#e. Visualization

#Diagram 1
graph_sem(model = fit1, 
          rect_height = 0.5,
          ellipses_width = 1.5,
          ellipses_height = 0.7,
          variance_diameter = 0.5,
          text_size = 3,
          spacing_y = 1)


#Diagram 2
#If the diagram size is too small, clear the plot history and rerun the code
semPaths(fit1,
         what = "std", 
         whatLabels = "est", 
         style = "lisrel", 
         residScale = 8, 
         rotation = 2, 
         layout = "tree", 
         cardinal = "lat cov", 
         curvePivot = TRUE, 
         sizeMan = 4, 
         sizeLat = 10)

#References

# The code for the pretest, Study 1 CFA, and Studies 1, 2, and 3 is partly based on the following resources: 

# 1.  Data carpentry contributors (2022) Data visualization with ggplot2. https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html 
#2. Gana, K. & Broc, G. (2019) Structural Equation Modelling with lavaan. Hoboken, NJ: Wiley.
#3. Crowson, M. (2019) Path analysis in R using Lavaan (video 3): Assessing and managing multivariate nonnormality. [Video] YouTube. https://www.youtube.com/watch?v=HvYW_GeHpD8&t=347s 
#4. Datanovia. (2018) ANCOVA in R. Comparing multiple means in R. https://www.datanovia.com/en/lessons/ancova-in-r/#:~:text=ANCOVA%20makes%20several%20assumptions%20about,Homogeneity%20of%20regression%20slopes
#5. Gard, A. (2020) Chi-Squared Testing for Independence in R. [Video] YouTube (Equitable Equations channel).  https://www.youtube.com/watch?v=XfsDtYuwVzs 

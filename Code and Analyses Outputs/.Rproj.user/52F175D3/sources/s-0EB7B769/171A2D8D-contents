#Title: "Underdogs, Sheep and Conformists: Biases in the Perception of Organizational Greed"
#AUTHORS NAMES
#In this script, we do CFA and SEM to ensure our scenarios capture the greed construct. We use the sem function from the lavaan package with Satorra-Bentler chi-square and robust se.
#We perform CFA and SEM separately but clearly the measurement model of the SEM is the CFA model and then values are very similar (SEM could have been calculated directly). The CFA section is to facilitate the reader task when following the code 
#Modification indices could be used by the reader to test alternative model. the modindices(fit, sort=TRUE) returns mod indices sorted
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
sem_data <- read.csv("Main_Data_Study1.csv")

#1. Normality assumption SEM
#Normality endogenous variable - Multivariate indicator normality

#Data frame transform
n_data <- sem_data[c("Dir1", "Dir2", "Dir3", "Dir4", "Ind1", "Ind2", "Ind3", "Ind4")]

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
           #Define exogenous latent variables and indicators
           Equality =~ Eq1 + Eq2
           Need =~ N1 + N2
           Equity =~ Des1 + Des2
           Depriv =~ Dep1 + Dep2 
           Blame =~ Bla1 + Bla2 + Bla3
        '
#b. CFA analysis - Satorra Bentler chi Square and robust standard errors
fit1 = cfa(m_model, 
           data = sem_data, 
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
         layout = "tree2", 
         cardinal = "lat cov", 
         curvePivot = TRUE, 
         sizeMan = 4, 
         sizeLat = 10)


#3.SEM 

#a. Measurement model specification for sem lavaan function
sem_model <-' 
          #Measurement model
              Greed =~ Dir1 + Dir 2 + Dir3 + Dir4 + Ind1 + Ind2 + Ind3 + Ind4
              Equality =~ Eq1 + Eq2
              Need =~ N1 + N2
              Equity =~ Des1 + Des2
              Depriv =~ Dep1 + Dep2 
              Blame =~ Bla1 + Bla2 + Bla3
           
           #Regression
              Greed ~ Equality + Need + Equity + Depriv + Blame
           
           '
#b. SEM analysis - Satorra Bentler chi Square and robust standard errors
fit2 = sem(sem_model, 
           data = sem_data, 
           auto.var=TRUE, 
           estimator = "MLM", 
           se="robust")

#c. Model summary

#Summary including fit measures, standardized values and rquare.
summary(fit2, 
        fit.measures=TRUE, 
        standardize=TRUE,
        rsquare=TRUE)

#reliability values
reliability(fit2)

#Model Diagrams

#Diagram 1

#Define customized layout
layout_sem = read.csv("sem_table_layout.csv")


#Graph
graph_sem(model = fit2, 
          rect_height = 0.5,
          ellipses_width = 1.5,
          ellipses_height = 0.7,
          variance_diameter = 0.5,
          text_size = 3, 
          layout =  layout_sem
          )

#Diagram 2 #Covariances not included here. Previous plot is not very visually pleasing in this respect.
#Again, if the plot is tool small and/or superimposes, clean the plot history and rerun the code
semPaths(fit2,
         what = "std", 
         whatLabels = "est", 
         style = "lisrel", 
         residScale = 8, 
         rotation = 2, 
         layout = "tree2", 
         cardinal = "lat cov", 
         curvePivot = TRUE, 
         sizeMan = 4, 
         sizeLat = 10,
         exoCov = FALSE)


#Appendix

#Multi_CFA-SEM. (Optional -One of the models do not seem to fit fine, probably because of the groups sample size)


#Data transform - inclusion of a variable to define groups
multi_data <- transform(sem_data, 
                        GroupSize = paste(ï..Size, Group)) #try removing "ï.." if you get an error. You might get a question mark, but inside the quotation mark, but the symbol is an "i" with an umlaut or two dots on top. Use str(sem_data) and check the characters that antecede the name of the variable Size

#a. Multi-group CFA, Satorra-Bentler chi square and robust se.
m_fit = sem(sem_model, 
            data = multi_data, 
            auto.var = TRUE,
            estimator = "MLM", 
            se = "robust",
            group = "GroupSize")


#b. Summary
summary(m_fit, 
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)
reliability(m_fit)


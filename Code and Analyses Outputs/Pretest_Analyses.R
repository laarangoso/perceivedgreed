#Title: "Consumer biases in the perception of Organizational Greed"
#Arango, L., Singaraju, S., Niininen, O. & D'Souza, C.
#In this script, we test normality of the pretest data using visual methods and the Shapiro-Wilk test
#After testing normality, we use evaluate mean differenes throught visual methods first and then the Kruskal-Wallis test (Post-hoc: Dunn's Test)
#This code was created using R version 4.1 and it's likely to break if previous versions are used to run it

#Package installation and loading
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(FSA)){install.packages("FSA")}

library(tidyverse)
library(data.table)
library(ggpubr)
library(FSA)


#Data upload
pre_data <- read.csv("PreTest_GreedScores.csv")

#1. #Normality testing

#a. Density plots for the different groups - individual graphs
ggplot(pre_data, aes(Greed_Score)) +
       ggtitle("Density Plots Greed Scores by Scenario") +
       theme(plot.title = element_text(hjust = 0.5)) +
       geom_density(fill="green", alpha =.3) + 
       labs (x= "Total Greed Score") + 
       geom_vline(xintercept = 5, color= "blue", linetype = "dashed") + 
       facet_grid(. ~ Scenario)

# b. Density plots for the different groups - single graph
ggplot(pre_data, aes(Greed_Score, fill= Scenario)) +
      ggtitle("Density Plot Greed Scores by Scenario") +
      geom_density(alpha =.6) + 
      labs (x= "Total Greed Score") + 
      geom_vline(xintercept = 5, color= "blue", linetype = "dashed") + 
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5))

#c. q plot - single graphs
qplot(sample = Greed_Score, 
      data= pre_data, 
      shape=Scenario, 
      color= Scenario) + 
      labs(y= "Greed Score")

# Shapiro Wilk test

#d. Summary stats per group
tapply(pre_data$Greed_Score, 
       pre_data$Scenario, 
       summary)

#e.Shapiro-Wilk normality tests by group
DT <- data.table(pre_data)

DT[,
   .(W = shapiro.test(Greed_Score)$statistic, 
     P.value = shapiro.test(Greed_Score)$p.value),
   by = .(Scenario)]


#2. Mean differences

#a. Summary stats per group
tapply(pre_data$Greed_Score, pre_data$Scenario, summary)

#b. Visualization, box plots
ggboxplot(pre_data, 
          x = "Scenario", 
          y = "Greed_Score", 
          color = "Scenario", 
          palette = c("#00AFBB", "#E7B800", "#FC4E07", "grey34"),
          order = c("Hidden-fees", "Sub_materials", "Rent_inc", "Unfair_com"),
          ylab = "Greed Score", 
          xlab = "Scenario",
          title = "Greed scores box plots"
          )

#c. Visualization, Mean Plots
ggline(pre_data, 
       x = "Scenario", 
       y = "Greed_Score", 
       add = c("mean_se", "jitter"), 
       order = c("Hidden-fees", "Sub_materials", "Rent_inc",  "Unfair_com"),
       ylab = "Greed Score", 
       xlab = "Scenario",
       title = "Greed data score: points and means")

#d. Kruskal-Wallis Test
kruskal.test(Greed_Score ~ Scenario, data = pre_data)


#e. Post-hoc Dunn test -Pairwise comparisons
dunnTest(Greed_Score ~ Scenario, data = pre_data, method = "bh")

#References

# The code for the pretest, Study 1 CFA, and Studies 1, 2, and 3 is partly based on the following resources: 

# 1.  Data carpentry contributors (2022) Data visualization with ggplot2. https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html 
#2. Gana, K. & Broc, G. (2019) Structural Equation Modelling with lavaan. Hoboken, NJ: Wiley.
#3. Crowson, M. (2019) Path analysis in R using Lavaan (video 3): Assessing and managing multivariate nonnormality. [Video] YouTube. https://www.youtube.com/watch?v=HvYW_GeHpD8&t=347s 
#4. Datanovia. (2018) ANCOVA in R. Comparing multiple means in R. https://www.datanovia.com/en/lessons/ancova-in-r/#:~:text=ANCOVA%20makes%20several%20assumptions%20about,Homogeneity%20of%20regression%20slopes
#5. Gard, A. (2020) Chi-Squared Testing for Independence in R. [Video] YouTube (Equitable Equations channel).  https://www.youtube.com/watch?v=XfsDtYuwVzs 

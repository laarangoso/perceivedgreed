#Title: "Underdogs, Sheep and Conformists: Biases in the Perception of Organizational Greed"
#AUTHORS NAMES
#In this script, we test normality of the pretest data using visual methods and the Shapiro-Wilk test
#After testing normality, we use evaluate mean differenes throught visual methods first and then the Kruskal-Wallis test (Post-hoc: Dunn's Test)
#This code was created using R version 4.1 and it's likely to break if previous versions are used to run it




#Packages installation and loading
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(sjPlot)){install.packages("sjPlot")}
if(!require(sjmisc)){install.packages("sjmisc")}


library("rcompanion")
library("tidyverse")
library(sjPlot)
library(sjmisc)
library(data.table)


#Data upload
mains_data <- read.csv("Study1_GScores.csv")


#1. Normality test

#a. Density plots for the different groups/cells - individual graphs
ggplot(mains_data, 
       aes(Score)) +
       ggtitle("Density Plots Greed Scores by Group") +
       theme(plot.title= element_text(hjust = 0.5)) +
       geom_density(fill="green", alpha =.3) + 
       labs (x= "Greed Score") + 
       geom_vline(xintercept = 40, color= "blue", linetype = "dashed") + 
       facet_grid(. ~ Group + Size)


# b. Density plots for the different groups - single graph
#Data frame introduce new variable
mains_data2<- transform(mains_data, 
                     GroupandSize= paste(Group, Size))
#Plot
ggplot(mains_data2, 
       aes(Score, fill= GroupandSize)) +
       ggtitle("Density Plot Scores by Group") +
       geom_density(alpha =.6) + 
       labs (x= "Greed Score") + 
       geom_vline(xintercept = 40, color= "blue", linetype = "dashed") + 
       theme_dark() + 
       theme(plot.title = element_text(hjust = 0.5)) 


#c. q plot - single graphs
qplot(sample = Score, 
      data= mains_data2, 
      shape=GroupandSize, 
      color= GroupandSize) + 
      labs(y= "Greed Score")

#Shapiro-Wilk test

#d. Summary stats per group
tapply(mains_data2$Score, 
       mains_data2$GroupandSize, 
       summary)

#e.Shapiro-Wilk tests by group
#Data frame transform, drop unneccesary variables, reorganize data
mains_data3 <- subset(mains_data2, 
                   select =c(Score, GroupandSize))
Dat <- data.table(mains_data3)

#Test
Dat[,
   .(W = shapiro.test(Score)$statistic, 
     P.value = shapiro.test(Score)$p.value),
   by = .(GroupandSize)]


#2. Mean differences: SheirerRayHare Test - Non parametric two way ANOVa (ranks)

#a.Test p Values
scheirerRayHare(Score ~ Group + Size,
                data=mains_data)


#b. Interaction Plots

#b1. Traditional plot

#The following line of code might be necessary if the y axis of the plot is only partially visible
dev.off()
#Reorder x-axis elements to avoid default alphabetical order
Size_order  = factor(mains_data$Size, levels=c("Small", "Large"))
#Plot
interaction.plot(Size_order, 
                 mains_data$Group, 
                 mains_data$Score, 
                 xlab= "Company Size", 
                 ylab= "Greed Score", 
                 main = "Interaction Size and Group Predicting Greed Score", 
                 ylim = c(48, 53), 
                 trace.label=" Group", 
                 col =c("blue", "red")) +
                 theme(plot.title = element_text(hjust = 0.5),
                 ) 

#b2. Regression plot

#Define regression
mod1 <- lm(Score ~ Size*Group, mains_data)
#Plot
plot1 <-plot_model(mod1, 
                   type = "int", 
                   terms = c("Group", "Size"), 
                   axis.title = "Greed Score", 
                   title = "Interaction Size and Group Predicting Greed Score",
                   set_theme(base=theme_classic(), 
                   title.align = "center"),
                   ) 

plot1 + labs(x = "Company Size")

#Title: "Consumer biases in the perception of Organizational Greed"
#Arango, L., Singaraju, S., Niininen, O. & D'Souza, C.

#In this script, we run a general linear model, one-way ANCOVA and test assumptions.
#This code was created using R version 4.1 and it's likely to break if previous versions are used to run it

#Packages installation and loading
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(sjPlot)){install.packages("sjPlot")}
if(!require(sjmisc)){install.packages("sjmisc")}
if(!require(ARTool)){install.packages("ARTool")}


library("rcompanion")
library("tidyverse")
library(sjPlot)
library(sjmisc)
library(data.table)
library(ARTool)
library(ggplot2)


#1. Distribution of greed scores

#Data upload
freq_data <- read.csv("Study3_GScores.csv")


# Density plots for the different groups - single graph: Greed Scores

#Plot
ggplot(freq_data, 
       aes(GreedScore, fill= Group)) +
       ggtitle("Density Plot Scores by Behavior Frequency") +
       geom_density(alpha =.6) + 
       labs (x= "Greed Score") + 
       geom_vline(xintercept = 5, color= "blue", linetype = "dashed") + 
       theme_dark() + 
       theme(plot.title = element_text(hjust = 0.5)) 


#ANCOVA ASSUMPTIONS

#Load data
anc_data3 <- read.csv("Combined Scores_Study3.csv")

# 1. Normality of residuals

#Prepare data
# Fit  model
model3 <- lm(Greed ~ Equality + Need + Equity
             + Deprivation + Blame + Group, data = anc_data3)


# Inspect the model diagnostic metrics
model_metrics3 <- augment(model3) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details


#a. Graph -residuals normality
hist(model_metrics3$.resid, col = "blue", 
     main = "Residuals histogram",
     xlab = "Residual value")

# b. QQ plot
qqnorm(model_metrics3$.resid,
       main = "Residuals QQ plot",
)  + abline(0,1)

# c. Shapiro test
shapiro_test(model_metrics3$.resid)

#2. Correlation between covariates

# Correlations matrix
cor(anc_data3[, c("Equality", "Need", "Equity", "Deprivation", "Blame")])


#3. Homoscedasicity test residuals (Equality of variance)
levene_test(.resid ~ Group, data = model_metrics3)

# 4. Linearity of the relationship between DV and covariates per group  

#a. Equality
ggscatter(
  anc_data3, x = "Equality", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )

# b. Need
ggscatter(
  anc_data3, x = "Need", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )

#c. Equity
ggscatter(
  anc_data3, x = "Equity", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )

#d. Deprivation
ggscatter(
  anc_data3, x = "Deprivation", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )

#e. Blame
ggscatter(
  anc_data3, x = "Blame", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )


#5. Homogeneity of regression slopes

#a. Equality
anc_data3 %>% anova_test(Greed ~ Group*Equality)

#b. Need
anc_data3 %>% anova_test(Greed ~ Group*Need)

#c. Equity
anc_data3 %>% anova_test(Greed ~ Group*Equity)

#d. Deprivation
anc_data3 %>% anova_test(Greed ~ Group*Deprivation)

#e. Blame
anc_data3 %>% anova_test(Greed ~ Group*Blame)


#6. General linear model: one-way ANCOVA. Covariates: Equality, Need, Equity, Deprivation, Blame 



#a.model
model3 <- lm(Greed ~ Equality + Need 
             + Equity + Deprivation + Blame + Group,
             anc_data3)

#b. Summary
summary.aov(model3)


# c. etas
etaSquared(model3)

#d. Aligned Rank Transformed Data

ranked_model3 <- art(Greed ~ as.factor(Group), data = anc_data3)

anova(ranked_model3)

#e. Plot

ggplot(anc_data3, aes(Group, Greed))+
  geom_boxplot(aes(fill=Group), outlier.shape = NA) + labs (y="Greed Score") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Pastel1") + ylim(4,7)


#Descriptives: Frequency Tables

# 1. Gender
#create frequency matrix
gender3 <- matrix(c(48,77, 55, 69), byrow=F, nrow=2)

colnames(gender3) <- c("Common", "Uncommon")
rownames(gender3) <- c("Male", "Female")

gender3

#Chi Square test
gender3_test<- chisq.test(gender3)
gender3_test

#2. Age 
#Create frequency matrix


age3 <- matrix(c(3, 31, 33, 32, 26,
                 9, 38, 28, 20, 29), byrow=F, 
               nrow=5)
colnames(age3) <- c("Common", "Uncommon")
rownames(age3) <- c("18-24", "25-34", "35-44", "45-54", "55+")

#chi square test
age3_test <- chisq.test(age3)
age3_test

#3. Education 
#Create frequency matrix
education3 <- matrix(c(1, 27, 21, 47, 29, 1, 30, 15, 58, 20),
                     byrow =F,
                     nrow=5)

colnames(education3) <- c("Common", "Uncommon")
rownames(education3) <- c("Less than high school", "High School", 
                          "Technical/Diploma", "University degree", "Postgraduate")

#chi square test
education_test3 <- chisq.test(education3)
education_test3

#4. Income 
#Create frequency matrix
income3 <- matrix(c(30, 36, 41, 18,
                    35, 39, 37, 13
), byrow=F, 
nrow=4)

colnames(income3) <- c("Common", "Uncommon")
rownames(income3) <- c("Less than 25k", "25k-<45k", 
                       "45k-<85k", "85k+")

#chi square test
income_test3 <- chisq.test(income3)
income_test3

#References

# The code for the pretest, Study 1 CFA, and Studies 1, 2, and 3 is partly based on the following resources: 

# 1.  Data carpentry contributors (2022) Data visualization with ggplot2. https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html 
#2. Gana, K. & Broc, G. (2019) Structural Equation Modelling with lavaan. Hoboken, NJ: Wiley.
#3. Crowson, M. (2019) Path analysis in R using Lavaan (video 3): Assessing and managing multivariate nonnormality. [Video] YouTube. https://www.youtube.com/watch?v=HvYW_GeHpD8&t=347s 
#4. Datanovia. (2018) ANCOVA in R. Comparing multiple means in R. https://www.datanovia.com/en/lessons/ancova-in-r/#:~:text=ANCOVA%20makes%20several%20assumptions%20about,Homogeneity%20of%20regression%20slopes
#5. Gard, A. (2020) Chi-Squared Testing for Independence in R. [Video] YouTube (Equitable Equations channel).  https://www.youtube.com/watch?v=XfsDtYuwVzs 


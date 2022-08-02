#Title: "Title: "Consumer biases in the perception of organizational greed"
#AUTHORS NAMES
#In this script, we run a general linear model, one-way ANCOVA and test assumptions.
#This code was created using R version 4.1 and it's likely to break if previous versions are used to run it
# This code is based on code at: https://www.datanovia.com/en/lessons/ancova-in-r/#:~:text=Homogeneity%20of%20regression%20slopes.,by%20groups%20should%20be%20parallel.


#Note: Sample is composed exclusively of American particpants.
#Black: Represents black sheep data (US companies hurting US consumers)
#White: Represents white sheep data (Australian company hurting Australian consumers)


#Packages installation and loading
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(sjPlot)){install.packages("sjPlot")}
if(!require(sjmisc)){install.packages("sjmisc")}
if(!require(data.table)){install.packages("data.table")}
if(!require(ARTool)){install.packages("ARTool")}


library(rcompanion)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(data.table)
library(ARTool)


#Distribution of greed scores
# 1.. Density plots for the different groups - single graph: Greed Scores

#Data upload
sheep_data <- read.csv("Study2_GScores.csv")


#Plot
ggplot(sheep_data, 
       aes(GreedScore, fill= Group)) +
  ggtitle("Density Plot Scores by Group") +
  geom_density(alpha =.6) + 
  labs (x= "Greed Score") + 
  geom_vline(xintercept = 5, color= "blue", linetype = "dashed") + 
  theme_dark() + 
  theme(plot.title = element_text(hjust = 0.5)) 


#ANCOVA ASSUMPTIONS

#Load data
anc_data2 <- read.csv("Combined Scores_Study 2.csv")


# 1. Normality of residuals

#Prepare data
# Fit  model
model2 <- lm(Greed ~ Equality + Need + Equity
            + Deprivation + Blame + Group, data = anc_data2)


# Inspect the model diagnostic metrics
model_metrics2 <- augment(model2) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details


#a. Graph -residuals normality
hist(model_metrics2$.resid, col = "blue", 
     main = "Residuals histogram",
     xlab = "Residual value")

# b. QQ plot
qqnorm(model_metrics2$.resid,
       main = "Residuals QQ plot",
       )  + abline(0,1)

# c. Shapiro test
shapiro_test(model_metrics2$.resid)

#2. Correlation between covariates

# Correlations matrix
cor(anc_data2[, c("Equality", "Need", "Equity", "Deprivation", "Blame")])


#3. Homoscedasicity test residuals (Equality of variance)
levene_test(.resid ~ Group, data = model_metrics2)

# 4. Linearity of the relationship between DV and covariates per group  

#a. Equality
ggscatter(
  anc_data2, x = "Equality", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )

# b. Need
ggscatter(
  anc_data2, x = "Need", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )

#c. Equity
ggscatter(
  anc_data2, x = "Equity", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )

#d. Deprivation
ggscatter(
  anc_data2, x = "Deprivation", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )

#e. Blame
ggscatter(
  anc_data2, x = "Blame", y = "Greed",
  color = "Group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )


#5. Homogeneity of regression slopes

#a. Equality
anc_data2 %>% anova_test(Greed ~ Group*Equality)

#b. Need
anc_data2 %>% anova_test(Greed ~ Group*Need)

#c. Equity
anc_data2 %>% anova_test(Greed ~ Group*Equity)

#d. Deprivation
anc_data2 %>% anova_test(Greed ~ Group*Deprivation)

#e. Blame
anc_data2 %>% anova_test(Greed ~ Group*Blame)


#6. General linear model: one-way ANCOVA. Covariates: Equality, Need, Equity, Deprivation, Blame 

#a.model
model2 <- lm(Greed ~ Equality + Need 
              + Equity + Deprivation + Blame + Group,
              anc_data2)

#b. Summary
summary.aov(model2)


# c. etas
etaSquared(model2)

#d. Aligned Rank Transformed Data

ranked_model2 <- art(Greed ~ as.factor(Group), data = anc_data2)

anova(ranked_model2)

#e. Plot
ggplot(anc_data2, aes(Group, Greed))+
  geom_boxplot(aes(fill=Group), outlier.shape = NA) + labs (y="Greed Score") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Pastel1") + ylim(4,7)


#Descriptives: Frequency Tables

# 1. Gender
#create frequency matrix
gender2 <- matrix(c(47,52, 71, 59), byrow=T, nrow=2)

colnames(gender2) <- c("White_S", "Black_S")
rownames(gender2) <- c("Male", "Female")

#Chi Square test
gender2_test<- chisq.test(gender2)
gender2_test

#2. Age 
#Create frequency matrix


age2 <- matrix(c(4, 33, 31, 21, 29,
                 5, 38, 34, 16, 18), byrow=F, 
              nrow=5)
colnames(age2) <- c("White_S", "Black_S")
rownames(age2) <- c("18-24", "25-34", "35-44", "45-54", "55+")

#chi square test
age2_test <- chisq.test(age2)
age2_test

#3. Education 
#Create frequency matrix
education2 <- matrix(c(0, 26, 18, 59, 15, 0, 33, 11, 51, 16),
                     byrow =F,
                    nrow=5)

colnames(education2) <- c("White_S", "Black_S")
rownames(education2) <- c("Less than high school", "High School", 
                         "Technical/Diploma", "University degree", "Postgraduate")

#chi square test
education_test2 <- chisq.test(education2)
education_test2

#4. Income 
#Create frequency matrix
income2 <- matrix(c(23, 37, 40, 18,
                   27, 29, 33, 22
                   ), byrow=F, 
                  nrow=4)

colnames(income2) <- c("White_S", "Black_S")
rownames(income2) <- c("Less than 25k", "25k-<45k", 
                      "45k-<85k", "85k+")

#chi square test
income_test2 <- chisq.test(income2)
income_test2



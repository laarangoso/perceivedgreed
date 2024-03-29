#Title: "Consumer biases in the perception of Organizational Greed"
#Arango, L., Singaraju, S., Niininen, O. & D'Souza, C.

#In this script,  run a general linear model (ANCOVA) and test its assumptions.

#This code was created using R version 4.1 and it's likely to break if previous versions are used to run it



#Packages installation and loading
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(sjPlot)){install.packages("sjPlot")}
if(!require(sjmisc)){install.packages("sjmisc")}
if(!require(car)){install.packages("car")}
if(!require(lsr)){install.packages("lsr")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(statix)){install.packages("rstatix")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(broom)){install.packages("broom")}
if(!require(ARTool)){install.packages("ARTool")}



library(rcompanion)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(data.table)
library(car)
library(lsr)
library(ggpubr)
library(rstatix)
library(tidyr)
library(emmeans)
library(broom)
library(ARTool)



#Data upload and preparation
mains_data <- read.csv("Study1_GScores.csv")
anc_data <- read.csv("Combined Scores.csv")

#ASSUMPTIONS CHECK 


# 1. Normality of residuals

# Fit  model
model <- lm(Greed ~ Size*Group + Equality + Need +
              Equity + Deprivation + Blame, data = anc_data)

#Obtain residuals

model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details


#a. Graph -residuals normality
hist(model.metrics$.resid, col = "blue", 
     main = "Residuals histogram",
     xlab = "Residual value")

# b. QQ plot
qqnorm(model.metrics$.resid,
       main = "Residuals QQ plot",
       )  + abline(0,1)

# c. Shapiro test
shapiro_test(model.metrics$.resid)


#Assumption of normality for ANCOVA violated. 
#We then rely on the central limit theorem.

#2. Correlation between covariates

# Correlations matrix
cor(anc_data[, c("Equality", "Need", "Equity", "Deprivation", "Blame")])


#3. Homoscedasicity test residuals (Equality of variance)

levene_test(.resid ~ Size*Group, data = model.metrics)


#4. Linearity of the relationship between DV and covariates per group  

# a. Equality and Greed
ggscatter(
  anc_data, x = "Equality", y = "Greed",
  facet.by  = c("Size", "Group"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

# b. Need and Greed
ggscatter(
  anc_data, x = "Need", y = "Greed",
  facet.by  = c("Size", "Group"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

# c. Equity and Greed
ggscatter(
  anc_data, x = "Equity", y = "Greed",
  facet.by  = c("Size", "Group"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

# d. Deprivation and Greed
ggscatter(
  anc_data, x = "Deprivation", y = "Greed",
  facet.by  = c("Size", "Group"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

# e. Blame and Greed
ggscatter(
  anc_data, x = "Equality", y = "Greed",
  facet.by  = c("Size", "Group"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)


# 5. Homogeneity of regression slopes

#a. Equality
anc_data %>%
  unite(col = "sizeandgroup", Size, Group) %>%
  anova_test(Greed ~ sizeandgroup*Equality)

#b. Need
anc_data %>%
  unite(col = "sizeandgroup", Size, Group) %>%
  anova_test(Greed ~ sizeandgroup*Need)

#c. Equity 
anc_data %>%
  unite(col = "sizeandgroup", Size, Group) %>%
  anova_test(Greed ~ sizeandgroup*Equity)

#Deprivation
anc_data %>%
  unite(col = "sizeandgroup", Size, Group) %>%
  anova_test(Greed ~ sizeandgroup*Deprivation)

#Blame
anc_data %>%
  unite(col = "sizeandgroup", Size, Group) %>%
  anova_test(Greed ~ sizeandgroup*Equality)




# a. ANCOVA model: Factors: Size (Big vs Small) * Group (Small vs Big),
#Covariates: Equality, need, equity, deprivation and blame

output = lm(Greed ~ Size*Group + Equality + Need + Equity + Deprivation + Blame,
            data = anc_data)

# b. Summary
summary.aov(output)

 # c. etas
etaSquared(output)

#Aligned Rank Transformed Data

ranked_model <- art(Greed ~ as.factor(Size) * as.factor(Group) 
                    , data = anc_data)
#Summary
anova(ranked_model)


#b. Interaction Plot


# Line plot

#a.
res.aov <- anc_data %>% 
  anova_test(Greed ~ Equality + Size*Group)
get_anova_table(res.aov)

# b
pwc <- anc_data %>% 
  group_by(Size) %>%
  emmeans_test(
    Greed ~ Group, covariate = Equality,
    p.adjust.method = "bonferroni"
  )


#c. 
lp <- ggline(
  get_emmeans(pwc), x = "Size", y = "emmean", 
  color = "Group", palette = "jco"
) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = Group), 
    width = 0.1
  )


# d.
pwc <- pwc %>% add_xy_position(x = "Size", fun = "mean_se", step.increase = 0.2)
pwc.filtered <- pwc %>% filter(Size == "Big")
lp + 
  stat_pvalue_manual(
    pwc.filtered, hide.ns = TRUE, tip.length = 0,
    bracket.size = 0
  ) +
  labs(
    subtitle = get_test_label(res.aov,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


#Descriptives: Frequency Tables

# 1. Gender
#create frequency matrix
gender <- matrix(c(53, 55, 61, 69, 76, 68, 61, 53), byrow=T, nrow=2)

colnames(gender) <- c("Small-Local", "Small-Foreign", "Large-Local", "Large-Foreign")
rownames(gender) <- c("Male", "Female")

#Chi Square test
gender_test<- chisq.test(gender)
gender_test

#2. Age 
#Create frequency matrix
age <- matrix(c(0, 9, 6, 8,
                 42, 43, 30, 40,
                 43, 33, 34, 34,
                 21, 18, 29, 24,
                 23, 20, 23, 16), byrow=T, 
              nrow=5)
colnames(age) <- c("Small-Local", "Small-Foreign", "Large-Local", "Large-Foreign")
rownames(age) <- c("18-24", "25-34", "35-44", "45-54", "55+")

#chi square test
age_test <- chisq.test(age)
age_test

#3. Education 
#Create frequency matrix
education <- matrix(c(1, 0, 3, 0,
                22, 30, 31, 28,
                14, 18, 14, 13,
                75, 62, 57, 62,
                17, 13, 17, 19), byrow=T, 
              nrow=5)

colnames(education) <- c("Small-Local", "Small-Foreign", "Large-Local", "Large-Foreign")
rownames(education) <- c("Less than high school", "High School", 
                   "Technical/Diploma", "University degree", "Postgraduate")

#chi square test
education_test <- chisq.test(education)
education_test

#4. Income 
#Create frequency matrix
income <- matrix(c(23, 31, 31, 38,
                      36, 38, 31, 25,
                      53, 41, 47, 42,
                      17, 13, 13, 17
                      ), byrow=T, 
                    nrow=4)

colnames(income) <- c("Small-Local", "Small-Foreign", "Large-Local", "Large-Foreign")
rownames(income) <- c("Less than 25k", "25k-<45k", 
                         "45k-<85k", "85k+")

#chi square test
income_test <- chisq.test(income)
income_test

#References

# The code for the pretest, Study 1 CFA, and Studies 1, 2, and 3 is partly based on the following resources: 

# 1.  Data carpentry contributors (2022) Data visualization with ggplot2. https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html 
#2. Gana, K. & Broc, G. (2019) Structural Equation Modelling with lavaan. Hoboken, NJ: Wiley.
#3. Crowson, M. (2019) Path analysis in R using Lavaan (video 3): Assessing and managing multivariate nonnormality. [Video] YouTube. https://www.youtube.com/watch?v=HvYW_GeHpD8&t=347s 
#4. Datanovia. (2018) ANCOVA in R. Comparing multiple means in R. https://www.datanovia.com/en/lessons/ancova-in-r/#:~:text=ANCOVA%20makes%20several%20assumptions%20about,Homogeneity%20of%20regression%20slopes
#5. Gard, A. (2020) Chi-Squared Testing for Independence in R. [Video] YouTube (Equitable Equations channel).  https://www.youtube.com/watch?v=XfsDtYuwVzs 


sal <- read.csv("Small_Local.csv")

install.packages("lavaan")
install.packages("semPlot")
install.packages("stringi")


library(lavaan)
library(semPlot)
library(stringi)

str(sal)


model1 <-'
        #measurement model
        greed =~Dir2 + Dir 3 + Dir4
        
        '
fit1 <- sem(model1, data= sal)         
summary(fit1, standardize = TRUE)         
fitMeasures(fit1, c("cfi", "rmsea", "srmr"))

semPaths(fit1, what="paths", whatLabels="stand", rotation=1)

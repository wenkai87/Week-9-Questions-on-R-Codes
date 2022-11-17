rm(list = ls())
setwd("C:/Users/lilia/Desktop/Wen Kai Imperial MPH/Advanced Statistics and Data Science/Advanced Statistics and Data Science")

library(Hmisc)
library(tidyverse)
library(mice)
library(VIM)
library(geepack)

s <- read.csv("elsa.csv", header = TRUE)
emlm <- s[,c("id","age","admin2", "sex", "healthgen", "bmi", "numgrp")]


### Data Prepraration: Get to Know Your Variables and Tidy Up the Values ###
# if you going to change the values of the columns, use g[,'variable'] instead of the short cut g$variable 

## Inspect data frame
str(emlm)                                      # check class of all variables
emlm                                           # inspect data frame for blank value, invalid value, types of value


## Inspect individual variable and address the invalid/ blank values
describe (emlm[,'age'])

describe(emlm[,'admin2'])                      # wrongly tells you there are 10 distinct values  
summary(emlm[,'admin2'])                       # reveals that you have 9 distinct values and 1 blank values - so need to replace blank with 'NA'
emlm[,'admin2'][emlm[,'admin2']==""] <-NA      # this is identical to emlm$admin1[emlm$admin1] == " " ] <- NA

describe(emlm[,'sex'])                         # tells you there are 3 distinct values - male, female and -8
table (emlm[,'sex'])
summary(emlm[,'sex'])
emlm[emlm[,'sex']==-8,'sex']<-NA
table(emlm[,'sex'], exclude = NULL)            # check whether -8 has been changed to NA
emlm[,'sex2'] <- ifelse(emlm[,'sex'] == "Female", 1,
                 ifelse(emlm[,'sex'] == "Male", 0, NA))
table(emlm[,'sex2'], exclude = NULL)

table(emlm[,'healthgen'])
describe (emlm[,'healthgen'])
emlm[emlm[,'healthgen'] == -1 | emlm[,'healthgen'] == -8 | emlm[,'healthgen'] == -9,'healthgen'] <- NA
emlm[,'healthgen2'] <- ifelse(emlm[,'healthgen'] == "Very bad",1,
                       ifelse(emlm[,'healthgen'] == "Bad",2,
                       ifelse(emlm[,'healthgen'] == "Fair",3,
                       ifelse(emlm[,'healthgen'] == "Good",4,
                       ifelse(emlm[,'healthgen'] == "Very good",5,NA)))))
table(emlm[,'healthgen2'], exclude = NULL)

describe(emlm[,'bmi'])

describe(emlm[,'numgrp'])
emlm[,'cogdecline'] <- ifelse(emlm[,'numgrp'] == "Worse scores", 1,
                        ifelse(emlm[,'numgrp'] == "Better scores", 0, NA))
table(emlm[,'cogdecline'], exclude = NULL)


### Data Prepraration: Working on data frame, as.factor () Variables +/- Remove Missing Rows from Data Frame ###

#Create a subset off the data frame #
e2 <- emlm[,c('cogdecline', 'admin2', 'age', 'sex2', 'bmi', 'healthgen2')]    
head(e2)             # compared with emlm, you have dropped 5 variables: id, age, sex, healthgen,numgrp
head(emlm)

str(e2)

e2[,'cogdecline'] <- as.numeric(e2[,'cogdecline'])   # Factors (with as.factor) are variables that have discrete values, 
                                                     # ,which may or may not be ordered/ categorical data
e2[,'sex2'] <- as.factor(e2[,'sex2'])
e2[,'healthgen2'] <- as.factor(e2[,'healthgen2'])    
e2[,'age'] <- as.numeric(e2[,'age'])  
e2[,'bmi'] <- as.numeric(e2[,'bmi'])                # Numerics (with as.numeric) are numbers, with infinite other numbers between them. 
                                                    # So for example 5 is a number, as is 6, but so are 5.01, 5.001, 5.0001 etc.

# don't need to as.factor admin2 for now because you'll do it in the geeglm later

str(e2)                                             # check whether the variables have been converted

# Re-level your categorical for your regression model as you see fit
e2[,'healthgen2'] <- relevel(e2[,'healthgen2'], 2)

# Remove incomplete rows
e3 <- e2
e3 <- e2[is.na(e2$cogdecline)==F &
           is.na(e2$admin2)==F &
           is.na(e2$age)==F &
           is.na(e2$sex)==F &
           is.na(e2$bmi)==F &
           is.na(e2$healthgen)==F,]
dim(e3)  # 5414
dim(e2)  # 11392
head(e3)

### Logistic Regression Model without MI or GEE or Mixed Effect ### 
log.regression.e3 <- glm(cogdecline ~ age + sex2 + bmi + healthgen2, data = e3, family = binomial)
summary(log.regression.e3)

### GEE Model with Independence Correlation Structure ###
fit.gee.in.e3 <- geeglm(cogdecline ~ age + sex2 + bmi + healthgen2, 
                        data = e3, 
                        family = binomial,             # binary outcome
                        id = as.numeric(factor(admin2)),
                        corstr="independence")
summary(fit.gee.in.e3)
attributes(fit.gee.in.e) # find the object with the coefs to exponentiate
exp(fit.gee.in.e$coefficients) # also need "std.err" to get CIs


### GEE Model with Exchangeable Correlation Structure ###
fit.gee.ex.e3 <- geeglm(cogdecline ~ age + sex2 + bmi + healthgen2, 
                        data = e3, 
                        family = binomial,                # binary outcome
                        id=as.numeric(factor(admin2)),    # group level variable is admin2
                        corstr="exchangeable")
                                                          # all pairs of observations (people) within the same level of admin2
                                                          # have the same correlation, and this correlation coefficient
                                                          # (often called alpha) needs to be estimated from the data
summary(fit.gee.ex.e3)


### Imputation ###
# ??MICE package can only work on numeric data. Need to change healthgen and sex into numeric data
e4 <- e2

e4[,'cogdecline'] <- as.factor(e4[,'cogdecline'])   
e4[,'sex2'] <- as.factor(e4[,'sex2'])
e4[,'healthgen2'] <- as.factor(e4[,'healthgen2'])    
e4[,'age'] <- as.numeric(e4[,'age'])  
e4[,'bmi'] <- as.numeric(e4[,'bmi']) 

class(e4[,'bmi'])
table(e4[,'bmi'])
describe (e4[,'bmi'])

## Logistic Regression with MI but without GEE

# mice package to create a number of imputed datasets that replace missing values with plausible values
# https://data.library.virginia.edu/getting-started-with-multiple-imputation-in-r/
# replicate (create) 5 datasets. Each missing value in the dataset is now replaced with value generated by mice()
impute.data <- mice(e4,m=5,maxit=50,seed=2525, printFlag=FALSE) 
impute.data$method 

# now with 5 complete datasets, run the ols regression and obtain a different regression coefficient for each dataset, reflecting the effect of predictors on outcome
# now with 5 coefficients that are different from each other, pool together the coefficients into 1 final regression coefficient
fit.impute.data.glm <- with(impute.data,glm(cogdecline ~ age + sex2 + bmi + healthgen2, family=binomial))
summary(fit.impute.data.glm)
summary(pool(fit.impute.data.glm))


## Logistic Regression with MI and GEE
fit.impute.data.glm.gee <- with(impute.data,geeglm(cogdecline ~ age + sex2 + bmi + healthgen2, 
                                                family=binomial, 
                                                id=as.numeric(factor(admin2)),    
                                                corstr="independence"))
summary(fit.impute.data.glm.gee)
summary(pool(fit.impute.data.glm.gee))
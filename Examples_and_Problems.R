## R code for all examples and problems in "Primer of Applied Regression and Analysis of Variance" from Stanton A. Glantz & Bryan K. Slinker; 1. edition; 1990

setwd("~/Dokumente/Computer/git/multiple_Regression_and_ANOVA/")

library("ggplot2")

# Chapter 1
# Examples
# Table 1-1
table.1.1 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marswhw.dat"), header = F, sep="")
table.1.1 <- data.frame(weight = table.1.1$V1,
                        height = table.1.1$V2,
                        water.consumption = table.1.1$V3)
write.csv(table.1.1, file = "Table_1.1.csv",
          row.names = F)

# Fig. 1-1 plotting weight against height from Table 1-1
ggplot(table.1.1, aes(x=height, y=weight)) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
  se=FALSE)    # Don't add shaded confidence region
lm(formula = weight ~ height, data = table.1.1)

# Fig. 1-2 plotting weight against height from Table 1-1 with water_consumption as factor
ggplot(table.1.1, aes(x=height, y=weight, color=as.factor(water.consumption))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
multiple.regression <- lm(formula = weight ~ height + water.consumption, data = table.1.1)
multiple.regression 
summary(multiple.regression)

# Intercept <-> c1
# height <-> m*x
# water.consumption <-> c2*C #(Cups)
# f(x) = c1 + m*x + c2*C

####
# Example for Dummy varialbes (D)
table.1.2 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marssmok.dat"), header = F, sep="")
table.1.2 <- data.frame(weight = table.1.2$V1,
                        height = table.1.2$V2,
                        scd.hnd.smk = table.1.2$V3)
write.csv(table.1.2, file = "Table_1.2.csv",
          row.names = F)
# Fig. 1-3 plotting weight against height from Table 1-1 with second hand smoke as factor
ggplot(table.1.2, aes(x=height, y=weight, color=as.factor(scd.hnd.smk))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
multiple.regression <- lm(formula = weight ~ height + scd.hnd.smk, data = table.1.2)
multiple.regression 
summary(multiple.regression)

# weight (c)
# height (x)
# exposure to second hand smoke: (scd.hnd.smk (C)) D = 1
# no exposure to second hand smoke: (scd.hnd.smk(C)) D = 0
# f(x) = c + m*x + C*D

# Chapter 2
# Examples
# data for Fig. 2-1 is not provided; data for Fig. 2-3:
table.2.1 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marswh.dat"), header = F, sep="")
table.2.1 <- data.frame(weight = table.2.1$V1,
                        height = table.2.1$V2)
write.csv(table.2.1, file = "Table_2.1.csv",
          row.names = F)
# plotting data
# Fig. 1-1 plotting weight against height from Table 1-1
ggplot(table.2.1, aes(x=height, y=weight)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
summary(lm(formula = weight ~ height, data = table.2.1))
# Intercept: b_0
#   "(Intercept)"
# slope: b_1
#   "height"
# standard error of the estimate: s_(y|x)
# [variability about the line of means; "average deviation of the data about the regression line"]
#   "Residual standard error"
# y = b_0 + b_1*x + s_(y|x)
# used to compute confidence intervals and test hypotheses
# standard error of the slope: column "Std.Error" in row "height"
# standard error of the intercept: column "Std.Error" in row "(Intercept)"
# t-statistics in the columns "t value", corresponding P-value in "Pr(>|t|)" and significance stars

# R-squared (coefficient of determination): 
# Square of the Pearson product-moment correlation coefficient
# R^2 describes how well the regression line represents the realaionship between the variables

# Testing the Regression as a Whole:
# F-statistic
# in the case of a simple linear regression: 
# is the slope of the regression line sign. different from 0?
# in multiple regression:
# t-test: test for each independent variable
# F-statistic:  test for ALL independent variables

# determine confidence intervals:
confint(lm(formula = weight ~ height, data = table.2.1))

## Heat exchange in gray seals
table.C.2 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/seals.dat"), header = F, sep="")
table.C.2 <- data.frame(C.b = table.C.2$V1,
                        degree.C = table.C.2$V2)
write.csv(table.C.2, file = "Table_C.2.csv",
          row.names = F)

ggplot(table.C.2, aes(x=degree.C, y=C.b)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
summary(lm(formula = C.b ~ degree.C, data = table.C.2))
# linear fit should not be used if you examine the datapoints more closely
# examine the plots of raw data!

# Problems: 
# 2.1
# From output of summary(lm()) of table.2.1
results.2.1 <- summary(lm(formula = weight ~ height, data = table.2.1))
#   slope: 0.4441
results.2.1$coefficients[[2]]
#   standard error of the slope: 0.0643
results.2.1$coefficients[[4]]
#   intercept: -6.0076
results.2.1$coefficients[[1]]
#   standard error of the intercept: 2.3921
results.2.1$coefficients[[3]]
#   correlation coefficient (sqrt(R-squared)): sqrt(0.8564) = 0.9254
sqrt(results.2.1$r.squared)
#   standard error of the estimate (Residual standard error): 0.964 
results.2.1$sigma
#   F statistic for overall goodness of fit: 47.71
results.2.1$fstatistic[[1]]

#####
#input.data <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-01.dat"), header = F, sep="")



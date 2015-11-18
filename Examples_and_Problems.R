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

# 2.2
table.D.1 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-01.dat"), header = F, sep="")
table.D.1 <- data.frame(Sedation.score = table.D.1$V1,
                        Cortisol = table.D.1$V2)
write.csv(table.D.1, file = "Table_D.1.csv",
          row.names = F)

ggplot(table.D.1, aes(x=Cortisol,y=Sedation.score)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

results.2.2 <- summary(lm(formula = Sedation.score ~ Cortisol, data = table.D.1))
#   correlation coefficient (sqrt(R-squared)):
sqrt(results.2.2$r.squared)
#   standard error of the estimate (Residual standard error):
results.2.2$sigma
# p.value F-statistic:
1 - pf(results.2.2$fstatistic[[1]], results.2.2$fstatistic[[2]], results.2.2$fstatistic[[3]])

# 2.3
table.D.2 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-02.dat"), header = F, sep="")
table.D.2 <- data.frame(Breast.Cancer = table.D.2$V1,
                        Lung.Cancer = table.D.2$V2,
                        Animal.Fat = table.D.2$V3)
write.csv(table.D.2, file = "Table_D.2.csv",
          row.names = F)

ggplot(table.D.2, aes(x=Lung.Cancer, y=Breast.Cancer)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

results.2.3 <- summary(lm(formula = Breast.Cancer ~ Lung.Cancer, data = table.D.2))
results.2.3

# 2.4 
table.D.3 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-03.dat"), header = F, sep="")
table.D.3 <- data.frame(CFU = table.D.3$V1,
                        time.a.MIC = table.D.3$V2,
                        Dose = table.D.3$V3)
write.csv(table.D.3, file = "Table_D.3.csv",
          row.names = F)

# analyse only the datasets with Dose == 0
D.3.subset <- subset(table.D.3, Dose == 0)

ggplot(D.3.subset, aes(x=time.a.MIC, y=CFU)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=T)    # add shaded confidence region

results.2.4 <- summary(lm(formula = CFU ~ time.a.MIC, data = D.3.subset))
results.2.4
confint(lm(formula = CFU ~ time.a.MIC, data = D.3.subset))


# 2.5
table.D.4 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-04.dat"), header = F, sep="")
table.D.4 <- data.frame(RV.resistance = table.D.4$V1,
                        plasma.renin = table.D.4$V2,
                        group.code = table.D.4$V3)
write.csv(table.D.4, file = "Table_D.4.csv",
          row.names = F)

# A mean values; significant relationship between RV.resistance and plasma.renin?
require(plyr)
table.D.4.A <- ddply(table.D.4, .(group.code), summarize,
      mean.RV.resistance = round(mean(RV.resistance), 2),
      sd.RV.resistance = round(sd(RV.resistance), 2),
      mean.plasma.renin = round(mean(plasma.renin), 2),
      sd.plasma.renin = round(sd(plasma.renin), 2),
      n = length(group.code))


ggplot(table.D.4.A, aes(x=mean.plasma.renin, y=mean.RV.resistance)) +
  geom_point(shape=1) + 
  # Errorbar
  geom_errorbar(aes(
    ymin=table.D.4.A$mean.RV.resistance - table.D.4.A$sd.RV.resistance, 
    ymax=table.D.4.A$mean.RV.resistance + table.D.4.A$sd.RV.resistance,
   # xmin=table.D.4.A$mean.plasma.renin - table.D.4.A$sd.plasma.renin, 
    #xmax=table.D.4.A$mean.plasma.renin + table.D.4.A$sd.plasma.renin),
    width=.2)) +
  geom_errorbarh(aes(
       xmin=table.D.4.A$mean.plasma.renin - table.D.4.A$sd.plasma.renin, 
       xmax=table.D.4.A$mean.plasma.renin + table.D.4.A$sd.plasma.renin),
       width=.2) +
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, # Don't add shaded confidence region
              fullrange=TRUE)    # extend line

results.2.4A <- summary(lm(formula = mean.RV.resistance ~ mean.plasma.renin , data = table.D.4.A))
results.2.4A


# 2.6
## failed as server is not responding
table.D.5 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-05.dat"), header = F, sep="")
table.D.5 <- data.frame(U.Ca = table.D.5$V1,
                        D.Ca = table.D.5$V2,
                        D.p = table.D.5$V3,
                        U.Na = table.D.5$V4,
                        G.fr = table.D.5$V5)
write.csv(table.D.5, file = "Table_D.5.csv",
          row.names = F)

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
# Example for Dummy variables (D)
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
table.C.1 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/seals.dat"), header = F, sep="")
table.C.1 <- data.frame(C.b = table.C.1$V1,
                        degree.C = table.C.1$V2)
write.csv(table.C.1, file = "Table_C.1.csv",
          row.names = F)

ggplot(table.C.1, aes(x=degree.C, y=C.b)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
summary(lm(formula = C.b ~ degree.C, data = table.C.1))
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

# 2.5.A mean values; significant relationship between RV.resistance and plasma.renin?
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

results.2.5A <- summary(lm(formula = mean.RV.resistance ~ mean.plasma.renin , data = table.D.4.A))
results.2.5A

# 2.5.B
# relationship: changes in renal resistance and renin
table.D.4 <- read.csv(file = "Table_D.4.csv")
ggplot(table.D.4, aes(x=plasma.renin, y=RV.resistance)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=F)    # add shaded confidence region
results.2.5B <- summary(lm(formula = RV.resistance ~ plasma.renin, data = table.D.4))
results.2.5B

# 2.5.C
# compare
results.2.5A
results.2.5B

# 2.5.D
# do NOT compute a regression line for the mean values
# you ignore the variablity of the data points

# 2.6
table.D.5 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-05.dat"), header = F, sep="")
table.D.5 <- data.frame(U.Ca = table.D.5$V1,
                        D.Ca = table.D.5$V2,
                        G.fr= table.D.5$V3,
                        U.Na = table.D.5$V4,
                        D.p = table.D.5$V5)
write.csv(table.D.5, file = "Table_D.5.csv",
          row.names = F)

ggplot(table.D.5, aes(x = D.Ca, y = U.Ca)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=F)    # add shaded confidence region
results.2.6.D.Ca <- summary(lm(formula = D.Ca ~ U.Ca, data = table.D.5))
results.2.6.D.Ca$r.squared;
# p.value F-statistic:
1 - pf(results.2.6.D.Ca$fstatistic[[1]], results.2.6.D.Ca$fstatistic[[2]], results.2.6.D.Ca$fstatistic[[3]])

results.2.6.G.fr <- summary(lm(formula = G.fr ~ U.Ca, data = table.D.5))
results.2.6.G.fr$r.squared;
# p.value F-statistic:
1 - pf(results.2.6.G.fr$fstatistic[[1]], results.2.6.G.fr$fstatistic[[2]], results.2.6.G.fr$fstatistic[[3]])

results.2.6.U.Na <- summary(lm(formula = U.Na ~ U.Ca, data = table.D.5))
results.2.6.U.Na$r.squared;
# p.value F-statistic:
1 - pf(results.2.6.U.Na$fstatistic[[1]], results.2.6.U.Na$fstatistic[[2]], results.2.6.U.Na$fstatistic[[3]])

results.2.6.D.p <- summary(lm(formula = D.p ~ U.Ca, data = table.D.5))
results.2.6.D.p$r.squared;
# p.value F-statistic:
1 - pf(results.2.6.D.p$fstatistic[[1]], results.2.6.D.p$fstatistic[[2]], results.2.6.D.p$fstatistic[[3]])

# 2.7
table.D.6 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-06.dat"), header = F, sep="")
table.D.6 <- data.frame(type.F = table.D.6$V1,
                        T.1 = table.D.6$V2,
                        T.2= table.D.6$V3)
write.csv(table.D.6, file = "Table_D.6.csv",
          row.names = F)

ggplot(table.D.6, aes(x = T.1, y = type.F)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=F)    # add shaded confidence region
results.2.7 <- summary(lm(formula = type.F ~ T.1 , data = table.D.6))
results.2.7

## To Do
# function for to get certain values from summary(lm())
#		- p.Value of F-statistik
#		- sqrt(R^2)

## Chapter 3
# Fig 3-1
library(latticeExtra)

cloud(y~x+z, d, panel.3d.cloud=panel.3dbars, col.facet='grey', 
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")))

data.3.1 <- read.csv("Table_1.1.csv")
multiple.regression <- lm(formula = weight ~ height + water.consumption, data = data.3.1)
multiple.regression 
summary(multiple.regression)

library(latticeExtra)
cloud(weight ~ height + as.factor(water.consumption), data.3.1, panel.3d.cloud=panel.3dbars, col.facet='grey', 
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")))

# Fig 3-2
table.1.1 <- read.csv("Table_1.1.csv")
multiple.regression <- lm(formula = weight ~ height + water.consumption, data = table.1.1)
multiple.regression 
summary(multiple.regression)
# getting the p-value of the F-statistic:
f = summary(multiple.regression)$fstatistic
pf(f[1],f[2],f[3],lower.tail=F)
# getting residual mean square ( = square of the standard error of the estimate)
s.y.x <- (summary(multiple.regression)$sigma)^2
s.y.x
# determine confidence intervals:
confint(lm(formula = weight ~ height + water.consumption, data = table.1.1))

## getting the incremental sum of squares:
#anova(lm(formula = weight ~ height + water.consumption, data = table.1.1))
# used to test wether adding independent variables helps to predict the dependent variable
# F = (mean square associated with adding x2 (x1 is already in the equation)) / (mean square residual for equation containing x1 and x2)
test.3.2 <- anova(lm(formula = weight ~ height + water.consumption, data = table.1.1))
#test whether water.consumption adds information after taking into account height
F.statistic.3.2 <-  test.3.2$`Mean Sq`[[2]]/test.3.2$`Mean Sq`[[3]]
F.statistic.3.2 
#denominator degree of freedom:
test.3.2$Df[[2]]
#denominator degree of freedom:
test.3.2$Df[[3]]
## ! the order in the formula matters if the independent variables are correlated! formula = weight ~ height + water.consumption OR formula = weight ~ water.consumption + height 
# p.value F-statistic:
# 1 - pf(F-value, nominator degree of freedom, denominator degree of freedom)
p.value.F.statistic.3.2 <- 1 - pf(F.statistic.3.2 , test.3.2$Df[[2]], test.3.2$Df[[2]])
p.value.F.statistic.3.2

# Fig 3-7
table.1.2 <- read.csv("Table_1.2.csv")
table.1.2.m.reg <- lm(formula = weight ~ height + scd.hnd.smk, data = table.1.2)
summary(table.1.2.m.reg)





# Fig 3-9 and Fig. 3-10
table.C.2 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/endotoxn.dat"), header = F, sep="")
table.C.2 <- data.frame(P = table.C.2$V1,
                        A = table.C.2$V2,
                        E = table.C.2$V3)
write.csv(table.C.2, file = "Table_C.2.csv",
          row.names = F)

# Fig. 1-2 plotting weight against height from Table 1-1 with water_consumption as factor
ggplot(table.C.2, aes(x=A, y=P, color=as.factor(E))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
C.2.m.reg <- lm(formula = P ~ A + E, data = table.C.2)
C.2.m.reg 
summary(C.2.m.reg )


# Fig. 3-11
table.C.3 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/leucine.dat"), header = F, sep="")
table.C.3 <- data.frame(log.L = table.C.3$V1,
                        log.W = table.C.3$V2,
                        A = table.C.3$V3)
write.csv(table.C.3, file = "Table_C.3.csv",
          row.names = F)

# plotting with a simple linear regression 
ggplot(table.C.3, aes(x = log.W, y = log.L)) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

C.3.reg <- lm(formula = log.L ~ log.W, data = table.C.3)
C.3.reg 
summary(C.3.reg)

ggplot(table.C.3, aes(x = log.W, y = log.L, color = as.factor(A) )) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

C.3.m.reg <- lm(formula = log.L ~ log.W + A, data = table.C.3)
C.3.m.reg 
summary(C.3.m.reg )


# Fig. 3-13

table.C.4 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/diabetes.dat"), header = F, sep="")
table.C.4 <- data.frame(H = table.C.4$V1,
                        B = table.C.4$V2,
                        D = table.C.4$V3,
                        S = table.C.4$V4,
                        A = table.C.4$V5,
                        W = table.C.4$V6,
                        T = table.C.4$V7,
                        C = table.C.4$V8,
                        G = table.C.4$V9)
write.csv(table.C.4, file = "Table_C.4.csv",
          row.names = F)

C.4.mreg <- lm(formula = H ~ B + D + S + A + W + T + C + G, data = table.C.4)
summary(C.4.mreg)

# determine confidence intervals:
confint(lm(formula = H ~ B + D + S + A + W + T + C + G, data = table.C.4))

# Fig. 3-15
table.C.5 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/babybird.dat"), header = F, sep="")
table.C.5 <- data.frame(V = table.C.5$V1,
                        O = table.C.5$V2,
                        C = table.C.5$V3)
write.csv(table.C.5, file = "Table_C.5.csv",
          row.names = F)

C.5.mreg <- lm(formula = V ~ O + C , data = table.C.5)
summary(C.5.mreg)
# 31.1 % increase in ventilation with each 1 percent increase in conc(CO2)

# Fig 3-16
# Polynomal regression
table.C.1.preg  <- read.csv("Table_C.1.csv")
# define polynomal function [y = b0  + b1*x + b2*x^2] within lm():
C.1.preg <- lm(formula = C.b ~ degree.C + I(degree.C^2), data = table.C.1.preg)
summary(C.1.preg)

# plotting with polynomial function fit
fit.func <- function(x, b0, b1, b2)(b0  + b1*x + b2*x^2)
arguments <- list(b0 = C.1.preg$coefficients[[1]], 
                  b1 = C.1.preg$coefficients[[2]], 
                  b2 = C.1.preg$coefficients[[3]])

ggplot(table.C.1.preg, aes(x = degree.C, y = C.b)) +
  geom_point(data =  table.C.1.preg, aes(x = degree.C, y = C.b)) +
  stat_function(fun = fit.func, args = arguments) # plot function with corresponding parameters


# other nonlinear Regressions
# 1. convert nonlinear regression equation into a multiple linear regression equation
#   z = b0 + b1*x + b2*sin(x) + b3 * (x^3 + ln(x)) + b4*e^y ==>
#   x1 = x; x2 = sin(x); x3 = x^3 + ln(x); x4 = e^y ==>
#   z = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4
# 2. transform independent variable
#   y = a*e^(bx)
#   ln(y) = ln(a) + bx  ==> y' = ln(y), a = ln(a) ==>
#   y' = a' + bx
# PROBLEM:
# implicit assumpitons
# e.g.: log-transformation: weight of deviations of small values is increased

# INTERACTIONS BETWEEN INDEPENDENT VARIABLES
# Fig. 3-18
# interaction terms:
#   y = b0 + b1*x1 + b2*x2 + b12*x1*x2 = b0 + b1*x1 + b2*x2 + b12*x12
# if b12 is significantly different to 0 => significant interaction between independent variables
#   y = b0 + b1*x1 + (b2 + b12*x1)*x2
# sensitivity of y to changes in x2 depends on x1

table.C.6 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/saltbact.dat"), header = F, sep="")
table.C.6 <- data.frame(T.H2O = table.C.6$V1,
                        time = table.C.6$V2,
                        NaCl = table.C.6$V3)
write.csv(table.C.6, file = "Table_C.6.csv",
          row.names = F)

C.6.mreg <- lm(formula = T.H2O ~ time + NaCl + I(time*NaCl), data = table.C.6)
summary(C.6.mreg)
C.6.mreg.no.IA <- lm(formula = T.H2O ~ time + NaCl, data = table.C.6)
summary(C.6.mreg.no.IA)

ggplot(table.C.6, aes(x=time , y=T.H2O, color=as.factor(NaCl))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines


# Fig 3-22
table.C.7 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/muscle.dat"), header = F, sep="")
table.C.7 <- data.frame(delta.F = table.C.7$V1,
                        L = table.C.7$V2,
                        F = table.C.7$V3)
write.csv(table.C.7, file = "Table_C.7.csv",
          row.names = F)
# non-linear model and interaction between variables:
C.7.mreg <- lm(formula = delta.F ~ L + I(L^2) + I(L*F), data = table.C.7)
summary(C.7.mreg)
# L <=> delta L; I(L^2) <=> delta L squared; I(L*F) <=> delta L * F [quantifies how the relationship changes in response to changes in developed force F]

ggplot(table.C.7, aes(x=L , y=delta.F, color=as.factor(F))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

##
# P-3.1
# formula = weight ~ height + scd.hnd.smk  + heigth*scd.hnd.smk 
# solve with scd.hnd.smk = 1 and scd.hnd.smk = 0

# P-3.2
table.1.2 <- read.csv("Table_1.2.csv")
table.1.2.m.reg <- lm(formula = weight ~ height + scd.hnd.smk, data = table.1.2)
summary(table.1.2.m.reg)

ggplot(table.1.2, aes(x=height , y=weight, color=as.factor(scd.hnd.smk))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

# here the 2D representation was chosen instead
# the slope of the regression plane of scd.hnd.smk is a boolean variable
# ==> there are no intermediate values
# but you can see the significance of differences between the two conditions, the parallel shift of the regression line

# P-3.3
table.1.1.mod <- read.csv("Table_1.1.csv")
table.1.1.mod$water.consumption <- c(2,4,6,10,8,12,18,14,16,20,22,24)
table.1.1.mod.mreg <- lm(formula = weight ~ height + water.consumption, data = table.1.1.mod)
summary(table.1.1.mod.mreg)
## Errata from book:
#Problem 3.3
#The answer (p. 888) is incorrect.
#The incorrect answer prints the regression equation as 
## W = -15.9 + 0.759*H - 0.252*W
#whereas the correct regression equation is as computet!

# P-3-4
# "ROOT MSE" = 0.3087879

# P-3-5
table.D.7 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-07.dat"), header = F, sep="")
table.D.7 <- data.frame(year = table.D.7$V1,
                        Adv.Rev = table.D.7$V2,
                        Smk.Rel.CSM = table.D.7$V3,
                        Smk.Rel.ApY = table.D.7$V4)
write.csv(table.D.7, file = "Table_D.7.csv",
          row.names = F)

# non-linear model and interaction between variables:
D.7.mreg <- lm(formula =  Smk.Rel.ApY ~ year + Adv.Rev  + Smk.Rel.CSM, data = table.D.7)
summary(D.7.mreg)
# L <=> delta L; I(L^2) <=> deSmk.Rel.ApYlta L squared; I(L*F) <=> delta L * F [quantifies how the relationship changes in response to changes in developed force F]

# P-3-6
table.C.5 <- read.csv("Table_C.5.csv")
C.5.mreg <- lm(formula = V ~ O + C , data = table.C.5)
results.C.5.mreg <- summary(C.5.mreg)

# R²; indep. variables account for only 27% of total variation observed in V:
results.C.5.mreg$r.squared
#   F statistic for overall goodness of fit; regression equation provides a better prediction of the obersvations than the mean value of V
results.C.5.mreg$fstatistic[[1]]
# high uncertainty for values of dep. variable for combinations fo indep. variables; (157%)
results.C.5.mreg$sigma


# P-3-7

table.D.8 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-08.dat"), header = F, sep="")
table.D.8 <- data.frame(P = table.D.8$V1,
                        N = table.D.8$V2,
                        E.group = table.D.8$V3,
                        D.group = table.D.8$V4)
write.csv(table.D.8, file = "Table_D.8.csv",
          row.names = F)

# Table of the data of Sato et al.
Xamoterol <- c(0,0,0,0,1,1,1,1)
E.group <- c(1,2,3,4,1,2,3,4)
P.mean <- c(125,138,150,162,135,146,158,161)
P.sd <- c(22,25,30,31,30,27,30,36)
N.mean <- c(234,369,543,754,211,427,766,1068)
N.sd <- c(87,165,205,279,106,257,355,551)
n <- c(6,6,6,6,10,10,10,10)
table.D.8.Sato <- data.frame(P.mean, P.sd, N.mean, N.sd, n, E.group, D.group = Xamoterol)

ggplot(table.D.8.Sato, aes(x=N.mean, y=P.mean, color=as.factor(D.group) )) +
  geom_point(shape=1) + 
  # Errorbar
  geom_errorbar(aes(
    ymin=table.D.8.Sato$P.mean - table.D.8.Sato$P.sd, 
    ymax=table.D.8.Sato$P.mean + table.D.8.Sato$P.sd,
     width=.2)) +
  geom_errorbarh(aes(
    xmin=table.D.8.Sato$N.mean - table.D.8.Sato$N.sd, 
    xmax=table.D.8.Sato$N.mean + table.D.8.Sato$N.sd),
    width=.2) +
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, # Don't add shaded confidence region
              fullrange=TRUE)    # extend line

D.8.Sato.lm <- summary(lm(formula = P.mean ~ N.mean + D.group , data = table.D.8.Sato))
D.8.Sato.lm 

table.D.8.Sato.D0 <- subset(table.D.8.Sato, D.group == 0)
D.8.Sato.lm.D0 <- summary(lm(formula = P.mean ~ N.mean, data = table.D.8.Sato.D0))
table.D.8.Sato.D1 <- subset(table.D.8.Sato, D.group == 1)
D.8.Sato.lm.D1 <- summary(lm(formula = P.mean ~ N.mean, data = table.D.8.Sato.D1))

# Intersection of the two regression lines with D.group = 0 and 1
# 131.2 + 0.03049x = 110.4 + 0.07013x
# switch from stimulant -> blocker:
#  524.7225 pg/mL
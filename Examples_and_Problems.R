## R code for all examples and problems in "Primer of Applied Regression and Analysis of Variance" from Stanton A. Glantz & Bryan K. Slinker; 1. edition; 1990

setwd("~/Dokumente/Computer/git/multiple_Regression_and_ANOVA/")
library("ggplot2")

# Chapter 1
# Examples
# Table 1-1
tab.1.1 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marswhw.dat"), header = F, sep="")
tab.1.1 <- data.frame(weight = tab.1.1$V1,
                        height = tab.1.1$V2,
                        water.consumption = tab.1.1$V3)
write.csv(tab.1.1, file = "tab.1.1.csv",
          row.names = F)

# Fig. 1-1 plotting weight against height from Table 1-1
ggplot(tab.1.1, aes(x=height, y=weight)) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
  se=FALSE)    # Don't add shaded confidence region
lm(formula = weight ~ height, data = tab.1.1)

# Fig. 1-2 plotting weight against height from Table 1-1 with water_consumption as factor
ggplot(tab.1.1, aes(x=height, y=weight, color=as.factor(water.consumption))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
multiple.regression <- lm(formula = weight ~ height + water.consumption, data = tab.1.1)
multiple.regression 
summary(multiple.regression)

# Intercept <-> c1
# height <-> m*x
# water.consumption <-> c2*C #(Cups)
# f(x) = c1 + m*x + c2*C

####
# Example for Dummy variables (D)
tab.1.2 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marssmok.dat"), header = F, sep="")
tab.1.2 <- data.frame(weight = tab.1.2$V1,
                        height = tab.1.2$V2,
                        scd.hnd.smk = tab.1.2$V3)
write.csv(tab.1.2, file = "tab.1.2.csv",
          row.names = F)
# Fig. 1-3 plotting weight against height from Table 1-1 with second hand smoke as factor
ggplot(tab.1.2, aes(x=height, y=weight, color=as.factor(scd.hnd.smk))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
multiple.regression <- lm(formula = weight ~ height + scd.hnd.smk, data = tab.1.2)
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
tab.2.1 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marswh.dat"), header = F, sep="")
tab.2.1 <- data.frame(weight = tab.2.1$V1,
                        height = tab.2.1$V2)
write.csv(tab.2.1, file = "tab.2.1.csv",
          row.names = F)
# plotting data
# Fig. 1-1 plotting weight against height from Table 1-1
ggplot(tab.2.1, aes(x=height, y=weight)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
summary(lm(formula = weight ~ height, data = tab.2.1))
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
confint(lm(formula = weight ~ height, data = tab.2.1))

## Heat exchange in gray seals
tab.C.1 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/seals.dat"), header = F, sep="")
tab.C.1 <- data.frame(C.b = tab.C.1$V1,
                        degree.C = tab.C.1$V2)
write.csv(tab.C.1, file = "tab.C.1.csv",
          row.names = F)

ggplot(tab.C.1, aes(x=degree.C, y=C.b)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
summary(lm(formula = C.b ~ degree.C, data = tab.C.1))
# linear fit should not be used if you examine the datapoints more closely
# examine the plots of raw data!

# Problems: 
# 2.1
# From output of summary(lm()) of tab.2.1
results.2.1 <- summary(lm(formula = weight ~ height, data = tab.2.1))
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
tab.D.1 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-01.dat"), header = F, sep="")
tab.D.1 <- data.frame(Sedation.score = tab.D.1$V1,
                        Cortisol = tab.D.1$V2)
write.csv(tab.D.1, file = "tab.D.1.csv",
          row.names = F)

ggplot(tab.D.1, aes(x=Cortisol,y=Sedation.score)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

results.2.2 <- summary(lm(formula = Sedation.score ~ Cortisol, data = tab.D.1))
#   correlation coefficient (sqrt(R-squared)):
sqrt(results.2.2$r.squared)
#   standard error of the estimate (Residual standard error):
results.2.2$sigma
# p.value F-statistic:
1 - pf(results.2.2$fstatistic[[1]], results.2.2$fstatistic[[2]], results.2.2$fstatistic[[3]])

# 2.3
tab.D.2 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-02.dat"), header = F, sep="")
tab.D.2 <- data.frame(Breast.Cancer = tab.D.2$V1,
                        Lung.Cancer = tab.D.2$V2,
                        Animal.Fat = tab.D.2$V3)
write.csv(tab.D.2, file = "tab.D.2.csv",
          row.names = F)

ggplot(tab.D.2, aes(x=Lung.Cancer, y=Breast.Cancer)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

results.2.3 <- summary(lm(formula = Breast.Cancer ~ Lung.Cancer, data = tab.D.2))
results.2.3

# 2.4 
tab.D.3 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-03.dat"), header = F, sep="")
tab.D.3 <- data.frame(CFU = tab.D.3$V1,
                        time.a.MIC = tab.D.3$V2,
                        Dose = tab.D.3$V3)
write.csv(tab.D.3, file = "tab.D.3.csv",
          row.names = F)

# analyse only the datasets with Dose == 0
D.3.subset <- subset(tab.D.3, Dose == 0)

ggplot(D.3.subset, aes(x=time.a.MIC, y=CFU)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=T)    # add shaded confidence region

results.2.4 <- summary(lm(formula = CFU ~ time.a.MIC, data = D.3.subset))
results.2.4
confint(lm(formula = CFU ~ time.a.MIC, data = D.3.subset))


# 2.5
tab.D.4 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-04.dat"), header = F, sep="")
tab.D.4 <- data.frame(RV.resistance = tab.D.4$V1,
                        plasma.renin = tab.D.4$V2,
                        group.code = tab.D.4$V3)
write.csv(tab.D.4, file = "tab.D.4.csv",
          row.names = F)

# 2.5.A mean values; significant relationship between RV.resistance and plasma.renin?
require(plyr)
tab.D.4.A <- ddply(tab.D.4, .(group.code), summarize,
      mean.RV.resistance = round(mean(RV.resistance), 2),
      sd.RV.resistance = round(sd(RV.resistance), 2),
      mean.plasma.renin = round(mean(plasma.renin), 2),
      sd.plasma.renin = round(sd(plasma.renin), 2),
      n = length(group.code))


ggplot(tab.D.4.A, aes(x=mean.plasma.renin, y=mean.RV.resistance)) +
  geom_point(shape=1) + 
  # Errorbar
  geom_errorbar(aes(
    ymin=tab.D.4.A$mean.RV.resistance - tab.D.4.A$sd.RV.resistance, 
    ymax=tab.D.4.A$mean.RV.resistance + tab.D.4.A$sd.RV.resistance,
   # xmin=tab.D.4.A$mean.plasma.renin - tab.D.4.A$sd.plasma.renin, 
    #xmax=tab.D.4.A$mean.plasma.renin + tab.D.4.A$sd.plasma.renin),
    width=.2)) +
  geom_errorbarh(aes(
       xmin=tab.D.4.A$mean.plasma.renin - tab.D.4.A$sd.plasma.renin, 
       xmax=tab.D.4.A$mean.plasma.renin + tab.D.4.A$sd.plasma.renin),
       width=.2) +
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, # Don't add shaded confidence region
              fullrange=TRUE)    # extend line

results.2.5A <- summary(lm(formula = mean.RV.resistance ~ mean.plasma.renin , data = tab.D.4.A))
results.2.5A

# 2.5.B
# relationship: changes in renal resistance and renin
tab.D.4 <- read.csv(file = "tab.D.4.csv")
ggplot(tab.D.4, aes(x=plasma.renin, y=RV.resistance)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=F)    # add shaded confidence region
results.2.5B <- summary(lm(formula = RV.resistance ~ plasma.renin, data = tab.D.4))
results.2.5B

# 2.5.C
# compare
results.2.5A
results.2.5B

# 2.5.D
# do NOT compute a regression line for the mean values
# you ignore the variablity of the data points

# 2.6
tab.D.5 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-05.dat"), header = F, sep="")
tab.D.5 <- data.frame(U.Ca = tab.D.5$V1,
                        D.Ca = tab.D.5$V2,
                        G.fr= tab.D.5$V3,
                        U.Na = tab.D.5$V4,
                        D.p = tab.D.5$V5)
write.csv(tab.D.5, file = "tab.D.5.csv",
          row.names = F)

ggplot(tab.D.5, aes(x = D.Ca, y = U.Ca)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=F)    # add shaded confidence region
results.2.6.D.Ca <- summary(lm(formula = D.Ca ~ U.Ca, data = tab.D.5))
results.2.6.D.Ca$r.squared;
# FIXME: create Function
# p.value F-statistic:
1 - pf(results.2.6.D.Ca$fstatistic[[1]], results.2.6.D.Ca$fstatistic[[2]], results.2.6.D.Ca$fstatistic[[3]])

results.2.6.G.fr <- summary(lm(formula = G.fr ~ U.Ca, data = tab.D.5))
results.2.6.G.fr$r.squared;
# p.value F-statistic:
1 - pf(results.2.6.G.fr$fstatistic[[1]], results.2.6.G.fr$fstatistic[[2]], results.2.6.G.fr$fstatistic[[3]])

results.2.6.U.Na <- summary(lm(formula = U.Na ~ U.Ca, data = tab.D.5))
results.2.6.U.Na$r.squared;
# p.value F-statistic:
1 - pf(results.2.6.U.Na$fstatistic[[1]], results.2.6.U.Na$fstatistic[[2]], results.2.6.U.Na$fstatistic[[3]])

results.2.6.D.p <- summary(lm(formula = D.p ~ U.Ca, data = tab.D.5))
results.2.6.D.p$r.squared;
# p.value F-statistic:
1 - pf(results.2.6.D.p$fstatistic[[1]], results.2.6.D.p$fstatistic[[2]], results.2.6.D.p$fstatistic[[3]])

# 2.7
tab.D.6 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-06.dat"), header = F, sep="")
tab.D.6 <- data.frame(type.F = tab.D.6$V1,
                        T.1 = tab.D.6$V2,
                        T.2= tab.D.6$V3)
write.csv(tab.D.6, file = "tab.D.6.csv",
          row.names = F)

ggplot(tab.D.6, aes(x = T.1, y = type.F)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=F)    # add shaded confidence region
results.2.7 <- summary(lm(formula = type.F ~ T.1 , data = tab.D.6))
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

data.3.1 <- read.csv("tab.1.1.csv")
multiple.regression <- lm(formula = weight ~ height + water.consumption, data = data.3.1)
multiple.regression 
summary(multiple.regression)

library(latticeExtra)
cloud(weight ~ height + as.factor(water.consumption), data.3.1, panel.3d.cloud=panel.3dbars, col.facet='grey', 
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")))

# Fig 3-2
tab.1.1 <- read.csv("tab.1.1.csv")
multiple.regression <- lm(formula = weight ~ height + water.consumption, data = tab.1.1)
multiple.regression 
summary(multiple.regression)
# getting the p-value of the F-statistic:
f = summary(multiple.regression)$fstatistic
pf(f[1],f[2],f[3],lower.tail=F)
# getting residual mean square ( = square of the standard error of the estimate)
s.y.x <- (summary(multiple.regression)$sigma)^2
s.y.x
# determine confidence intervals:
confint(lm(formula = weight ~ height + water.consumption, data = tab.1.1))

# FIXME: Create function
## getting the incremental sum of squares:
#anova(lm(formula = weight ~ height + water.consumption, data = tab.1.1))
# used to test wether adding independent variables helps to predict the dependent variable
# F = (mean square associated with adding x2 (x1 is already in the equation)) / (mean square residual for equation containing x1 and x2)
test.3.2 <- anova(lm(formula = weight ~ height + water.consumption, data = tab.1.1))
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
tab.1.2 <- read.csv("tab.1.2.csv")
tab.1.2.m.reg <- lm(formula = weight ~ height + scd.hnd.smk, data = tab.1.2)
summary(tab.1.2.m.reg)





# Fig 3-9 and Fig. 3-10
tab.C.2 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/endotoxn.dat"), header = F, sep="")
tab.C.2 <- data.frame(P = tab.C.2$V1,
                        A = tab.C.2$V2,
                        E = tab.C.2$V3)
write.csv(tab.C.2, file = "tab.C.2.csv",
          row.names = F)

# Fig. 1-2 plotting weight against height from Table 1-1 with water_consumption as factor
ggplot(tab.C.2, aes(x=A, y=P, color=as.factor(E))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
C.2.m.reg <- lm(formula = P ~ A + E, data = tab.C.2)
C.2.m.reg 
summary(C.2.m.reg )


# Fig. 3-11
tab.C.3 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/leucine.dat"), header = F, sep="")
tab.C.3 <- data.frame(log.L = tab.C.3$V1,
                        log.W = tab.C.3$V2,
                        A = tab.C.3$V3)
write.csv(tab.C.3, file = "tab.C.3.csv",
          row.names = F)

# plotting with a simple linear regression 
ggplot(tab.C.3, aes(x = log.W, y = log.L)) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

C.3.reg <- lm(formula = log.L ~ log.W, data = tab.C.3)
C.3.reg 
summary(C.3.reg)

ggplot(tab.C.3, aes(x = log.W, y = log.L, color = as.factor(A) )) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

C.3.m.reg <- lm(formula = log.L ~ log.W + A, data = tab.C.3)
C.3.m.reg 
summary(C.3.m.reg )


# Fig. 3-13

tab.C.4 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/diabetes.dat"), header = F, sep="")
tab.C.4 <- data.frame(H = tab.C.4$V1,
                        B = tab.C.4$V2,
                        D = tab.C.4$V3,
                        S = tab.C.4$V4,
                        A = tab.C.4$V5,
                        W = tab.C.4$V6,
                        T = tab.C.4$V7,
                        C = tab.C.4$V8,
                        G = tab.C.4$V9)
write.csv(tab.C.4, file = "tab.C.4.csv",
          row.names = F)

C.4.mreg <- lm(formula = H ~ B + D + S + A + W + T + C + G, data = tab.C.4)
summary(C.4.mreg)

# determine confidence intervals:
confint(lm(formula = H ~ B + D + S + A + W + T + C + G, data = tab.C.4))

# Fig. 3-15
tab.C.5 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/babybird.dat"), header = F, sep="")
tab.C.5 <- data.frame(V = tab.C.5$V1,
                        O = tab.C.5$V2,
                        C = tab.C.5$V3)
write.csv(tab.C.5, file = "tab.C.5.csv",
          row.names = F)

C.5.mreg <- lm(formula = V ~ O + C , data = tab.C.5)
summary(C.5.mreg)
# 31.1 % increase in ventilation with each 1 percent increase in conc(CO2)

# Fig 3-16
# Polynomal regression
tab.C.1.preg  <- read.csv("tab.C.1.csv")
# define polynomal function [y = b0  + b1*x + b2*x^2] within lm():
C.1.preg <- lm(formula = C.b ~ degree.C + I(degree.C^2), data = tab.C.1.preg)
summary(C.1.preg)

# plotting with polynomial function fit
fit.func <- function(x, b0, b1, b2)(b0  + b1*x + b2*x^2)
arguments <- list(b0 = C.1.preg$coefficients[[1]], 
                  b1 = C.1.preg$coefficients[[2]], 
                  b2 = C.1.preg$coefficients[[3]])

ggplot(tab.C.1.preg, aes(x = degree.C, y = C.b)) +
  geom_point(data =  tab.C.1.preg, aes(x = degree.C, y = C.b)) +
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

tab.C.6 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/saltbact.dat"), header = F, sep="")
tab.C.6 <- data.frame(T.H2O = tab.C.6$V1,
                        time = tab.C.6$V2,
                        NaCl = tab.C.6$V3)
write.csv(tab.C.6, file = "tab.C.6.csv",
          row.names = F)

C.6.mreg <- lm(formula = T.H2O ~ time + NaCl + I(time*NaCl), data = tab.C.6)
summary(C.6.mreg)
C.6.mreg.no.IA <- lm(formula = T.H2O ~ time + NaCl, data = tab.C.6)
summary(C.6.mreg.no.IA)

ggplot(tab.C.6, aes(x=time , y=T.H2O, color=as.factor(NaCl))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines


# Fig 3-22
tab.C.7 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/muscle.dat"), header = F, sep="")
tab.C.7 <- data.frame(delta.F = tab.C.7$V1,
                        L = tab.C.7$V2,
                        F = tab.C.7$V3)
write.csv(tab.C.7, file = "tab.C.7.csv",
          row.names = F)
# non-linear model and interaction between variables:
C.7.mreg <- lm(formula = delta.F ~ L + I(L^2) + I(L*F), data = tab.C.7)
summary(C.7.mreg)
# L <=> delta L; I(L^2) <=> delta L squared; I(L*F) <=> delta L * F [quantifies how the relationship changes in response to changes in developed force F]

ggplot(tab.C.7, aes(x=L , y=delta.F, color=as.factor(F))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

##
# P-3.1
# formula = weight ~ height + scd.hnd.smk  + heigth*scd.hnd.smk 
# solve with scd.hnd.smk = 1 and scd.hnd.smk = 0

# P-3.2
tab.1.2 <- read.csv("tab.1.2.csv")
tab.1.2.m.reg <- lm(formula = weight ~ height + scd.hnd.smk, data = tab.1.2)
summary(tab.1.2.m.reg)

ggplot(tab.1.2, aes(x=height , y=weight, color=as.factor(scd.hnd.smk))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

# here the 2D representation was chosen instead
# the slope of the regression plane of scd.hnd.smk is a boolean variable
# ==> there are no intermediate values
# but you can see the significance of differences between the two conditions, the parallel shift of the regression line

# P-3.3
tab.1.1.mod <- read.csv("tab.1.1.csv")
tab.1.1.mod$water.consumption <- c(2,4,6,10,8,12,18,14,16,20,22,24)
tab.1.1.mod.mreg <- lm(formula = weight ~ height + water.consumption, data = tab.1.1.mod)
summary(tab.1.1.mod.mreg)
## Errata from book:
#Problem 3.3
#The answer (p. 888) is incorrect.
#The incorrect answer prints the regression equation as 
## W = -15.9 + 0.759*H - 0.252*W
#whereas the correct regression equation is as computet!

# P-3-4
# "ROOT MSE" = 0.3087879

# P-3-5
tab.D.7 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-07.dat"), header = F, sep="")
tab.D.7 <- data.frame(year = tab.D.7$V1,
                        Adv.Rev = tab.D.7$V2,
                        Smk.Rel.CSM = tab.D.7$V3,
                        Smk.Rel.ApY = tab.D.7$V4)
write.csv(tab.D.7, file = "tab.D.7.csv",
          row.names = F)

# non-linear model and interaction between variables:
D.7.mreg <- lm(formula =  Smk.Rel.ApY ~ year + Adv.Rev  + Smk.Rel.CSM, data = tab.D.7)
summary(D.7.mreg)
# L <=> delta L; I(L^2) <=> deSmk.Rel.ApYlta L squared; I(L*F) <=> delta L * F [quantifies how the relationship changes in response to changes in developed force F]

# P-3-6
tab.C.5 <- read.csv("tab.C.5.csv")
C.5.mreg <- lm(formula = V ~ O + C , data = tab.C.5)
results.C.5.mreg <- summary(C.5.mreg)

# R²; indep. variables account for only 27% of total variation observed in V:
results.C.5.mreg$r.squared
#   F statistic for overall goodness of fit; regression equation provides a better prediction of the obersvations than the mean value of V
results.C.5.mreg$fstatistic[[1]]
# high uncertainty for values of dep. variable for combinations fo indep. variables; (157%)
results.C.5.mreg$sigma


# P-3-7

tab.D.8 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-08.dat"), header = F, sep="")
tab.D.8 <- data.frame(P = tab.D.8$V1,
                        N = tab.D.8$V2,
                        E.group = tab.D.8$V3,
                        D.group = tab.D.8$V4)
write.csv(tab.D.8, file = "tab.D.8.csv",
          row.names = F)

# Table of the data of Sato et al.
Xamoterol <- c(0,0,0,0,1,1,1,1)
E.group <- c(1,2,3,4,1,2,3,4)
P.mean <- c(125,138,150,162,135,146,158,161)
P.sd <- c(22,25,30,31,30,27,30,36)
N.mean <- c(234,369,543,754,211,427,766,1068)
N.sd <- c(87,165,205,279,106,257,355,551)
n <- c(6,6,6,6,10,10,10,10)
tab.D.8.Sato <- data.frame(P.mean, P.sd, N.mean, N.sd, n, E.group, D.group = Xamoterol)

ggplot(tab.D.8.Sato, aes(x=N.mean, y=P.mean, color=as.factor(D.group) )) +
  geom_point(shape=1) + 
  # Errorbar
  geom_errorbar(aes(
    ymin=tab.D.8.Sato$P.mean - tab.D.8.Sato$P.sd, 
    ymax=tab.D.8.Sato$P.mean + tab.D.8.Sato$P.sd,
     width=.2)) +
  geom_errorbarh(aes(
    xmin=tab.D.8.Sato$N.mean - tab.D.8.Sato$N.sd, 
    xmax=tab.D.8.Sato$N.mean + tab.D.8.Sato$N.sd),
    width=.2) +
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, # Don't add shaded confidence region
              fullrange=TRUE)    # extend line

D.8.Sato.lm <- summary(lm(formula = P.mean ~ N.mean + D.group , data = tab.D.8.Sato))
D.8.Sato.lm 

tab.D.8.Sato.D0 <- subset(tab.D.8.Sato, D.group == 0)
D.8.Sato.lm.D0 <- summary(lm(formula = P.mean ~ N.mean, data = tab.D.8.Sato.D0))
tab.D.8.Sato.D1 <- subset(tab.D.8.Sato, D.group == 1)
D.8.Sato.lm.D1 <- summary(lm(formula = P.mean ~ N.mean, data = tab.D.8.Sato.D1))

# FIXME: Function for comparison of two regression lines
# Intersection of the two regression lines with D.group = 0 and 1
# 131.2 + 0.03049x = 110.4 + 0.07013x
# switch from stimulant -> blocker:
#  524.7225 pg/mL
## compare Slopes and Intercepts of the two regression lines 
# t = (difference of regression slopes) / (standard error of difference of regression slopes)
# t = (b1 - b2) / (sqrt(S²b1 + S²b2) )
# with v = n1 + n2 - 4 df
t.slope <- (D.8.Sato.lm.D0$coefficients[[2]] - D.8.Sato.lm.D1$coefficients[[2]]) / (sqrt(((D.8.Sato.lm.D0$coefficients[[4]])^2 + (D.8.Sato.lm.D1$coefficients[[4]])^2)))
2*pt(abs(t.slope), df = 4, lower= FALSE) # two-tailed t-test

t.intercept <- (D.8.Sato.lm.D0$coefficients[[1]] - D.8.Sato.lm.D1$coefficients[[1]]) / (sqrt(((D.8.Sato.lm.D0$coefficients[[3]])^2 + (D.8.Sato.lm.D1$coefficients[[3]])^2)))
2*pt(abs(t.intercept), df = 4, lower= FALSE) # two-tailed t-test

# B
D.8.mreg <- lm(formula = P ~ N + D.group , data = tab.D.8)
summary(D.8.mreg)

tab.D.8.D0 <- subset(tab.D.8, D.group == 0)
D.8.lm.D0 <- summary(lm(formula = P ~ N, data = tab.D.8.D0))
tab.D.8.D1 <- subset(tab.D.8, D.group == 1)
D.8.lm.D1 <- summary(lm(formula = P ~ N, data = tab.D.8.D1))

# t-test for difference in slope
t.slope <- (D.8.lm.D0$coefficients[[2]] - D.8.lm.D1$coefficients[[2]]) / (sqrt(((D.8.lm.D0$coefficients[[4]])^2 + (D.8.lm.D1$coefficients[[4]])^2)))
2*pt(abs(t.slope), df = (length(tab.D.8.D0$D.group) + length(tab.D.8.D1$D.group) -4), lower= FALSE) # two-tailed t-test

t.intercept <- (D.8.lm.D0$coefficients[[1]] - D.8.lm.D1$coefficients[[1]]) / (sqrt(((D.8.lm.D0$coefficients[[3]])^2 + (D.8.lm.D1$coefficients[[3]])^2)))
2*pt(abs(t.intercept), df = (length(tab.D.8.D0$D.group) + length(tab.D.8.D1$D.group) -4), lower= FALSE) # two-tailed t-test

## differences in the raw numbers seem to be due to differences in rounding


# P-3.8
tab.D.5 <- read.csv("tab.D.5.csv")
D.5.mreg <- summary(lm(formula = U.Ca ~ D.Ca + D.p + U.Na + G.fr , data = tab.D.5))
D.5.mreg

# P-3.9
tab.D.9 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-09.dat"), header = F, sep="")
tab.D.9 <- data.frame(L.uptake = tab.D.9$V1,
                      time = tab.D.9$V2,
                      Cholesterol = tab.D.9$V3)
write.csv(tab.D.9, file = "tab.D.9.csv",
          row.names = F)

ggplot(tab.D.9, aes(x=time , y=L.uptake, color=as.factor(Cholesterol))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

D.9.lm <- summary(lm(formula = L.uptake ~  time + Cholesterol , data = tab.D.9))
D.9.lm
# as the relationships don't seem to be linear but rather quadric:
D.9.quadric <- summary(lm(formula = L.uptake ~ -1 + time + I(time^2) + I(time*Cholesterol) + I(time^2*Cholesterol) , data = tab.D.9)) # the -1 forces the intercept to be 0r
D.9.quadric

# P-3-10

tab.D.10 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-10.dat"), header = F, sep="")
tab.D.10 <- data.frame(R = tab.D.10$V1,
                      B = tab.D.10$V2,
                      C.Code = tab.D.10$V3)
write.csv(tab.D.10, file = "tab.D.10.csv",
          row.names = F)

ggplot(tab.D.10, aes(x=B , y=R, color=as.factor(C.Code))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

D.10.lm <- summary(lm(formula = R ~  B + C.Code + I(B*C.Code) , data = tab.D.10))
D.10.lm

# testing whether the combined regression is sig. different from 1:
D.10.lm.B <- summary(lm(formula = R ~  B , data = tab.D.10))
D.10.lm.B

# P-3-11
tab.D.3 <- read.csv("tab.D.3.csv")
# Dose 0: 1-4 h intervals; 1: 6-12 h intervals
ggplot(tab.D.3, aes(x=time.a.MIC , y=CFU, color=as.factor(Dose))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
D.3.lm <- summary(lm(formula = CFU ~  time.a.MIC + Dose + I(time.a.MIC*Dose), data = tab.D.3))
# I(time.a.MIC*Dose) := relationship between time.a.MIC and dummy variable "Dose"
D.3.lm
# shows differences in the Intercepts ("(Intercept)" and "Dose") and slopes ("time.a.MIC","I(time.a.MIC*Dose)") 
# => both groups of data are different

# P-3-12
tab.D.6 <- read.csv("tab.D.6.csv")

D.6.lm <- summary(lm(formula = type.F ~ T.1 + T.2, data = tab.D.6))
# I(time.a.MIC*Dose) := relationship between time.a.MIC and dummy variable "Dose"
D.6.lm


# P-3-13
tab.D.11 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-11.dat"), header = F, sep="")
tab.D.11 <- data.frame(S = tab.D.11$V1,
                      Delta.S = tab.D.11$V2,
                      Freq = tab.D.11$V3)
write.csv(tab.D.11, file = "tab.D.11.csv",
          row.names = F)

ggplot(tab.D.11, aes(x=Delta.S , y=S, color=as.factor(Freq))) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
# Interaction model as the slope of the 4 lines increases with F:
D.11.lm <- summary(lm(formula = S ~ Delta.S + Freq + I(Delta.S*Freq), data = tab.D.11))
# I(time.a.MIC*Dose) := relationship between time.a.MIC and dummy variable "Dose"
D.11.lm

# P-3-14
tab.D.12 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-12.dat"), header = F, sep="")
tab.D.12 <- data.frame(V = tab.D.12$V1,
                       O = tab.D.12$V2,
                       C = tab.D.12$V3)
write.csv(tab.D.12, file = "tab.D.12.csv",
          row.names = F)

D.12.lm <- summary(lm(formula = V ~ O + C , data = tab.D.12))
# I(time.a.MIC*Dose) := relationship between time.a.MIC and dummy variable "Dose"
D.12.lm

# P-3-15

# Baby birds:
tab.C.5 <- read.csv("tab.C.5.csv")
tab.D.12 <- read.csv("tab.D.12.csv")
# Adding dummy-variables and combine data sets
tab.C.5.D <- cbind(tab.C.5, D = rep(1,length(tab.C.5[,1]))) # dummy variable for baby birds = 1
tab.D.12.D <- cbind(tab.D.12, D = rep(0,length(tab.D.12[,1])))
tab.P.3.15 <- rbind(tab.C.5.D, tab.D.12.D)

# Interaction of D(bird type) with O and C
tab.P.3.15.lm <- summary(lm(formula = V ~ O + C + D + I(O*D) + I(C*D)  , data = tab.P.3.15))
tab.P.3.15.lm

#  D: Intercept of the adult plane
# I(O*D): Higher sensitiviy of the adults to oxygen compared to baby birds (by 7 Units)
# I(C*D): Higher sensitivity of the adults to carbon dioxide compared to baby birds (by 2.3 Units)
# !! compare with P-3-14 !!

# P-3-16
# compute 95%- confidence intervals to oxygen
# determine confidence intervals:
tab.D.12 <- read.csv("tab.D.12.csv")
confint(lm(formula = V ~ O, data = tab.D.12), level = 0.912) #  91.2 confint to get the results in the errata
# confidence interval includes 0 ! => cannot conclude that the parameter is different from zero!

# P-3-17

tab.D.2 <- read.csv("tab.D.2.csv")

ggplot(tab.D.2, aes(x=Lung.Cancer, y=Breast.Cancer)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

P.3.17.lm <- summary(lm(formula = Breast.Cancer ~  Animal.Fat + Lung.Cancer, data = tab.D.2))
P.3.17.lm
P.3.17.lm.interact <- summary(lm(formula = Breast.Cancer ~ Animal.Fat + Lung.Cancer  + I(Animal.Fat*Lung.Cancer) , data = tab.D.2))
P.3.17.lm.interact


# Chapter 4
# regression diagnostics
# 1. regression model
# 2. normality
# 3. equal variance
# 4. independence

# Table 4-1
tab.C.8A <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marsint1.dat"), header = F, sep="")
tab.C.8A  <- data.frame(Intelligence = tab.C.8A$V1,
                       Foot.Size = tab.C.8A$V2)
write.csv(tab.C.8A, file = "tab.C.8A.csv",
          row.names = F)
C.8A.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8A))
C.8A.lm

tab.C.8B <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marsint2.dat"), header = F, sep="")
tab.C.8B  <- data.frame(Intelligence = tab.C.8B$V1,
                        Foot.Size = tab.C.8B$V2)
write.csv(tab.C.8B, file = "tab.C.8B.csv",
          row.names = F)
C.8B.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8B))
C.8B.lm

tab.C.8C <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marsint3.dat"), header = F, sep="")
tab.C.8C  <- data.frame(Intelligence = tab.C.8C$V1,
                        Foot.Size = tab.C.8C$V2)
write.csv(tab.C.8C, file = "tab.C.8C.csv",
          row.names = F)
C.8C.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
C.8C.lm

tab.C.8D <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marsint4.dat"), header = F, sep="")
tab.C.8D  <- data.frame(Intelligence = tab.C.8D$V1,
                        Foot.Size = tab.C.8D$V2)
write.csv(tab.C.8D, file = "tab.C.8D.csv",
          row.names = F)
C.8D.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))
C.8D.lm

Fig.C.8A <- ggplot(tab.C.8A, aes(x=Foot.Size, y=Intelligence)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
# model misspecification:
Fig.C.8B <- ggplot(tab.C.8B, aes(x=Foot.Size, y=Intelligence)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
# outlier (levarage points):
Fig.C.8C <- ggplot(tab.C.8C, aes(x=Foot.Size, y=Intelligence)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
# extreme outlier (levarage points):
Fig.C.8D <- ggplot(tab.C.8D, aes(x=Foot.Size, y=Intelligence)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
require("gridExtra")
grid.arrange(Fig.C.8A, Fig.C.8B, Fig.C.8C, Fig.C.8D, ncol=2)
# Fig.4.2

# testing for problems: Looking at residuals
# 1. Assumption of constant variance: plots of the residuals as function of indep. or dep. variable
# 2. Normalize residuals to identify outliers

# plotting residuals:
tab.C.8A.res <- cbind(tab.C.8A, residuals = C.8A.lm$residuals)
Fig.C.8A.res <- ggplot(tab.C.8A.res, aes(x=Foot.Size, y=residuals)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
Fig.C.8A.res

tab.C.8B.res <- cbind(tab.C.8B, residuals = C.8B.lm$residuals)
Fig.C.8B.res <- ggplot(tab.C.8B.res, aes(x=Foot.Size, y=residuals)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
Fig.C.8B.res

tab.C.8C.res <- cbind(tab.C.8C, residuals = C.8C.lm$residuals)
Fig.C.8C.res <- ggplot(tab.C.8C.res, aes(x=Foot.Size, y=residuals)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
Fig.C.8C.res

tab.C.8D.res <- cbind(tab.C.8D, residuals = C.8D.lm$residuals)
Fig.C.8D.res <- ggplot(tab.C.8D.res, aes(x=Foot.Size, y=residuals)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
Fig.C.8D.res

require("gridExtra")
grid.arrange(Fig.C.8A.res, Fig.C.8B.res, Fig.C.8C.res, Fig.C.8D.res, ncol=2)


# Fig.4.7
data.4.7 <- read.csv("tab.1.1.csv")
fig.4.7.mreg <- summary(lm(formula = weight ~ height + water.consumption, data = data.4.7))

data.4.7.res <- cbind(data.4.7, residuals = fig.4.7.mreg$residuals)
fig.4.7.res.hght <- ggplot(data.4.7.res, aes(x=height, y=residuals)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
fig.4.7.res.hght

fig.4.7.res.wtr <- ggplot(data.4.7.res, aes(x=water.consumption, y=residuals)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
fig.4.7.res.wtr

fig.4.7.res.wgt <- ggplot(data.4.7.res, aes(x=weight, y=residuals)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
fig.4.7.res.wgt

# predicted weight
# y = b0 + x1*b1 +x2*b2
data.4.7.res <- cbind(data.4.7.res, pred.weight = fig.4.7.mreg$coefficients[[1]] + fig.4.7.mreg$coefficients[[2]]*data.4.7.res$height + fig.4.7.mreg$coefficients[[3]]*data.4.7.res$water.consumption)

fig.4.7.res.prdwgt <- ggplot(data.4.7.res, aes(x=pred.weight, y=residuals)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
fig.4.7.res.prdwgt

# raw residuals => identify outliers; Problem: values depend on scale and units
# residuals:
resid(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))

# standardized residuals => normalize  raw residuals by the standard error of the estimate
#   standard residual 1: residual is 1 standard deviation off the regression plane
#   normal distribution: ~66% fall into 1 standard deviation of the mean, ~95% fall within 2 standard deviations
#   points above 3 are likely outliers
tab.C.8C <- read.csv("tab.C.8C.csv")
C.8C.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
# computing standardized residuals
# FIXME check for built-in-functions and/or create function
# Problem: both deviate from the solution 2.76 for the 3. data-point
C.8C.lm.stand.res <- C.8C.lm$residuals/C.8C.lm$sigma
# summary(tab.C.10.lm.pw)$sigma  # getting sigma if you do:
# summary(C.8C.lm <- lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
#
# built-in function: rstandard()
# rstandard(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
# uses a different formula with the leverage included!
# e_1 / (s_e * sqrt(1-leverage_1))

# check for outliers
boxplot(C.8C.lm.stand.res)
summary(C.8C.lm.stand.res > 2)

# compare with C.8D
tab.C.8D <- read.csv("tab.C.8D.csv")
C.8D.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))
# computing standardized residuals
# FIXME check for built-in-functions and/or create function
# Problem: both deviate from the solution 2.76 for the 3. data-point
C.8D.lm.stand.res <- C.8D.lm$residuals/C.8D.lm$sigma
# built-in function: rstandard()
# rstandard(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))
# uses a different formula with the leverage included!
# e_1 / (s_e * sqrt(1-leverage_1))

# check for outliers
boxplot(C.8D.lm.stand.res)
summary(C.8D.lm.stand.res > 2)
# no outliers in the standardized residuals! But compare with the graph above!

##
# Test for Normality of the Residuals
# 1. plot frequency distribution of residuals
# 2. normal probability plot of the residuals
#   plot of the cumulative freq. of the distribution of the resuduals vs. the residuals on a special scale
#   produces a straight line if distr. is normal

tab.C.8A <- read.csv("tab.C.8A.csv")
C.8A.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8A))
C.8.A.lm.residuals <- data.frame(residuals = C.8A.lm$residuals)
ggplot(C.8.A.lm.residuals, aes(x=residuals)) + 
  geom_dotplot(binwidth = 0.3) # plotting discreet values
# Problem: You get only consistent plots if you set the binwidth to certain cut-offs

tab.C.8B <- read.csv("tab.C.8B.csv")
C.8A.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8B))
C.8.A.lm.residuals <- data.frame(residuals = C.8A.lm$residuals)
ggplot(C.8.A.lm.residuals, aes(x=residuals)) + 
  geom_dotplot(binwidth = 0.3) # plotting discreet values

tab.C.8C <- read.csv("tab.C.8C.csv")
C.8A.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
C.8.A.lm.residuals <- data.frame(residuals = C.8A.lm$residuals)
ggplot(C.8.A.lm.residuals, aes(x=residuals)) + 
  geom_dotplot(binwidth = 0.3) # plotting discreet values

tab.C.8D <- read.csv("tab.C.8D.csv")
C.8A.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))
C.8.A.lm.residuals <- data.frame(residuals = C.8A.lm$residuals)
ggplot(C.8.A.lm.residuals, aes(x=residuals)) + 
  geom_dotplot(binwidth = 0.3) # plotting discreet values

# normal probability plots
tab.C.8A <- read.csv("tab.C.8A.csv")
C.8A.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8A))
res.ordered <- sort(C.8A.lm$residuals)
rank.res <- c(1:length(C.8A.lm$residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(C.8A.lm$residuals) # cumulative frequency
C.8A.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
C.8A.norm.plot <- ggplot(C.8A.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
C.8A.norm.plot

tab.C.8B <- read.csv("tab.C.8B.csv")
C.8B.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8B))
res.ordered <- sort(C.8B.lm$residuals)
rank.res <- c(1:length(C.8B.lm$residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(C.8B.lm$residuals) # cumulative frequency
C.8B.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
C.8B.norm.plot <- ggplot(C.8B.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
C.8B.norm.plot

tab.C.8C <- read.csv("tab.C.8C.csv")
C.8C.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
res.ordered <- sort(C.8C.lm$residuals)
rank.res <- c(1:length(C.8C.lm$residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(C.8C.lm$residuals) # cumulative frequency
C.8C.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
C.8C.norm.plot <- ggplot(C.8C.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
C.8C.norm.plot

tab.C.8D <- read.csv("tab.C.8D.csv")
C.8D.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))
res.ordered <- sort(C.8D.lm$residuals)
rank.res <- c(1:length(C.8D.lm$residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(C.8D.lm$residuals) # cumulative frequency
C.8D.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
C.8D.norm.plot <- ggplot(C.8D.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
C.8D.norm.plot

require("gridExtra")
# fig 4-10
grid.arrange(C.8A.norm.plot,C.8B.norm.plot,C.8C.norm.plot,C.8D.norm.plot, ncol=2)

# leverage (h_ij)
# quantifies how much the observed value (of a dependent variable) affects the estimated value (of the dependent variable)
# leverage only indicates the potential for influence!
# the expected (avarage) value of the leverage is:
#   (k+1)/n 
#     (k beeing the number of independent variable in the regression equation)
# if h_ij > 2(k+1)/n then this is considered a high leverage

tab.C.8D <- read.csv("tab.C.8D.csv")
Fig.C.8D <- ggplot(tab.C.8D, aes(x=Foot.Size, y=Intelligence)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
C.8D.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))

# computing the leverage and expected leverage
tab.C.8D$leverage <- hatvalues(tab.C.8D.lm) # function to compute leverage
no.of.var <- 1 # here: ONE independent variable
tab.C.8D$leverage.expected <- (no.of.var + 1)/(length(tab.C.8D$D))
tab.C.8D$leverage > 2*tab.C.8D$leverage.expected # exceeds the leverage twice the expected value? If so, this point needs special attention.

# Studentized residuals
# "refined" normalization of the residuals -> standardized residuals taking into account the specific standard error; "internally Studentized residual", using all the data ; takes the leverage into account; see above "standardized residuals"
# alternatively: "externally Studentized residual" or "Studentized deleted residual"; s_y|x is computed after deleting the point associated with the residual; if it is an outlier the variance  will change if not there is not much an effect on the variance

tab.C.8A <- read.csv("tab.C.8A.csv")
C.8A.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8A))
C.8A.lm.stud.res <- rstandard(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8A))
# e_1 / (s_e * sqrt(1-leverage_1))
C.8A.lm.stud.del.res <- rstudent(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8A))
# gives the Studentized deleted residuals

tab.C.8C <- read.csv("tab.C.8C.csv")
C.8C.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
C.8C.lm.stud.res <- rstandard(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
# e_1 / (s_e * sqrt(1-leverage_1))
C.8C.lm.stud.del.res <- rstudent(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
# gives the Studentized deleted residuals

tab.C.8D <- read.csv("tab.C.8D.csv")
C.8D.lm <- summary(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))
C.8D.lm.stud.res <- rstandard(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))
# e_1 / (s_e * sqrt(1-leverage_1))
C.8D.lm.stud.del.res <- rstudent(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))
# gives the Studentized deleted residuals


# Cook's distance
# assesment of the actual influence of a data point on the regression equation (by deleting it)

tab.C.8A <- read.csv("tab.C.8A.csv")
summary(C.8A.lm <-lm(formula = Intelligence ~  Foot.Size, data = tab.C.8A))
cooks.distance(C.8A.lm)
# if the cook's distance is a big number, it indicates that the corresponding point has a major effect on the regression coefficients
# it depends on the internally studentized residuals and the leverage
# tends to follow a F-distribution with numerators: k+1 and denominator n-k-1 degrees of freedom
# points > 1  are worth for further investigation
# points > 4 are potentially serious outliers.

# FIXME: function to get all components of the regression equation computed if all points were excluded one by one
# FIXME: Plot of the regression intercept vs regression slopes of the corresponding regression equations

tab.C.8C <- read.csv("tab.C.8C.csv")
summary(C.8C.lm <-lm(formula = Intelligence ~  Foot.Size, data = tab.C.8C))
cooks.distance(C.8C.lm)

tab.C.8D <- read.csv("tab.C.8D.csv")
summary(C.8D.lm <-lm(formula = Intelligence ~  Foot.Size, data = tab.C.8D))
cooks.distance(C.8D.lm)

## other Diagnostics:
# PRESS residual
# DEFITS
# DFBETAS

## What to do with influential observations?
# I. Problems with the data:
# 1. check for data entry errors
tab.C.8E <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/marsint5.dat"), header = F, sep="")
tab.C.8E  <- data.frame(Intelligence = tab.C.8E$V1,
                        Foot.Size = tab.C.8E$V2)
write.csv(tab.C.8E, file = "tab.C.8E.csv",
          row.names = F)


# plotting raw data 
fig.4.14 <- ggplot(tab.C.8E, aes(x=Foot.Size, y=Intelligence)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
fig.4.14
# residual plot
summary(dat.4.14.lm <- lm(formula = Intelligence ~  Foot.Size, data = tab.C.8E))
# diagnostics
temp.fig.4.15 <- data.frame(Intelligence = dat.4.14.lm$model[[1]],  
                            Foot.Size = dat.4.14.lm$model[[2]], 
                            residuals = dat.4.14.lm$residuals,
                            stud.del.res = rstudent(lm(formula = Intelligence ~  Foot.Size, data = tab.C.8E )),# Studentized deleted residuals)
                            cooks.dist = cooks.distance(dat.4.14.lm))
fig.4.15 <- ggplot(temp.fig.4.15, aes(x=Foot.Size, y=residuals)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
fig.4.15

# 2. check for technical error
# describe and justify pot hoc changes

# II. Problems with the model
# 1. leaving out important variables
#   -> analysis of residuals
# 2. not taking into account nonlinearities or interactions
#   -> analysis of residuals

tab.1.1 <- read.csv("tab.1.1.csv")
# plot residuals of regression weight on height against water consumpition
summary(tab.1.1.lm <- lm(formula = weight ~ height, data = tab.1.1))

fig.4.16.data <- data.frame(residuals = tab.1.1.lm$residuals, water.consumption = tab.1.1$water.consumption)
fig.4.16 <- ggplot(fig.4.16.data, aes(x=water.consumption, y=residuals)) +
  geom_point(shape=1) + 
  geom_hline(y= 0, col = "darkgrey")
fig.4.16
# 1. no uniform distribution around zero if you don't include water.consumption in the regression equation
# 2. linear trend of water.consumption visible; it is an determinant of weight
# FIXME: add grafik with water.consumption in the regression equation (not presented in the book)

# non-linear trends
# plots of non-linear data with a linear model
# Fig.4.17
tab.C.8B <- read.csv("tab.C.8B.csv")
summary(tab.C.8B.lm <- lm(formula = Intelligence ~ Foot.Size, data = tab.C.8B))
tab.C.8B$residuals <- tab.C.8B.lm$residuals
Fig.4.17A <- ggplot(tab.C.8B, aes(x=Foot.Size, y=residuals)) +
  geom_point(shape=1) + 
  geom_hline(y= 0, col = "darkgrey")
Fig.4.17A
Fig.4.17B <- ggplot(tab.C.8B, aes(x=Intelligence, y=residuals)) +
  geom_point(shape=1) + 
  geom_hline(y= 0, col = "darkgrey")
Fig.4.17B

# Fig.4.18
tab.C.1 <- read.csv("tab.C.1.csv")
summary(tab.C.1.lm <- lm(formula = C.b ~ degree.C, data = tab.C.1))
tab.C.1$residuals <- tab.C.1.lm$residuals
Fig.4.18A <- ggplot(tab.C.1, aes(x=degree.C, y=residuals)) +
  geom_point(shape=1) + 
  geom_hline(y= 0, col = "darkgrey")
Fig.4.18A
Fig.4.18B <- ggplot(tab.C.1, aes(x=C.b, y=residuals)) +
  geom_point(shape=1) + 
  geom_hline(y= 0, col = "darkgrey")
Fig.4.18B

# Data transformations
# 1. nonlinear  patter in the residuals
#   tansformation to "flatten out" the residual plot
# 2. variance of residuals is not constant
#   random component of the measurement is a constant percentage error
#   -> residual variance increases with the dependent variable
#   "variance stablizing transformation" by Transformation of the dependent variale to make the resiuals more closely meet the assumption of constant variance
#   might introduce nonlinearities
#   transformations of the dependent variable introduce implicit weighting of different points into the process of estimating the parameters!

# Common transformations for linearization and/or Variance Stabilization
# Transformation        regression equation
# Y       vs. X^i       y = b0 + sum(b_i * X^i)     # X + poly(X,2,raw=TRUE); b0 + b1*X + b2*I(X^2)
# ln(Y)   vs. ln(X)     y = b0 * X^b1               # power function
# ln(Y)   vs. X         y = b0 * e^(b1*X)           # exponential; stabilization; Y => 0
# ln(Y)   vs. 1/X       y = b0 + e^(b1/X)           # inverse exponential
# 1/Y     vs. 1/X       y = x / (b0 + b1*X)         # hyperbola
# 1/Y     vs. X         y = 1 / (b0 + b1*X)         # stabilization
# Y       vs. 1/X       y = b0 + b1/X               #
# sqrt(Y) vs. X         y = (b0 + b1 * X)^2         # stabilization of count data

# # linear model
# summary(tab.D.4.lm <- lm(formula = RV.resistance ~ plasma.renin, data = tab.D.4))
# # quadric model:
# summary(tab.D.4.quad <- lm(formula =  RV.resistance ~ plasma.renin + I(plasma.renin^2), data = tab.D.4))



# Quadric functions 
# y = b0 + b1*X + b2*X^2
function1 <- function(x){-2*x^2 + 2*x + 2}
function2 <- function(x){ 2*x^2 + 2*x + 2}
ggplot(data.frame(x=c(0,0.75)), aes(x)) +
  stat_function(fun=function1, geom="line", aes(colour="b0,b1,b2>0")) +
  stat_function(fun=function2, geom="line", aes(colour="b0,b1>0; b2<0"))
  #scale_colour_manual("Function", value=c("blue","red"), breaks=c("square","exp"))

# logarithmic functions
# y = b0 + b1*log(x)
function3 <- function(x) {5 + -1*log(x)}
function4 <- function(x) {10 + 1*log(x)}
ggplot(data.frame(x=c(0,1)), aes(x)) +
  stat_function(fun = function3 , geom="line", aes(colour="b1 < 0")) +
  stat_function(fun = function4 , geom="line", aes(colour="b1 > 0")) 

# FIXME
# Add the other example equations with comments of the transformation type

tab.C.9 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/placenta.dat"), header = F, sep="")
tab.C.9 <- data.frame(D = tab.C.9$V1,
                      U = tab.C.9$V2)
write.csv(tab.C.9, file = "tab.C.9.csv",
          row.names = F)

ggplot(tab.C.9, aes(x=U, y=D)) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
summary(tab.C.9.lm <- lm(formula = D ~ U, data = tab.C.9))

#regression diagnostics of the linear model:
tab.C.9$raw.residuals <-tab.C.9.lm$residuals # raw residuals
tab.C.9$stud.del.res <- rstudent(tab.C.9.lm) # Studentized deleted residuals
tab.C.9$cooks.dist <-cooks.distance(tab.C.9.lm) # Cook's distance
tab.C.9$leverage <- hatvalues(tab.C.9.lm) # Leverage
#no.of.var <- 1 # here: ONE independent variable
#tab.C.9$leverage.expected <- (no.of.var + 1)/(length(tab.C.9$D))
#tab.C.9$leverage > 2*tab.C.9$leverage.expected # exceeds the leverage twice the expected value? If so, this point needs special attention.
# looking for outliers with a boxplot
boxplot(tab.C.9$stud.del.res)
boxplot(tab.C.9$leverage)
boxplot(tab.C.9$cooks.dist)
# => identifikation of an outlier

# plotting the raw residuals
fig.4.22.A <- ggplot(tab.C.9, aes(x=D, y=raw.residuals)) +
  geom_point(shape=1) + 
  geom_hline(y=0, col="darkgrey", size=2)
fig.4.22.B <- ggplot(tab.C.9, aes(x=U, y=raw.residuals)) +
  geom_point(shape=1) + 
  geom_hline(y=0, col="darkgrey", size=2)
require("gridExtra")
grid.arrange(fig.4.22.A, fig.4.22.B, ncol=2)
# => identifikation of an outlier by the pattern


# computing the regression line without the 23. data-point
tab.C.9 <- read.csv("tab.C.9.csv")
# remove 23. row
tab.C.9.w.o.23 <- tab.C.9[-c(23), ]

ggplot(tab.C.9.w.o.23, aes(x=U, y=D)) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
summary(tab.C.9.w.o.23.lm <- lm(formula = D ~ U, data = tab.C.9.w.o.23))

#regression diagnostics of the linear model:
tab.C.9.w.o.23$raw.residuals <-tab.C.9.w.o.23.lm$residuals # raw residuals
tab.C.9.w.o.23$stud.del.res <- rstudent(tab.C.9.w.o.23.lm) # Studentized deleted residuals
tab.C.9.w.o.23$cooks.dist <-cooks.distance(tab.C.9.w.o.23.lm) # Cook's distance
tab.C.9.w.o.23$leverage <- hatvalues(tab.C.9.w.o.23.lm) # Leverage
#no.of.var <- 1 # here: ONE independent variable
#tab.C.9.w.o.23$leverage.expected <- (no.of.var + 1)/(length(tab.C.9.w.o.23$D))
#tab.C.9.w.o.23$leverage > 2*tab.C.9.w.o.23$leverage.expected # exceeds the leverage twice the expected value? If so, this point needs special attention.
# looking for outliers with a boxplot
boxplot(tab.C.9.w.o.23$stud.del.res)
boxplot(tab.C.9.w.o.23$leverage)
boxplot(tab.C.9.w.o.23$cooks.dist)
# => identifikation of an outlier

# plotting the raw residuals
fig.4.22.A.w.o.23 <- ggplot(tab.C.9.w.o.23, aes(x=D, y=raw.residuals)) +
  geom_point(shape=1) + 
  geom_hline(y=0, col="darkgrey", size=2)
fig.4.22.B.w.o.23 <- ggplot(tab.C.9.w.o.23, aes(x=U, y=raw.residuals)) +
  geom_point(shape=1) + 
  geom_hline(y=0, col="darkgrey", size=2)
require("gridExtra")
grid.arrange(fig.4.22.A.w.o.23, fig.4.22.B.w.o.23, ncol=2)
# => identifikation of an outlier by the pattern

# computing the regression as a quadric function
tab.C.9.quad <- read.csv("tab.C.9.csv")
summary(tab.C.9.quad.lm <- lm(formula = D ~ U + I(U^2), data = tab.C.9.quad))
# plotting quadric function
fit.func <- function(x, b0, b1, b2)(b0  + b1*x + b2*x^2)
arguments <- list(b0 = tab.C.9.quad.lm$coefficients[[1]], 
                  b1 = tab.C.9.quad.lm$coefficients[[2]], 
                  b2 = tab.C.9.quad.lm$coefficients[[3]])
ggplot(tab.C.9.quad, aes(x = U, y = D)) +
   geom_point(data =  tab.C.9.quad, aes(x = U, y = D)) +
   stat_function(fun = fit.func, args = arguments, col="blue") # plot function with corresponding parameters
 
#regression diagnostics of the linear model:
tab.C.9.quad$raw.residuals <-tab.C.9.quad.lm$residuals # raw residuals
tab.C.9.quad$stud.del.res <- rstudent(tab.C.9.quad.lm) # Studentized deleted residuals
tab.C.9.quad$cooks.dist <-cooks.distance(tab.C.9.quad.lm) # Cook's distance
tab.C.9.quad$leverage <- hatvalues(tab.C.9.quad.lm) # Leverage
#no.of.var <- 1 # here: ONE independent variable
#tab.C.9.quad$leverage.expected <- (no.of.var + 1)/(length(tab.C.9.quad$D))
#tab.C.9.quad$leverage > 2*tab.C.9.quad$leverage.expected # exceeds the leverage twice the expected value? If so, this point needs special attention.
# looking for outliers with a boxplot
boxplot(tab.C.9.quad$stud.del.res)
boxplot(tab.C.9.quad$leverage)
boxplot(tab.C.9.quad$cooks.dist)
# => identifikation of an outlier

# plotting the raw residuals
fig.4.22.A.quad <- ggplot(tab.C.9.quad, aes(x=D, y=raw.residuals)) +
  geom_point(shape=1) + 
  geom_hline(y=0, col="darkgrey", size=2)
fig.4.22.B.quad <- ggplot(tab.C.9.quad, aes(x=U, y=raw.residuals)) +
  geom_point(shape=1) + 
  geom_hline(y=0, col="darkgrey", size=2)
require("gridExtra")
grid.arrange(fig.4.22.A.quad, fig.4.22.B.quad, ncol=2)
# => identifikation of an outlier by the pattern

## comparison of quadric to linear fit
#   better goodness of fit (R^2)
#   better residual distribution
# of the quadric fit
## Problem: data suggests a nonlinear relationship based on one extreme point with a large gap in the data
## => collect more data


tab.C.10 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/chicken.dat"), header = F, sep="")
tab.C.10 <- data.frame(R = tab.C.10$V1,
                       M = tab.C.10$V2)
write.csv(tab.C.10, file = "tab.C.10.csv",
          row.names = F)

summary(tab.C.10.lm <- lm(formula = R ~ M, data = tab.C.10))
ggplot(tab.C.10, aes(x=M, y=R)) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
# variation of R increases with increasing M
#regression diagnostics of the linear model:
tab.C.10$raw.residuals <-tab.C.10.lm$residuals # raw residuals
tab.C.10$stand.residuals <- tab.C.10.lm$residuals/summary(tab.C.10.lm)$sigma  # standardized residuals
tab.C.10$stud.del.res <- rstudent(tab.C.10.lm) # Studentized deleted residuals
tab.C.10$cooks.dist <-cooks.distance(tab.C.10.lm) # Cook's distance
tab.C.10$leverage <- hatvalues(tab.C.10.lm) # Leverage

# Analysis of residuals
fig.4.27.A <- ggplot(tab.C.10, aes(x=M, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
# "megaphone" shape -> constant variance assumption is violated

# normal probability plot
res.ordered <- sort(tab.C.10$stand.residuals)
rank.res <- c(1:length(tab.C.10$stand.residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(tab.C.10$stand.residuals) # cumulative frequency
tab.C.10.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
fig.4.27.B <- ggplot(tab.C.10.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
# S-shape indicate that residuals are not normally distributed (straight line expected)

require("gridExtra")
grid.arrange(fig.4.27.A, fig.4.27.B, ncol=2)

# Solution: transformation of the data to stabilize the variance
# sqrt(R) vs. M
# 1/R     vs. M
# ln(R)   vs. M
# ln(R)   vs. ln(M)

tab.C.10 <- read.csv("tab.C.10.csv")
tab.C.10.var <- data.frame(M = tab.C.10$M,
                           sqrt.R = sqrt(tab.C.10$R),
                           inverse.R = (1/tab.C.10$R),
                           ln.R = log(tab.C.10$R),
                           ln.M = log(tab.C.10$M))
fig.4.28.A <- ggplot(tab.C.10.var , aes(x=M, y=sqrt.R)) +
  geom_point(shape=1) # sqrt(R) vs. M  
fig.4.28.B <- ggplot(tab.C.10.var , aes(x=M, y=ln.R)) +
  geom_point(shape=1) # ln(R) vs. M   
fig.4.28.C <- ggplot(tab.C.10.var , aes(x=M, y=inverse.R)) +
  geom_point(shape=1) # 1/R vs. M  
fig.4.28.D <- ggplot(tab.C.10.var , aes(x=ln.M, y=ln.R)) +
  geom_point(shape=1) # ln(R) vs. ln(M)  
require("gridExtra")
grid.arrange(fig.4.28.A, fig.4.28.B, fig.4.28.C, fig.4.28.D, ncol=2)


tab.C.10.pw <- read.csv("tab.C.10.csv")
summary(tab.C.10.lm.pw <- lm(formula = log(R) ~ log(M), data = tab.C.10.pw))
# Results:
# log(R) = 3.95973 + 0.99697*log(M)
# exp(log(R)) = exp(3.95973 + 0.99697*log(M)) = exp(3.95973) + exp(0.99697*log(M))
# R' = 52*M^0.99607

# diagnostics of the  model:
tab.C.10.pw$raw.residuals <-tab.C.10.lm.pw$residuals # raw residuals
tab.C.10.pw$stand.residuals <- tab.C.10.lm.pw$residuals/summary(tab.C.10.lm.pw)$sigma  # standardized residuals
tab.C.10.pw$stud.del.res <- rstudent(tab.C.10.lm.pw) # Studentized deleted residuals
tab.C.10.pw$cooks.dist <-cooks.distance(tab.C.10.lm.pw) # Cook's distance
tab.C.10.pw$leverage <- hatvalues(tab.C.10.lm.pw) # Leverage
 
# Analysis of residuals
fig.4.30.A <- ggplot(tab.C.10.pw, aes(x=log(M), y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)

# normal probability plot
res.ordered <- sort(tab.C.10.pw$stand.residuals)
rank.res <- c(1:length(tab.C.10.pw$stand.residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(tab.C.10.pw$stand.residuals) # cumulative frequency
tab.C.10.pw.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
fig.4.27.B <- ggplot(tab.C.10.pw.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
# S-shape indicate that residuals are not normally distributed (straight line expected)

# ToDo: make a collection of functions for quality control


tab.C.11 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/examples/copzinc.dat"), header = F, sep="")
tab.C.11 <- data.frame(M = tab.C.11$V1,
                       Cu = tab.C.11$V2,
                       Zn = tab.C.11$V3)
write.csv(tab.C.11, file = "tab.C.11.csv",
          row.names = F)

# linear model:
summary(tab.C.11.lm <- lm(formula = M ~ Cu + Zn, data = tab.C.11))
# Because of the poor fit (R^2 = 0.3)  
# checking for outliers:
# diagnostics of the  model:
tab.C.11.lm.res <- tab.C.11
tab.C.11.lm.res$raw.residuals <- tab.C.11.lm$residuals # raw residuals
tab.C.11.lm.res$stand.residuals <- tab.C.11.lm$residuals/summary(tab.C.11.lm)$sigma  # standardized residuals; points > 2 are potential outliers
tab.C.11.lm.res$stud.res <- rstandard(tab.C.11.lm) # rstandard() gives "internally Studentized residual"; points > 2 are potential outliers
tab.C.11.lm.res$stud.del.res <- rstudent(tab.C.11.lm) # Studentized deleted residuals; points > 2 are potential outliers
tab.C.11.lm.res$cooks.dist <- cooks.distance(tab.C.11.lm) # Cook's distance; > 1 [investigate], > 4 [potentially serious outlier]; effect of a single data point on the regression
tab.C.11.lm.res$leverage <- hatvalues(tab.C.11.lm) # Leverage; investigate if > 2*(k+1)/n [k:= no. independent variables; n:= no. of data points]; effect of a single data point on the regression

# normal probability plot: Fig 4-32
res.ordered <- sort(tab.C.11.lm.res$stand.residuals)
rank.res <- c(1:length(tab.C.11.lm.res$stand.residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(tab.C.11.lm.res$stand.residuals) # cumulative frequency
tab.C.11.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
fig.4.32 <- ggplot(tab.C.11.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
# residuals are NOT normally distributed

# plot of raw residuals
# correct model?
fig.4.33.A <- ggplot(tab.C.11.lm.res, aes(x=Cu, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)

fig.4.33.B <- ggplot(tab.C.11.lm.res, aes(x=Zn, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)
# systematic nonlinear trend for Zn -> quadric?

# quadric model:
summary(tab.C.11.lm <- lm(formula = M ~ Cu + Zn + I(Zn^2), data = tab.C.11))
# better R^2; better residual standard error compared with linear model

# compare residuals with those of the linear model
# diagnostics of the  model:
tab.C.11.quad.res <- tab.C.11
tab.C.11.quad.res$raw.residuals <- tab.C.11.lm$residuals # raw residuals
tab.C.11.quad.res$stand.residuals <- tab.C.11.lm$residuals/summary(tab.C.11.lm)$sigma  # standardized residuals; points > 2 are potential outliers
tab.C.11.quad.res$stud.res <- rstandard(tab.C.11.lm) # rstandard() gives "internally Studentized residual"; points > 2 are potential outliers
tab.C.11.quad.res$stud.del.res <- rstudent(tab.C.11.lm) # Studentized deleted residuals; points > 2 are potential outliers
tab.C.11.quad.res$cooks.dist <- cooks.distance(tab.C.11.lm) # Cook's distance; > 1 [investigate], > 4 [potentially serious outlier]
tab.C.11.quad.res$leverage <- hatvalues(tab.C.11.lm) # Leverage; investigate if > 2*(k+1)/n [k:= no. independent variables; n:= no. of data points]

# normal probability plot: Fig 4-32
res.ordered <- sort(tab.C.11.quad.res$stand.residuals)
rank.res <- c(1:length(tab.C.11.quad.res$stand.residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(tab.C.11.quad.res$stand.residuals) # cumulative frequency
tab.C.11.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
fig.4.35 <- ggplot(tab.C.11.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

# plot of raw residuals
fig.4.36.A <- ggplot(tab.C.11.quad.res, aes(x=Cu, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)

fig.4.36.B <- ggplot(tab.C.11.quad.res, aes(x=Zn, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)

# formula = M ~ Cu + Zn + I(Zn^2)
# independence of Cu to Zn and vice versa
# Cu linear
# Zn nonlinear

# looking at the raw data:
fig.4.38.A <- ggplot(tab.C.11, aes(x=Cu, y=M)) +
  geom_point(shape=1)

fig.4.38.B <- ggplot(tab.C.11, aes(x=Zn, y=M)) +
  geom_point(shape=1)
# saturation

# looking at the Cu-data for each level of Zn
ggplot(tab.C.11, aes(x = Cu, y = M, colour=factor(Zn))) + 
  geom_point() + 
  facet_grid(. ~ Zn)+
  stat_smooth(method = "loess") 
# transformations for saturable functions should be suitable!

# transformations for saturable functions:
# 1/y vs. 1/x
# ln(y) vs. ln(x)
# ln(y) vs. 1/x

# transformation model:
tab.C.11 <- read.csv("tab.C.11.csv")
summary(tab.C.11.trns <- lm(formula = log(M) ~ I(1/Cu) + I(1/Zn), data = tab.C.11))
# better R^2; better residual standard error compared with linear model

# compare residuals with those of the linear model
# diagnostics of the  model:
tab.C.11.trns.res <- tab.C.11
tab.C.11.trns.res$raw.residuals <- tab.C.11.trns$residuals # raw residuals
tab.C.11.trns.res$stand.residuals <- tab.C.11.trns$residuals/summary(tab.C.11.trns)$sigma  # standardized residuals; points > 2 are potential outliers
tab.C.11.trns.res$stud.res <- rstandard(tab.C.11.trns) # rstandard() gives "internally Studentized residual"; points > 2 are potential outliers
tab.C.11.trns.res$stud.del.res <- rstudent(tab.C.11.trns) # Studentized deleted residuals; points > 2 are potential outliers
tab.C.11.trns.res$cooks.dist <- cooks.distance(tab.C.11.trns) # Cook's distance; > 1 [investigate], > 4 [potentially serious outlier]
tab.C.11.trns.res$leverage <- hatvalues(tab.C.11.trns) # Leverage; investigate if > 2*(k+1)/n [k:= no. independent variables; n:= no. of data points]
tab.C.11.trns.res

# normal probability plot: Fig 4-41
res.ordered <- sort(tab.C.11.trns.res$stand.residuals)
rank.res <- c(1:length(tab.C.11.trns.res$stand.residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(tab.C.11.trns.res$stand.residuals) # cumulative frequency
tab.C.11.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
fig.4.35 <- ggplot(tab.C.11.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

# plot of raw residuals
fig.4.36.A <- ggplot(tab.C.11.trns.res, aes(x=Cu, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)

fig.4.36.B <- ggplot(tab.C.11.trns.res, aes(x=Zn, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)

# rewriting the regression model:
# summary(tab.C.11.trns)$coefficients ->
# M = 25.1 * exp(-0.32/Cu) * exp(-7.18/Zn)
# multipicative model = interaction of Cu and Zn 
# reciprocal of the independent variables = saturable process

#################################################################################################
# Problems Chapter 4

# P-4-1
tab.D.4 <- read.csv("tab.D.4.csv")

summary(tab.D.4.lm <- lm(formula = RV.resistance ~ plasma.renin, data = tab.D.4))
# better R^2; better residual standard error compared with linear model

# compare residuals with those of the linear model
# diagnostics of the  model:
tab.D.4.lm.res <- tab.D.4
tab.D.4.lm.res$raw.residuals <- tab.D.4.lm$residuals # raw residuals
tab.D.4.lm.res$stand.residuals <- tab.D.4.lm$residuals/summary(tab.D.4.lm)$sigma  # standardized residuals; points > 2 are potential outliers
tab.D.4.lm.res$stud.res <- rstandard(tab.D.4.lm) # rstandard() gives "internally Studentized residual"; points > 2 are potential outliers
tab.D.4.lm.res$stud.del.res <- rstudent(tab.D.4.lm) # Studentized deleted residuals; points > 2 are potential outliers
tab.D.4.lm.res$cooks.dist <- cooks.distance(tab.D.4.lm) # Cook's distance; > 1 [investigate], > 4 [potentially serious outlier]
tab.D.4.lm.res$leverage <- hatvalues(tab.D.4.lm) # Leverage; investigate if > 2*(k+1)/n [k:= no. independent variables; n:= no. of data points]
tab.D.4.lm.res

## FixMe: create functions for easy identification of potential outliers based on the residual analytics above
subset(tab.D.4.lm.res, stand.residuals > 2)
subset(tab.D.4.lm.res, stud.res > 2)
subset(tab.D.4.lm.res, stud.del.res > 2 )
subset(tab.D.4.lm.res, cooks.dist > 1)
k <- 1 # FixMe: create function to extract number of variables
exp.leverage <- (2*(k+1)/length(tab.D.4.lm.res$leverage))
print(c("expected leverage: ", exp.leverage), quote = F)
subset(tab.D.4.lm.res, leverage > exp.leverage)
  # look at points 12, 1, 28

tab.D.4.lm.res$point <- c(1:length(tab.D.4[,1]))
# looking at all the data
ggplot(tab.D.4.lm.res, aes(x=plasma.renin, y=RV.resistance, colour=factor(group.code))) +
  geom_point(shape=1) +
  geom_text(aes(label=ifelse(leverage>exp.leverage,as.character(point),'')),hjust=0, vjust=0, col="black")+ 
  geom_text(aes(label=ifelse(stud.del.res>2,as.character(point),'')),hjust=0, vjust=0, col="red")


# looking at the data for each level of group.code
ggplot(tab.D.4, aes(x = plasma.renin, y = RV.resistance , colour=factor(group.code))) + 
  geom_point() + 
  facet_grid(. ~ group.code)

# looking at the residuals:
# normal probability plot
res.ordered <- sort(tab.D.4.lm.res$stand.residuals)
rank.res <- c(1:length(tab.D.4.lm.res$stand.residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(tab.D.4.lm.res$stand.residuals) # cumulative frequency
tab.D.4.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
ggplot(tab.D.4.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

# plot of raw residuals
ggplot(tab.D.4.lm.res, aes(x=RV.resistance, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)
  # => unequal distribution
# plot of raw residuals
ggplot(tab.D.4.lm.res, aes(x=plasma.renin, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)


# P-4-2
tab.D.13 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-13.dat"), header = F, sep="")
tab.D.13 <- data.frame(I.gf = tab.D.13$V1,
                       G.h = tab.D.13$V2)
write.csv(tab.D.13, file = "tab.D.13.csv",
          row.names = F)

# plotting raw data
ggplot(tab.D.13, aes(x=G.h, y=I.gf)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

summary(tab.D.13.lm <- lm(formula = I.gf ~ G.h, data = tab.D.13))

# diagnostics of the  model:
tab.D.13.lm.res <- tab.D.13
tab.D.13.lm.res$raw.residuals <- tab.D.13.lm$residuals
tab.D.13.lm.res$stand.residuals <- tab.D.13.lm$residuals/summary(tab.D.13.lm)$sigma
tab.D.13.lm.res$stud.res <- rstandard(tab.D.13.lm) 
tab.D.13.lm.res$stud.del.res <- rstudent(tab.D.13.lm) 
tab.D.13.lm.res$cooks.dist <- cooks.distance(tab.D.13.lm) 
tab.D.13.lm.res$leverage <- hatvalues(tab.D.13.lm) 
# values
subset(tab.D.13.lm.res, stand.residuals > 2)
subset(tab.D.13.lm.res, stud.res > 2)
subset(tab.D.13.lm.res, stud.del.res > 2 )
subset(tab.D.13.lm.res, cooks.dist > 1)
k <- 1 # FixMe: create function to extract number of variables
exp.leverage <- (2*(k+1)/length(tab.D.13.lm.res$leverage))
print(c("expected leverage: ", exp.leverage), quote = F)
subset(tab.D.13.lm.res, leverage > exp.leverage)
# look at points 20, 21
tab.D.13.lm.res

# looking at the raw data with all point above the leverage threshold value
tab.D.13.lm.res$point <- c(1:length(tab.D.13.lm.res[,1]))
ggplot(tab.D.13.lm.res, aes(x=G.h, y=I.gf)) +
  geom_point(shape=1) +
  geom_text(aes(label=ifelse(leverage>exp.leverage,as.character(point),'')),hjust=0, vjust=0, col="red")

# P-4-2-A
#   low Correlation: Pearson product-moment correlation coefficient sqrt(R^2)
#   R^2: 0.1236 -> sqrt(R^2) = 0.3515679
#   F-Statistik: 0.1181
#   large positiv intercept! 0 G.h should result in 0 I.gf!
# P-4-2-B
#   Points 20 & 21
# P-4-2-C
#   high leverage: the point is far away from the expected leverage value, BUT:
#   low Cook's distance: low impact of this point alone (because there is more than one "outlier")
#   leverage only indicates the potential for influence!


# P-4-3
tab.D.14 <- read.csv(url("http://people.vetmed.wsu.edu/slinkerb/appliedregression/Data%20files/Datadisk/problems_1ed/d-14.dat"), header = F, sep="")
tab.D.14 <- data.frame(A = tab.D.14$V1,
                       U.ag = tab.D.14$V2)
write.csv(tab.D.14, file = "tab.D.14.csv",
          row.names = F)

summary(tab.D.14.lm <- lm(formula = A ~ U.ag, data = tab.D.14))

# diagnostics of the  model:
tab.D.14.lm.res <- tab.D.14
tab.D.14.lm.res$raw.residuals <- tab.D.14.lm$residuals
tab.D.14.lm.res$stand.residuals <- tab.D.14.lm$residuals/summary(tab.D.14.lm)$sigma
tab.D.14.lm.res$stud.res <- rstandard(tab.D.14.lm) 
tab.D.14.lm.res$stud.del.res <- rstudent(tab.D.14.lm) 
tab.D.14.lm.res$cooks.dist <- cooks.distance(tab.D.14.lm) 
tab.D.14.lm.res$leverage <- hatvalues(tab.D.14.lm) 
# values
subset(tab.D.14.lm.res, stand.residuals > 2)
subset(tab.D.14.lm.res, stud.res > 2)
subset(tab.D.14.lm.res, stud.del.res > 2 )
subset(tab.D.14.lm.res, cooks.dist > 1)
k <- 1 # FixMe: create function to extract number of variables
exp.leverage <- (2*(k+1)/length(tab.D.14.lm.res$leverage))
print(c("expected leverage: ", exp.leverage), quote = F)
subset(tab.D.14.lm.res, leverage > exp.leverage)
# look at points 20, 21
tab.D.14.lm.res

# looking at the raw data with all point above the leverage threshold value
tab.D.14.lm.res$point <- c(1:length(tab.D.14.lm.res[,1]))
ggplot(tab.D.14.lm.res, aes(x=U.ag, y=A)) +
  geom_point(shape=1) +
  geom_text(aes(label=ifelse(leverage>exp.leverage,as.character(point),'')),hjust=0, vjust=0, col="red")+
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

# normal probability plot
res.ordered <- sort(tab.D.14.lm.res$stand.residuals)
rank.res <- c(1:length(tab.D.14.lm.res$stand.residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(tab.D.14.lm.res$stand.residuals) # cumulative frequency
tab.D.14.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
ggplot(tab.D.14.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

# plot of raw residuals
ggplot(tab.D.14.lm.res, aes(x=A, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)
# => unequal distribution
# plot of raw residuals
ggplot(tab.D.14.lm.res, aes(x=U.ag, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)

# quadric model
# quadric model:
summary(tab.D.14.quad <- lm(formula = A ~ U.ag + I(U.ag^2), data = tab.D.14))

# diagnostics of the  model:
tab.D.14.quad.res <- tab.D.14
tab.D.14.quad.res$raw.residuals <- tab.D.14.quad$residuals
tab.D.14.quad.res$stand.residuals <- tab.D.14.quad$residuals/summary(tab.D.14.quad)$sigma
tab.D.14.quad.res$stud.res <- rstandard(tab.D.14.quad) 
tab.D.14.quad.res$stud.del.res <- rstudent(tab.D.14.quad) 
tab.D.14.quad.res$cooks.dist <- cooks.distance(tab.D.14.quad) 
tab.D.14.quad.res$leverage <- hatvalues(tab.D.14.quad) 
# values
subset(tab.D.14.quad.res, stand.residuals > 2)
subset(tab.D.14.quad.res, stud.res > 2)
subset(tab.D.14.quad.res, stud.del.res > 2 )
subset(tab.D.14.quad.res, cooks.dist > 1)
k <- 1 # FixMe: create function to extract number of variables
exp.leverage <- (2*(k+1)/length(tab.D.14.quad.res$leverage))
print(c("expected leverage: ", exp.leverage), quote = F)
subset(tab.D.14.quad.res, leverage > exp.leverage)
# look at points 20, 21
tab.D.14.quad.res

# normal probability plot
res.ordered <- sort(tab.D.14.quad.res$stand.residuals)
rank.res <- c(1:length(tab.D.14.quad.res$stand.residuals)) 
cum.freq.res <- (rank.res - 0.5)/length(tab.D.14.quad.res$stand.residuals) # cumulative frequency
tab.D.14.quad.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
ggplot(tab.D.14.quad.norm , aes(x=res.ordered, y=cum.freq.res)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

# plot of raw residuals
ggplot(tab.D.14.quad.res, aes(x=A, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)
# => unequal distribution
# plot of raw residuals
ggplot(tab.D.14.quad.res, aes(x=U.ag, y=raw.residuals)) +
  geom_point(shape=1)+ 
  geom_hline(y=0, col="darkgrey", size=2)

## FixMe: 
# create function for:
#   normal probability plot using ggplot
#   plot of raw residuals using ggplot
#   analysis of residuals with different models taken into account

tab.D.14 <- read.csv("tab.D.14.csv")

# function for residual.analytics
#   d.f:
#     input data.frame
# defining y and x columns
#   y <- colnames(tab.D.14)[1]
#   x <- colnames(tab.D.14)[2]
#   formula.to.choose:
#     - "lm": linear model (= standard)
#     - else: quad model
residual.analytics <- function(d.f, x, y, formula.to.choose = "lm"){
  # choose model
  if (formula.to.choose == "lm"){
    # linear model [y ~ x]
    formula.1 <- as.formula(paste(y , " ~ " , x))
  }
  else
    # quadric model [y ~ x + x^2]
    formula.1 = as.formula(paste(y , " ~ " , x , " + I(" , x , "^2)"))
  
  # get residuals 
  d.f.lm <- lm(formula = formula.1, data = d.f)
  d.f.lm.res <- d.f
  d.f.lm.res$raw.residuals <- d.f.lm$residuals
  d.f.lm.res$stand.residuals <- d.f.lm$residuals/summary(d.f.lm)$sigma
  d.f.lm.res$stud.res <- rstandard(d.f.lm) 
  d.f.lm.res$stud.del.res <- rstudent(d.f.lm) 
  d.f.lm.res$cooks.dist <- cooks.distance(d.f.lm) 
  d.f.lm.res$leverage <- hatvalues(d.f.lm) 
  # critical values
  d.f.lm.res$stand.res.crit <- d.f.lm.res$stand.residuals > 2
  d.f.lm.res$stud.res.crit <- d.f.lm.res$stud.res > 2
  d.f.lm.res$stud.del.res.crit <- d.f.lm.res$stud.del.res > 2 
  d.f.lm.res$cooks.dist.crit <- d.f.lm.res$cooks.dist > 1
  k <- 1 # FixMe: create function to extract number of variables
  d.f.lm.res$exp.leverage <- (2*(k+1)/length(d.f.lm.res$leverage))
  d.f.lm.res$leverage.crit <- d.f.lm.res$leverage > d.f.lm.res$exp.leverage
  d.f.lm.res
  }

# call function
d.f.lm.res <- residual.analytics(tab.D.14, colnames(tab.D.14)[2], colnames(tab.D.14)[1])
d.f.lm.res
d.f.lm.res <- residual.analytics(tab.D.14, "U.ag", "A")
d.f.lm.res

# function for analytic plots:
residual.analtic.plots <- function(d.f.lm.res, x, y){
  # normal probability plot
  res.ordered <- sort(d.f.lm.res$stand.residuals)
  rank.res <- c(1:length(d.f.lm.res$stand.residuals)) 
  cum.freq.res <- (rank.res - 0.5)/length(d.f.lm.res$stand.residuals) # cumulative frequency
  tab.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
  normal.probability.plot <- ggplot(tab.norm , aes(x=res.ordered, y=cum.freq.res)) +
    geom_point(shape=1) + 
    geom_smooth(method=lm,   # Add linear regression line
                se=FALSE)    # Don't add shaded confidence region
  return (normal.probability.plot)
}
# function call
plot.1 <- residual.analtic.plots(d.f.lm.res,"U.ag","A")
plot.1

  # plot of raw residuals
  raw.residuals.x <- ggplot(d.f.lm.res, aes(x=x, y=raw.residuals)) +
    geom_point(shape=1)+ 
    geom_hline(y=0, col="darkgrey", size=2)
  # => unequal distribution
  # plot of raw residuals
  raw.residuals.y <- ggplot(d.f.lm.res, aes(x=U.ag, y=raw.residuals)) +
    geom_point(shape=1)+ 
    geom_hline(y=0, col="darkgrey", size=2)
  
}


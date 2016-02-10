# Functions

# Creation date: 
#   2015-12-04
# Created by: 
#   HS
# Objective:
#   function for residual.analytics
# Usage: 
#   
# Input: 
# 
# Output:
#   


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
  
  ## FixMe: übrige Eingaben werden als Formel interpretiert
  
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
tab.D.14 <- read.csv("tab.D.14.csv")
d.f.lm.res <- residual.analytics(tab.D.14, colnames(tab.D.14)[2], colnames(tab.D.14)[1])
d.f.lm.res
d.f.lm.res <- residual.analytics(tab.D.14, "U.ag", "A")
d.f.lm.res

# Creation date: 
#   2015-12-04
# Created by: 
#   HS
# Objective: 
#   plotting polynomal regression
# Usage: 
#   
# Input: 
# 
# Output:
#   

# function for analytic plots:
residual.analtic.plots <- function(d.f.lm.res){
  require("ggplot2")
  # normal probability plot
  res.ordered <- sort(d.f.lm.res$stand.residuals)
  rank.res <- c(1:length(d.f.lm.res$stand.residuals)) 
  cum.freq.res <- (rank.res - 0.5)/length(d.f.lm.res$stand.residuals) # cumulative frequency
  tab.norm <- data.frame(res.ordered,rank.res,cum.freq.res )
  normal.probability.plot <- ggplot(tab.norm , aes(x=res.ordered, y=cum.freq.res)) +
    geom_point(shape=1) + 
    geom_smooth(method=lm,   # Add linear regression line
                se=FALSE)    # Don't add shaded confidence region
  ## FIXME: ADD markings for points with high leverage/ cook's distance
  print(normal.probability.plot)
  
  # plot of raw residuals
  raw.residuals.x <- ggplot(d.f.lm.res, aes(d.f.lm.res[,2], raw.residuals)) +
    geom_point(shape=1)+ 
    geom_hline(y=0, col="darkgrey", size=2)+
    labs(x=colnames(d.f.lm.res[2]), y = "raw residuals")
  ## FIXME: ADD markings for points with high leverage/ cook's distance
  print(raw.residuals.x)
  
  # plot of raw residuals
  raw.residuals.y <- ggplot(d.f.lm.res, aes(d.f.lm.res[,1], raw.residuals)) +
    geom_point(shape=1)+ 
    geom_hline(y=0, col="darkgrey", size=2)+
    labs(x=colnames(d.f.lm.res[1]), y = "raw residuals")
  ## FIXME: Add posssibiliy to export graphs with corresponding flag in the function call
  # ggsave(raw.residuals.y, filename="test.png")#format(Sys.time(), "%H:%M:%S %d-%b-%Y ")
  ## FIXME: ADD markings for points with high leverage/ cook's distance
  print(raw.residuals.y)
}
# function call
tab.D.14 <- read.csv("tab.D.14.csv")
residual.analtic.plots(d.f.lm.res)
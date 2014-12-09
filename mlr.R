# INFO 232
# April Dawn Kester
# Research project

#import data
install.packages("foreign")
install.packages("nnet")
install.packages("ggplot2")
install.packages("reshape2")

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

#read from dataset
dat <- read.csv(file="beta_working.csv",head=TRUE,sep=",")

#sample
#The outcome variable is prog, program type. 
#The predictor variables are social economic status, ses, a three-level categorical variable 
#and writing score, write, a continuous variable.
#with(ml, table(ses, prog))
with(dat, table(score_5B, Q1))

#with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))
with(dat, do.call(rbind, tapply(avg_score, Q1, function(x) c(M = mean(x), SD = sd(x)))))

#ml$prog2 <- relevel(ml$prog, ref = "academic")
#test <- multinom(prog2 ~ ses + write, data = ml)

dat$Q1_2 <- factor(dat$Q1)
dat$Q1_2 <- relevel(dat$Q1_2, ref = "1")
test <- multinom(Q1_2 ~ score_5B + avg_score, data = dat)

z <- summary(test)$coefficients/summary(test)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(test))
head(pp <- fitted(test))

#dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
#predict(test, newdata = dses, "probs")

dses <- data.frame(score_5B = c("MIN", "Low MIN", "MAX", "High MAX"), avg_score = mean(dat$avg_score))
predict(test, newdata = dses, "probs")

#dwrite <- data.frame(score_5B = rep(c("MIN", "Low MIN", "MAX", "High MAX"), each = 40), avg_score = rep(c(30:70),3))
#pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))
#by(pp.write[, 3:5], pp.write$score_5B, colMeans)


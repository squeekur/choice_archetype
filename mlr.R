# INFO 232
# April Dawn Kester
# Research project

#import data
install.packages("foreign")
install.packages("nnet")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("car")
install.packages("mvtnorm")

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(car)
library(mvtnorm)

#read from dataset
dat <- read.csv(file="beta_working.csv",head=TRUE,sep=",")

#sample
#The outcome variable is prog, program type. 
#The predictor variables are social economic status, ses, a three-level categorical variable 
#and writing score, write, a continuous variable.
#with(ml, table(ses, prog))
with(dat, table(score_5B, disease))

#with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))
with(dat, do.call(rbind, tapply(avg_score, disease, function(x) c(M = mean(x), SD = sd(x)))))
with(dat, do.call(rbind, tapply(score_5, disease, function(x) c(M = mean(x, na.rm=TRUE), SD = sd(x, na.rm=TRUE)))))
with(dat, do.call(rbind, tapply(D9, disease, function(x) c(M = mean(x, na.rm=TRUE), SD = sd(x, na.rm=TRUE)))))
with(dat, do.call(rbind, tapply(D7, disease, function(x) c(M = mean(x, na.rm=TRUE), SD = sd(x, na.rm=TRUE)))))

#ml$prog2 <- relevel(ml$prog, ref = "academic")
#test <- multinom(prog2 ~ ses + write, data = ml)

dat$temp_2 <- factor(dat$cereal, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$icecream, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$entree, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$apparel, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$date, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$job, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$doctor, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$car, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$home, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$vacation, levels = c("postpone", "defer", "eliminate", "scrutinize"))
dat$temp_2 <- factor(dat$disease, levels = c("postpone", "defer", "eliminate", "scrutinize"))

#mod1<-lm(cbind(cereal, icecream, entree, apparel, date, job, doctor, car, home, vacation, disease) ~ score_5 + avg_score + D9 + D7, data = dat)
#Manova(mod1)

#UNIVARIATE
dat$temp_2 <- relevel(dat$temp_2, ref = "postpone")
test <- multinom(temp_2 ~ score_5 + avg_score, data = dat)
test
z <- summary(test)$coefficients/summary(test)$standard.errors
#z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#UNIVARIATE FIT
dat$temp_2 <- relevel(dat$temp_2, ref = "postpone")
fit1 <- multinom(temp_2 ~ score_5 + avg_score + D9 + D7, data = dat)
fit2 <- multinom(temp_2 ~ score_5 + avg_score, data = dat)
fit3 <- multinom(temp_2 ~ avg_score, data = dat)
anova(fit1, fit2, fit3, test="Chisq") 

#MULTIVARIATE 
test <- multinom(cbind(cereal, icecream, entree, apparel, date, job, doctor, car, home, vacation, disease) ~ score_5 + avg_score, data = dat)

test
z <- summary(test)$coefficients/summary(test)$standard.errors
#z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(test))
head(pp <- fitted(test))

#dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
#predict(test, newdata = dses, "probs")

dses <- data.frame(score_5 = mean(dat$score_5, na.rm=TRUE), avg_score = dat$avg_score, D9 = mean(dat$D9, na.rm=TRUE), D7 = mean(dat$D7, na.rm=TRUE))
predict(test, newdata = dses, "probs")

#dwrite <- data.frame(score_5B = rep(c("MIN", "Low MIN", "MAX", "High MAX"), each = 40), avg_score = rep(c(30:70),3))
#pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))
#by(pp.write[, 3:5], pp.write$score_5B, colMeans)


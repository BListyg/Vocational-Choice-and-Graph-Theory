library(foreign)
library(scales)
library(arm)

huang <- read.spss(file = '~/Downloads/20180625 Ben Listyg Data Requested.sav', to.data.frame = T)

huang$OCC_TITLE <- trimws(huang$OCC_TITLE)

ben <- read.csv("https://raw.githubusercontent.com/BListyg/Vocational-Interests-Data/master/riasec_data20180501.csv",header = T)

ben <- data.frame(
  OCC_TITLE = ben$title,
  R = ben$realisitc,
  I = ben$investigative,
  A = ben$artistic,
  S = ben$social,
  E = ben$enterprising,
  C = ben$conventional,
  A_MEDIAN = ben$income
)

#####

par(mfrow=c(1,2))

plot(huang$R, 
     log(huang$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Huang and Pearce (2013)\nlog(Income) ~ Realistic Interests\nn = ',nrow(huang)),sep = '',collapse = ''),
     xlab='Realistic Interests',
     ylab='log(Income)',
     abline(lm(log(huang$A_MEDIAN) ~ huang$R, data=huang),col='blue',lwd=5)
     )

plot(scales::rescale(x = ben$R,to=c(1,7)), 
     log(ben$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Listyg (20??)\nlog(Income) ~ Realistic Interests\nn = ',nrow(ben[complete.cases(ben),])),sep = '',collapse = ''),
     xlab='Realistic Interests',
     ylab='log(Income)',
     abline(lm(log(ben$A_MEDIAN) ~ ben$R, data=ben),col='blue',lwd=5)
     )

#####

par(mfrow=c(1,2))

plot(huang$I, 
     log(huang$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Huang and Pearce (2013)\nlog(Income) ~ Investigative Interests\nn = ',nrow(huang)),sep = '',collapse = ''),
     xlab='Investigative Interests',
     ylab='log(Income)',
     abline(lm(log(huang$A_MEDIAN) ~ huang$I, data=huang),col='blue',lwd=5)
)

plot(scales::rescale(x = ben$I,to=c(1,7)), 
     log(ben$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Listyg (20??)\nlog(Income) ~ Investigative Interests\nn = ',nrow(ben[complete.cases(ben),])),sep = '',collapse = ''),
     xlab='Investigative Interests',
     ylab='log(Income)',
     abline(lm(log(ben$A_MEDIAN) ~ ben$I, data=ben),col='blue',lwd=5)
)

#####

par(mfrow=c(1,2))

plot(huang$A, 
     log(huang$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Huang and Pearce (2013)\nlog(Income) ~ Artistic Interests\nn = ',nrow(huang)),sep = '',collapse = ''),
     xlab='Artistic Interests',
     ylab='log(Income)',
     abline(lm(log(huang$A_MEDIAN) ~ huang$A, data=huang),col='blue',lwd=5)
)

plot(scales::rescale(x = ben$A,to=c(1,7)), 
     log(ben$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Listyg (20??)\nlog(Income) ~ Artistic Interests\nn = ',nrow(ben[complete.cases(ben),])),sep = '',collapse = ''),
     xlab='Artistic Interests',
     ylab='log(Income)',
     abline(lm(log(ben$A_MEDIAN) ~ ben$A, data=ben),col='blue',lwd=5)
)

#####

par(mfrow=c(1,2))

plot(huang$S, 
     log(huang$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Huang and Pearce (2013)\nlog(Income) ~ Social Interests\nn = ',nrow(huang)),sep = '',collapse = ''),
     xlab='Social Interests',
     ylab='log(Income)',
     abline(lm(log(huang$A_MEDIAN) ~ huang$S, data=huang),col='blue',lwd=5)
)

plot(scales::rescale(x = ben$S,to=c(1,7)), 
     log(ben$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Listyg (20??)\nlog(Income) ~ Social Interests\nn = ',nrow(ben[complete.cases(ben),])),sep = '',collapse = ''),
     xlab='Social Interests',
     ylab='log(Income)',
     abline(lm(log(ben$A_MEDIAN) ~ ben$S, data=ben),col='blue',lwd=5)
)

#####

par(mfrow=c(1,2))

plot(huang$E, 
     log(huang$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Huang and Pearce (2013)\nlog(Income) ~ Enterprising Interests\nn = ',nrow(huang)),sep = '',collapse = ''),
     xlab='Enterprising Interests',
     ylab='log(Income)',
     abline(lm(log(huang$A_MEDIAN) ~ huang$E, data=huang),col='blue',lwd=5)
)

plot(scales::rescale(x = ben$E,to=c(1,7)), 
     log(ben$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Listyg (20??)\nlog(Income) ~ Enterprising Interests\nn = ',nrow(ben[complete.cases(ben),])),sep = '',collapse = ''),
     xlab='Enterprising Interests',
     ylab='log(Income)',
     abline(lm(log(ben$A_MEDIAN) ~ ben$E, data=ben),col='blue',lwd=5)
)

#####

par(mfrow=c(1,2))

plot(huang$C, 
     log(huang$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Huang and Pearce (2013)\nlog(Income) ~ Conventional Interests\nn = ',nrow(huang)),sep = '',collapse = ''),
     xlab='Conventional Interests',
     ylab='log(Income)',
     abline(lm(log(huang$A_MEDIAN) ~ huang$E, data=huang),col='blue',lwd=5)
)

plot(scales::rescale(x = ben$C,to=c(1,7)), 
     log(ben$A_MEDIAN),
     ylim=c(8,13),
     main=paste(c('Listyg (20??)\nlog(Income) ~ Conventional Interests\nn = ',nrow(ben[complete.cases(ben),])),sep = '',collapse = ''),
     xlab='Conventional Interests',
     ylab='log(Income)',
     abline(lm(log(ben$A_MEDIAN) ~ ben$C, data=ben),col='blue',lwd=5)
)

library(foreign)
library(cowplot)
library(scales)

ben <- read.csv('https://raw.githubusercontent.com/BListyg/Vocational-Interests-Data/master/riasec_data20180501.csv', header = T)[,c(2:7,14,11,12,8)]

rescaled_interests <- data.frame(apply(X = ben[,c(1:6)], MARGIN = 2, FUN = function(x){
  rescale(x = x, to = c(1,7))
}))

colnames(rescaled_interests) <- paste('r_',colnames(rescaled_interests),sep = "")

ben<-cbind(ben,rescaled_interests)

head(ben)

#Distribution Plots

par(mfrow=c(1,2))
plot(density(jason$R), main = 'Realistic Scores - Huang and Pearce (2013)')
plot(density(rescale(ben$realisitc, to = c(1,7)), na.rm = T), main = 'Realistic Scores - 2018 Data (Rescaled)')

par(mfrow=c(1,2))
plot(density(jason$I), main = 'Investigative Scores - Huang and Pearce (2013)')
plot(density(rescale(ben$investigative, to = c(1,7)), na.rm = T), main = 'Investigative Scores - 2018 Data (Rescaled)')

par(mfrow=c(1,2))
plot(density(jason$A), main = 'Artistic Scores - Huang and Pearce (2013)')
plot(density(rescale(ben$artistic, to = c(1,7)), na.rm = T), main = 'Artistic Scores - 2018 Data (Rescaled)')

par(mfrow=c(1,2))
plot(density(jason$S), main = 'Social Scores - Huang and Pearce (2013)')
plot(density(rescale(ben$social, to = c(1,7)), na.rm = T), main = 'Social Scores - 2018 Data (Rescaled)')

par(mfrow=c(1,2))
plot(density(jason$E), main = 'Enterprising Scores - Huang and Pearce (2013)')
plot(density(rescale(ben$enterprising, to = c(1,7)), na.rm = T), main = 'Enterprising Scores - 2018 Data (Rescaled)')

par(mfrow=c(1,2))
plot(density(jason$C), main = 'Conventional Scores - Huang and Pearce (2013)')
plot(density(rescale(ben$conventional, to = c(1,7)), na.rm = T), main = 'Conventional Scores - 2018 Data (Rescaled)') 

par(mfrow=c(1,2))
plot(density(log(jason$A_MEDIAN)), main = 'Income - Huang and Pearce (2013)', xlim=c(9,12.5))
plot(density(log(ben$income), na.rm = T), main = 'Income - 2018 Data',xlim=c(9,12.5))

# Income Plots

par(mfrow=c(1,2))
plot(jason$R, log(jason$A_MEDIAN), xlab=NA, ylim = c(9.0,12.5))
abline(lm(log(jason$A_MEDIAN) ~ jason$R))
plot(ben$r_realisitc, log(ben$income), ylim = c(9.0,12.5))
abline(lm(log(ben$income) ~ ben$r_realisitc))

par(mfrow=c(1,2))
plot(jason$I, log(jason$A_MEDIAN), xlab = 'Realistic Scores',ylab='Log(Income)',ylim = c(9.0,12.5))
abline(lm(log(jason$A_MEDIAN) ~ jason$I))

plot(ben$r_investigative, log(ben$income), ylim = c(9.0,12.5), xlab = 'Realistic Scores',ylab='Log(Income)')
abline(lm(log(ben$income) ~ ben$r_investigative))

par(mfrow=c(1,2))
plot(jason$A, log(jason$A_MEDIAN), xlab=NA, ylim = c(9.0,12.5))
abline(lm(log(jason$A_MEDIAN) ~ jason$A))
plot(rescale(ben$artistic, to = c(1,7)), log(ben$income), ylim = c(9.0,12.5))
abline(lm(log(ben$income) ~ rescale(ben$artistic, to = c(1,7))))

par(mfrow=c(1,2))
plot(jason$S, log(jason$A_MEDIAN), xlab=NA, ylim = c(9.0,12.5))
abline(lm(log(jason$A_MEDIAN) ~ jason$S))
plot(rescale(ben$social, to = c(1,7)), log(ben$income), ylim = c(9.0,12.5))
abline(lm(log(ben$income) ~ rescale(ben$social, to = c(1,7))))

par(mfrow=c(1,2))
plot(jason$E, log(jason$A_MEDIAN), xlab=NA, ylim = c(9.0,12.5))
abline(lm(log(jason$A_MEDIAN) ~ jason$E))
plot(rescale(ben$enterprising, to = c(1,7)), log(ben$income), ylim = c(9.0,12.5))
abline(lm(log(ben$income) ~ rescale(ben$enterprising, to = c(1,7))))

par(mfrow=c(1,2))
plot(jason$C, log(jason$A_MEDIAN), xlab=NA, ylim = c(9.0,12.5))
abline(lm(log(jason$A_MEDIAN) ~ jason$C))
plot(rescale(ben$conventional, to = c(1,7)), log(ben$income), ylim = c(9.0,12.5))
abline(lm(log(ben$income) ~ rescale(ben$conventional, to = c(1,7))))

#

merged_jason<-merge(x = jason, y = setNames(data.frame(ben$title, ben$career_cluster), c('OCC_TITLE','career_cluster')), by = 'OCC_TITLE')

#

  suppressWarnings(
    plot_grid(
      ggplot(data = jason, aes(x = R, y = log(A_MEDIAN))) + 
        geom_point(col='black', size=0.1) +
        geom_smooth(method = "lm", se = FALSE, fullrange = T) +
        xlab('Realistic Scores\nHuang and Pearce (2013)') +
        ylab('Log(Income)') +
        ylim(c(9.5,12.5)),
      
      ggplot(merged_jason,aes(y = log(merged_jason$A_MEDIAN), x = merged_jason$R,
                              colour = merged_jason$career_cluster)) + 
        geom_point(col='black', size=0.1) + 
        geom_smooth(method = "lm", fill = NA, fullrange = T) + 
        theme(legend.position="none") +
        ylim(c(9.5,12.5)) +
        xlab('Realistic Scores\nHuang and Pearce (2013)') +
        ylab('') + stat_smooth( aes( y = log(merged_jason$A_MEDIAN), x = R),
                                inherit.aes = FALSE,se=FALSE, method = 'lm', col = 'red', lwd=2.5, 
                                linetype='F1'),
      
      ggplot(data = ben, aes(x = ben$r_realisitc, y = log(income))) + 
        geom_point(col='black', size=0.1) +
        geom_smooth(method = "lm", se = FALSE, fullrange = T) +
        xlab('Realistic Scores\nListyg (2018)') +
        ylab('')+
        ylim(c(9.5,12.5)),
      
      ggplot(ben,aes(y = log(ben$income), x = ben$r_realisitc, 
                     colour = ben$career_cluster)) + 
        geom_point(col='black', size=0.1) + 
        geom_smooth(method = "lm", fill = NA, fullrange = T) + 
        theme(legend.position="none") +
        ylim(c(9.5,12.5)) +
        xlab('Conventional Scores\nListyg (2018)') +
        ylab('') + stat_smooth( aes( y = log(ben$income), x = r_realisitc), 
                                inherit.aes = FALSE,se=FALSE, method = 'lm', col = 'red', 
                                lwd=2.5, linetype='F1')
      
      ,ncol = 4,
      align = 'hv'))
  
suppressWarnings(plot_grid(ggplot(data = jason, aes(x = I, y = log(A_MEDIAN))) + 
                             geom_point(col='black', size=0.1) +
                             geom_smooth(method = "lm", se = FALSE, fullrange = T) +
                             xlab('Investigative Scores\nHuang and Pearce (2013)') +
                             ylab('Log(Income)') +
                             ylim(c(9.5,12.5)),
                           ggplot(merged_jason,aes(y = log(merged_jason$A_MEDIAN), x = merged_jason$I, colour = merged_jason$career_cluster)) + 
                             geom_point(col='black', size=0.1) + 
                             geom_smooth(method = "lm", fill = NA, fullrange = T) + 
                             theme(legend.position="none") +
                             ylim(c(9.5,12.5)) +
                             xlab('Investigative Scores\nHuang and Pearce (2013)') +
                             ylab('') + stat_smooth( aes( y = log(merged_jason$A_MEDIAN), x = I), inherit.aes = FALSE,se=FALSE, method = 'lm', col = 'red', lwd=2.5, linetype='F1'),
                           ggplot(data = ben, aes(x = ben$r_investigative, y = log(income))) + 
                             geom_point(col='black', size=0.1) +
                             geom_smooth(method = "lm", se = FALSE, fullrange = T) +
                             xlab('Investigative Scores\nListyg (2018)') +
                             ylab('')+
                             ylim(c(9.5,12.5)),
                           ggplot(ben,aes(y = log(ben$income), x = ben$r_investigative, colour = ben$career_cluster)) + 
                             geom_point(col='black', size=0.1) + 
                             geom_smooth(method = "lm", fill = NA, fullrange = T) + 
                             theme(legend.position="none") +
                             ylim(c(9.5,12.5)) +
                             xlab('Investigative Scores\nListyg (2018)') +
                             ylab('') + stat_smooth( aes( y = log(ben$income), x = r_investigative), inherit.aes = FALSE,se=FALSE, method = 'lm', col = 'red', lwd=2.5, linetype='F1')
                           ,ncol = 4,
                           align = 'hv'))
suppressWarnings(plot_grid(ggplot(data = jason, aes(x = C, y = log(A_MEDIAN))) + 
                             geom_point(col='black', size=0.1) +
                             geom_smooth(method = "lm", se = FALSE, fullrange = T) +
                             xlab('Conventional Scores\nHuang and Pearce (2013)') +
                             ylab('Log(Income)') +
                             ylim(c(9.5,12.5)),
                           ggplot(merged_jason,aes(y = log(merged_jason$A_MEDIAN), x = merged_jason$C, colour = merged_jason$career_cluster)) + 
                             geom_point(col='black', size=0.1) + 
                             geom_smooth(method = "lm", fill = NA, fullrange = T) + 
                             theme(legend.position="none") +
                             ylim(c(9.5,12.5)) +
                             xlab('Conventional Scores\nHuang and Pearce (2013)') +
                             ylab('') + stat_smooth( aes( y = log(merged_jason$A_MEDIAN), x = C), inherit.aes = FALSE,se=FALSE, method = 'lm', col = 'red', lwd=2.5, linetype='F1', fullrange = T),
                           ggplot(data = ben, aes(x = ben$r_conventional, y = log(income))) + 
                             geom_point(col='black', size=0.1) +
                             geom_smooth(method = "lm", se = FALSE, fullrange = T) +
                             xlab('Conventional Scores\nListyg (2018)') +
                             ylab('')+
                             ylim(c(9.5,12.5)),
                           ggplot(ben,aes(y = log(ben$income), x = ben$r_conventional, colour = ben$career_cluster)) + 
                             geom_point(col='black', size=0.1) + 
                             geom_smooth(method = "lm", fill = NA, fullrange = T) + 
                             theme(legend.position="none") +
                             ylim(c(9.5,12.5)) +
                             xlab('Conventional Scores\nListyg (2018)') +
                             ylab('') + stat_smooth( aes( y = log(ben$income), x = r_conventional), inherit.aes = FALSE,se=FALSE, method = 'lm', col = 'red', lwd=2.5, linetype='F1', fullrange = T)
                           ,ncol = 4,
                           align = 'hv'))

suppressWarnings(plot_grid(ggplot(data = jason, aes(x = S, y = log(A_MEDIAN))) + 
                             geom_point(col='black', size=0.1) +
                             geom_smooth(method = "lm", se = FALSE, fullrange = T) +
                             xlab('Social Scores\nHuang and Pearce (2013)') +
                             ylab('Log(Income)') +
                             ylim(c(9.5,12.5)),
                           ggplot(merged_jason,aes(y = log(merged_jason$A_MEDIAN), x = merged_jason$C, colour = merged_jason$career_cluster)) + 
                             geom_point(col='black', size=0.1) + 
                             geom_smooth(method = "lm", fill = NA, fullrange = T) + 
                             theme(legend.position="none") +
                             ylim(c(9.5,12.5)) +
                             xlab('Social Scores\nHuang and Pearce (2013)') +
                             ylab('') + stat_smooth( aes( y = log(merged_jason$A_MEDIAN), x = S), inherit.aes = FALSE,se=FALSE, method = 'lm', col = 'red', lwd=2.5, linetype='F1', fullrange = T),
                           ggplot(data = ben, aes(x = ben$r_social, y = log(income))) + 
                             geom_point(col='black', size=0.1) +
                             geom_smooth(method = "lm", se = FALSE, fullrange = T) +
                             xlab('Social Scores\nListyg (2018)') +
                             ylab('')+
                             ylim(c(9.5,12.5)),
                           ggplot(ben,aes(y = log(ben$income), x = ben$r_social, colour = ben$career_cluster)) + 
                             geom_point(col='black', size=0.1) + 
                             geom_smooth(method = "lm", fill = NA, fullrange = T) + 
                             theme(legend.position="none") +
                             ylim(c(9.5,12.5)) +
                             xlab('Social Scores\nListyg (2018)') +
                             ylab('') + stat_smooth( aes( y = log(ben$income), x = r_conventional), inherit.aes = FALSE,se=FALSE, method = 'lm', col = 'red', lwd=2.5, linetype='F1', fullrange = T)
                           ,ncol = 4,
                           align = 'hv'))

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# ggplot(ben) + 
#   aes(x = r_social, y = log(income)) + 
#   stat_smooth(method = "lm", se = FALSE, fullrange = T) +
#   # Put the points on top of lines
#   geom_point() +
#   facet_wrap("career_cluster",labeller = labeller(career_cluster=label_wrap_gen(10)))+
#   labs(x = "Social", y = "Log(Income)") + 
#   theme(strip.text.x = element_text(size = 8))

ggplot(ben) + 
  aes(x = r_realisitc, y = log(income)) + 
  stat_smooth(method = "lm", se = FALSE, fullrange = T) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("career_cluster")+
  labs(x = "Realistic", y = "Log(Income)") + 
  theme(strip.text.x = element_text(size = 8))

ggplot(ben) + 
  aes(x = r_investigative, y = log(income)) + 
  stat_smooth(method = "lm", se = FALSE, fullrange = T) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("career_cluster")+
  labs(x = "Investigative", y = "Log(Income)") + 
  theme(strip.text.x = element_text(size = 8))

ggplot(ben) + 
  aes(x = r_artistic, y = log(income)) + 
  stat_smooth(method = "lm", se = FALSE, fullrange = T) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("career_cluster")+
  labs(x = "Artisitc", y = "Log(Income)") + 
  theme(strip.text.x = element_text(size = 8))

ggplot(ben) + 
  aes(x = r_social, y = log(income)) + 
  stat_smooth(method = "lm", se = FALSE, fullrange = T) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("career_cluster")+
  labs(x = "Social", y = "Log(Income)") + 
  theme(strip.text.x = element_text(size = 8))

ggplot(ben) + 
  aes(x = r_enterprising, y = log(income)) + 
  stat_smooth(method = "lm", se = FALSE, fullrange = T) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("career_cluster")+
  labs(x = "Enterprising", y = "Log(Income)") + 
  theme(strip.text.x = element_text(size = 8))

ggplot(ben) + 
  aes(x = r_conventional, y = log(income)) + 
  stat_smooth(method = "lm", se = FALSE, fullrange = T) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("career_cluster")+
  labs(x = "Conventional", y = "Log(Income)") + 
  theme(strip.text.x = element_text(size = 8))

  # We also need to help the x-axis, so it doesn't 
  # create gridlines/ticks on 2.5 days









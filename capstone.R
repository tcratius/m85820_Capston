# Plotting 
library(scales)
library(ggplot2)

# testing
library(lawstat)
library(fitdistrplus)

# Time values.
library(lubridate)

# Data manipulation 
library(dplyr)

# Unknown if used in code
library(logspline)
library(viridis)
library(tidyr)


# 1. Import Cleaned data from folder three.
park.space.read <- read.csv(choose.files(), as.is = T)
#time <- read.csv(choose.files())

park.space <- park.space.read
str(park.space.read)
View(park.space.read)
park.space$Date <- as.POSIXct(park.space$Date, tz='', '%Y-%m-%d %H:%M:%S')
# park.space <- as.factor(park.space$Parking.space.num)

# 2 inspect the data.
# notes of 684 parking space sampled over 6 day period only 597 parking spaces 
# had vehicles occupying the space(s) with a difference on 97 unoccupied 
# parking spot surveyed, difference of 188 unoccupied.

cars <- data.frame(park.space)

names(cars)[names(cars) == 'Forward'] <- 'Drive'

################# BEST    ############################  
# Combine the totals for each day for forward and reverse grouped by date
combine.ps.Drive <- cars %>%
  group_by(Date) %>%
  filter(Drive == 1) %>%
  summarise(T.Drive = sum(Drive)) #%>%
  # mutate(Prob.For = pnorm(T.Drive, mean(T.Drive), sd(T.Drive)))

combine.ps.reverse <- cars %>%
  group_by(Date) %>%
  filter(Reverse == 1 | Date == '2018-08-07 15:30:00') %>%
  summarise(T.Reverse = sum(Reverse))# %>%
  #mutate(Prob.Rev = pnorm(T.Reverse, mean(T.Reverse), sd(T.Reverse)))

combine.ps.empty <- cars %>%
  group_by(Date) %>%
  filter(Unoccupied == 1) %>%
  summarise(T.Empty = sum(Unoccupied))# %>%
  # mutate(Pro.Empty = pnorm(T.Empty, mean(T.Empty), sd(T.Empty)) )



par(mfrow=c(1,2))

hist(combine.ps.Drive$T.Drive, 
     main = "Histogram: Total of Cars\nParked Drive",
     xlab = "Cars Parked Drive")
qqnorm(combine.ps.Drive$T.Drive)
qqline(combine.ps.Drive$T.Drive)

hist(combine.ps.reverse$T.Reverse,
     main = "Histogram: Total of Cars\nParked Reverse",
     xlab = "Cars Parked Reverse")
qqnorm(combine.ps.reverse$T.Reverse)
qqline(combine.ps.reverse$T.Reverse)

hist(combine.ps.empty$T.Empty,
     main = "Histogram: Total of Empty\nParking Spaces",
     xlab = "Empty Parking Spaces")
qqnorm(combine.ps.empty$T.Empty)
qqline(combine.ps.empty$T.Empty)

par(mfrow=c(1,3))
boxplot(combine.ps.Drive$T.Drive, xlab="Cars Parked Drive")
boxplot(combine.ps.reverse$T.Reverse, xlab="Cars Parked Reverse")
boxplot(combine.ps.empty$T.Empty, xlab="Empty Parking Spaces")

############
# Library fitdistr
descdist(combine.ps.reverse$T.Reverse) # Beta distribution


fit.beta <- fitdist(combine.ps.reverse$T.Reverse, "beta") # Beta

plot(fit.beta)

#########################################
# combine and remove outliers to avoid transformation with beta distribution of T.Reverse
cars.parked <- cbind(combine.ps.Drive, combine.ps.reverse, combine.ps.empty, deparse.level = 1)

cars.parked[,3] <- NULL
cars.parked[,4] <- NULL

mean.for <- mean(cars.parked$T.Drive)
std.for <- sd(cars.parked$T.Drive)
SE.for <- std.for/sqrt(length(cars.parked$T.Drive))
round(mean.for, 2)
round(std.for, 2)
round(SE.for, 2)
pnorm(std.for, mean.for, std.for/sqrt(10))
1- pnorm(std.for, mean.for, std.for/sqrt(10))

mean.rev <- mean(cars.parked$T.Reverse)
std.rev <- sd(cars.parked$T.Reverse)
SE.rev <- std.rev/sqrt(length(cars.parked$T.Reverse))
round(mean.rev, 2)
round(std.rev, 2)
round(SE.rev, 2)
pnorm((6), mean.rev, std.rev/sqrt(10))

pnorm((1.5), mean.rev, std.rev/sqrt(10))

mean.empty <-  mean(cars.parked$T.Empty)
std.empty <- sd(cars.parked$T.Empty)
SE.empty <- std.empty/sqrt(length(cars.parked$T.Empty))
round(mean.empty, 2)
round(std.empty, 2)
round(SE.empty, 2)
pnorm(std.empty, mean.empty, std.empty/sqrt(10))
1 - pnorm(std.empty, mean.empty, std.empty/sqrt(10))

#*************
# Shapiro-Wilk test of normality.
# Ho: equal normal distribution.
# Drive W = 0.95242, p-value = 0.6972
# W = 0.67942, p-value = 0.0004965
# ***********
shapiro.test(cars.parked$T.Drive)
shapiro.test(cars.parked$T.Reverse)
shapiro.test(cars.parked$T.Empty)

#*************
# signifcant difference between the two variances
# Ho:Rv = Fv
# Ha:Rv != Fv
# p-value = 0.5981 At 0.95 confidence level, the differences between variance of Drive and reverse are not significant.     
# ***********
#var.test(cars.parked$T.Reverse ~ cars.parked$T.Empty, alternative = 'two.sided')

#*************
# symmetry test both are symmetrical 
# Test statistic = 0.38534, p-value = 0.686
# Reverse Test statistic = 2.3855, p-value = 0.07
#***********
symmetry.test(cars.parked$T.Drive, side = 'both', boot = T) 
symmetry.test(cars.parked$T.Reverse, side = 'both', boot = T)
symmetry.test(cars.parked$T.Empty, side = 'both', boot = T)

#*************
# Ho: R = F = E
# Ha R != F 
# W = 1.5, p-value = 0.0001344
# 
#***********
#*************
# Ho: R < F
# Ha E > F 
# W = 1.5, p-value = 0.0001344
# 
#***********
rev.driv.test <- wilcox.test(cars.parked$T.Reverse, cars.parked$T.Drive, 
                             conf.level = 0.95, "less", exact = F)
rev.driv.test
# -0.6471723 
cor.test(cars.parked$T.Drive, cars.parked$T.Empty, method='pearson')

# ? no correlation.
cor.test(cars.parked$T.Drive, cars.parked$T.Reverse, method='pearson')

# -0.7603683 
cor.test(cars.parked$T.Reverse, cars.parked$T.Empty, method='pearson')
# 0.1494754
cor.test(cars.parked$T.Drive, as.numeric(cars.parked$Date), method = 'pearson')
# 0.3388834
cor.test(cars.parked$T.Reverse, as.numeric(cars.parked$Date), method = 'pearson')
dmodel <- lm(T.Drive ~ T.Empty, data=cars.parked)
summary(dmodel)

#-be approximately normally distributed
qqnorm(residuals)
qqline(residuals, lty=2)
shapiro.test(residuals)

#-have constant variance
plot(drive.fitted, residuals, xlab="Fitted Driving Forward to Park", ylab="Residual (count)")
abline(h=0, lty=2)

#-be approximately independent
plot(residuals, type='b', ylab="Residual")
abline(h=0, lty=2, col='grey')

FED <- ggplot(cars.parked, aes(T.Drive, T.Empty)) + 
  xlab("Drive") + ggtitle("Linear model: Empty vs Drive\n") +
  ylab("Empty") + geom_point(colour = "red", size = 3) + 
  geom_smooth(method=lm, fullrange=TRUE) + 
  theme_classic()
FED

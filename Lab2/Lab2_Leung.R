#Audrey Leung
#Info 271B - Lab 2

# 1. Data Import and Error Checking: Using the GSS dataset.

library(ggplot2)
library(car)
load(url("http://courses.ischool.berkeley.edu/i271b/f14/data/GSS.Rdata"))
head(GSS)

# a. Examine the “agewed” variable (age when married).

p = ggplot(GSS, aes(x = agewed))
p + geom_histogram(binwidth=5)
table(GSS$agewed)

# i. What are the value(s) of agewed, if any, that do not meaningfully correspond to ages?
# b. Recode any value(s) that do not correspond to age as NA. 

GSS$agewed[GSS$agewed == 0] = NA
GSS$agewed[GSS$agewed == 1] = NA
GSS$agewed[GSS$agewed == 99] = NA

hist(GSS$agewed, breaks=50)
table(GSS$agewed)

# i. What is the mean of the agewed variable?
mean(GSS$agewed, na.rm=TRUE)
# Result: 22.79201

# 2. Checking assumptions
# a. Produce a QQ plot for the agewed variable.
qqplot = qplot(sample = GSS$agewed, stat="qq")
qqplot

# b. Perform a Shapiro-Wilk test on the agewed variable.
shapiro.test(GSS$agewed)

# c. What is the variance of agewed for men? What is the variance of agewed for women?
by(GSS$agewed, GSS$sex, var, na.rm=TRUE)
# Male Result: 23.68
# Female Result: 24.30

# d. Perform a Levene’s test for the agewed variable grouped by men and women.
leveneTest(GSS$agewed,GSS$sex,na.rm=TRUE)

# 3a. Pick one metric variable from the dataset (other than agewed) 
# and any other variable of interest to you. 
# Produce a plot or visualization that allows you to look at your two variables in an interesting or informative way.
GSS$tvhours[GSS$tvhours == 99] = NA
p = ggplot(GSS, aes(x=politics,y=tvhours))
p + geom_boxplot() + facet_grid(.~sex)

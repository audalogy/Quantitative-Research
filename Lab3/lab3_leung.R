#Audrey Leung
#Info 271B - Lab 3

# Data Import: Using the GSS dataset.

library(ggplot2)
library(car)
load(url("http://courses.ischool.berkeley.edu/i271b/f14/data/GSS.Rdata"))
summary(GSS)
head(GSS)

###Task 1: Chi-square - Testing for relationships between two categorical variables
### marital status (marital) and political orientation (politics).

#check data
p = ggplot(GSS, aes(x = marital))
p + geom_histogram(binwidth=1)


#set "NA" = NA. NA is factor variable; change to logical variable.
GSS$politics[GSS$politics == "NA"] = NA
GSS$marital[GSS$marital == "NA"] = NA

#check data in table
table(GSS$politics)
table(GSS$marital)

cs = chisq.test(GSS$marital, GSS$politics)
cs

###1c. Effect Size Calculation
cramers_v = function(cs)
{
  cv = sqrt(cs$statistic / (sum(cs$observed) * (min(dim(cs$observed))-1)))
  print.noquote("Cramer's V:")
  return(as.numeric(cv))
}

# run function on our chi-square test 
cramers_v(cs)

###Task 2: Correlation analysis

# Cleaning data to remove "meaningless" values
table(GSS$agewed)
GSS$agewed[GSS$agewed == 0] = NA #not applicable
GSS$agewed[GSS$agewed == 99] = NA #no answer
table(GSS$tvhours)
GSS$tvhours[GSS$tvhours == 98] = NA #don't know
GSS$tvhours[GSS$tvhours == 99] = NA #NA

qqnorm(GSS$agewed)
qqnorm(GSS$tvhours)

# Correlation Test
cor.test(GSS$agewed, GSS$tvhours, na.rm=TRUE)

###Task 3: Create a new binary/dummy variable, “married”
#that denotes whether an individual is currently married or not currently married. 
#Conduct an independent sample t-test to evaluate the hypothesis that number of 
#children (childs) is greater for those who are married than those who are not married. 

# Cleaning data:
table(GSS$childs)
GSS$childs[GSS$childs == 9] = NA #don't know
GSS$marital[GSS$marital == "NA"] = NA
table(GSS$marital)

# Creating dummy variable:
#GSS$married = as.numeric(GSS$marital=="married")
GSS$married = (GSS$marital=="married")
table(GSS$married)

# The means look different between groups
by(GSS$childs, GSS$married, mean, na.rm=TRUE)

## Checking Assumptions:

# Look at distribution as box plot:
plot = ggplot(GSS, aes(x=married, y=childs),na.rm=TRUE)
plot + geom_boxplot()

# QQ Plot suggests this is not normal
qqnorm(GSS$childs)
# The Shapiro test suggests that this is not normal
shapiro.test(GSS$childs)
# Levene Test suggests variances are not equal
leveneTest(GSS$childs, GSS$married)

# Running the one-tailed t-test:
ind.t.test <-t.test(GSS$childs ~ GSS$married, GSS, alternative="less")
ind.t.test

# Practical Significance

t<-ind.t.test$statistic[[1]]
df<-ind.t.test$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
round(r, 3)

# Cohen's d effect size (difference between the means divided by their pooled standard error)

cohens_d <- function(x, y) {
  # this function takes two vectors as inputs, and compares
  # their means
  
  # compute the pooled standard error
  lx = length(x)
  ly = length(y)
  # numerator of the pooled variance:
  num = (lx-1)*var(x, na.rm=T) + (ly-1)*var(y, na.rm=T)
  pooled_var = num / (lx + ly - 2) # variance
  pooled_sd = sqrt(pooled_var)
  
  # compute cohen's d
  cd = abs(mean(x, na.rm=T) - mean(y, na.rm=T)) / pooled_sd
  return(cd)
}

# get the vectors of loggdp for each of our two groups
married_t = GSS$childs[GSS$married=="FALSE"]
married_f = GSS$childs[GSS$married=="TRUE"]

# plug them into our cohen's d function
cohens_d(married_t, married_f)

###Task 4: Consider just the subpopulation of 23-year olds in this sample. 
#Conduct a Wilcox rank-sum test to determine whether your new “married” variable 
#from Task 3 is associated with the number of children (childs) 
#for respondents who are 23 years old. 

# Clean data
table(GSS$age)
GSS$age[GSS$age == 99] = NA #no answer
GSS$age[GSS$age == 98] = NA #don't know
GSS$childs[GSS$childs == 9] = NA #don't know
GSS$marital[GSS$marital == "NA"] = NA

#Assign subpopulation to dummy variable
age23 <- subset(GSS, GSS$age == 23)
table(age23$married)
summary(age23$married)
p = ggplot(age23, aes(x = age23))
p + geom_histogram(binwidth=1)

# Creating dummy variable for married:
age23$married = as.numeric(age$marital=="married")
table(age23$married)
summary(age23$married)

# mean of your new “married” variable among 23-year-olds 
# (e.g., the proportion of cases in the category coded “1”)
mean(age23$married, na.rm=TRUE)

#Wilcox rank-sum test
wilcox.age23<-wilcox.test(age23$childs ~ age23$married) # y ~ x (x needs to be categorical)
wilcox.age23
#get error because the grouping factor only has one level

# Calculate effect size using Cohen's d

cohens_d <- function(x, y) {
  # this function takes two vectors as inputs, and compares
  # their means
  
  # first, compute the pooled standard error
  lx = length(x)
  ly = length(y)
  # numerator of the pooled variance:
  num = (lx-1)*var(x, na.rm=T) + (ly-1)*var(y, na.rm=T)
  pooled_var = num / (lx + ly - 2) # variance
  pooled_sd = sqrt(pooled_var)
  
  # finally, compute cohen's d
  cd = abs(mean(x, na.rm=T) - mean(y, na.rm=T)) / pooled_sd
  return(cd)
}

# use cohen's d effect size for the Wilcoxon rank-sum:
age_married_t = age23$childs[age23$married==1]
age_married_f = age23$childs[age23$married==0]

# plug them into our cohen's d func tion
cohens_d(age_married_t, age_married_f)

# For Cohen's d, effect sizes typically fall into the range 0-.2 (small)
# .3-.5 (medium), and .6-.8 (large). Anything greater than .9 is very large.

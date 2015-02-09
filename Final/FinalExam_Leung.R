### Audrey Leung
### Info 271B Final Exam

## Use packages
library(MASS)
library(car)

## Set working directory
setwd("/Users/Audrey/Dropbox/MIMSFall2014/i271B/Final Exam")

## Load data
Dating = read.csv("dating.csv")  
summary(Dating)
head(Dating)
str(Dating)


## a. Deal with missing values ("Refused" "Other" "Don't know") =================
Dating$life_quality[Dating$life_quality == "Refused"] = NA
Dating$life_quality[Dating$life_quality == "Don't know"] = NA
#Dating$years_in_relationship[Dating$years_in_relationship == "0"] = NA
Dating$years_in_relationship[Dating$years_in_relationship == "Refused"] = NA
Dating$use_internet[Dating$use_internet == "Refused"] = NA
Dating$use_internet[Dating$use_internet == "Don't know"] = NA
Dating$use_internet[Dating$use_internet == " "] = NA

#Convert factor to numeric values
Dating$life_qual = as.numeric(as.character(Dating$life_quality))
Dating$yrsinrel_num = as.numeric(as.character(Dating$years_in_relationship))

##Test data 
#summary(Dating)
#head(Dating$life_quality)
#str(Dating$life_qual)
#table(Dating$life_qual)
table(Dating$years_in_relationship)
table(Dating$yrsinrel_num)
summary(Dating$yrsinrel_num)
table(Dating$use_internet)


## b. reverse the scale for life_quality =======================================
Dating$life_qual_rev = factor(6-Dating$life_qual)
#str(Dating$life_qual_rev)
Dating$life_qual_num = as.numeric(as.character(Dating$life_qual_rev))
#str(Dating$life_qual_num)
mean(Dating$life_qual_num, na.rm=TRUE)

# ^Ans. 3.392921

#1: 110
#2: 335
#3: 762
#4: 618
#5: 407
#Other(0): 8
#Refused: 12

## c. Mean of years_in_relationship =============================================
mean(Dating$yrsinrel_num, na.rm=TRUE)

# ^ Ans. 13.47697

## d. How many cases when you select rows in dataset with no missing values? ===========

lim_rows_prelim = complete.cases(Dating$life_qual_num, Dating$yrsinrel_num)
sum(lim_rows_prelim)
# ^ Ans. 2176  

# Test Data
table(Dating$use_internet)
levels(Dating$use_internet)

# Create dummy variable for use_internet
Dating$use_internet_num = factor(Dating$use_internet)
# Dating$use_internet_num2 = ifelse(Dating$use_internet=="Yes",1,2)
# Dating$use_internet_num3 = (Dating$use_internet=="Yes")
table(Dating$use_internet_num)
# summary(Dating$use_internet_num)

lim_rows = complete.cases(Dating$life_qual_num, Dating$yrsinrel_num, Dating$use_internet_num)
sum(lim_rows)
# ^ Ans. 1090

# Pull those rows out to create a subset of the dataset
Dating_lim = Dating[lim_rows,]

## e. Fit an OLS model to the data from the previous step that predicts life_quality 
# (dependent variable) as a linear function of years_in_relationship (independent variable). 
# What is the slope coefficient you get? 
# Is it statistically significant? 
# What about practically significant?

scatterplot(Dating_lim$life_qual_num, Dating_lim$yrsinrel_num)
hist(Dating_lim$life_qual_num)
hist(Dating_lim$yrsinrel_num)
# First transform years_in_relationship, which is positively skewed
# to make it more normal using simple log transformation
hist(log10(Dating$yrsinrel_num))
# log10 of 0 = undefined, so do not normalize data using log10

# outcome/dependent variable (life quality) ~ predictor/indepdendent/explanatory variable (yrs in relationship)
dating_model1 = lm(life_qual_num ~ yrsinrel_num, data = Dating_lim, na.action=na.exclude)
summary(dating_model1)
# R^2 = summary(dating_model1)$r.squared
# sqrt(summary(dating_model1)$r.squared)
cor.test(Dating$life_qual_num, Dating$yrsinrel_num, method="spearman")

dating_model1a = polr(factor(life_qual_num) ~ yrsinrel_num, data = Dating_lim)
summary(dating_model1a)

dating_model1b = glm(life_qual_num ~ yrsinrel_num, data = Dating_lim, na.action=na.exclude)
summary(dating_model1b)

### Regression Diagnostics =======================================================
plot(dating_model1)

## Outliers 
rstudent(dating_model1)
qqPlot(dating_model1, main="QQ Plot") #qq plot for studentized residual
studentfit <- studres(dating_model1) 
hist(studentfit, freq=FALSE, 
     main="Distribution of Studentized Residuals")

## Influential Cases
cooks.distance(dating_model1)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(Dating_lim)-length(dating_model1$coefficients)-2)
plot(dating_model1, which=4, cook.levels=cutoff)
outlierTest(dating_model1) # Bonferonni p-value for most extreme observations
# Influence Plot 
influencePlot(dating_model1,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Leverage
leveragePlots(dating_model1) # leverage plots
lev = hat(model.matrix(dating_model1))
plot(lev)
Dating_lim[lev >0.02,]

## Nonlinearity Assumption
# component + residual plot 
crPlots(dating_model1)

## Independence Assumption
durbinWatsonTest(dating_model1) 
# -The Durbin-Watson value is 1.99, which is so close to 2 that the assumption of independence has almost certainly been met.

# Confidence Intervals
confint(dating_model1)

### g. Fit a second OLS model to the data. Keep life_quality as your dependent variable,
# but now use both years_in_relationship and use_internet as your explanatory variables. 
# What is the slope coefficient for use_internet? 
# Is it statistically significant? 
# What about practically significant?

dating_model2 = lm(life_qual_num ~ yrsinrel_num + use_internet_num, data = Dating_lim, na.action=na.exclude)
summary(dating_model2)
summary(dating_model2)$r.squared 

dating_model2a = polr(factor(life_qual_num) ~ yrsinrel_num + use_internet_num, data = Dating_lim)
summary(dating_model2a)

dating_model2b = glm(life_qual_num ~ yrsinrel_num + use_internet_num, data = Dating_lim, na.action=na.exclude)
summary(dating_model2b)


### Regression Diagnostics =======================================================
plot(dating_model2)

## Outliers 
rstudent(dating_model2)
qqPlot(dating_model2, main="QQ Plot") #qq plot for studentized residual
studentfit <- studres(dating_model2) 
hist(studentfit, freq=FALSE, 
     main="Distribution of Studentized Residuals")

## Influential Cases
cooks.distance(dating_model2)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(Dating_lim)-length(dating_model2$coefficients)-2)
plot(dating_model2, which=4, cook.levels=cutoff)
outlierTest(dating_model2) # Bonferonni p-value for most extreme observations
# Influence Plot 
influencePlot(dating_model2,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Leverage
leveragePlots(dating_model2) # leverage plots
lev = hat(model.matrix(dating_model2))
plot(lev)
Dating_lim[lev >0.02,]

## Nonlinearity Assumption
# component + residual plot 
crPlots(dating_model2)

## Independence Assumption
durbinWatsonTest(dating_model2) 
# -The Durbin-Watson value is 1.99, which is so close to 2 that the assumption of independence has almost certainly been met.

## Multicollinearity Assumption
vif(dating_model2)
# - since the average VIF is not substantially greater than 1, then the regression is probably not biased

# Confidence Intervals
confint(dating_model2)


### f. Compute the F-ratio and associated p-value between your two regression models. 
# Assess and describe any improvement from your first model to your second.
anova(dating_model1, dating_model2)

#reduction in residual SS is statistically signif. 
#Adding variable reduced residual sum of squares
#p-value tells us what AIC would say. If p-value < .05, then AIC value went down

AIC(dating_model1)
AIC(dating_model2)

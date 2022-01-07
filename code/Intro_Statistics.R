#######################################################################################
#
#
#                                 Class XXX Statistics intro
#                             
#
#
#######################################################################################
######
# See slides on GitHub for theory
#####
########## Descriptive stats
library(dplyr)
head(iris)
data=iris
data[55,4]=NaN
## mean
mean(iris$Sepal.Length) # take mean of the entire column
mean(data$Sepal.Length) # take mean of the entire column
mean(data$Sepal.Length, na.rm = TRUE) # take mean of the entire column, you can remove NaN this way
colMeans(iris[,1:4]) # take mean of every coloumn 
rowMeans(iris[,1:4]) # take mean of every row Note that these functions require NUMERIC data

## Median
median(iris$Sepal.Length)









# remeber you can use dplyr and tidyr functions to help you
summary_stats=iris %>% group_by(Species) %>% summarise(mean_length=mean(Sepal.Length), N=n(), mode_length=mode(Sepal.Length))

########## Exercise 1
##########################################
# R does not have an inbuilt mode function, try building one yourself (note: DescTools pacakge has a function Mode)



## Max, Min, Min k









## Data spread









## Correlations
cor.test(iris$Sepal.Length,iris$Petal.Length) # pearson correlation looks at the relationship between two continupus variables 
# it is a simple ratio of the covariance between the two variables normalized by their standard deviation 
# note that the correlation istelf is just a description, the statistic we get is PARAMETRIC 


# when your data is ordinal or when you want to run a non-parametric test
cor.test(iris$Sepal.Length,iris$Petal.Length, method = "spearman")
# this looks at the monotonic relationship between variables by ranking your data
# does increasing rank in one increase the other variable 

# Confidence intervals 













########## Inferential stats
## T-tests in R 
# All the different kinds of t-test you would like to run can be done with the t.test() function using different inputs 
# let us compare the loudness of music on spotify between groups of explicit vs non-explicity 

# one sample tests:
hist(iris$Sepal.Length)
mean(iris$Sepal.Length)

# let us test if the mean of the Spela Length is different than zero!
t.test(iris$Sepal.Length,var.equal = TRUE)
# Now let us test if it is BIGGER than zero
t.test(iris$Sepal.Length,var.equal = TRUE, alternative = "greater")
# Now let us test if it is LESS than zero
t.test(iris$Sepal.Length,var.equal = TRUE, alternative = "less")
# Now let us test if it is LESS than 7
t.test(iris$Sepal.Length,var.equal = TRUE, alternative = "less", mu = 7)


# two sample tests:

hot100=read.csv('/Users/jasondsc/Documents/GitHub/Sta-R-tistics/data/Hot_100_Audio_Features.csv') # read data
hot100=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100=hot100[!is.na(hot100$loudness),] # remove nas

sample1=hot100$loudness[hot100$spotify_track_explicit==FALSE] # collect sample 1 (non-explicit)
sample2=hot100$loudness[hot100$spotify_track_explicit==TRUE] # collect sample 2 (explicit)

# simple two sample t-test (two-sided) assuming equal variance 
t.test(sample1,sample2, var.equal = TRUE)
# simple two sample t-test (two-sided) un-equal variance 
t.test(sample1,sample2)
# simple two sample t-test (two-sided) assuming equal variance, testing that sample 1 is LOWER than sample 2
t.test(sample1,sample2, var.equal = TRUE, alternative = "less")
# simple two sample t-test (two-sided) assuming equal variance, testing that sample 1 is GREATER than sample 2
t.test(sample1,sample2, var.equal = TRUE, alternative = "greater")

########## Exercise 2
##########################################
# what happens when we lower the N? take a subsample of data from each group and run a t-test three times. Do this for a N of 5, 10, 25, 50, 100
# what do you notice? 


# Let us try paired tests: REMINDER that this means repeated measures (i.e., every observation comes in a PAIR)
# anxiety dataset: two groups were measured across time, looked at their anxiety across treatment 
# comparing the final scores across the two groups is a UNPAIRED test (see above)
# comparing data within a group across time is a PAIRED test
library(datarium)
data("anxiety")
sample1=anxiety$t1[anxiety$group=="grp1"]
sample2=anxiety$t2[anxiety$group=="grp1"]

t.test(sample1,sample2, var.equal = TRUE, paired = TRUE)
# OR YOU CAN RUN THIS: IT IS THE SAME
t.test(sample1-sample2, var.equal = TRUE)


########## Theory Question 
##########################################
# run some code and see if testing a paired test is the same as running a one-sample ttest against zero? Why would this be the case?
# try and reason out your answer 


## assumptions of a t-test
########
# 1. Independence 
# we first assume that the data are independent of one another... this complicated (will discuss in course)
# does a correlation between two variables break this assumption?

# 2. Normality 
# we can test this many ways: first let us look at the data 

hist(iris$Sepal.Length)
hist(hot100$loudness[hot100$spotify_track_explicit==FALSE])
hist(anxiety$t1[anxiety$group=="grp1"])

# we can also run a Shapiro-Wilk test to test for normality 
shapiro.test(iris$Sepal.Length)
shapiro.test(hot100$loudness[hot100$spotify_track_explicit==FALSE]) # is sample size a problem? Why?

# Q-Q plot
# here we plot our datas quantiles against a theortical normal dist quantiles
# we should hopefully see that our data lines up nicely with the theoretical one
# along the x-y diagonal line. Any deviation from this line would indicate some 
# non-normal property of the distribution

qqnorm(iris$Sepal.Length, pch = 1, frame = FALSE)
qqline(iris$Sepal.Length, col = "pink", lwd = 5)

car::qqPlot(iris$Sepal.Length) # same thing different package (possibly 'nicer')

# let us generate some theoretical data
data_sim=rnorm(10,0,1)
qqnorm(data_sim, pch = 1, frame = FALSE)
qqline(data_sim, col = "pink", lwd = 5)
# what happens as you add more samples?
data_sim=runif(10,-5,5)
qqnorm(data_sim, pch = 1, frame = FALSE)
qqline(data_sim, col = "pink", lwd = 5)

data_sim=rpois(10,2)
qqnorm(data_sim, pch = 1, frame = FALSE)
qqline(data_sim, col = "pink", lwd = 5)

# 3. Homogenity of variance 
# checking that the variances of the two groups are the same 
sample1=anxiety$t1[anxiety$group=="grp1"]
sample2=anxiety$t1[anxiety$group=="grp2"]
var.test(sample1,sample2)

## Permutations








## Bootstrapping









## ANOVAs
# let us try a one way anova 
hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
levels(hot100_4anova$key) # these are the levels of your first factor! 
anova0=aov(danceability ~ key, hot100_4anova)

summary(anova0) # summary of effects

# now let us look at post-hoc test
TukeyHSD(anova0)
pairwise.t.test(hot100_4anova$danceability, hot100_4anova$key,
                p.adjust.method = "BH")
# as you can see these are A LOT of post-hoc tests that you need to run....

# let us try another anova:
anova0=aov(danceability ~spotify_track_explicit , hot100_4anova)
summary(anova0) # summary of effects

########## Theory Question 
##########################################
# How is a one-way anova with two groups similar to a t-test? Do you need to run pot-hoc tests after you 
# run an one way anova with only two factors? why? 

# compare it to a t-test
sample1=hot100_4anova$danceability[hot100_4anova$spotify_track_explicit==FALSE] # collect sample 1 (non-explicit)
sample2=hot100_4anova$danceability[hot100_4anova$spotify_track_explicit==TRUE] # collect sample 2 (explicit)

# simple two sample t-test (two-sided) assuming equal variance 
t.test(sample1,sample2, var.equal = TRUE)

## assumptions of an anova
########
anova0=aov(danceability ~ key, hot100_4anova)

summary(anova0) # summary of effects
# 1. Homogeneity of variances
# one check you can do is plot the residulas against the fitted values, there should be no relationship 
plot(anova0, 1)

# you can also run a Levne Test
car::leveneTest(danceability ~ key, hot100_4anova)
# despite a seemingly small if any relationship we get a significant Levene test... WHY?

# 2. Normality
# let us plot a q-qplot
plot(anova0, 2)

# Can check the normality of the residuals 
# Extract the residuals
aov_residuals = residuals(object = anova0 )
hist(aov_residuals)
# Run Shapiro-Wilk test
shapiro.test(aov_residuals )

## Running a two-way anova is just as easy!
# we will look at how explict tracks and key affect dancibility

hot100_4anova=hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary=as.numeric(hot100_4anova$key)>=6

anova0=aov(danceability ~ spotify_track_explicit + key_binary, hot100_4anova)
summary(anova0)

# compute cell means
hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))

# we can also add an interaction term! (note you can use : or * for an interaction, this simply determines if it is fully corssed or not...
# try running a three way interaction and you will notice the difference between the two operators)

anova0=aov(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(anova0)

## Regression
## linear Regression
#########################################
hot100=read.csv('/Users/jasondsc/Documents/GitHub/Sta-R-tistics/data/Hot_100_Audio_Features.csv') # read data

# let us run a regression to test the relationship between two variables
lm0=lm(danceability ~ loudness, hot100)
summary(lm0)
sjPlot::tab_model(lm0)
confint(lm0)
########## Theory Question 
##########################################
# What does a significant intercept mean?
# what does a significant slope mean? 
# How does sample size affect our ability to find effects? Try and use a smaller sample and replicate this effect

# we can add multiple predictors to our models
lm0=lm(danceability ~ loudness + valence + liveness, hot100)
summary(lm0)
sjPlot::tab_model(lm0)

# We can add predictors in the model that we do NOT care about but wish to control for their effect
# these are called covariates and we can 'regress' their effect out of the data
# we can add multiple predictors to our models
lm0=lm(danceability ~ loudness + valence + liveness +tempo, hot100)
summary(lm0)
sjPlot::tab_model(lm0)

# we can save the residuals from a regression and use them for other purposes
# this is the equivelant of removing the effect of tempo on louness for you to then
# use it in another analysis
# Please note that if you run another regression you can cimply build ONE large model while covarying out the effect of tempo (see above) 
lm0=lm(danceability ~ tempo, hot100)
corrected4tempo=lm0$residuals


# we can also test for an interaction term!
lm0=lm(danceability ~ loudness + valence + loudness*valence, hot100)
summary(lm0)
sjPlot::tab_model(lm0)

## How are ANOVAs like Regressions?
# first let us run a regression with binary or categorical predictors 

# we will look at how explict tracks and which key affect dancibility
hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova=hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary=as.numeric(hot100_4anova$key)>=6

# Let us dummy code our variables and save them as factors 
hot100_4anova$key_binary=plyr::mapvalues(hot100_4anova$key_binary, c(TRUE, FALSE), c(1,0))
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(1,0))

hot100_4anova$key_binary=as.factor(hot100_4anova$key_binary)
hot100_4anova$spotify_track_explicit=as.factor(hot100_4anova$spotify_track_explicit)

lm0=lm(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(lm0)
sjPlot::tab_model(lm0)


# compare the outputs of the lm0 and anov0
# how are they different?

anov0= aov(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(anov0)

summary(lm0)
# Look at the cell means? How do they relate to the regression?
hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))


## variable coding
# let us play around with the coding of the effects!
# before we did what is called dummy coding (i.e., 0 and 1s)
# let us try effect coding (i.e., -1 and 1:

hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova=hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary=as.numeric(hot100_4anova$key)>=6

hot100_4anova$key_binary=plyr::mapvalues(hot100_4anova$key_binary, c(TRUE, FALSE), c(1,-1))
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(1,-1))


lm0=lm(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(lm0)

temp=hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))
print(temp)
# Grand mean
mean(temp$m)

# let us try effect coding (i.e., -0.5 and 0.5:

hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova=hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary=as.numeric(hot100_4anova$key)>=6

hot100_4anova$key_binary=plyr::mapvalues(hot100_4anova$key_binary, c(TRUE, FALSE), c(0.5,-0.5))
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(0.5,-0.5))


lm0=lm(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(lm0)

temp=hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))
print(temp)

# Grand mean
mean(temp$m)
# look at slopes from table above (diff from explicit to non-explicit)


## assumptions of an Regression
########
sample1=hot100_4anova[sample(10000,length(hot100_4anova)),]
lm0=lm(danceability ~  key, sample1)
summary(lm0)
# 1. Linearity of relationship
## this can be checked with a residual vs fitted plot. The data should show no relationship 
plot(lm0,1)

# 2. Normality
## qq-plots 
plot(lm0,2)

# 3. Homogenaity of variance 
## can check if the residulas are spread out equally along the range of the predictors
# one potential solution to both normality and heteroscedasticity is a log or square root transform of the data
# try taking the log or square root of your predictors before fitting the model, see what happens
plot(lm0,3)

# 4. Looking for outliers
## we also want to check that the data does not have any outliers, this may affect if our regression will be significant or not 

# Cook's distance
# the rule of thumb would be an observation might be an outlier if it's Cook's distance exceeds 4/(n - p - 1)(P. Bruce and Bruce 2017), 
# where n is the number of observations and p the number of predictor variables.
# look at the plots below and determine if there is an outlier
plot(lm0, 4)
# Residuals vs Leverage
plot(lm0, 5)

## Logistic Regression
#######################
# so far we have looked at predicting continuous y values.... but what happens when the y isn't continuous? 
# let us say we want to predict if a song will be explicit based on some of its properties...

hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(1,0))

glm0= glm(spotify_track_explicit ~ danceability + energy + loudness + valence, hot100_4anova, family = 'binomial')
summary(glm0)
sjPlot::tab_model(glm0) # reports odds ratio
sjPlot::tab_model(glm0, transform = NULL) # reports log odds (see slides for explination)

# we can build a regression model and use it to predict held back data:
ids=sample(length(hot100_4anova$SongID),ceiling(length(hot100_4anova$SongID)/3))
sample1=hot100_4anova[ids,]
sample2=hot100_4anova[!(seq(1,length(hot100_4anova$SongID)) %in% ids),]

lm0= lm(danceability ~  energy + loudness + valence, sample1)

predicted_labels=predict(lm0, sample2) # predict dancibility from regression above
error_predicted=predicted_labels-sample2$danceability # look at the error between true and predicted values 

## Model comparison 
## Heirachical regressions 




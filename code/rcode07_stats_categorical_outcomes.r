############################################################################

# Course:  Introduction to R
# Author:  Wolfgang Viechtbauer (https://www.wvbauer.com)
# License: CC BY-NC-SA 4.0
#
# last updated: 2024-02-19

############################################################################

# restart the R session (Menu 'Session' - 'Restart R')

# read in the code from rcode_helper.r

source("rcode_helper.r")

# load the data

load("data_survey_edit.rdata")

############################################################################

# one-way contingency tables

table(dat$sex)
table(dat$smoke)

# two-way contingency table

table(dat$sex, dat$smoke)
table(sex = dat$sex, smoke = dat$smoke)

# table with margin totals added

addmargins(table(dat$sex, dat$smoke))

# proportions

table(dat$sex)
prop.table(table(dat$sex))

table(dat$sex, dat$smoke)

prop.table(table(dat$sex, dat$smoke))

# note: proportions are computed out of the total sample size

# add margin proportions

addmargins(prop.table(table(dat$sex, dat$smoke)))

# can also compute proportions over rows (1) or columns (2)

prop.table(table(dat$sex, dat$smoke), margin=1)
prop.table(table(dat$sex, dat$smoke), margin=2)

############################################################################

# testing for association between two categorical variables

# chi-square test
# https://en.wikipedia.org/wiki/Pearson's_chi-squared_test#Testing_for_statistical_independence

chisq.test(dat$sex, dat$smoke)

# can also use contingency table as input

chisq.test(table(dat$sex, dat$smoke))

# this can be useful when you get the numbers for a contingency table for
# example from a paper directly

tab <- matrix(c(194,55,148,35), nrow=2, ncol=2, byrow=TRUE)
tab
chisq.test(tab)

# Fisher's exact test
# https://en.wikipedia.org/wiki/Fisher's_exact_test

fisher.test(dat$sex, dat$smoke)
fisher.test(table(dat$sex, dat$smoke))

# can also examine the association between larger tables

table(dat$marital, dat$smoke)
prop.table(table(dat$marital, dat$smoke), margin=1)
chisq.test(dat$marital, dat$smoke)

# but note the warning: the chi^2 test may not be accurate here

# try running Fisher's exact test

fisher.test(dat$marital, dat$smoke)

# but this does not run by default; after consulting the documentation, we can
# get this to run by increasing the 'workspace' value (the default is 200000)

fisher.test(dat$marital, dat$smoke, workspace=20000000)

############################################################################

# logistic regression
# https://en.wikipedia.org/wiki/Logistic_regression

res <- glm(smoke ~ sex, data=dat, family=binomial)
summary(res)

# the outcome variable for logistic regression must be coded 0/1 or can be a
# factor variable (but then you have to figure out what the reference level is,
# so you know which of the two events is being predicted)

factor(dat$smoke)

res <- glm(factor(smoke) ~ sex, data=dat, family=binomial)
summary(res)

# the *second* level is the event that the model is predicting (so, the model
# above is one that predicts the probability, or more precisely the log odds,
# of being a smoker)

# it may be safer to do the coding manually

dat$smoke1 <- ifelse(dat$smoke == "yes", 1, 0)
dat$smoke1

res <- glm(smoke1 ~ sex, data=dat, family=binomial)
summary(res)

# get 95% CI for the model coefficients

confint(res)

# back-transform to odds ratios

coef(res)
exp(coef(res))
exp(confint(res))

data.frame(OR = exp(coef(res)), CI = exp(confint(res)))
round(data.frame(OR = exp(coef(res)), CI = exp(confint(res))), digits=2)

############################################################################

# predictors can be mix of continuous and categorical variables

res <- glm(smoke1 ~ age + sex + pss, data=dat, family=binomial)
summary(res)

# again, can get odds ratios (with corresponding 95% CIs) (and remove intercept)

round(data.frame(OR = exp(coef(res)), CI = exp(confint(res))), digits=2)[-1,]

# compute predicted probabilities

newdat <- data.frame(age = 30, sex = "male", pss = 40)
predict(res, newdata=newdat, type="response")

# note: by default, predict() provides the predicted log odds for a logistic
# regression model; when type="response", it provides the predicted probability

# a three-way interaction

res <- glm(smoke1 ~ age * sex * pss, data=dat, family=binomial)
summary(res)

# full versus reduced model comparison (likelihood ratio test)

res1 <- glm(smoke1 ~ age * sex * pss, data=dat, family=binomial)
res0 <- glm(smoke1 ~ age + sex + pss, data=dat, family=binomial)

anova(res0, res1, test="Chisq")

############################################################################

# ROC curves
# https://en.wikipedia.org/wiki/Receiver_operating_characteristic

# install (if necessary) the 'pROC' package and load it

loadpkg(pROC)

# fit model (note: set na.action=na.exclude, so that the predict() function
# below will return NA for persons where data are missing)

res <- glm(smoke1 ~ age + sex + pss, data=dat, family=binomial, na.action=na.exclude)
summary(res)

# add predicted probabilities to the data frame

dat$pred <- predict(res, type="response")

# create ROC curve

res <- roc(smoke1 ~ pred, data=dat)
res
plot(res)
grid()

# area under the curve

auc(res)

# using the 'Epi' package

loadpkg(Epi)

ROC(dat$pred, dat$smoke1, plot="ROC")

############################################################################

# analysis of 2x2 tables

tab <- table(pred = dat$pred > 0.2, smoker = dat$smoke)
tab

# typically we want the 'outcome of interest' to be in the first column (and we
# want the prediction that a person is a smoker in the first row)

tab <- tab[c(2,1), c(2,1)]
tab

# note: the twoby2() function comes from the 'Epi' package we loaded above

twoby2(tab)

# the 'epiR' package also provides a useful function

loadpkg(epiR)

epi.tests(tab)

# https://en.wikipedia.org/wiki/Sensitivity_and_specificity
# https://en.wikipedia.org/wiki/Confusion_matrix
#
# but careful: on Wikipedia, the 2x2 table uses rows for the actual condition
# and columns for the predicted condition

############################################################################

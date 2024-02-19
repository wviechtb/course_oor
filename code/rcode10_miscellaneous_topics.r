############################################################################

# Course:  Introduction to R
# Author:  Wolfgang Viechtbauer (https://www.wvbauer.com)
# License: CC BY-NC-SA 4.0
#
# last updated: 2024-02-08

############################################################################

# restart the R session (Menu 'Session' - 'Restart R')

# read in the code from rcode_helper.r

source("rcode_helper.r")

############################################################################

# load the data

load("data_survey_edit.rdata")

# inspect the first 6 rows of the dataset

head(dat)

# open the codebook pdf via this script

browseURL("data_survey.pdf")

############################################################################

# attach some description to variables (variable labels)

# a simple solution: using comment()

comment(dat$source) <- "Primary source of stress."
comment(dat$lotr1) <- "In uncertain times, I usually expect the best."
comment(dat$lotr2) <- "If something can go wrong for me, it will."
comment(dat$lotr3) <- "I'm always optimistic about my future."
comment(dat$lotr4) <- "I hardly ever expect things to go my way."
comment(dat$lotr5) <- "I rarely count on good things happening to me."
comment(dat$lotr6) <- "Overall, I expect more good things to happen to me than bad."

# labels do not show up when looking at the dataset

head(dat)

# but we can extract them

comment(dat$source)

# or get all of them

sapply(dat, comment)

# only show labels for variables that have one

res <- sapply(dat, comment)
res <- res[!sapply(res, is.null)]
res

# print as a data frame

res <- data.frame(label=unlist(res))
print(res, right=FALSE)

# there are packages specifically for creating/extracting labels; for example:
#
# https://cran.r-project.org/package=tinylabels
# https://cran.r-project.org/package=labelled

############################################################################

# psychometrics
# https://en.wikipedia.org/wiki/Psychometrics

head(dat)

# install (if necessary) the 'psych' package and load it

loadpkg(psych)

# install (if necessary) the 'GPArotation' package and load it

loadpkg(GPArotation)

# Cronbach's alpha
# https://en.wikipedia.org/wiki/Cronbach's_alpha

# say we want to compute Cronbach's alpha for the lotr scale; then we just
# need to pass the data for the 6 items of this scale to the alpha() function
# from the psych package

alpha(dat[c("lotr1", "lotr2", "lotr3", "lotr4", "lotr5", "lotr6")])

# but having to specify the variables (especially if there are many) in this
# way is tedious; in rcode04.r, we used the grep() function to find the
# position of variables in a dataset

names(dat)
grep("lotr", names(dat))

# careful: variable 68 is the sum score of the 6 items, which we do not want
# to include in the computation of Cronbach's alpha; so we really need to
# search for variable names that include 'lotr' followed by a number; grep()
# actually searches for a 'regular expression', which allows for the
# specification of very complex search patterns; for more details, see:
# https://en.wikipedia.org/wiki/Regular_expression

# can also read more about regular expressions as used in R
help(regexp)

# this will search for 'lotr' followed by a number between 0 and 9

grep("lotr[0-9]", names(dat))

# and now we can do

alpha(dat[grep("lotr[0-9]", names(dat))])

# use the same trick to compute Cronbach's alpha for other scales

alpha(dat[grep("mastery[0-9]", names(dat))])
alpha(dat[grep("pss[0-9]", names(dat))])
alpha(dat[grep("rses[0-9]", names(dat))])

# for the PANAS, we might have to do this manually

alpha(dat[c("panas1", "panas4", "panas6", "panas7", "panas9", "panas12", "panas13", "panas15", "panas17", "panas18")])
alpha(dat[c("panas2", "panas3", "panas5", "panas8", "panas10", "panas11", "panas14", "panas16", "panas19", "panas20")])

# or could do something like this

alpha(dat[paste0("panas", c(1,4,6,7,9,12,13,15,17,18))])
alpha(dat[paste0("panas", c(2,3,5,8,10,11,14,16,19,20))])

# scree plot
# https://en.wikipedia.org/wiki/Scree_plot

sub <- dat[grep("pss[0-9]", names(dat))]

scree(sub, factors=FALSE)

# parallel analysis
# https://en.wikipedia.org/wiki/Parallel_analysis

fa.parallel(sub, fa="fa", n.iter=1000, sim=FALSE)

# principal component analysis (PCA)
# https://en.wikipedia.org/wiki/Principal_component_analysis

principal(sub, nfactors=2, rotate="oblimin")

# exploratory factor analysis (EFA) using principal axis factoring (PAF)
# https://en.wikipedia.org/wiki/Exploratory_factor_analysis

fa(sub, nfactors=2, rotate="oblimin", fm="pa")

############################################################################

# the 'psych' package has a bunch of other useful functions; for example, the
# describe() function provides summary statistics for all quantitative
# variables in a dataset

describe(dat, omit=TRUE)

# the 'skimr' package also creates nice summaries of datasets

# install (if necessary) the 'skimr' package and load it

loadpkg(skimr)

skim(dat)
skim_without_charts(dat) # if there are problems with drawing the mini histograms

# note: for character variables, the information provided is not so useful;
# it helps to turn such variables into factors

dat$sex <- factor(dat$sex)
skim(dat)

############################################################################

# some built-in color palettes

rm(list=ls())

load("data_survey_edit.rdata")

par(mar=c(5,9,4,2))

boxplot(pss ~ marital, data=dat, col=rainbow(8),
        xlab="PSS", ylab="", pch=19, horizontal=TRUE, las=1,
        main="Perceived Stress by Marital Status", boxwex=0.6)

# some of the built-in color palettes

palette.pals()

# plot 8 colors from each palette (and from rainbow)

pals  <- palette.pals()
npals <- length(pals)
cols  <- 8

par(mar=c(2,8,2,2), las=1)
plot(NA, NA, xlim=c(1,8), ylim=c(1,npals+1), xlab="", ylab="", xaxt="n", yaxt="n")
axis(side=2, at=1:(npals+1), labels=c(pals, "Rainbow"))
for (i in 1:npals) {
   points(1:cols, rep(i,cols), pch=19, cex=4, col=palette.colors(cols, palette=pals[i]))
}
points(1:cols, rep(i+1,cols), pch=19, cex=4, col=rainbow(cols))

# now pick one you like

par(mar=c(5,9,4,2))

boxplot(pss ~ marital, data=dat, col=palette.colors(8, palette="Set 2"),
        xlab="PSS", ylab="", pch=19, horizontal=TRUE, las=1,
        main="Perceived Stress by Marital Status", boxwex=0.6)

# palette.colors() is also useful if you quickly need a number of colors for
# points or lines corresponding to different groups (e.g., in a scatterplot)

# there are even more palettes

hcl.pals()

############################################################################

# just in case, clear the workspace

rm(list=ls())

# merge two datasets by a common id

dat1 <- data.frame(id  = c(1, 3, 4, 5, 7),
                   age = c(30, 34, 28, 21, 29),
                   sex = c("f", "m", "m", "f", "m"))

dat2 <- data.frame(id = c(1, 2, 4, 5, 6),
                   pss = c(29, 22, 19, 31, 27))

dat1
dat2

dat <- merge(dat1, dat2, by="id")
dat

dat <- merge(dat1, dat2, by="id", all.x=TRUE)
dat

dat <- merge(dat1, dat2, by="id", all=TRUE)
dat

# a more complex situation where dat2 includes (for some subjects) repeated
# measurements of a particular variable

dat2 <- data.frame(id   = c(1,1,1,2,3,3,4,5,6,6),
                   time = c(1,2,3,1,1,3,1,1,1,2),
                   pss  = c(30,27,22,25,36,22,33,29,39,32))
dat2

dat <- merge(dat1, dat2, all=TRUE, sort=FALSE)
dat
dat <- dat[order(dat$id, dat$time),]
rownames(dat) <- NULL
dat

# combining two datasets containing different subjects (with some common
# variables and possibly with some different variables)

dat1 <- data.frame(id = c(1,2,3), pss = c(30,27,32), rses=c(34,NA,37))
dat2 <- data.frame(id = c(4,5),   pss = c(22,18), smoker=c("no","yes"))
dat1
dat2

# this does not work since the variables in the two datasets are not identical

dat <- rbind(dat1, dat2)

# create variables with NAs for variables not present in a dataset

dat1[setdiff(names(dat2), names(dat1))] <- NA
dat2[setdiff(names(dat1), names(dat2))] <- NA
dat1
dat2

# now bind the rows together (note that the position of the variables does not
# matter, since rbind() correctly figures out how to match things up)

dat <- rbind(dat1, dat2)
dat

# note: dplyr::bind_rows(dat1, dat2) also works without having to first create
# the variables with NAs

############################################################################

# restructure a dataset from wide to long format

dat.wide <- data.frame(subj = 1:5,
                       y1 = c(5,3,6,7,3),
                       y2 = c(4,NA,6,5,4),
                       y3 = c(2,3,4,4,1))
dat.wide

dat.long <- reshape(dat.wide, direction="long", varying=list(2:4),
                    v.names="y", timevar="timepoint", idvar="id")
dat.long

dat.long <- dat.long[order(dat.long$id),]
dat.long

dat.long$id <- NULL
rownames(dat.long) <- NULL
dat.long

# restructure a dataset from long to wide format

dat <- reshape(dat.long, direction="wide", idvar="subj", timevar="timepoint",
               v.names="y", sep="")
dat
rownames(dat) <- NULL
dat

# the reshape package (https://cran.r-project.org/package=reshape) is also
# supposed to be very good for this kind of restructuring

############################################################################

# per-group operations in long format datasets

dat.long

# get the mean of y for each subject (note: this automatically handles NAs)

aggregate(y ~ subj, data=dat.long, FUN=mean)

# this essentially does the same as using by(), but by() returns a vector,
# while aggregate returns a data frame

by(dat.long$y, dat.long$subj, mean)
c(by(dat.long$y, dat.long$subj, mean, na.rm=TRUE))

# add the mean of y for each subject to the dataset

dat.long$ym <- ave(dat.long$y, dat.long$subj, FUN=mean)
dat.long

# unfortunately, cannot use na.rm=TRUE with ave(), so we have to write our own
# little function to compute the mean with na.rm set to TRUE automatically

dat.long$ym <- ave(dat.long$y, dat.long$subj,
                   FUN = function(x) mean(x, na.rm=TRUE))
dat.long

############################################################################

# dichotomizing / categorizing / collapsing

rm(list=ls())

# load the data

load("data_survey_edit.rdata")

# dichotomize a quantitative variable ('median split')

dat$highpa <- ifelse(dat$posaff > median(dat$posaff), 1, 0)
head(dat)
table(dat$highpa)
table(dat$posaff, dat$highpa)

# categorize a quantitative variable

dat$pss
table(dat$pss)

dat$pss.lvl <- ifelse(dat$pss > 25, 1, 0)
table(dat$pss.lvl)

dat$pss.lvl <- cut(dat$pss, breaks=c(0, 20, 30, 40, 50))
table(dat$pss.lvl)

# (0,20] means 'just above 0 to 20 inclusive'

dat$pss.lvl <- cut(dat$pss, breaks=c(0, 20, 30, 40, 50), labels=1:4)
table(dat$pss.lvl)

dat$pss.lvl <- cut(dat$pss, breaks=c(0, 20, 30, 40, 50), labels=c("low", "medium", "high", "very high"))
table(dat$pss.lvl)

# collapse some levels of a categorical variable

dat$stress <- dat$source
table(dat$stress, useNA="always")

dat$stress <- ifelse(dat$stress == "children",       "interpers", dat$stress)
dat$stress <- ifelse(dat$stress == "family",         "interpers", dat$stress)
dat$stress <- ifelse(dat$stress == "friendships",    "interpers", dat$stress)
dat$stress <- ifelse(dat$stress == "spouse/partner", "interpers", dat$stress)
table(dat$stress, useNA="always")

# a shorter way

dat$stress <- NULL
dat$stress <- ifelse(dat$source %in% c("children", "family", "friendships", "spouse/partner"), "interpers", dat$source)
table(dat$stress, useNA="always")

# collapse it further

dat$stress <- ifelse(dat$stress %in% c("health/illness", "lack of time", "life in general", "money/finances"), "other", dat$stress)
table(dat$stress, useNA="always")

############################################################################

rm(list=ls())

# change more than 2 categories into color names

dat <- data.frame(x = c("grp1", "grp2", "grp2", "grp3", NA, "grp1"))
dat

# using nested if-else statements

ifelse(dat$x == "grp1", "red", ifelse(dat$x == "grp2", "blue", "green"))

# using as.numeric(factor())

c("red", "blue", "green")[as.numeric(factor(dat$x, levels=c("grp1", "grp2", "grp3")))]

# can even leave out the as.numeric()

c("red", "blue", "green")[factor(dat$x, levels=c("grp1", "grp2", "grp3"))]

# can even leave out the 'levels' part

c("red", "blue", "green")[factor(dat$x)]

# using with() together with {} and base code

with(dat, {
   cols <- NA
   cols[x == "grp1"] <- "red"
   cols[x == "grp2"] <- "blue"
   cols[x == "grp3"] <- "green"
   cols
})

# this also works nicely when multiple groups should receive the same color name

with(dat, {
   cols <- NA
   cols[x %in% c("grp1", "grp2")] <- "red"
   cols[x %in% "grp3"] <- "green"
   cols
})

# using switch() with sapply()

sapply(dat$x, switch,
   grp1 = "red",
   grp2 = "blue",
   grp3 = "green",
   NA
)

# using car::recode()

loadpkg(car)

recode(dat$x , "
   'grp1' = 'red';
   'grp2' = 'blue';
   'grp3' = 'green'
")

# using dplyr::case_when()

loadpkg(dplyr)

with(dat, case_when(
   x == "grp1" ~ "red",
   x == "grp2" ~ "blue",
   x == "grp3" ~ "green"
))

# let's put this into practice with a real example

loadpkg(palmerpenguins)

# https://allisonhorst.github.io/palmerpenguins/

head(penguins)

# notes:
# - the dataset is a tibble
# - species is already a factor

penguins$species

plot(bill_length_mm ~ flipper_length_mm, data=penguins,
     xlab="Flipper length (mm)", ylab="Bill length (mm)", bty="l",
     pch=19, col=c("darkorange","purple","cyan4")[species])

# so Adelie -> darkorange, Chinstrap -> purple, Gentoo -> cyan4

# same idea also works for using different plotting symbols for the groups

plot(bill_length_mm ~ flipper_length_mm, data=penguins, pch=NA,
     xlab="Flipper length (mm)", ylab="Bill length (mm)", bty="l")

grid()

points(bill_length_mm ~ flipper_length_mm, data=penguins,
       pch=c(19,17,15)[species], col=c("darkorange","purple","cyan4")[species])

# note: we do not have to use penguins$species when using the 'data' argument

legend("bottomright", pch=c(19,17,15), col=c("darkorange","purple","cyan4"),
       legend=c("Adelie","Chinstrap","Gentoo"), bty="n", title="Penguin species")

############################################################################

# string manipulation

# pasting together things as strings

paste("R", "is so awesome!")
paste("Mean flipper length:", round(mean(penguins$flipper_length_mm, na.rm=TRUE), 2))

# say the 'id' variable includes the subject initials and their year of birth

id <- c("DB1965", "PL1967", "ES1975")
id

# want to extract the year values

substr(id, start=3, stop=6)

as.numeric(substr(id, start=3, stop=6))

# but say that some subjects have more than two initials

id <- c("DB1965", "PLK1967", "ES1975")
id

# then the code above doesn't work

as.numeric(substr(id, start=3, stop=6))

# check the string lengths

nchar(id)

# assuming that the last 4 values are always the years values, we can use the
# string length to determine where we need to start extracting substrings

nchar(id)-3

substr(id, start=nchar(id)-3, stop=nchar(id))
as.numeric(substr(id, start=nchar(id)-3, stop=nchar(id)))

# some packages for string manipulations:
# https://cran.r-project.org/package=stringi
# https://cran.r-project.org/package=stringr

############################################################################

# multilevel / mixed-effects modeling
# https://en.wikipedia.org/wiki/Multilevel_model
# https://en.wikipedia.org/wiki/Mixed_model

# nlme package:
# - older, no further development
# - only for general linear mixed-effects models
# - handles serial correlation & heteroscedastic errors
#
# lme4 package:
# - newer, still under active development
# - handles large data sets more efficiently
# - general and generalized linear mixed-effects models
# - handles crossed random effects more easily
#
# both also handle non-linear models
# (there are other packages, but these are the most popular)

library(nlme)

head(Orthodont)
help(Orthodont)

# random intercepts and slopes model

res <- lme(distance ~ age * Sex, random = ~ 1 + age | Subject, data=Orthodont)
summary(res)

############################################################################

# survival analysis
# https://en.wikipedia.org/wiki/Survival_analysis

rm(list=ls())

library(survival)

head(leukemia)
help(leukemia)

# Kaplan-Meier estimator / plot
# https://en.wikipedia.org/wiki/Kaplan–Meier_estimator

res <- survfit(Surv(time, status) ~ x, data = leukemia)
summary(res)

# Kaplan-Meier plot with 95% CI

plot(res, lty = c("dotted", "dashed"), lwd=3)
legend("topright", inset=.02, c("Maintained","Nonmaintained"),
       lty = c("dotted","dashed"), lwd=3)

# Kaplan-Meier plot with 95% CI

plot(res, col=c("blue","red"), lwd=3)
lines(res, conf.int=TRUE, col=c("blue","red"), lty="dotted") # add confidence intervals
legend("topright", inset=.02, c("Maintained","Nonmaintained"),
       lty = c("dotted","dashed"), lwd=3, bg="white")

# log-rank test
# https://en.wikipedia.org/wiki/Logrank_test

survdiff(Surv(time, status) ~ x, data=leukemia)

# Cox proportional hazards regression model
# https://en.wikipedia.org/wiki/Proportional_hazards_model

res <- coxph(Surv(time, status) ~ x, data=leukemia)
summary(res)

# predicted survivor function
# https://en.wikipedia.org/wiki/Survival_function

newdat <- data.frame(x=c("Maintained","Nonmaintained"))
pred <- summary(survfit(res, newdata=newdat))
pred

plot(pred$surv[,1] ~ pred$time, ylim=c(0,1), type="s", col="blue")
lines(pred$surv[,2] ~ pred$time, type="s", col="red")
legend("topright", legend=c("Maintained","Nonmaintained"),
       lty="solid", col=c("blue","red"), inset=.02)

# testing the proportional hazards assumption

cox.zph(res)

############################################################################

# how to replace missing values (e.g., with the mean)

rm(list=ls())

load("data_survey_edit.rdata")

dat$pss

# mean imputation (not a good method ...)

dat$pss[is.na(dat$pss)] <- mean(dat$pss, na.rm=TRUE)
dat$pss

############################################################################

# multiple imputation
# https://en.wikipedia.org/wiki/Imputation_(statistics)#Multiple_imputation

rm(list=ls())

loadpkg(mice)

load("data_survey_edit.rdata")

sub <- dat[c("pss", "age", "smoke", "rses")]

# lm() does listwise deletion when there are missing values

res <- lm(pss ~ age + smoke + rses, data=sub)
summary(res)

# set up predictor matrix (1 means that the variable in the column is used to
# predict the variable in the row, a 0 means that it is not used)

predMatrix <- make.predictorMatrix(sub)
predMatrix

# set up imputation methods vector (pmm = 'predictive mean matching')

impMethod <- make.method(sub)
impMethod

# run mice

imp <- mice(sub, method = impMethod, predictorMatrix = predMatrix, seed = 12345)

# fit model in each imputed dataset

fit <- with(imp, lm(pss ~ age + smoke + rses))

# pool results

pool <- pool(fit)
summary(pool)

# a nice book about multiple imputation and the mice package: van Buuren, S.
# (2018) Flexible imputation of missing data (2nd ed.). Chapman & Hall.
#
# freely available here: https://stefvanbuuren.name/fimd/

############################################################################

# how to change the position of a variable in a dataset

rm(list=ls())

load("data_survey_edit.rdata")

head(dat)

# move 'lotr' variable after 'lotr6' item

loadpkg(dplyr)

dat <- relocate(dat, "lotr", .after = "lotr6")

head(dat)

# note: this could be done without loading any additional packages, but the
# code to do this in an elegant/flexible way gets a bit cumbersome

############################################################################

# how to create an 'axis break'

# just for illustration purposes, make two subjects in the dataset really old

tmp <- dat
tmp$age[1] <- 229
tmp$age[2] <- 237

plot(tmp$age, tmp$pss, pch=19, bty="n",
     xlab="Age", ylab="Stress", xlim=c(0,250), ylim=c(10,50))

library(plotrix)

gap.plot(tmp$age, tmp$pss, pch=19,
         xlab="Age", ylab="Stress", xlim=c(0,250), ylim=c(10,50), bty="n",
         gap=c(104,218), gap.axis="x", xtics=c(0,25,50,75,100,225,250))

############################################################################

# how to obtain standardized regression coefficients

res <- lm(posaff ~ lotr + rses, data=dat)
summary(res)

# create a data frame which includes the variable we need for our model and
# has all rows with at least one missing value removed ('listwise deletion')

tmp <- na.omit(dat[c("posaff", "lotr", "rses")])

# check that we get the same results as before

res <- lm(posaff ~ lotr + rses, data=tmp)
summary(res)

# scale (z-score) the outcome and the predictor variables

res <- lm(scale(posaff) ~ scale(lotr) + scale(rses), data=tmp)
summary(res)

# remove the intercept term from the model (which must be 0 here)

res <- lm(scale(posaff) ~ 0 + scale(lotr) + scale(rses), data=tmp)
summary(res)

# note: we create the 'tmp' dataset above, since scale() standardizes each
# variable seprately and if there are different sets of missings in the
# variables, then the variables would be standardized based on different
# subsets of the data; by first creating 'tmp', we know that all variables are
# standardized based on the same set of subjects (with complete data on the
# variables of interest)

############################################################################

# creating a 'Table 1' for a paper

rm(list=ls())

load("data_survey_edit.rdata")

# install (if necessary) the 'tableone' and 'labelled' packages and load them

loadpkg(tableone)
loadpkg(labelled)

# attach some variable labels to some variables in the dataset

var_label(dat) <- list(age="Age", marital="Marital Status",
                       educ="Educational Level", pss="PSS")

# create Table 1 and print it

tab <- CreateTableOne(vars=c("age", "marital", "educ", "pss"),
                      strata="sex" , data=dat)
print(tab, varLabels=TRUE)

# instead of testing, show standardized mean differences

print(tab, varLabels=TRUE, test=FALSE, smd=TRUE)

# turn marital into a factor and set the reference level

dat$marital <- relevel(factor(dat$marital), ref="single")

# recreate the table

tab <- CreateTableOne(vars=c("age", "marital", "educ", "pss"),
                      strata="sex" , data=dat)
print(tab, varLabels=TRUE, test=FALSE, smd=TRUE)

# turn educ into a factor and set the levels in the desired order

dat$educ <- factor(dat$educ)
levels(dat$educ)
levels(dat$educ)[c(3,5,2,4,6,1)]
dat$educ <- factor(dat$educ, levels=levels(dat$educ)[c(3,5,2,4,6,1)])
levels(dat$educ)

# recreate the table

tab <- CreateTableOne(vars=c("age", "marital", "educ", "pss"),
                      strata="sex" , data=dat)
print(tab, varLabels=TRUE, test=FALSE, smd=TRUE)

# save table as a csv file

out <- print(tab, varLabels=TRUE, test=FALSE, smd=TRUE, quote=FALSE)
write.csv(out, "table1.csv")

# can now import this into Excel, edit if needed, and copy-paste into Word

# the 'table1' package also provides this kind of functionality

loadpkg(table1)

label(dat$age)     <- "Age"
label(dat$marital) <- "Marital Status"
label(dat$educ)    <- "Educational Level"
label(dat$pss)     <- "PSS"

table1(~ age + marital + educ + pss | sex, data=dat)

# this creates an HTML table, which one can copy-paste into Word

############################################################################

# some other techniques/packages/functions:
# - generalized estimating equations: gee, geepack, glmtoolbox
# - time series: ts(), arima()
# - bootstrapping: boot
# - CFA/SEM: lavaan, sem
# - machine learning: nnet, rpart, lasso2, lars, glmnet, elasticnet, randomForest, ...
#
# non-parametric methods:
# - cor() with arguments method="kendall" or method="spearman"
# - wilcox.test() – Wilcoxon rank sum and signed rank tests
# - kruskal.test() – Kruskal-Wallis rank sum test
# - friedman.test() – Friedman rank sum test

############################################################################

# confirmatory factor analysis (CFA) / structural equation modeling (SEM)
# https://en.wikipedia.org/wiki/Confirmatory_factor_analysis
# https://en.wikipedia.org/wiki/Structural_equation_modeling

# install (if necessary) the 'lavaan' package and load it

loadpkg(lavaan)

# load the data

load("data_survey_edit.rdata")

# fit a one-factor CFA model to the items of the PSS scale

model1 <- 'PSS =~ pss1 + pss2 + pss3 + pss4 + pss5 + pss6 + pss7 + pss8 + pss9 + pss10'

res1 <- cfa(model1, data=dat, estimator="ML", std.lv=TRUE)
summary(res1, fit.measures=TRUE, standardized=TRUE)

# fit a two-factor CFA model to the items of the PSS scale

model2 <- '
PSSpos =~ pss1 + pss2 + pss3 + pss6 + pss9 + pss10
PSSneg =~ pss4 + pss5 + pss7 + pss8'

res2 <- cfa(model2, data=dat, estimator="ML", std.lv=TRUE)
summary(res2, fit.measures=TRUE, standardized=TRUE)

# LRT comparing the fit of the two models

anova(res1, res2)

# an example of a structural equation model

model <- '
# measurement model
  RSES =~ rses1 + rses2 + rses3 + rses4 + rses5 + rses6 + rses7 + rses8 + rses9 + rses10
  PSSpos =~ pss1 + pss2 + pss3 + pss6 + pss9 + pss10
  PSSneg =~ pss4 + pss5 + pss7 + pss8
  LOTR =~ lotr1 + lotr2 + lotr3 + lotr4 + lotr5 + lotr6
# regressions
  PSSpos ~ RSES
  PSSneg ~ RSES
  LOTR ~ PSSpos + PSSneg'

res <- sem(model, data=dat, estimator="ML", std.lv=TRUE)
summary(res, fit.measures=TRUE, standardized=TRUE)

# plot the model

loadpkg(lavaanPlot)
lavaanPlot(model=res, coef=TRUE, stars="regress")

# packages semPlot (https://cran.r-project.org/package=semPlot) and tidySEM
# (https://cran.r-project.org/package=tidySEM) also provide functions for
# plotting SEM models

############################################################################

# we have mostly been using 'base R graphics' throughout this course; but
# there are other plotting systems, most notably the 'ggplot2' package

# restart the R session (Menu 'Session' - 'Restart R')

# read in the code from rcode_helper.r

source("rcode_helper.r")

# load the data

load("data_survey_edit.rdata")

dat$stress <- ifelse(dat$source %in% c("children", "family", "friendships", "spouse/partner"), "interpers", dat$source)
dat$stress <- ifelse(dat$stress %in% c("health/illness", "lack of time", "life in general", "money/finances"), "other", dat$stress)

sub <- na.omit(dat[c("stress", "pss", "posaff")])

# based on: https://gist.github.com/andrewheiss/20766a3e7c2c03db48a4c54b5b5fdf39

loadpkg(ggplot2)
loadpkg(ggridges)  # for geom_density_ridges()
loadpkg(patchwork) # for combining multiple plots
loadpkg(ggthemes)  # for lots of themes including theme_few()

# note: theme_gray() is default / theme_few(), theme_bw(), and theme_classic() are also nice

theme_set(theme_few() + theme(plot.title = element_text(face = "bold")))

p1 <- ggplot(sub, aes(x = pss)) +
  geom_histogram(binwidth = 2, color = "white", bg = "gray60") +
  labs(title = "1: geom_histogram()")

p2 <- ggplot(sub, aes(x = stress, fill = stress)) +
  geom_bar() +
  labs(title = "2: geom_bar()") +
  guides(fill = "none")

p3 <- ggplot(sub, aes(x = pss, y = posaff)) +
  geom_point() +
  geom_smooth() +
  labs(title = "3: geom_point() + geom_smooth()")

p4 <- ggplot(sub, aes(x = pss, y = posaff)) +
  geom_hex() +
  guides(fill = "none") +
  labs(title = "4: geom_hex()")

p5 <- ggplot(sub, aes(x = pss, y = posaff)) +
  geom_density_2d() +
  labs(title = "5: geom_density_2d()")

p6 <- ggplot(sub, aes(x = pss, y = stress, fill = stress)) +
  geom_boxplot() +
  guides(fill = "none") +
  labs(title = "6: geom_boxplot()")

p7 <- ggplot(sub, aes(x = pss, y = stress, fill = stress)) +
  geom_violin() +
  guides(fill = "none") +
  labs(title = "7: geom_violin()")

p8 <- ggplot(sub, aes(x = pss, fill = stress)) +
  geom_density(alpha = 0.5) +
  guides(fill = "none") +
  labs(title = "8: geom_density()")

p9 <- ggplot(sub, aes(x = pss, y = stress, fill = stress)) +
  geom_density_ridges() +
  guides(fill = "none") +
  labs(title = "9: ggridges::geom_density_ridges()")

p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(nrow = 3)

# see:  https://ggplot2-book.org
#       https://ggplot2.tidyverse.org

############################################################################

# Some other notes about the tidyverse: https://www.tidyverse.org

# The tidyverse is a collection of R packages that provide an integrated
# workflow for various things you want to do in data science. They also
# provide some new data structures (i.e., 'tibbles') and a different syntax
# that heavily relies on using 'pipes'. To illustrate, let's first install (if
# necessary) and load the 'dplyr' package.

loadpkg(dplyr)

# Say you want to compute the mean of the 'pss' variable in the survey
# dataset. The 'base R' way of doing this would be as follows.

load("data_survey_edit.rdata")

mean(dat$pss, na.rm=TRUE)

# The following is an example of using pipes and using a function called
# pull() that accomplishes the same thing.

dat %>% pull(pss) %>% mean(na.rm=TRUE)

# The idea is that we can read this left to right: dat is passed to pull(),
# which (as the name says) pulls the pss variable from dat, which we then pass
# to the mean() function.

# Or say we want the mean pss values for the male and female subjects. Using
# 'base R', we could accomplish this as follows.

mean(dat$pss[dat$sex == "male"],   na.rm=TRUE)
mean(dat$pss[dat$sex == "female"], na.rm=TRUE)

# Instead, we could use pipes and the filter() and pull() functions.

dat %>% filter(sex == "male")   %>% pull(pss) %>% mean(na.rm=TRUE)
dat %>% filter(sex == "female") %>% pull(pss) %>% mean(na.rm=TRUE)

# Often, sequences using pipes are split up across multiple lines.

dat %>%
   filter(sex == "male") %>%
   pull(pss) %>%
   mean(na.rm=TRUE)

# To learn more, see https://www.tidyverse.org. These RStudio 'cheatsheets'
# are also quite nice and focused on tidyverse packages (but not exclusively):
# https://posit.co/resources/cheatsheets/

############################################################################

# note that a 'pipe operator' was also added to 'base R' (the 'native pipe')

dat |> pull(pss) |> mean(na.rm=TRUE)

# pull() is a function from dplyr; if we want to do thing entirely in 'base R'
# without a tidyverse package, we could do the following

dat |> _$pss |> mean(na.rm=TRUE)

# this looks a bit weird; a bit nicer in my opinion is this

dat |> with(mean(pss, na.rm=TRUE))

############################################################################

# where to go from here? maybe: https://www.bigbookofr.com

############################################################################

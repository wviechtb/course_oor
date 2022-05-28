############################################################################

# Course:  Introduction to R
# Author:  Wolfgang Viechtbauer (https://www.wvbauer.com)
# License: CC BY-NC-SA 4.0
#
# last updated: 2022-05-27

############################################################################

# restart the R session (Menu 'Session' - Restart R)

# read in the code from rcode_helper.r

source("rcode_helper.r")

############################################################################

# psychometrics
# https://en.wikipedia.org/wiki/Psychometrics

# read in data

load("data_survey_edit.rdata")

head(dat)

# install (if necessary) the 'psych' package and load it

loadpkg(psych)

# install (if necessary) the 'GPArotation' package

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

# heatmaps
# https://en.wikipedia.org/wiki/Heat_map

rm(list=ls())

# read in data

load("data_survey_edit.rdata")

# create a dataset for which we want to create a heatmap

mat <- cor(dat[c("age", "lotr", "mastery", "pss", "rses", "posaff", "negaff")],
           use = "complete.obs")
mat
dat <- as.data.frame(mat)
rownames(dat) <- 1:nrow(dat)
dat <- cbind(var=colnames(dat), dat)
dat

# remove first column

dat[2:8]
dat[-1]

# a basic heatmap

heatmap(dat[-1])

# heatmap doesn't take data frames as input; need to supply a matrix

heatmap(as.matrix(dat[-1]))

# add row names based on 'var' variable

heatmap(as.matrix(dat[-1]), labRow=dat$var)

# switch to viridis color scheme

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var)

# reverse the order of the columns

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE)

# no dendograms

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE,
        Rowv=NA, Colv=NA)

# more space for margins (default is c(5,5))

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE,
        Rowv=NA, Colv=NA, margins=c(7,7))

# don't rescale values

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE,
        Rowv=NA, Colv=NA, margins=c(7,7), scale="none")

# add a legend (manually)

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE,
        Rowv=NA, Colv=NA, margins=c(7,16), scale="none")

legend("right", fill=hcl.colors(21), legend=round(seq(min(dat[-1]),max(dat[-1]),length=21), 2))

# install (if necessary) the 'pheatmap' package and load it

loadpkg(pheatmap)

pheatmap(as.matrix(dat[-1]), col=hcl.colors(50), labels_row=dat$var)

pheatmap(as.matrix(dat[-1]), col=hcl.colors(50),
         labels_row=dat$var, fontsize=16)

pheatmap(as.matrix(dat[-1]), col=hcl.colors(50),
         labels_row=dat$var, fontsize=16,
         cluster_rows=FALSE, cluster_col=FALSE)

# install (if necessary) the 'heatmaply' package and load it

loadpkg(heatmaply)

heatmaply(dat[-1])
ggheatmap(dat[-1])

############################################################################

# plot of intensity at x and y coordinates

set.seed(1234)
n <- 200
dat <- data.frame(x = runif(n), y = runif(n))
#dat$intensity <- 0.55 - 1 * (dat$x-0.7)^2 + 1 * (dat$y-0.2)^3 + rnorm(n, 0, .04)
dat$intensity <- 0.55 - 1 * (dat$x-0.7)^2 + 1 * (dat$y-0.23)^3 + rnorm(n, 0, .04)
head(dat)
range(dat$intensity)

plot(dat$x, dat$y, pch=19, col=hcl.colors(100)[round(dat$intensity*100)])

# install (if necessary) the 'akima' package and load it

loadpkg(akima)

# interpolate points

res <- interp(dat$x, dat$y, dat$intensity)

filled.contour(res, color=hcl.colors, xlab="x", ylab="y")

res$z[is.na(res$z)] <- 0

filled.contour(res, color=hcl.colors, xlab="x", ylab="y")

filled.contour(res, color=hcl.colors, xlab="x", ylab="y", nlevels=100)

# something really fancy

loadpkg(plotly)

plot_ly(x = ~ res$x, y = ~ res$y, z = ~ res$z) %>%
add_surface(
   contours = list(
      z = list(
         show = TRUE,
         usecolormap = TRUE,
         highlightcolor = "#ff0000",
         project = list(z=TRUE)
      )
   )
) %>%
layout(
   scene = list(
      camera = list(
         eye = list(x=1.87, y=0.88, z=-0.64)
      )
   )
)

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

############################################################################

# restructure a dataset from wide to long format

dat.wide <- data.frame(subj = 1:5,
                       y1 = c(5,3,6,7,3),
                       y2 = c(4,NA,6,5,4),
                       y3 = c(2,3,4,4,1))
dat.wide

dat.long <- reshape(dat.wide, direction="long", varying=list(2:4),
                    v.names="y", timevar="week", idvar="id")
dat.long

dat.long <- dat.long[order(dat.long$id),]
dat.long

dat.long$id <- NULL
rownames(dat.long) <- 1:nrow(dat.long)
dat.long

# restructure a dataset from long to wide format

reshape(dat.long, direction="wide", idvar="subj", timevar="week",
        v.names = "y", sep="")

############################################################################

# per-group operations in long format datasets

dat.long

# get the mean of y for each subject

aggregate(dat.long$y, by=list(dat.long$subj), FUN=mean)

aggregate(dat.long$y, by=list(dat.long$subj), FUN=mean, na.rm=TRUE)

by(dat.long$y, dat.long$subj, mean)
c(by(dat.long$y, dat.long$subj, mean, na.rm=TRUE))

# add the mean of y for each subject to the dataset

dat.long$ym <- ave(dat.long$y, dat.long$subj, FUN=mean)
dat.long

dat.long$ym <- ave(dat.long$y, dat.long$subj,
                   FUN = function(x) mean(x, na.rm=TRUE))
dat.long

############################################################################

# dichotomizing / categorizing / collapsing

rm(list=ls())

# read in data

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

plot(bill_length_mm ~ flipper_length_mm, data=penguins,
     xlab="Flipper length (mm)", ylab="Bill length (mm)", bty="l",
     pch=c(19,17,15)[species], col=c("darkorange","purple","cyan4")[species])

# note: we do not have to use penguins$species when using the 'data' argument

############################################################################

# string manipulation

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

# set up predictor matrix

predMatrix <- make.predictorMatrix(sub)
predMatrix

# set up imputation methods vector

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

############################################################################

# some other techniques/packages/functions:
# - generalized estimating equations: gee, geepack
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

# we have mostly been using 'base R graphics' throughout this course; but
# there are other plotting systems, most notably the 'ggplot2' package

# restart the R session (Menu 'Session' - Restart R)

# read in data

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
# https://www.rstudio.com/resources/cheatsheets/

############################################################################

# note that a 'pipe operator' was also added to 'base R' (the 'native pipe')

dat |> pull(pss) |> mean(na.rm=TRUE)

# pull() is a function from dplyr; if we want to do thing entirely in 'base R'
# without a tidyverse package, we could do the following

dat |> (function(x) x$pss)() |> mean(na.rm=TRUE)

# this is a bit ugly; could do this

dat |> subset(select=pss, drop=TRUE) |> mean(na.rm=TRUE)

# or with the help of the 'pipebind' package, one can do this

loadpkg(pipebind)

dat |> bind(d, d$pss) |> mean(na.rm=TRUE)

############################################################################

# where to go from here? maybe: https://www.bigbookofr.com

############################################################################

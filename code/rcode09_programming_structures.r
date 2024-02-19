############################################################################

# Course:  Introduction to R
# Author:  Wolfgang Viechtbauer (https://www.wvbauer.com)
# License: CC BY-NC-SA 4.0
#
# last updated: 2023-01-30

############################################################################

# restart the R session (Menu 'Session' - 'Restart R')

# read in the code from rcode_helper.r

source("rcode_helper.r")

############################################################################

# data: growth of orange trees

Orange

help(Orange)

# copy to 'dat' (less typing)

dat <- Orange

# mean circumference per tree

mean(dat$circumference[dat$Tree==1])
mean(dat$circumference[dat$Tree==2])
mean(dat$circumference[dat$Tree==3])
mean(dat$circumference[dat$Tree==4])
mean(dat$circumference[dat$Tree==5])

# much quicker

by(dat$circumference, dat$Tree, mean)

# for() loops (note: have to print explicitly within loops)

for (i in 1:5) {
   print(i)
}

# can use 'indexing variable' i within the loop now

for (i in 1:5) {
   print(mean(dat$circumference[dat$Tree==i]))
}

# saving results computed within the loop

for (i in 1:5) {
   res <- mean(dat$circumference[dat$Tree==i])
}

res # only last value

# set up a vector where we can store the results

res <- rep(NA, 5)
res

for (i in 1:5) {
   res[i] <- mean(dat$circumference[dat$Tree==i])
}

res

############################################################################

# plotting with loops

plot(dat$age, dat$circumference, pch=19, xlab="Days", ylab="Trunk Circumference (mm)")
lines(dat$age, dat$circumference)

# not quite right, so how we can do this in the correct way?

plot(dat$age, dat$circumference, pch=19, xlab="Days", ylab="Trunk Circumference (mm)")

lines(dat$age[dat$Tree==1], dat$circumference[dat$Tree==1])
lines(dat$age[dat$Tree==2], dat$circumference[dat$Tree==2])
lines(dat$age[dat$Tree==3], dat$circumference[dat$Tree==3])
lines(dat$age[dat$Tree==4], dat$circumference[dat$Tree==4])
lines(dat$age[dat$Tree==5], dat$circumference[dat$Tree==5])

# now use a loop

plot(dat$age, dat$circumference, pch=19, xlab="Days", ylab="Trunk Circumference (mm)")

for (i in 1:5) {
   lines(dat$age[dat$Tree==i], dat$circumference[dat$Tree==i])
}

# add a bit of color

plot(dat$age, dat$circumference, pch=19, xlab="Days", ylab="Trunk Circumference (mm)")

cols <- c("blue", "red", "green", "darkgray", "violet")

for (i in 1:5) {
   lines(dat$age[dat$Tree==i], dat$circumference[dat$Tree==i], col=cols[i], lwd=2)
}

points(dat$age, dat$circumference, pch=19)

############################################################################

# another example can be found in rcode_helper.r; the ridgeline() function
# uses a for-loop to estimate the density (using the density() function) for
# each group and then adds the density to the plot

rm(list=ls())

load("data_survey_edit.rdata")

res <- aov(pss ~ marital, data=dat)
res
summary(res)

# visualize densities of the 8 groups

par(mar=c(5,9,4,2))

ridgeline(dat$pss, dat$marital, xlim=c(10,50), xlab="PSS",
          main="Perceived Stress by Marital Status")

dev.off()

############################################################################

# digression: can also create such plots with ggplot2 + ggridges

loadpkg(ggplot2)
loadpkg(ggridges)

ggplot(dat, aes(x = pss, y = marital, fill = marital)) +
   geom_density_ridges() +
   theme_ridges() +
   theme(legend.position = "none")

############################################################################

# if-else statements (a very simple example)

x <- 3

if (x > 5) {
   print("x is bigger than 5.")
} else {
   print("x is not bigger than 5.")
}

############################################################################

# using if() to set options in scripts

dat <- Orange

plot.type <- "bw" # options: "bw", "grayscale", "color"

plot(dat$age, dat$circumference, pch=19, xlab="Days", ylab="Trunk Circumference (mm)")

if (plot.type == "bw") {
   cols <- rep("black", 5)
}

if (plot.type == "grayscale") {
   cols <- c("gray20", "gray50", "gray60", "gray80", "gray90")
}

if (plot.type == "color") {
   cols <- c("blue", "red", "green", "darkgray", "violet")
}

for (i in 1:5) {
   lines(dat$age[dat$Tree==i], dat$circumference[dat$Tree==i], col=cols[i], lwd=2)
}

points(dat$age, dat$circumference, pch=19)

############################################################################

# note: if-else statements are really different than ifelse(); ifelse() is a
# vectorized function that takes a logical vector as input and turns this
# vector into a different vector

load("data_survey_edit.rdata")

ifelse(dat$smoke == "yes", "red", "blue")

# if() only takes a single logical as input and then runs an entire block of
# code (if TRUE) and otherwise the else part (if FALSE) if there is one

############################################################################

# generating random numbers from a given distribution

rnorm(n=4, mean=100, sd=10)

rbinom(n=4, size=10, prob=0.5)

runif(n=4, min=1, max=10)

# rt(), rchisq(), rpois(), rgamma(), …

############################################################################

# a simple Monte Carlo simulation

pvals <- rep(NA, 10000)

for (i in 1:10000) {
   x1 <- rnorm(20, mean=0, sd=1)
   x2 <- rnorm(20, mean=0, sd=1)
   res <- t.test(x1, x2)
   pvals[i] <- res$p.value
}

sig.pvals <- ifelse(pvals <= .05, 1, 0)
mean(sig.pvals)

for (i in 1:10000) {
   x1 <- rnorm(20, mean=0, sd=1)
   x2 <- rnorm(20, mean=0.5, sd=1)
   res <- t.test(x1, x2)
   pvals[i] <- res$p.value
}

sig.pvals <- ifelse(pvals <= .05, 1, 0)
mean(sig.pvals)

# use the 'equation approach' to determine the power of the t-test in this scenario

power.t.test(n=20, delta=0.5, sd=1)

############################################################################

# while() function

iters <- 10000
pvals <- rep(NA, iters)

i <- 1

while (i <= iters) {

   x1 <- rchisq(20, df=1)
   x2 <- rchisq(20, df=1)
   res <- t.test(x1, x2)
   pvals[i] <- res$p.value

   i <- i + 1

}

sig.pvals <- ifelse(pvals <= .05, 1, 0)
mean(sig.pvals)

# plot density of a chi-square distributed variable with df=1

curve(dchisq(x, df=1), lwd=3)

############################################################################

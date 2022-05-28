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

# read in data (note: from now on, we will be working with the edited dataset
# where we reversed coded some items and then computed some sum scores for the
# various scales in the dataset; we did this in rcode04.r, but if you skipped
# this part and do not have the 'data_survey_edit.rdata' file, then you can
# download this edited dataset from the course materials page)

load("data_survey_edit.rdata")

############################################################################

# frequency tables
# https://en.wikipedia.org/wiki/Frequency_distribution

table(dat$marital)

# barplots
# https://en.wikipedia.org/wiki/Bar_chart

barplot(table(dat$marital))

# note: if you get the following error message when trying to create a plot:
#
# Error in plot.new() : figure margins too large
#
# this indicates that the size of the 'plotting device' is too small to
# accommodate the plot you are trying to create; this can happen more easily
# in RStudio because the pane for plots at the bottom right is a bit small; if
# this happen, you just have to make the size of the plotting pane larger

# problem: the x-axis labels might not all be visible

# make the axis labels perpendicular to the axes (for x and y)

barplot(table(dat$marital), las=2)

# problem: the x-axis labels might be too large to fit

# check default margins (order is: bottom, left, top, right)

par()$mar

# make the bottom margin larger (and reduce the top margin)

par(mar=c(9,4,4,2))
barplot(table(dat$marital), las=2)

# horizontal bars

par(mar=c(4,9,4,2))
barplot(table(dat$marital), horiz=TRUE, las=1)

# las=1 to make the axis labels horizontal

# note: par() allows you to query/set various graphical parameters

help(par)

############################################################################

# histograms
# https://en.wikipedia.org/wiki/Histogram

hist(dat$age)

# par() settings stay active for new plots, so close the plotting device

dev.off()

# in RStudio, can also click on 'Clear all Plots' (the broom symbol)

hist(dat$age)

# add an x-axis label and add a proper title

hist(dat$age, xlab="Age", main="Histogram of Age")

# can adjust the x-axis and y-axis limits

hist(dat$age, xlab="Age", main="Histogram of Age", xlim=c(0,100), ylim=c(0,100))

# adjust the number of cells/bins

hist(dat$age, xlab="Age", main="Histogram of Age")
hist(dat$age, xlab="Age", main="Histogram of Age", breaks=5)
hist(dat$age, xlab="Age", main="Histogram of Age", breaks=30)
hist(dat$age, xlab="Age", main="Histogram of Age", breaks=seq(0,100,by=10))

# change the color of the bins

hist(dat$age, xlab="Age", main="Histogram of Age", col="skyblue")

# also change the color of the borders

hist(dat$age, xlab="Age", main="Histogram of Age", col="skyblue", border="white")

############################################################################

# built-in color names

colors()

# colors can also be specified as R (red), G (green), B (blue) hex values
# (https://en.wikipedia.org/wiki/Hexadecimal) or in terms of RGB intensities,
# but note that (by default) the intensities need to be given on a 0-1 scale

# these three are the same

"skyblue"
"#87CEEB"
rgb(135/255, 206/255, 235/255)

# https://www.google.com/search?q=color+picker
# https://duckduckgo.com/?q=color+picker

# how did I know the hex/RGB values for 'skyblue'?

# this gives the RGB intensities on a 0-255 scale

col2rgb("skyblue")

# can convert these intensities to hex values

as.hexmode(c(135, 206, 235))

############################################################################

# boxplots (a bit pointless for a single variable)
# https://en.wikipedia.org/wiki/Box_plot

boxplot(dat$age)

boxplot(dat$age, ylab="Age", main="Boxplot of Age")

#     -+-     <- maximum
#      |
#      |
#   +--+--+   <- 3rd quartile
#   |     |
#   |     |
#   +-----+   <- 2nd quartile (median)
#   |     |
#   +--+--+   <- 1st quartile
#      |
#     -+-     <- minimum

# the lines extending from the box are called the 'whiskers'
#
# individual points (potential outliers) are shown if they are more than 1.5
# times the IQR above the third or below the first quartile; then the whiskers
# extent to the most extreme points that are not outliers

# if you always want the whiskers to extent to the minimum/maximum

boxplot(dat$age, ylab="Age", main="Boxplot of Age", range=0)

############################################################################

# kernel density estimate of a distribution
# https://en.wikipedia.org/wiki/Kernel_density_estimation

density(dat$age)
plot(density(dat$age))

# adjust the 'bandwidth' (values > 1 lead to more smoothness)

plot(density(dat$age, adjust=1.5), main="Distribution of Age")

# superimpose density on top of histogram

hist(dat$age, xlab="Age", main="Histogram of Age", xlim=c(0,100), freq=FALSE)
lines(density(dat$age, adjust=1.5), lwd=3)

############################################################################

# scatterplots
# https://en.wikipedia.org/wiki/Scatter_plot

plot(dat$pss, dat$posaff)

# note: if you get an error ("need finite 'xlim' values" or "'x' and 'y'
# lengths differ") then your dataset probably does not include the 'pss'
# and/or the 'posaff' variable we created earlier (in rode04.r)

# adjust some settings

plot(dat$pss, dat$posaff, xlab="Stress", ylab="Positive Affect",
     main="Scatterplot of Stress versus Positive Affect",
     pch=19, xlim=c(10,50), ylim=c(10,50), col="blue")

# note: 'pch' specifies the plotting symbol

# read more about the possible values for 'pch'

help(points)

# distinguish two groups

ifelse(dat$smoke == "yes", "red", "blue")

plot(dat$pss, dat$posaff, xlab="Stress", ylab="Positive Affect",
     main="Scatterplot of Stress versus Positive Affect",
     pch=19, xlim=c(10,50), ylim=c(10,50),
     col=ifelse(dat$smoke == "yes", "red", "blue"), cex=0.5)

# note: 'cex' is for changing the size of the plotting symbol

# add a legend

legend("topright", inset=.02, pch=c(19,19), cex=0.8,
       col=c("red","blue"), legend=c("smoker","non-smoker"))

# if the x and y values are measured coarsely, we may get overlapping points,
# which can make it more difficult to see any trends/patterns; one solution is
# to indicate the number of overlapping points at a location via the point sizes

# binning of x-y values

res <- xyTable(dat$pss, dat$posaff)
res

# shows how often (number) each unique combination of pss (x) and posaff (y)
# occurred in the dataset; can use this to plot the x and y values from 'res'
# with point sizes proportional to the number of points that overlap at a
# particular location

plot(res$x, res$y, cex=res$number, pch=19, xlab="Stress", ylab="PA")

# make the point sizes a bit smaller

plot(res$x, res$y, cex=res$number*0.2, pch=19, xlab="Stress", ylab="PA")

# note: adjusting 'cex' is akin to adjusting the radius, but the area of a
# circle is proportional to the radius squared; so to make the point areas
# proportional to the number of overlapping points, need to take the square
# root of the number of overlapping points

plot(res$x, res$y, cex=sqrt(res$number)*0.4, pch=19, xlab="Stress", ylab="PA")

# another solution is to 'jitter' the points

head(dat$pss)
head(jitter(dat$pss))

plot(jitter(dat$pss), jitter(dat$posaff), xlab="Stress", ylab="PA", pch=19)

# more jittering (move x and y values randomly by +-0.5)

plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19, cex=0.5)

# note: the jittering is random, so each time you rerun the command above, the
# plot will look (slightly) different; to make the look of the plot fully
# reproducible, we can set the 'seed' of the random number generator before
# creating the plot (the seed number is arbitrary)

set.seed(1234)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19, cex=0.5)

############################################################################

# another possibility: using transparency ('alpha blending')

plot(dat$pss, dat$posaff, xlab="Stress", ylab="PA", pch=19)

# last number for rgb(): 0 = full transparency, 1 = no transparency

plot(dat$pss, dat$posaff, xlab="Stress", ylab="PA", pch=19, col=rgb(0,0,0,.2))

# https://en.wikipedia.org/wiki/RGBA_color_model

############################################################################

# exercise

set.seed(1234)
x <- round(rnorm(10000), 1)
y <- round(0.3 * x + rnorm(10000), 1)
plot(x, y, pch=19)

# now try to use some of the methods above to deal with the overplotting

# ...

# clean up

rm(x, y)

############################################################################

# not recommended: using the 'Export' button under 'Plots' in RStudio (or if
# using R directly, right-clicking on an image and using save/copy); this
# isn't fully reproducible, since the plot will look different depending on
# the size of the plotting pane / window; always use *code* to save plots

# saving a plot (in various formats) (saved to current working directory)

# with pdf(), we open the pdf plotting device; then we create the plot, then
# with dev.off() we close the plotting device (don't forget this last step!)

pdf("plot_stress_vs_posaff.pdf")
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     pch=19, xlab="Stress", ylab="Positive Affect")
dev.off()

# same procedure with other plotting devices

png("plot_stress_vs_posaff.png")
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     pch=19, xlab="Stress", ylab="Positive Affect")
dev.off()

# now you have full control over the width, height, pointsize, resolution,
# etc., and so every time you run this code, you will get the exact same plot

png("plot_stress_vs_posaff.png", width=1000, height=800, pointsize=20)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     pch=19, xlab="Stress", ylab="Positive Affect")
dev.off()

tiff("plot_stress_vs_posaff.tiff", width=1000, height=800, pointsize=20)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     pch=19, xlab="Stress", ylab="Positive Affect")
dev.off()

jpeg("plot_stress_vs_posaff.jpg", width=1000, height=800, pointsize=20)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     pch=19, xlab="Stress", ylab="Positive Affect")
dev.off()

# to get more info on the main 'graphical devices' available

help(device)

# let's consider the figure requirements for a PLOS ONE article:
# https://journals.plos.org/plosone/s/figures
#
# - either tiff or eps (tiffs are easier to work with)
# - dimensions: width and height around 1000-2250 pixels (height up to 2625)
# - resolution: either 300 or 600 dpi
# - for tiffs, use 'lzw' compression
# - text within figures: Arial, Times, or Symbol font
# - no superfluous white space around figure
# - no alpha channels (i.e., no transparency)

tiff("plot_stress_vs_posaff.tiff", width=2000, height=1600, res=300, compression="lzw")

par(mar=c(4.2,4.2,1,1))

set.seed(1234)

plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     pch=19, cex=0.8, xlim=c(10,50), ylim=c(10,50),
     xlab="Stress", ylab="Positive Affect")

dev.off()

# fonts are a tricky issue in general and there can be differences across 
# operating systems (the following commands are specific to different OSs)
#
# Windows typically uses Arial as the default font

windowsFonts()

# the same should go for macOS

quartzFonts()

# under Linux, the default font should be Helvetica

X11Fonts()

# Arial and Helvetica look very similar, so the difference will only be
# noticeable to font-nerds who really care about such things ...

# under Windows, you can try the following: 

# windowsFonts(sans = windowsFont("Comic Sans MS"))
# tiff(...)
# <code to create the figure>
# dev.off()

# under macOS, you can try the following:

# quartzFonts(sans = quartzFont(rep("Comic Sans MS", 4)))
# tiff(...)
# <code to create the figure>
# dev.off()

# under Linux, using family="" should switch the font, so you use:
# tiff(..., family="Arial")
# <code to create the figure>
# dev.off()

############################################################################

# some advanced plotting

# load the 'MASS' package (comes with R, so no need to install it)

library(MASS)

# 2d kernel density estimation
# https://en.wikipedia.org/wiki/Multivariate_kernel_density_estimation

res <- kde2d(dat$pss, dat$posaff)

# error: 'missing or infinite values in the data are not allowed'

# the kde2d() function is a bit picky and does not allow any missing values in
# either the x or the y variable; we can get around this by first creating a
# temporary dataset that just includes the two variables of interest and then
# using na.omit() which omits any rows with missing values

tmp <- dat[c("pss","posaff")]
tmp <- na.omit(tmp)

res <- kde2d(tmp$pss, tmp$posaff)
res

# contour plots
# https://en.wikipedia.org/wiki/Contour_line

contour(res, xlab="Stress", ylab="PA")

contour(res, nlevels=14, xlab="Stress", ylab="PA")

# filled contour plots

filled.contour(res, xlab="Stress", ylab="PA")

filled.contour(res, color=topo.colors, xlab="Stress", ylab="PA")
filled.contour(res, color=gray.colors, xlab="Stress", ylab="PA")
filled.contour(res, color=terrain.colors, xlab="Stress", ylab="PA")
filled.contour(res, color=heat.colors, xlab="Stress", ylab="PA")
filled.contour(res, color=cm.colors, xlab="Stress", ylab="PA")

# use the 'viridis' color palette

filled.contour(res, color=hcl.colors, xlab="Stress", ylab="PA")

# check if filled.contour() has a 'color' argument; what is going on here?

# a 3d perspective plot

persp(res, xlab="Stress", ylab="Positive Affect", zlab="Density")

persp(res, xlab="Stress", ylab="Positive Affect", zlab="Density",
      col="gray80", border="gray50", ticktype="detailed",
      theta=135, phi=25, shade=0.7, ltheta=60)

# let's get fancy

nrz <- nrow(res$z)
ncz <- ncol(res$z)
nbcol <- 100
color <- hcl.colors(nbcol)
zfacet <- res$z[-1, -1] + res$z[-1, -ncz] + res$z[-nrz, -1] + res$z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)

persp(res, xlab="Stress", ylab="Positive Affect", zlab="Density",
      col=color[facetcol], theta=135, phi=35, border="gray50")

# something really fancy

loadpkg(plotly)

add_surface(plot_ly(x = res$x, y = res$y, z = res$z))

# clean up

rm(tmp, res, nrz, ncz, nbcol, color, zfacet, facetcol)

############################################################################

# scatterplots with smoothed densities

smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA")

smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA",
              colramp=hcl.colors, col="white")

smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA",
              colramp=hcl.colors, col="white", nbin=200)

############################################################################

# a 3d scatterplot plot

# install (if necessary) the 'rgl' package and load it

loadpkg(rgl)

plot3d(dat$pss, dat$posaff, dat$negaff, size=5, xlab="Stress", ylab="PA", zlab="NA")

# if this doesn't work, can try running the following and then again plot3d()

options(rgl.printRglwidget = TRUE)

############################################################################

# also check out:
# - https://www.r-graph-gallery.com
# - https://www.data-to-viz.com

############################################################################

# subplots

# set up plotting device with 2 rows and 2 columns

par(mfrow=c(2,2))

par(mar=c(5,5,5,2))
hist(dat$age, xlab="Age", main="(a) Histogram of Age", xlim=c(0,100), ylim=c(0,100))

par(mar=c(4,9,4,2))
barplot(table(dat$marital), horiz=TRUE, las=1, xlab="Frequency", main="(b) Marital Status")

par(mar=c(5,5,5,2))
set.seed(1234)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19, cex=0.5,
     main="(c) Stress versus Positive Affect")

par(mar=c(5,5,5,2))
smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA",
              colramp=hcl.colors, col=NA,
              main="(d) Density of Stress versus PA")

# close plot

dev.off()

# there is also layout() which allows for more complex arrangements

matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE)

layout(matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE))
layout.show(3)

hist(dat$age, xlab="Age", main="(a) Histogram of Age", xlim=c(0,100), ylim=c(0,100))

par(mar=c(4,9,4,2))
barplot(table(dat$marital), horiz=TRUE, las=1, xlab="Frequency", main="(b) Marital Status")

par(mar=c(5,5,5,2))
smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA",
              colramp=hcl.colors, col=NA,
              main="(c) Density of Stress versus PA")

# close plot

dev.off()

# there is yet another way of doing this with split.screen(); we'll skip this

############################################################################

# building up plots by adding elements

# after we have created a plot with a function like plot(), we can add further
# elements (points, lines, text, etc.) to the plot as needed

dat.m <- dat[dat$sex == "male",]
dat.f <- dat[dat$sex == "female",]

#png("stress_pa_association.png", width=2000, height=1900, res=300)

# set up an empty plot but with labeled axes and axis limits set

plot(NA, xlab="Stress", ylab="Positive Affect", xlim=c(10,50), ylim=c(8,50))

# add horizontal and vertical dotted reference lines

abline(h=30, lty="dotted")
abline(v=30, lty="dotted")

# add points for the males and females

set.seed(1234)
points(jitter(dat.m$pss, amount=0.5), jitter(dat.m$posaff, amount=0.5), pch=19, col="#1fc3aa", cex=0.6)
points(jitter(dat.f$pss, amount=0.5), jitter(dat.f$posaff, amount=0.5), pch=19, col="#8624f5", cex=0.6)

# add a title

title("Stress versus Positive Affect")

# add legend

legend("topright", inset=.02, pch=c(19,19), legend=c("male","female"), col=c("#1fc3aa","#8624f5"))

# add an arbitrary (curved) line (just for illustration purposes)

xs <- 10:50
ys <- 65 - 1.8*xs + 0.016 * xs^2
lines(xs, ys, lwd=3)

# add some text

text(10, 50, "Happy People!!!", pos=4, font=2)
text(10,  8, "Flat Affect?!?",  pos=4, font=2)
text(50,  8, "Academics ...",   pos=2, font=2)

#dev.off()

############################################################################

# how to control what is shown on the x- and/or y-axis

plot(NA, xlab="Stress", ylab="Positive Affect", 
     xlim=c(10,50), ylim=c(8,50), xaxt="n")

# xaxt="n" means: do not add the axis tick marks and numbers
# then we add the axis 'manually' (side=1 means: at the bottom)

axis(side=1, at=c(10,30,50))

# you can also change what text is shown at the tick marks

plot(NA, xlab="Stress", ylab="Positive Affect", 
     xlim=c(10,50), ylim=c(8,50), xaxt="n")

axis(side=1, at=c(10,30,50), label=c("Low","Medium","High"))

# use yaxt="n" and axis(side=2, ...) to do the same for the y-axis

# change the type of box that is drawn around a plot

plot(NA, xlab="Stress", ylab="Positive Affect",
     xlim=c(10,50), ylim=c(8,50), bty="l")

plot(NA, xlab="Stress", ylab="Positive Affect",
     xlim=c(10,50), ylim=c(8,50), bty="n")

############################################################################

# some notes:
#
# las: to adjust the orientation of the axis labels (las=1 to make them
#      horizontal, las=2 to make them perpendicular)
# pch: plotting symbol (pch=19 for filled circles; see help(points) for options)
# cex: to adjust the point/text size in plots (the default is 1)
# col: to adjust the color of what is plotted
#
# xlab: to set the x-axis label
# ylab: to set the y-axis label
# main: to set the plot title
# xlim: to set the x-axis limits
# ylim: to set the y-axis limits
#
# par(mar=c(bottom,left,top,right)) to adjust the size of the plot margins;
# the default values are par(mar=c(5.1,4.1,4.1,2.1)), so just change the
# appropriate number to increase/decrease the size of the corresponding margin
#
# par(mfrow=c(2,2)) to split the plotting device into a two rows and two columns
#
# barplot()       for a barplot
# hist()          for a histogram
# boxplot()       for a boxplot
# plot(density()) for a plot of a kernel density estimate
# plot(x,y)       for a scatterplot of variable x versus y
#
# legend()        to add a legend to a plot
# title()         to add a title to a plot
# abline()        to add horizontal/vertical lines to a plot
# points()        to add points to a plot
# lines()         to add lines to a plot
# text()          to add text to a plot

############################################################################

# note: above, we are using 'base R graphics' to create plots; there are other
# plotting systems available for R; an alternative popular choice these days
# is provided by the 'ggplot2' package

loadpkg(ggplot2)

# illustration of a basic scatterplot with ggplot2
ggplot(dat, aes(x=pss, y=posaff, color=sex)) +
    geom_point()

# see:  https://ggplot2-book.org
#       https://ggplot2.tidyverse.org
# also: https://r-graphics.org

############################################################################

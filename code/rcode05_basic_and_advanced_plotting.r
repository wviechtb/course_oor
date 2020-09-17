############################################################################

# restart the R session (Menu 'Session' - Restart R)

# read in data

load("data_survey_edit.rdata")

############################################################################

# frequency tables
# https://en.wikipedia.org/wiki/Frequency_distribution

table(dat$marital)

# barplots
# https://en.wikipedia.org/wiki/Bar_chart

barplot(table(dat$marital))

# note: might get the following error message when trying to create a plot:
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

# add an x-axis label, change the color of the bars, add a title

hist(dat$age, xlab="Age", col="gray", main="Histogram of Age")

# can adjust the x-axis and y-axis limits

hist(dat$age, xlab="Age", col="gray", main="Histogram of Age",
     xlim=c(0,100), ylim=c(0,100))

# adjust the number of cells/bins

hist(dat$age, xlab="Age", col="gray", main="Histogram of Age")
hist(dat$age, xlab="Age", col="gray", main="Histogram of Age", breaks=5)
hist(dat$age, xlab="Age", col="gray", main="Histogram of Age", breaks=30)

############################################################################

# built-in color names

colors()

# colors can also be specified as R (red), G (green), B (Blue) hex values
# (https://en.wikipedia.org/wiki/Hexadecimal) or in terms of RGB intensities,
# but note that (by default) the intensities need to be given on a 0-1 scale

# these three are the same

"skyblue"
"#87CEEB"
rgb(135/255, 206/255, 235/255)

# https://www.google.com/search?q=color+picker
# https://duckduckgo.com/?q=color+picker&ia=answer

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

############################################################################

# kernel density estimate of a distribution
# https://en.wikipedia.org/wiki/Kernel_density_estimation

density(dat$age)
plot(density(dat$age))

# adjust the 'bandwidth' (values > 1 lead to more smoothness)

plot(density(dat$age, adjust=1.5), main="Distribution of Age")

# superimpose density on top of histogram

hist(dat$age, xlab="Age", col="gray", main="Histogram of Age",
     xlim=c(0,100), freq=FALSE)
lines(density(dat$age, adjust=1.5), lwd=3)

############################################################################

# scatterplots
# https://en.wikipedia.org/wiki/Scatter_plot

plot(dat$pss, dat$posaff)

# note: if you get an error ("need finite 'xlim' values" or "'x' and 'y'
# lengths differ") then your dataset probably does not include the 'pss'
# and/or the 'posaff' variable we created earlier (in rode04)

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

legend("topright", legend=c("smoker","non-smoker"), col=c("red","blue"),
       pch=c(19,19), inset=.02, cex=0.8)

# if x and y values are measured coarsely, may get overlapping points, which
# can make it more difficult to see any trends/patterns; one solution is to
# indicate the number of overlapping points at a location via the point sizes

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
# creating the plot (the seed number is completely arbitrary)

set.seed(1234)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19, cex=0.5)

############################################################################

# a small digression on installing packages
#
# the usual command for installing add-on packages is:
#
# install.packages("package_name")
#
# but if you put this in your script, then running the entire script will
# reinstall the package even if it is already installed, which is annoying;
# we can do the following to only install the package if it isn't already
# installed

# install (if not already installed) the 'hexbin' package

if (!suppressWarnings(require(hexbin))) install.packages("hexbin")

# load the 'hexbin' package

library(hexbin)

# another way to deal with overlapping points: binning into hexagon cells

res <- hexbin(dat$pss, dat$posaff)
plot(res, xlab="Stress", ylab="Positive Affect")

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

pdf("plot_stress_vs_posaff.pdf")
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19)
dev.off()

postscript("plot_stress_vs_posaff.eps")
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19)
dev.off()

png("plot_stress_vs_posaff.png")
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19)
dev.off()

png("plot_stress_vs_posaff.png", width=1000, height=800, pointsize=20)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19)
dev.off()

tiff("plot_stress_vs_posaff.tiff", width=1000, height=800, pointsize=20)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19)
dev.off()

jpeg("plot_stress_vs_posaff.jpg", width=1000, height=800, pointsize=20)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19)
dev.off()

# to get more info on the 'graphical devices' available

help(device)

############################################################################

# some advanced plotting

# load the 'MASS' package (comes with R, so no need to install it)

library(MASS)

# 2d kernel density estimation
# https://en.wikipedia.org/wiki/Multivariate_kernel_density_estimation

res <- kde2d(dat$pss, dat$posaff)

# error: 'missing or infinite values in the data are not allowed'

tmp <- dat[c("pss","posaff")]
tmp <- na.omit(tmp)

res <- kde2d(tmp$pss, tmp$posaff, n=100)
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

# install (if necessary) the 'viridis' package and load it

if (!suppressWarnings(require(viridis))) install.packages("viridis")

library(viridis)

# now we have even more color palettes

filled.contour(res, color=viridis, xlab="Stress", ylab="PA")
filled.contour(res, color=magma,   xlab="Stress", ylab="PA")
filled.contour(res, color=inferno, xlab="Stress", ylab="PA")
filled.contour(res, color=plasma,  xlab="Stress", ylab="PA")
filled.contour(res, color=cividis, xlab="Stress", ylab="PA")

# show help for viridis color palettes

help(viridis)

# a 3d perspective plot

persp(res, xlab="Stress", ylab="Positive Affect", zlab="Density")

persp(res, xlab="Stress", ylab="Positive Affect", zlab="Density",
      col="gray80", border="gray50", ticktype="detailed",
      theta=135, phi=25, shade=0.7, ltheta=60)

# let's get really fancy

nrz <- nrow(res$z)
ncz <- ncol(res$z)
nbcol <- 100
color <- hcl.colors(nbcol)
zfacet <- res$z[-1, -1] + res$z[-1, -ncz] + res$z[-nrz, -1] + res$z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)

persp(res, xlab="Stress", ylab="Positive Affect", zlab="Density",
      col=color[facetcol], theta=135, phi=35, border="gray50")

# something really fancy

if (!suppressWarnings(require(plotly))) install.packages("plotly")

library(plotly)

add_surface(plot_ly(x = res$x, y = res$y, z = res$z))

# clean up

rm(tmp, res, nrz, ncz, nbcol, color, zfacet, facetcol)

############################################################################

# scatterplots with smoothed densities

smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA")

smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA",
              colramp=viridis, col="white")

smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA",
              colramp=viridis, col="white", nbin=200)

############################################################################

# a 3d scatterplot plot

# install (if necessary) the 'rgl' package and load it

if (!suppressWarnings(require(rgl))) install.packages("rgl")

library(rgl)

plot3d(dat$pss, dat$posaff, dat$negaff, xlab="Stress", ylab="PA", zlab="NA")

############################################################################

# also check out:
# - https://www.r-graph-gallery.com/
# - https://www.data-to-viz.com/
# - https://rgraphgallery.blogspot.de/

############################################################################

# subplots

# set up plotting device with 2 rows and 2 columns

par(mfrow=c(2,2))

par(mar=c(5,5,5,2))
hist(dat$age, xlab="Age", col="gray", main="(a) Histogram of Age",
     xlim=c(0,100), ylim=c(0,100))

par(mar=c(9,4,5,2))
barplot(table(dat$marital), las=2, main="(b) Marital Status")

par(mar=c(5,5,5,2))
set.seed(1234)
plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19, cex=0.5,
     main="(c) Stress versus Positive Affect")

par(mar=c(5,5,5,2))
smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA",
              colramp=viridis, col=NA,
              main="(d) Density Estimate of Stress versus PA")

# close plot

dev.off()

# there is also layout() which allows for more complex arrangements

matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE)

layout(matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE))
layout.show(3)

hist(dat$age, xlab="Age", col="gray", main="(a) Histogram of Age",
     xlim=c(0,100), ylim=c(0,100))

par(mar=c(9,4,5,1))
barplot(table(dat$marital), las=2, main="(b) Marital Status")

par(mar=c(5,5,5,2))
smoothScatter(dat$pss, dat$posaff, xlab="Stress", ylab="PA",
              colramp=viridis, col=NA,
              main="(c) Density Estimate of Stress versus PA")

# close plot

dev.off()

# there is yet another way of doing this with split.screen(); we'll skip this

############################################################################

# building up plots by adding elements

# after we have created a plot with a function like plot(), we can add further
# elements (points, lines, text, etc.) to the plot as needed

dat.m <- dat[dat$sex == "male",]
dat.f <- dat[dat$sex == "female",]

#png("stress_pa_association.png")

plot(dat.m$pss, dat.m$posaff, pch=19, col="blue", xlab="Stress",
     ylab="Positive Affect", xlim=c(10,50), ylim=c(8,50))

# add points for the females

points(dat.f$pss, dat.f$posaff, pch=19, col="red")

# add a title

title("Stress versus Positive Affect")

# add horizontal and vertical dotted references lines

abline(h=30, lty="dotted")
abline(v=30, lty="dotted")

# add legend

legend("topright", legend=c("female","male"), col=c("red","blue"),
       pch=c(19,19), inset=.02)

# add a (curved) line

xs <- 10:50
ys <- 65 - 1.8*xs + 0.016 * xs^2
lines(xs, ys, lwd=3)

# add some text

text(10, 50, "Happy People!!!", pos=4, font=2)
text(10,  8, "Flat Affect?!?",  pos=4, font=2)
text(50,  8, "Academics ...",   pos=2, font=2)

#dev.off()

############################################################################

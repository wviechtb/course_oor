############################################################################

# Course:  Introduction to R
# Author:  Wolfgang Viechtbauer (https://www.wvbauer.com)
# License: CC BY-NC-SA 4.0
#
# last updated: 2024-02-04

############################################################################

# restart the R session (Menu 'Session' - 'Restart R')

# show installed packages

library()

# another way

installed.packages()

# only show where installed, version, and the 'priority'

installed.packages()[,c("LibPath", "Version", "Priority")]

# the 'base' and 'recommended' packages (see 'Priority' column) are installed
# with R automatically; it is also possible to install an updated version of
# recommended packages (if there is an update)

# in RStudio, there is also the 'Packages' pane (bottom-right)

# list loaded packages (have to explicitly use print())

print(.packages())

# in principle, it is possible to change the startup behavior of R so that it
# automatically loads additional packages, but this is not recommended (when
# sharing a script with others, they will not automatically load the same
# packages, which creates all kinds of confusion)

############################################################################

# number of packages currently available on CRAN

nrow(available.packages())

# growth of number of CRAN data packages over time

dat <- read.table(header=TRUE, colClasses=c("character", "integer", "Date"), text =
"vers  count  date
1.3 110 2001-06-21
1.4 129 2001-12-17
1.5 161 2002-06-12
1.7 219 2003-05-25
1.8 273 2003-11-16
1.9 357 2004-06-05
2.0 406 2004-10-12
2.1 548 2005-06-18
2.2 647 2005-12-16
2.3 739 2006-05-31
2.4 911 2006-12-12
2.5 1000 2007-04-12
2.6 1300 2007-11-16
2.7 1495 2008-03-18
2.8 1614 2008-10-20
2.9 1907 2009-04-17
2.10 2008 2009-10-26
2.13 3000 2011-05-12
2.15 3976 2012-09-21
3.0.2 5000 2013-11-08
3.1 5745 2014-08-22
3.2 6706 2015-06-07
3.2.2 7547 2015-12-07
3.2.3 7969 2016-03-01
3.3.1 9004 2016-08-22
3.3.2 9961 2017-01-28
3.4.3 11991 2017-12-15
3.6.3 15537 2020-04-01
4.0.2 16261 2020-09-15
4.1.2 18544 2021-12-06
4.1.3 18977 2022-03-14
4.2.0 18579 2022-05-27
4.2.2 19003 2023-01-12
4.2.3 19300 2023-03-19
4.3.2 20292 2024-01-23")

par(mar=c(6,5.5,4,2))
par(mgp=c(4,1,0))
plot(dat$date, dat$count, pch=19, cex=1.2, xlab="", ylab="Number of CRAN Packages",
     xaxt="n", yaxt="n", ylim=c(0,21000))
axis(side=1, at=dat$date, label=dat$date, las=2, cex.axis=.7)
axis(side=2, at=seq(0,21000,1000), las=2)
axis(side=3, at=dat$date, label=dat$vers, las=2, cex.axis=.7)
grid(nx=10, ny=10)

res <- loess(count ~ as.numeric(date), data=dat)
pred <- predict(res)
lines(dat$date, pred, lwd=3, col="gray70")

points(dat$date, dat$count, pch=19, cex=1.2)

############################################################################

# all packages available on CRAN:
# https://cran.r-project.org/web/packages/available_packages_by_name.html

# CRAN task views: https://cran.r-project.org/web/views/

# install the 'lme4' package (from CRAN)

install.packages("lme4")

# load the 'lme4' package

library(lme4)

# if you put install.packages("pkg") into your script, this will reinstall the
# package every time you rerun the script; to avoid this, you could put a # in
# front of the install.packages("pkg") so that it turns into a comment

# in rcode_helper.r, there is also a little helper function called loadpkg()
# that first checks if a package is already installed, if not it installs it,
# and then loads the package

# read in the code from rcode_helper.r

source("rcode_helper.r")

loadpkg(lme4)

# terminology:
# - package = book
# - library = place where you store books
# (don't say you are using the 'lme4 library' for your analysis!)

# don't run the following two commands right now, because this could take a
# while to complete in case you already have many packages installed and many
# of them can be updated

# updating packages (will get prompt for each package that can be updated)

update.packages()

# updating packages without getting prompted

update.packages(ask=FALSE)

############################################################################

# search among installed packages (title and description)

help.search("factor analysis")

# search all CRAN packages

RSiteSearch("structural equation")

############################################################################

# install (if necessary) and load the 'sos' package

loadpkg(sos)

# search all packages on CRAN for a term

findFn("structural equation")

############################################################################

# install (if necessary) and load the 'packagefinder' package
# https://www.zuckarelli.de/packagefinder/tutorial.html

loadpkg(packagefinder)

findPackage("structural equation", limit.results = 100)

############################################################################

# install (if necessary) and load the 'CRANsearcher' package

loadpkg(CRANsearcher)

CRANsearcher()

############################################################################

# install (if necessary) and load the 'pkgsearch' package
# https://r-hub.github.io/pkgsearch/

loadpkg(pkgsearch)

pkg_search("structural equation")

# there is also a nicer interface; need to install some packages first

loadpkg(shinyWidgets)
loadpkg(whoami)

pkg_search_addin("structural equation")

############################################################################

# also (potentially) useful:
# - https://rdrr.io
# - https://www.r-pkg.org
# - https://www.rdocumentation.org
# - https://rseek.org

############################################################################

# example: https://cran.r-project.org/package=lme4

# potential indicators of "good" packages:
# - written by known experts in the field
# - package has been around for some time
# - package has been updated
# - listed under one or multiple task views
# - has a 'vignette' or other supporting documentation
# - paper/book about package has been published
# - help files are comprehensive and free of errors
# - has been cited in papers / recommended by peers
# - number of downloads
# - ...

loadpkg(cranlogs)

# examine number of daily downloads for a package
tmp <- cran_downloads(packages="lme4", from="2010-01-01", to="last-day")
head(tmp)
plot(tmp$date, tmp$count, type="l", xlab="Date", ylab="Downloads")

# or use this: https://ipub.com/dev-corner/apps/r-package-downloads/

# Journal of Statistical Software: https://www.jstatsoft.org

# citing R and packages

citation()
citation("lme4")

############################################################################

# where to get help:
# - Google! (or DuckDuckGo or your favorite search engine)
# - https://www.r-project.org/help.html
# - mailing lists: https://www.r-project.org/mail.html
# - read posting guide first: https://www.r-project.org/posting-guide.html
#   - do your homework before posting
#   - provide reproducible (and simple) code that illustrates the problem
# - StackExchange: https://stackexchange.com
#   - https://stackoverflow.com
#   - https://stats.stackexchange.com
# - https://community.rstudio.com (for questions related to RStudio, packages
#   that have been written by RStudio/Posit staff, and esp. tidyverse stuff)
# - try asking ChatGPT (or some other large language model) for an answer
# - R User Group at Maastricht University (RUG@UM):
#   https://wviechtb.github.io/r-user-group/

############################################################################

# a reproducible example is a small self-contained example of the problem you
# are running into that someone can copy-paste directly into R to recreate the
# problem; then it is *much* easier for someone to come up with a solution

# such an example will typically contain data; do not send an attachment or a
# screenshot of your data (or something like that); it would take extra effort
# (writing some R code to import the data, manually typing some data into R)
# to recreate the dataset in R

# you need to provide the data in a way so that the dataset can be directly
# recreated in R; this is where the dput() function comes into play

# say the 'dat' object from above is my dataset I have in R; then you can
# create R code that will recreate the dataset with:

dput(dat)

# so you can take that output and paste it into your email/post like this

dat <- structure(...)

# now anybody can recreate the exact same dataset with this code; so your
# email/post might then look like this

###### From <you> to the mailing list / forum / message board

# Hi all,
#
# I am creating a scatterplot of a date variable on the x-axis versus a count
# variable on the y-axis. Here are my data and the plot so far.

dat <- structure(list(vers = c("1.3", "1.4", "1.5", "1.7", "1.8", "1.9",
"2.0", "2.1", "2.2", "2.3", "2.4", "2.5", "2.6", "2.7", "2.8", "2.9", "2.10",
"2.13", "2.15", "3.0.2", "3.1", "3.2", "3.2.2", "3.2.3", "3.3.1", "3.3.2",
"3.4.3", "3.6.3", "4.0.2", "4.1.2", "4.1.3", "4.2.0"), count = c(110L, 129L,
161L, 219L, 273L, 357L, 406L, 548L, 647L, 739L, 911L, 1000L, 1300L, 1495L,
1614L, 1907L, 2008L, 3000L, 3976L, 5000L, 5745L, 6706L, 7547L, 7969L, 9004L,
9961L, 11991L, 15537L, 16261L, 18544L, 18977L, 18579L), date =
structure(c(11494, 11673, 11850, 12197, 12372, 12574, 12703, 12952, 13133,
13299, 13494, 13615, 13833, 13956, 14172, 14351, 14543, 15106, 15604, 16017,
16304, 16593, 16776, 16861, 17035, 17194, 17515, 18353, 18520, 18967, 19065,
19139), class = "Date")), class = "data.frame", row.names = c(NA, -32L))

plot(dat$date, dat$count, xlab="", ylab="Number of CRAN Packages")

# I am trying to change the x-axis limits so that they go from 2000 to 2025. I
# tried using xlim=c(2000,2025) but this is obviously not correct.

plot(dat$date, dat$count, xlab="", ylab="Number of CRAN Packages", xlim=c(2000,2025))

# Any help/suggestions would be greatly appreciated.

###### From <somebody> replying

# Hi <you>,
#
# Wow, this is such a well-formulated question with a clear reproducible
# example that I will gladly take a bit of time out of my busy schedule to
# help you with this.
#
# The solution is to also use a proper date vector for the x-axis limits. You
# can create such a vector with as.Date(). Try this:

plot(dat$date, dat$count, xlab="", ylab="Number of CRAN Packages",
     xlim=as.Date(c("2000-01-01","2025-01-01")))

# Hope this helps!

###### ... end of story.

# in case the dataset is very large, just post a small part of it that is
# sufficient to create a reproducible example; for example:

dput(dat[1:10,])

############################################################################

# not all packages are on CRAN

# for example Bioconductor: https://www.bioconductor.org

############################################################################

# a note about 'masking': different packages may contain functions that have
# the same name; for example:

library(psych)
library(lavaan)

# - note that it says that function 'cor2cov' has been masked
# - what has happened is that both packages have a function called 'cor2cov'
# - so when you now use the cor2cov function, the one from the lavaan package
#   will be used (i.e., always the one from the package loaded last)
# - but what if you want to use the 'cor2cov' function from the psych package?
# - then you can use psych::cor2cov() to explicitly tell R to use the cor2cov
#   function from the psych package
# - the more packages you load, the more likely it is that two packages will
#   contain functions with the same name and hence that masking will occur
# - to avoid the headaches that this can create, only load packages at the
#   beginning of your script that you really need

############################################################################

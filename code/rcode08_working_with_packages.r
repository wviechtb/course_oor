############################################################################

# restart the R session (Menu 'Session' - Restart R)

# show installed packages

library()

# another way

installed.packages()

# only show where installed, version, and the 'priority'

installed.packages()[,c("LibPath", "Version", "Priority")]

# in RStudio, there is also the 'Packages' pane (bottom-right)

# list loaded packages (have to explicitly use print())

print(.packages())

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
4.0.2 16261 2020-09-15")

par(mar=c(6,5.5,4,2))
par(mgp=c(4,1,0))
plot(dat$date, dat$count, pch=19, cex=1.2, xlab="", ylab="Number of CRAN Packages", xaxt="n", yaxt="n")
axis(side=1, at=dat$date, label=dat$date, las=2, cex.axis=.7)
axis(side=2, at=seq(0,16000,1000), las=2)
axis(side=3, at=dat$date, label=dat$vers, las=2, cex.axis=.7)
grid(nx=10, ny=10)

res <- loess(count ~ as.numeric(date), data=dat)
pred <- predict(res)
lines(dat$date, pred, lwd=2, col="gray70")

points(dat$date, dat$count, pch=19, cex=1.2)

############################################################################

# all packages available on CRAN:
# https://cran.r-project.org/web/packages/available_packages_by_name.html

# CRAN task views: https://cran.r-project.org/web/views/

# install the 'lme4' package (from CRAN)

install.packages("lme4")

# load the 'lme4' package

library(lme4)

# terminology:
# - package = book
# - library = place where you store books
# (don't say you are using the 'lme4 library' for your analysis!)

# don't run the following two commands right now, because this could take a
# while to complete in case you already have many packages installed and many
# of them can be updated

# updating packages

update.packages()

# updating packages without getting prompted

update.packages(ask=FALSE)

############################################################################

# search among installed packages (title and description)

help.search("factor analysis")

# search all CRAN packages

RSiteSearch("structural equation")

# may need to remove the + from the search field and click Search! again

############################################################################

# install (if necessary) and load the 'sos' package

if (!suppressWarnings(require(sos))) install.packages("sos")

library(sos)

# search all packages on CRAN for a term

findFn("structural equation")

# get nothing if there are too many hits :(

findFn("factor analysis")

############################################################################

# install (if necessary) and load the 'packagefinder' package

if (!suppressWarnings(require(packagefinder))) install.packages("packagefinder")

library(packagefinder)

findPackage("structural equation", limit.results = 100)

############################################################################

# install (if necessary) and load the 'CRANsearcher' package

if (!suppressWarnings(require(CRANsearcher))) install.packages("CRANsearcher")

library(CRANsearcher)

CRANsearcher()

############################################################################

# also (potentially) useful:
# - https://rdrr.io/
# - https://www.r-pkg.org/
# - https://www.rdocumentation.org/
# - https://rseek.org/
# - https://crantastic.org/ (no longer available?)

############################################################################

# example: https://cran.r-project.org/package=lme4

# potential indicators of "good" packages:
# - written by a known expert in the field
# - package has been around for some time
# - package has been updated
# - listed under one or multiple task views
# - has a 'vignette' or other supporting documentation
# - paper/book about package has been published
# - help files are comprehensive and free of errors
# - has been cited in papers
# - ...

# Journal of Statistical Software: https://www.jstatsoft.org/

# citing R and packages

citation()
citation("lme4")

############################################################################

# where to get help:
# - Google! (or DuckDuckGo or your favoriate search engine)
# - https://www.r-project.org/help.html
# - mailing lists: https://www.r-project.org/mail.html
# - read posting guide first: https://www.r-project.org/posting-guide.html
#   - do your homework before posting
#   - provide reproducible (and simple) code that illustrates the problem
# - StackExchange: https://stackexchange.com/
#   - https://stackoverflow.com/
#   - https://stats.stackexchange.com/

############################################################################

# not all packages are on CRAN

# for example Bioconductor: https://www.bioconductor.org/

############################################################################

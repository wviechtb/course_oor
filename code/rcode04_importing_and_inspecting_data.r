############################################################################

# restart the R session (Menu 'Session' - Restart R)

# make sure the working directory is set to the directory/folder where the
# script and the data are stored; if not, first set it (see rcode01)

# reading in a rectangular tab-delimited plain-text data file
# - header=TRUE   : first row of the file gives the variable names
# - sep="\t"      : tab is the separator
# - as.is=TRUE    : don't convert strings to factors (the default now)
# - na.strings="" : blank values are interpreted as NA

dat <- read.table("data_survey.dat", header=TRUE,
                  sep="\t", as.is=TRUE, na.strings="")

# note: as long as you don't get an error message, data were read in

# illustrate an error (note that object 'tmp' is not created)

tmp <- read.table("data_survey.dat", header=TRUE,
                  sep=" ", as.is=TRUE, na.strings="")

# see data_survey.pdf for variable info / coding manual

# note: tab-delimited data can be easily exported from other software

# note: it is possible to read in Stata, SPSS, SAS, Excel, etc. files
# directly, but will not get into this for now; see the R Data Import/Export
# manual: https://cran.r-project.org/doc/manuals/R-data.html

# in RStudio, you can also use Menu 'File' - Import Dataset
# - if you use this, make sure you copy-paste the code to your script
# - also you may want to adjust the name of the object this creates

# inspecting large datasets by just printing them isn't very useful

dat

# there is also a (default) maximum to the amount of values that are printed
# (in RStudio, this is set to 1000; in R itself, this is set 99999); we can
# manually change this with the options() function

options(max.print = 99999)

# now RStudio shows all data

dat

# but if there are many variables, then these are wrapped, which is also
# confusing; instead, large datasets are more easily inspected with View()

View(dat)

# in RStudio, can also click on 'dat' in the 'Environment' pane (top right)

# other ways to inspect the data

str(dat)
head(dat)
head(dat, 10)
tail(dat)
ncol(dat)
nrow(dat)
dim(dat)
names(dat)
summary(dat)

# get more information on a quantitative variables

mean(dat$age)
sd(dat$age)
median(dat$age)
quantile(dat$age, probs=c(0.05,0.95))
IQR(dat$age)
min(dat$age)
max(dat$age)
range(dat$age)
max(dat$age) - min(dat$age)
summary(dat$age)
fivenum(dat$age)
table(dat$age)

# what if there are missings?

mean(dat$smokenum)
mean(dat$smokenum, na.rm=TRUE)

############################################################################

# a digression on getting help: how do we know that the mean() function has
# such an argument? read the help file!

help("mean")

# these also usually work

help(mean)
?mean

# in RStudio, can put cursor on command and hit F1

# the structure of help files:
#
# - Description = what does the function do?
# - Usage       = structure of the function (arguments)
# - Arguments   = what exactly do the arguments do?
# - Details     = additional details on the function
# - Value       = what output does the function produce?
# - References  = any references related to the function
# - See Also    = related functions
# - Examples    = examples!
#
# note: Details, Value, See Also, and Examples are optional
#
# also note that arguments often have 'default' values

# note: do not expect to understand everything on the help pages (they can be
# confusing / technical at times); the goal is to improve your understanding
# of a function over time

# help() is a function as well :)

help(help)

############################################################################

# note that the first argument for mean() is called 'x'; so if we want to be
# very explicit, we should use the following syntax

mean(x=dat$smokenum, na.rm=TRUE)

# some people prefer to put spaces before and after =

mean(x = dat$smokenum, na.rm = TRUE)

# the order of the arguments doesn't matter

mean(na.rm=TRUE, x=dat$smokenum)

# but R can also do 'positional matching' of arguments; all *non-named*
# arguments will be matched by their position; so, the following also works

mean(dat$smokenum, na.rm=TRUE)
mean(na.rm=TRUE, dat$smokenum)

# what will the following do? (check the help file for the 'mean' function)

mean(na.rm=TRUE, dat$smokenum, 0.05)

# recommendation: don't rely on positional matching too much, as this can be
# confusing (I usually don't name the very first argument, but otherwise
# explicitly name the other arguments)

# note: don't have to fully write out argument names (as long as this is
# unambiguous); so the following also works fine

mean(dat$smokenum, na=TRUE)

# recommendation: don't rely on abbreviated argument names too much (and this
# isn't really necessary with the tab completion functionality of RStudio)

# get mean of multiple variables

colMeans(dat[c("age", "smokenum")])
colMeans(dat[c("age", "smokenum")], na.rm=TRUE)

# combine some functions with comparisons (i.e., logicals)

dat$age > 30
table(dat$age > 30)
sum(dat$age > 30)
mean(dat$age > 30)
mean(dat$age > 30) * 100
round(mean(dat$age > 30) * 100, digits=2)

# note: TRUE is treated as 1, FALSE is treated as 0

# number of missing values in a variable

sum(is.na(dat$smokenum))
table(is.na(dat$smokenum))

# frequency table (by default) does not show the number of missing values

table(dat$smokenum)

# can show the number of missing values (if there are any or show always)

table(dat$smokenum, useNA="ifany")
table(dat$smokenum, useNA="always")

# mean number of cigarettes smoked by the smokers

mean(dat$smokenum, na.rm=TRUE)

# not correct, because 'smokenum' = 0 for non-smokers

dat$smoke == "yes"
dat$smokenum[dat$smoke == "yes"]
mean(dat$smokenum[dat$smoke == "yes"], na.rm=TRUE)

# alternatively

subset(dat, smoke == "yes", select=smokenum, drop=TRUE)
mean(subset(dat, smoke == "yes", select=smokenum, drop=TRUE), na.rm=TRUE)

# a more complex example

dat$smokenum[dat$smoke == "yes" & dat$age >= 30]
mean(dat$smokenum[dat$smoke == "yes" & dat$age >= 30], na.rm=TRUE)

# not sure if this is easier

mean(subset(dat, smoke == "yes" & age >= 30, select=smokenum, drop=TRUE), na.rm=TRUE)

# frequency table of a categorical variables

table(dat$smoke, useNA="always")
table(dat$marital, useNA="always")

# recode items as needed (see 'data_survey.pdf')

dat$lotr2 <- 6 - dat$lotr2
dat$lotr4 <- 6 - dat$lotr4
dat$lotr5 <- 6 - dat$lotr5

dat$mastery1  <- 5 - dat$mastery1
dat$mastery3  <- 5 - dat$mastery3
dat$mastery4  <- 5 - dat$mastery4
dat$mastery6  <- 5 - dat$mastery6
dat$mastery7  <- 5 - dat$mastery7

dat$pss4  <- 6 - dat$pss4
dat$pss5  <- 6 - dat$pss5
dat$pss7  <- 6 - dat$pss7
dat$pss8  <- 6 - dat$pss8

dat$rses3  <- 5 - dat$rses3
dat$rses5  <- 5 - dat$rses5
dat$rses8  <- 5 - dat$rses8
dat$rses9  <- 5 - dat$rses9
dat$rses10 <- 5 - dat$rses10

# compute a scale total

dat$lotr <- dat$lotr1 + dat$lotr2 + dat$lotr3 + dat$lotr4 + dat$lotr5 + dat$lotr6
head(dat)

# but this can be a lot of typing when there are many items

dat$lotr <- NULL

# a nice trick if the item names have a common substring

names(dat)
grep("lotr", names(dat))

dat[grep("lotr", names(dat))]

dat$lotr <- rowSums(dat[grep("lotr", names(dat))])
head(dat)

# do the same for the other scales

dat$mastery <- rowSums(dat[grep("mastery", names(dat))])
dat$pss     <- rowSums(dat[grep("pss",     names(dat))])
dat$rses    <- rowSums(dat[grep("rses",    names(dat))])

# PANAS has two subscales, one for positive and one for negative affect, so
# the trick above cannot be used (grepping for "panas" returns all items)

# so have to do this the tedious way

dat$posaff <- dat$panas1 + dat$panas4 + dat$panas6 + dat$panas7 +
              dat$panas9 + dat$panas12 + dat$panas13 + dat$panas15 +
              dat$panas17 + dat$panas18
head(dat)

# can save a bit of typing by using the with() command

dat$posaff <- with(dat, panas1 + panas4 + panas6 + panas7 + panas9  +
                        panas12 + panas13 + panas15 + panas17 + panas18)
dat$negaff <- with(dat, panas2 + panas3 + panas5 + panas8 + panas10 +
                        panas11 + panas14 + panas16 + panas19 + panas20)

# saving as a tab-delimited plain-text data file
# - row.names=FALSE : do not add row names (it's not a variable and can lead
#                     to problems when reading in the data in other software)
# - quote=FALSE     : do not put "" around strings
# - sep="\t"        : tab is the separator
# - na=""           : use a blank value for missing (NA) values

write.table(dat, file="data_survey_edit.dat", row.names=FALSE,
            quote=FALSE, sep="\t", na="")

# when saving this way, it should be fairly unproblematic to read in the data
# in other software; note: never overwrite the original data file!

# one can also save data (and any other object) in R's own file format

save(dat, file="data_survey_edit.rdata")

# advantages of .rdata files:
# - data are compressed
# - saving/loading large data files should be quicker
# - all properties of the data/objects are exactly preserved
# disadvantage:
# - cannot read in data in other software

############################################################################

# remove 'dat' from workspace

rm(dat)

# list objects in workspace

ls()

# load data using read.table()

dat <- read.table("data_survey_edit.dat", header=TRUE,
                  sep="\t", as.is=TRUE, na.strings="")

ls()

head(dat)

# remove 'dat' from workspace

rm(dat)

# load data

load("data_survey_edit.rdata")

ls()

head(dat)

# note: here we don't assign what we load to an object; instead, the object
# name(s) are the same as what we saved (note: an .rdata file can contain many
# objects, not just a single data frame)

############################################################################

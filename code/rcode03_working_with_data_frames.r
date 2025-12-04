############################################################################

# Course:  Introduction to R
# Author:  Wolfgang Viechtbauer (https://www.wvbauer.com)
# License: CC BY-NC-SA 4.0
#
# last updated: 2025-11-27

############################################################################

# restart the R session (Menu 'Session' - 'Restart R')

############################################################################

# most datasets are just a bunch of vectors (of the same length) combined into
# what is called a 'data frame'; let's create such an object manually

id  <- c("Bob", "Sue", "John")
age <- c(25, 21, 30)
sex <- c("Male", "Female", "Male")
grp <- c("Trt", "Trt", "Ctrl")

dat <- data.frame(id, age, sex, grp)
dat

# notes:
# - on the very left, we have the row names (not a variable!); by default,
#   they are just consecutive numbers, but don't have to be
# - character variables are not shown with quotes

# note: in the Environment pane, we now have a 'Data' object called 'dat'
#
# can click on 'dat' to view the contents (more useful for larger datasets)
#
# this is not for editing; we do not make any manual changes to our data!
# this is the same as using the View() command

View(dat)

# note: R is case-sensitive

view(dat)

# an important point: objects are not linked

age
dat

age <- c(35, 31, 45)

age
dat

# if you change the 'age' object, then the 'age' variable in 'dat' is unchanged

# R is not Excel ...

# clean up the workspace a bit (keep things tidy!)

ls()

rm(id, age, grp, sex)

ls()

# accessing individual variables within a data frame

dat$id
dat$age
dat$sex
dat$grp

# subsetting (value before the comma is the row, value after is the column)

dat
dat[1,2]

# subsetting (columns)

dat[,2]
dat[,c(1,4)]

# note: if you take a single column, you get a vector

# for data frames, we can also use a special notation to select columns

dat[2]
dat[c(1,4)]

# careful: with this notation, taking a single column returns a data frame
# with that column (and not a vector with the values of that column)

# can also refer to columns by their variable names

dat[,"age"]
dat[,c("id","grp")]

dat["age"]
dat[c("id","grp")]

# subsetting (rows)

dat[1,]
dat[c(1,2),]

# combine selection of rows and columns

dat[1:2, c("id","grp")]

# subsetting with logicals

dat$sex
dat$sex == "Male"
dat[dat$sex == "Male",]

# you can read this as: from 'dat', give me all rows where variable 'sex' from
# 'dat' is equal to 'Male'

# hence the following does not work

dat[sex == "Male",]

# have to be explicit where the variable for the comparison can be found

dat[dat$sex == "Male",]

# note: such subsetting will just return the dataset for the males, but this
# is not a permanent selection; examine object 'dat' again

dat

# if you want to make a permanent selection, you have to assign this output to
# an object (either overwrite the original object or make a new one)

dat.m <- dat[dat$sex == "Male",]
dat.f <- dat[dat$sex == "Female",]
dat.m
dat.f

# note: in R, we can have an unlimited number of objects (including data
# frames) available at the same time (see the Environment pane); this can get
# confusing quickly, so try to keep your workspace tidy (i.e., remove objects
# you no longer need)

rm(dat.m, dat.f)

# using the subset() command

subset(dat, sex == "Male")
subset(dat, age >= 25)
subset(dat, grp == "Trt")

# note: the subset() command is clever enough to look for the variables used
# for the subsetting inside of the dataset itself!

# subset() can also be used to select one or multiple columns

subset(dat, select = c(age, sex))

# can also use this to subset rows and select columns at the same time

subset(dat, sex == "Male", select = c(age, sex))
subset(dat, sex == "Male", select = age)
subset(dat, sex == "Male", select = age, drop = TRUE)

# drop = TRUE to turn the one column data frame into a vector

# add a new variable to a data frame

dat
dat$y <- c(5, 7, 999)
dat

# sort a vector

sort(dat$age)

# sort a data frame

sort_by(dat, ~ age)

# sort_by() was added in R 4.4.0; if you have an older version of R, you can use
# the following (and should upgrade the R version)

dat[order(dat$age),]

# but again, this is not permanent

dat

# to make this permanent, back-assign it

dat <- sort_by(dat, ~ age)
dat

# value replacement (suppose 999 actually stands for missing data)

dat$y
dat$y==999
dat$y[dat$y==999] <- NA
dat

# note: the variable that is being changed does not have to be the same
# variable that is used to select cases

dat$y[dat$id == "John"] <- 8
dat

# there is also the replace() function for the same purpose

dat$y <- replace(dat$y, dat$id == "John", 8)
dat

# rename a variable

dat
names(dat)
names(dat)[1]
names(dat)[1] <- "subject"
dat

# but in large datasets, counting variable names to figure out the position of
# the variable that you want to rename would be very tedious

names(dat)
names(dat) == "age"
names(dat)[names(dat) == "age"] <- "years"
dat

# remove a variable from a data frame

dat
dat$y <- NULL
dat

# generate a new variable based on an existing one

dat
dat$days <- dat$years * 365
dat

# sum/mean of several variables

dat$y1 <- c(2, 4, 3)
dat$y2 <- c(5, 5, 1)
dat

dat$ysum  <- dat$y1 + dat$y2
dat$ymean <- (dat$y1 + dat$y2) / 2
dat

# there are special functions for this

dat$ysum  <- NULL
dat$ymean <- NULL
dat

dat[c("y1","y2")]
rowSums(dat[c("y1","y2")])
rowMeans(dat[c("y1","y2")])

dat$ysum  <- rowSums(dat[c("y1","y2")])
dat$ymean <- rowMeans(dat[c("y1","y2")])
dat

# what if there are missing values?

dat$y1[2] <- NA
dat

# then the resulting mean/sum will also be NA

dat$ysum  <- rowSums(dat[c("y1","y2")])
dat$ymean <- rowMeans(dat[c("y1","y2")])
dat

# can avoid this with the 'na.rm' argument (set it to TRUE); then the mean or
# sum is taken over the non-missing values within each row

dat$ysum  <- rowSums(dat[c("y1","y2")], na.rm=TRUE)
dat$ymean <- rowMeans(dat[c("y1","y2")], na.rm=TRUE)
dat

# subsetting when there are missing values

dat
dat$y1 >= 2
dat[dat$y1 >= 2,] # not good :(

# easier

dat$y1 >= 2
which(dat$y1 >= 2)
dat[which(dat$y1 >= 2),]

# easiest

subset(dat, y1 >= 2)

############################################################################

# quick summary of the bracket notation:
#
# say 'x' is a vector (could also be something like dat$x), then we can use []
# to select one or multiple elements from that vector (e.g., x[2] or x[1:3])
#
# say 'dat' is a data frame, then we can use:
# - dat[] to select one or more columns (e.g., dat[3] or dat["age"])
# - dat[row(s),column(s)] to select one or more rows and one or more columns
#   (e.g., dat[1:3,3] or dat[1:3,"age"])
# - when leaving out row(s) or column(s), then this means to select all rows
#   or columns (e.g., dat[1:3,] or dat[,"age"])
#
# often we use 'logicals' for selection/subsetting (e.g., dat[dat$age > 21,])

############################################################################

# a few other object types

# matrices: similar to data frames (i.e., have rows and columns), but all
# elements in a matrix must be of the same type (numeric, character, etc.)

# arrays: similar to matrices but can have more than two dimensions

# lists: a collection of objects (components); a list allows you to gather a
# variety of (possibly unrelated) objects under one name

# example of a list with 4 components

w <- list(name="Fred", age=24, grades=c(7,8,6,9,5,6),
          address=c("14 Pine Ave, Nicetown", "104 South Street, Bad City"))
w

# note: data frames are really just a special case of lists, where each
# component is of the same length

# factors: a special data type for nominal (categorical) variables

gender <- c("Male","Male","Male","Male","Female","Male","Male","Female","Male")
gender
gender <- factor(gender)
gender

# internally, factors are stored as integers that are mapped to the levels
# (here: 1 = Female, 2 = Male)

# R now treats gender as a nominal variable

summary(gender)

############################################################################

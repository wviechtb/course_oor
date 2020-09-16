############################################################################

# restart the R session (Menu 'Session' - Restart R)

# most datasets are just a bunch of vectors (of the same length) combined into
# a 'data frame'; let's create such an object manually

id  <- 1:3
age <- c(25, 21, 30)
sex <- c("Male", "Male", "Female")
grp <- c("T", "C", "T")

dat <- data.frame(id, age, sex, grp)
dat

# notes:
# - on the very left, we have the row names (not a variable!)
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

# R is not Excel ...

# clean up the workspace a bit (keep things tidy!)

ls()

rm(id, age, grp, sex)

ls()

# accessing individual variables

dat$id
dat$age
dat$sex
dat$grp

# note: in older versions of R, putting a character variable (e.g., sex, grp)
# into a data frame would turn it into a 'factor' (more on factors later)

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

# note: taking a single column returns a data frame with that column

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

# note: this will just return the dataset for the males, but this is not a
# permanent selection; examine object 'dat' again

dat

# if you want to make a permanent selection, you have to assign this output to
# an object (either overwrite the original object or make new ones)

dat.m <- dat[dat$sex == "Male",]
dat.f <- dat[dat$sex == "Female",]
dat.m
dat.f

# note: in R, we can have an unlimited number of objects (including data
# frames) available at the same time (see Environment pane); this can get
# confusing quickly, so try to keep your workspace tidy

rm(dat.m)
rm(dat.f)

# using the subset() command

subset(dat, sex == "Male")
subset(dat, age >= 25)
subset(dat, grp == "T")

# overwrite the original object if you really don't need the rest of the data
# anymore further in the script (but don't do this now!)
#
# dat <- subset(dat, sex == "Male")

# subset() can subset rows and also select one or multiple columns

subset(dat, sex == "Male", select = age)
subset(dat, sex == "Male", select = c(age, sex))
subset(dat, sex == "Male", select = age, drop=TRUE)

# add a new variable to a data frame

dat$y <- c(5, 7, 999)
dat

# make a copy of an object

dat2 <- dat
dat2

# sort a vector

dat2$age
sort(dat2$age)
dat2

# as before, this is not a permanent change unless you 'back assign' it

dat2$age <- sort(dat2$age)
dat2

# note: this is NOT the right way to sort a data frame; it just sorts the age
# variable within dat2, but all of the other variables are unchanged!

rm(dat2)

# how to sort a data frame

dat
order(dat$age)

dat[order(dat$age),]
dat

dat <- dat[order(dat$age),]
dat

# value replacement (suppose 999 actually stands for missing data)

dat$y
dat$y==999
dat$y[dat$y==999] <- NA
dat

dat$age[dat$age > 24] <- 24
dat

dat$y[dat$age <= 21] <- 4
dat

dat$y[3] <- 8
dat

dat$y[dat$id == 2] <- 5
dat

# rename a variable

dat
names(dat)
names(dat)[1]
names(dat)[1] <- "subject"
dat

names(dat)
names(dat) == "age"
names(dat)[names(dat) == "age"] <- "years"
dat

# remove a variable from a data frame

dat
dat$y <- NULL
dat

# generate a new variable based on an existing one

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

dat$ysum  <- rowSums(dat[c("y1","y2")])
dat$ymean <- rowMeans(dat[c("y1","y2")])
dat

# what if there are missing values?

dat$y1[2] <- NA
dat

# then the resulting mean/sum will also be NA

dat$ymean <- rowMeans(dat[c("y1","y2")])
dat

# can avoid this with the 'na.rm' argument (set it to TRUE); then the mean or
# sum is taken over the non-missing values within each row

dat$ymean <- rowMeans(dat[c("y1","y2")], na.rm=TRUE)
dat

# subsetting when there are missing values

dat
dat$y1 >= 2
dat[dat$y1 >= 2,] # not good

# better but complex

is.na(dat$y1)
!is.na(dat$y1)
!is.na(dat$y1) & dat$y1 >= 2
dat[!is.na(dat$y1) & dat$y1 >= 2,]

# easier

dat$y1 >= 2
which(dat$y1 >= 2)
dat[which(dat$y1 >= 2),]

# easiest

subset(dat, y1 >= 2)

# a couple other examples

dat
subset(dat, !is.na(y1))
subset(dat, subject != 1)

############################################################################

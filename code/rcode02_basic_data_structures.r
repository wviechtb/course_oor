############################################################################

# just in case, clear the workspace

rm(list=ls())

# or even better: at the beginning of each new script, and more generally,
# whenever you switch to a different project/analysis, restart the R session
# (Menu 'Session' - Restart R)

# we start with some basic data structures

# scalars

x <- 2.5

# you can read this as: 'assign the value 2.5 to an object called x'

x

# can also use = but better use <-

x = 5
x

# note: 'x' is overwritten (when 'x' is an existing object name)

# character strings need to be in quotes and can have spaces

x <- "Hello World!"
x

# numeric vectors

x <- c(2,5,4)
x

# note: c() is the 'combine' function

# can add spaces

x <- c(2, 5, 4)
x

# character/string vectors

x <- c("Male", "Male", "Female", "Female")
x

# quickly create a vector of consecutive numbers

x <- c(20:100)
x

# actually don't even need c() here

x <- 20:100
x

# note: if the vector is too long to fit into a single line, the output is
# wrapped and the numbers in brackets (e.g., [1]) indicate the position of
# the value that comes next

# if you mix numbers and strings, you get a character vector

x <- c("Bob", "Sue", "John", 2, 5)
x

# sidenote: this is called 'type coercion' and R uses a set of rules to do so
# (for better or for worse ...)

# logicals (TRUE/FALSE are special keywords)

x <- c(TRUE, FALSE, FALSE)
x

# can be abbreviated to T/F, but better avoid this

x <- c(F, F, T)
x

# comparisons (note that they lead to logicals)

x <- c(2, 4, 6, 3, 5)
x

x > 3
x >= 3
x < 3
x <= 3
x == 3
x != 3

# note the == for comparing each element in x with the number 3; if you would
# use x = 3, then you would assign 3 to x, which is not what we want

# != means 'not equal to'

# and/or (the parentheses are not necessary here, but make the code clearer)

(x > 3) & (x < 6)
(x < 3) | (x > 5)

# missing values

# say x is equal to 2 and 4 for the first and third person, but unknown for
# the second person; how can we specify this?

x <- c(2,,4)

# nope!

x <- c(2, ,4)

# nope!

x <- c(2, NA, 4)
x

# note: NA = not available (another special keyword)

# object/variable names:
# - must begin with a letter
# - contain alphanumeric symbols (A-Z, a-z, 0-9)
# - can also use . and _
# - are case-sensitive
# - no (relevant) restriction on length
# names should not:
# - correspond to commands (confusion/errors)
# - contain spaces

# check if something is a command: simply type the name and run this

chicken
mean

# if it says "Error: object '...' not found", then you know that it is not a
# command (or some existing object)

# better avoid naming objects after commands -> confusing!

mean <- c(2, 3, 4)
mean

mean(mean)

# ahhhhhh!

# remove the 'mean' vector

rm(mean)

# sometimes we may come across 'named' vectors

age <- c(25, 21, 30)
age

age <- c("Bob"=25, "Sue"=21, "John"=30)
age

# can also simplify this

age <- c(Bob=25, Sue=21, John=30)
age

############################################################################

# basic arithmetic with + - * / ^ log() exp() and so on
# be careful with order of operations (use parentheses as needed)

2 * 5 - 4 / 2 + 4
2 * ((5 - 4) / 2) + 4

-2^2
(-2)^2

log(2)   # natural log
log10(2) # log to base 10

exp(1)

# a few special cases

2/0
(-2)/0
0/0

# Note: NaN = not a number (https://en.wikipedia.org/wiki/NaN)

# scientific notation

100000000
10^8
0.00000001
10^-8

# if you find scientific notation confusing

options(scipen=100)

100000000
0.00000001

# if you want to, you can set 'scipen' back to the default (0)

options(scipen=0)

# note: changes to the options are not permanent (if you restart R/RStudio,
# the default settings are in effect)

############################################################################

# let's check if our computers are broken

sqrt(2)

sqrt(2) * sqrt(2)

sqrt(2) * sqrt(2) - 2

# what is going on here?

print(sqrt(2) * sqrt(2), digits=18)

# wtf?!?

# this is actually a FAQ: https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f

############################################################################

# vectorized operations

# this means that a command does something to every element of an object

x <- c(2,4,3,5,7)
x

x * 2
x^2
log(x)

# an example of a non-vectorized operation

mean(x)

# an example where we use vectorized and non-vectorized functions

x
length(x)
x - mean(x)
(x - mean(x))^2
sum((x - mean(x))^2)
sum((x - mean(x))^2) / (length(x)-1)
sqrt(sum((x - mean(x))^2) / (length(x)-1))

sd(x)

############################################################################

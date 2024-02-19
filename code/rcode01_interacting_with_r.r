############################################################################

# Course:  Introduction to R
# Author:  Wolfgang Viechtbauer (https://www.wvbauer.com)
# License: CC BY-NC-SA 4.0
#
# last updated: 2024-02-19

############################################################################

# start RStudio

# open this script: Menu 'File' - Open File (or Ctrl+o / Command+o)

# elements ('panes') of RStudio:
# - top left:     Script Editor
# - bottom left:  Console
# - top right:    Environment, Command History, Connections
# - bottom right: File Browser, Plots, Packages, Help, Viewer

# first check that it says "R version 4.3.2" in the Console; if not, you do
# not have the current version of R installed (maybe you should update,
# especially if the version you have installed is quite old ...)

############################################################################

# using R interactively

# click on the Console (bottom left pane) and type in:

x <- c(4,2,3,6)
mean(x)

# x <- c(4,2,3,6) means: combine the numbers 4, 2, 3, and 6 (into a 'vector')
# and put this collection of numbers into an object called 'x'

# to see the contents of an object, just type its name

x

# that is in fact just a shortcut for using the print() function

print(x)

# note: object 'x' is listed under 'Environment' in RStudio (top right pane)

# the 'Environment' lists all objects in your 'workspace'; these can be simple
# things like 'x', datasets, the results from some statistical analysis, etc.

# try out tab completion (works in the Console and Editor):
# - type 'box' - should get a list with possible matches (may have to hit tab)
# - type 'p' - now 'boxplot' should be the first option
# - hit tab (or enter); should complete to 'boxplot'
# - now complete with '(x)' so we get

boxplot(x)

# in the Console, the up/down arrows scroll through your 'command history';
# hit Escape to stop showing commands from your command history

############################################################################

# running commands from a script file

# you can run a command from the script by putting the cursor in the same line
# as the command and then using the keyboard shortcut:
# - Windows: Ctrl+Enter
# - macOS:   Command+Enter
#
# try this out with the x <- c(4,2,3,6) and mean(x) lines above
#
# note that the cursor automatically moves to the next line each time you use
# the shortcut, so this way you can quickly run through a bunch of commands
#
# you can also select/highlight multiple lines and run them all at once
#
# note: tab completion and running commands from the script will not work if
# RStudio doesn't recognize that you are working with an R script (e.g., if
# the file extension is .txt instead of .r or .R); you can manually change the
# file type at the bottom right corner of the script editor (change it to 'R
# Script') but the more appropriate fix is to change the file extension (to .r
# or .R); under the 'File' menu, you can use 'Rename' to do so

############################################################################

# nesting of commands

vx <- var(x)
vx
sqrt(vx)

sqrt(var(x))

# so one can nest multiple commands inside of each other

# list objects in the environment (see also 'Environment' in RStudio)

ls()

# remove objects

rm(x, vx)

# now what are the objects in the environment?

ls()

# character(0) stands for an empty 'character vector' (more on this later)

# close R/RStudio (remember to choose 'no' when asked to save the workspace)

quit()

# or just close the window

# to avoid the prompt for saving the workspace when closing R/RStudio

quit(save="no")

############################################################################

# restart RStudio

# the script should automatically load (i.e., RStudio automatically reopens
# the script(s) you last worked on)

# list objects in the environment

ls()

# if ls() shows lots of objects in your workspace, then apparently you saved
# the workspace at some point in the past; RStudio automatically restores it

# to remove all objects from your workspace

rm(list=ls())

# (can also go to Menu 'Session' - 'Clear Workspace')

# to delete the (hidden) file that contains the workspace

unlink(".RData")

# my recommendations:
# Menu 'Tools' - 'Global Options':
# - uncheck 'Restore .RData into workspace at startup'
# - set 'Save workspace to .RData on exit' to 'Never'

# depending on your computer / OS, double-clicking on an .r (.R) file can
# automatically load an R script in RStudio (you might have to configure your
# computer first to always use RStudio for opening .r (.R) files)

############################################################################

# working directory

# the 'working directory' is the directory (i.e., folder) where R will look
# for files (e.g., datasets you want to load) or where it will save files to
# (e.g., graphs you want to save so that they can be imported into a paper or
# presentation; we will learn how to do this later on)

# check your working directory

getwd()

# if this is *not* the directory/folder where you put the course materials:
#
# Menu 'Session' - 'Set Working Directory' - 'To Source File Location'
#
# this sets the working directory to the location of the script (note that
# this actually runs the setwd() command with the correct location)

# check your working directory again

getwd()

# another approach: in the 'Files' tab in the bottom right pane, click your
# way to the directory/folder with the materials, then click 'More' (or the
# symbol that looks like a gear), and select 'Set As Working Directory'

# don't forget to save the script once in a while (Ctrl+s / Command+s) and add
# comments to the script as needed

############################################################################

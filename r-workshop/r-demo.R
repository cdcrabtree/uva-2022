#### R Demo ####

# There are 4 panes: your source code (where you can type and save code), the console (where code is run), the 
# files/plots pane (I usually leave this on the plots tab),  and the environment (where things are kept)
# Go to RStudio > Preferences > Pane Layout.
# You can change the panes to how you prefer or leave as is.

# Use # to make a comment in R (a line of text that won't run as code). Get in the habit of carefully commenting your 
# script so that it is easier for you and other people to understand later. 
# Here's a good guide on code comments https://bookdown.org/ndphillips/YaRrr/a-brief-style-guide-commenting-and-spacing.html

# when I add four #### at the end of a line, it creates a clickable subheading below ####

# everyone try making a subheading now ####

# This is an R script, i.e. a plain text file that allows you to write a lot of code at once and save for later. 
# You can also write code directly into the console, but don't do this. The problem is that this code isn't saved, which can make it difficult (read: impossible) for you to recreate your work.

#### Set your working directory ####
# A working directory is a file path that lets R know where to look for files and data you reference and where to put 
# any output you want to save (like a plot or new dataset that you created in R).

# if you have a Chromebook and are using rstudio cloud, then your working directory is basically just the cloud storage 
# space and you don't have to run this step. 

setwd("~/Dropbox/git/uva-2022/r-workshop")

# To run a command in R, place your cursor on the line of code and hit Ctrl + Enter in Windows or Cmd + Enter on a Mac.

#### Load packages ####

# Packages are extensions to R that enable additional functionality. 
# R comes with many built-in functions, but the packages extend these functions. 
# After you set the working directory and before you load your data, load the packages you need.

### Load packages by calling them from the library.
library(foreign)

# If you've never used a package before, you may have to install it first:
install.packages("library")
install.packages("readstata13")
library(readstata13)

# Once you've installed a package, you never have to do it again! (Unless you need to update it.) To load the package, 
# just use library(). You need to load libraries once in every new R session.

#### Using R as a Calculator ####

## Try inputting the following commands. 
## After typing each line of code, hit the return key to display the results.
2 * 2
(5-7) / 96
sqrt(2014)
2014^2

## Let's take the natural log of 100. 
## Like most things, R gives users at least a couple means to accomplish a task.
log(100, base=10)
log(100, 10)

#### Using R to Generate Data ####

## Let's generate a numeric vector.
c(1, 2, 3, 4)
10:4000
seq(1, 4)

## Let's create some more vectors.
-1:2
seq(2, 8, by = 2)
seq(0, 1, length = 5)
seq(0, 42, length = 6)
rep(42, 3)
rep(c(1, 2, 3), 10)

## R allows us to easily manipulate vectors.
c(1, 2, 3, 4)/2
seq(0, 1000, by = 10)/3.14

## Now let's generate some character vectors.
c("Germany", "France", "Spain", "Italy")
rep(rep(c('fish', 'bird'), c(2, 1)), 100)

## Finally, let's create some logical vectors.
c(TRUE, FALSE, TRUE, FALSE)
rep(c(F, T))
as.numeric(c(TRUE, FALSE, TRUE, FALSE))

## By now, you have probably noticed that R commands often take the form 'x()'. 
## These commands are called functions. We will use them a lot. 
## R has built-in help for functions. You can call this help in two ways.
help(rep)
?rep

## Honestly, the built-in help is not that great. 
## When in trouble with R, use google, or post the problem to StackExchange (http://stackoverflow.com/questions/tagged/r)

## Now we know how to create data. We often want to reuse it. 
## R is "object-oriented" meaning variables, data frames, models, outputs are all stored as objects in working memory. 
## So, we need to assign the data to variables. We use '<-' to do that. 
## Some people use '=', but this practice is discouraged.
variable.name <- c(1, 2, 3)
another.variable.name <- rep(c(1, 2), 100)
twos <- rep(2, 100)

## We can call variables by typing their name.
variable.name
twos

## We can also perform operations on variables.
another.variable.name*5
another.variable.name/twos

## We often need to create vectors of random numbers. Let's create some random data.
random.numbers <- rnorm(mean = 0, sd = 1, n = 1000) # what does rnorm do?
random.numbers
summary(random.numbers)
random.numbers.2 <- rbinom(n = 500, trials = 10, prob = 0.3) # what does rbinom do?
table(random.numbers.2)
random.numbers.3 <- runif(min = 0, max = 1, n = 500) # what does runif do?
summary(random.numbers.3)

## You often want to visually inspect variables.
random.numbers
View(random.numbers)

## Sometimes you want to view specific observations.
random.numbers[500]
random.numbers[900:920]

## Sometimes you want to view the beginning of the vector . . .
head(random.numbers, 20)

## . . . or the end of the vector.
tail(random.numbers, 20)

#### Using R for Data Management ####

## Most often we do not create data in R, but use existing data sources. 
## There are many ways to do this. 
## We first need to begin with some basics regarding directory structure.

## Determine your current working directory (i.e. where you are working in the file system).
getwd()

## List files in that directory.
list.files()

## Change your working directory to your data directory.
setwd("/Users/cdcrabtree/Dropbox/git/uva-2022/r-works")

## Let's import and view some data - first a .csv file.
data.csv <- read.csv("justices.csv", header = TRUE, sep = ",",)
View(data.csv) 

## How do we view just the beginning of the data?
head(data.csv)
tail(data.csv)

## Before importing other types of data (i.e. Excel and STATA files). 
## We need to install additional packages for R. 
## R comes with many built-in functions, but the packages extend these functions. 

## For importing Excel files, we need the 'gdata' package.
install.packages("gdata")

## For importing STATA files, we need the 'foreign' package.
install.packages("foreign")

## Or we can use 'rio' to import EVERYTHING.
install.packages("rio") # Leeper (http://thomasleeper.com/) does awesome work.

## Now we enable the packages. 
## They are not enabled by default, so we need to enable them for each session we use them.
library(gdata)
library(foreign)
library(rio)

## We are now ready to go back to importing data.
## For instance, we can import an Excel file ...
data.xsl <- read.xls(("justices.xls"), sheet = 1, header = FALSE)
View(data.xsl) 

## and a STATA file.
data <- rio::import("justices.dta")
View(data)

## We can even import data directly from the internet.
police <- read.csv("https://raw.githubusercontent.com/BuzzFeedNews/2015-12-fatal-police-shootings/master/guardian.csv")
View(police)

#### Managing R's Workspace ####

## We have created a lot of variables and data frames. Let's talk about managing them.

## First, let's list all objects in the working environment
ls() 

## That is a lot. Let's remove an object from the working environment.
rm("data.csv")
ls() 

## Say we want to start our session with a clean slate, but we want to save police first.
save(police, file = "police.rda")

## Or maybe we want to save everything in the current environment.
save.image(file = "everything.rda")

## Then we remove all the objects.
rm(list = ls())
ls() 

## This leaves libraries attached, however. Sometimes you might want to detach libraries.
## One reason for this is because libraries can occasionally conflict.
unloadNamespace("rio")

#### Exploring Data in R ####

## Let's import some data again.
justices <- read.csv("justices.csv")
View(justices)

## Or we can import our saved file.
load("everything.rda")

## Now let's explore the dataset.
names(justices)
dim(justices)
nrow(justices)
ncol(justices)
dim(justices)[1]
dim(justices)[2]
summary(justices)

## We can look at one variable. To do this, we use the '$' operator
justices$justiceName
table(justices$justiceName)
summary(justices$justiceName)

## We can also look at specific rows and columns of data.
justices[, 1]
justices[1, ]

## Or at individual datum.
justices[1, 1]

## Or at specific columns and rows of data.
justices[2:3, 4:5]

## We can also use 'which' to look at specific observations.
recent.df <- justices[which(justices$term > 2000), ]
justices[which(justices$justice == 112), ]
justices[which(justices$post_mn < 0), ]
nrow(police[which(justices$post_mn < 0), ])

## It is sometimes useful to determine how long vectors are.
length(justices$term)

## Or to figure out the unique number of observations in a vector.
length(unique(justices$term))
length(unique(justices$justiceName))

justices$justiceName <- tolower(justices$justiceName)

## Or to order the observations in ascending or descending order.
justices[order(justices$term), ]
justices[order(justices$term, decreasing=T),]

## Now we will use a new package to retrieve a different set of summary statistics. 
## We want to use the package 'Hmisc' - how do we install and load it?
install.packages("Hmisc")
library(Hmisc)
describe(justices)

NA

## Let's get some info about the variables in the dataset.
str(justices)

## We can more directly view a variable's class.
class(justices$justiceName)

justices$justiceName <- as.character(justices$justiceName)

## Or levels.
levels(justices$justiceName)

#### Data Manipulation in R ####

## Let's create a new variable. 
justices$new.post_mn <- justices$post_mn * 5

## Of course, this variable does not make any sense. So let's remove it.
names(justices)
justices <- justices[, -9]
## Another way to drop it would be to type 'police$new.age <- NULL'
names(justices)

## Let's create a new dummy variable for all observations where justices have a post_mn greater than 0.
justices$dummy <- 0
justices$dummy[justices$post_mn > 0] <- 1
table(justices$dummy)

## We can use the subset command to trim the data set as we like.
justices.new <- subset(justices, select = c(term, justice, post_mn))
dim(police.new)
summary(police.new)

## Maybe we only want data for judges with a post_mn greater than 0...
justices.new <- subset(justices, post_mn > 0)
dim(justices.new)
summary(justices.new)

## We can also exclude columns like this.
justices.new <- justices[, 2:4]
dim(justices.new)
summary(justices.new)

#### Basic Statistics in R ####

## Let's take the mean of a variable.
mean(justices$post_mn)
## What's up there?

## We can also calculate other statistics ...
## such as different measures of central tendency ...
median(justices$post_mn, na.rm = TRUE)
quantile(justices$post_mn, prob = 0.5, na.rm = TRUE)

## or spread.
sd(justices$post_mn, na.rm = TRUE)
IQR(justices$post_mn, na.rm = TRUE)
min(justices$post_mn, na.rm = TRUE)
max(justices$post_mn, na.rm = TRUE)
range(justices$post_mn, na.rm = TRUE)

## What can we do with qualitative data?
## We can create one-dimensional contigency tables.
table(justices$justiceName)

## We are often interested in calculating measures of association.
## We can do this by examine the correlation between 2 variables.
data <- rio::import("justices.dta")
View(data)
cor(data$term, data$post_mn, method = c("spearman"))

#### Basic Graphics in R ####

## Now let's create some basic graphics by examining data$enps in greater detail
hist(data$post_mn, col='slategray2', xlab="Ideology", ylab="Frequency", main="Histogram of Judicial Ideology", 
     font.main = 1, border = "white")
plot(density(data$post_mn), col='red', lwd=3, xlab="Ideology", ylab="Density", main="Kernel Density Plot of Ideology", 
     font.main = 1, bty='n')

## What do col, lwd, xlab, ylab, main, font.main, and bty do?

## Let's make a boxplot . . .
boxplot(data$post_mn)

## and then modify it based on what we just learned . . .
boxplot(data$post_mn)

## We can also make a boxplot of two variables 
boxplot(data$post_mn ~ data$justice)

## Sometimes we need to plot categorical data.
mosaicplot(table(justices$justiceName))

## Or visualize tables of categorical data.
library(datasets)
mosaicplot(Titanic, main = "Survival on the Titanic")

# try to save these plots

#### Exercises ####

# 1) What is the sum of 18, 10, 6, 1975, 813, and 111?
  
# 2) Create an object that stores the number you got for Question 1.

# 3) Read in 'const-data.csv'.

# 4) How is "totgr" distributed?
  
# 5) How many observations are in the data set from before the end of the Cold War.

# 6) Plot some variable from 'const-data.csv'. Make a good looking plot, if you can.

### EXTRA MATERIALS ###

#### Basic Estimation in R ####

## We have some data, we've examined it, and we've plotted it.
## We might now want to test some hypotheses.
## Let's say that we think that the effective number of legislative parties is a function of whether countries have an SMDP system.
## Given the nature of the dependent variable, we might test this with OLS.
lm(data$post_mn ~ data$term)

## We get some estimates this way, but we are really probably interested in inference here.
## For that, we need SEs.
mod <- lm(post_mn ~ term, data = data)
summary(mod)

## There are several ways to extract information about the model. 
coef(summary(mod))
coef(mod)
sqrt(diag(vcov(mod)))

## Now that we have a model, we might want to put its results in a table.
## If we use LaTeX, we can use the 'stargazer' package to help with this.
## http://jakeruss.com/cheatsheets/stargazer.html contains some great info on the library.
install.packages("stargazer")
library(stargazer)
stargazer(mod)

## But tables are kind of lame. So, let's present the results visually.
install.packages("dotwhisker")
library(dotwhisker)
dwplot(mod)

## We can save the plot as a 'ggplot' object and then make some additional changes.
## For more information about ggplot commands, see https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
## and http://ggplot2.org/book/.
cool.plot <- dwplot(mod)
cool.plot <- cool.plot + theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
  ggtitle("Results from Model") + theme(plot.title = element_text(face="bold")) + 
  theme(legend.position="none")
print(cool.plot)

#### Errata ####

## We often want to make sure that our code replicates. One way that we can do this is with the source() command.
source("file_name.r")

## We also can try to format our code well. This can help readability and thereby replication.
install.packages("formatR")
library(formatR)
code <- "fn <- function(x, y) { paste(x, '+', y, '-', x+y) }"
tidy_source(text = code)
# this code analyzes a data set of multiple samples
# the assumptions are that each factor is a separate sample and each row is an observation
# this code can analyze paired (dependent) or un-paired (independent) samples
#
# this code will run multiple tests
# the tests may or may not make sense for the data
# it is the responsibility of the user of this code to understand what tests
# are appropriate and what tests are not appropriate and to read and interpret
# the results from the tests that make sense

# browser()

# clear the environment
rm(list=ls()) 

#clear all graphics
graphics.off()

#clear the libraries
#only un-comment these lines and run them if you want to reset your libraries
#if ((length(names(sessionInfo()$otherPkgs))!=0))
#{
#  lapply(paste('package:',
#               names(sessionInfo()$otherPkgs),
#               sep=""),
#         detach,
#         character.only=TRUE,
#         unload=TRUE)
#}

#load required libraries
library("car")
library("dplyr")
library("ggplot2")
library("modeest")
library("nortest")
library("readxl")
library("reshape")
library("reshape2")
library("RVAideMemoire")
library("tidyr")
library("tidyverse")

# utility functions
remove_outliers_c <- function(x, low=0.05, high=0.95, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(low, high), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm, ...)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

remove_outliers_df <- function(x, cols=NULL, low=0.05, high=0.95, na.rm = TRUE, ...) {
  if (is.null(cols)) {
    cols=names(x)
  }
  
  cleaned_df <- x
  for (col in cols) {
    cleaned_df[col] <- remove_outliers_c(x[[col]],low,high, ...)
  }
  return(cleaned_df)
}

# constants
# the column name of each factor
trialNames<-c("Longevity")
# the column name to categorize the factor
variableName<-"Treatment"
# the column name to put the factor observation value
valueName<-"Longevity"
# a vector of the id names in the data
idNames<-c("id")
# test method one of "two.sided", "less", "greater"
testMethod="two.sided"
# outlier range (low,high)
# use (0,1) to keep all data
outlierRange=c(0,1)
# TRUE if observations are paired 
# FALSE if observations are not paired
isPairedData<-FALSE
# TRUE if use exact distribution 
# FALSE if allow duplicates
isExact<-FALSE
# TRUE if remove NA values 
# FALSE if leave NA values
isRemoveNA<-TRUE
# TRUE if raw data are already melted
# FALSE if raw data are not already melted 
isMelted<-FALSE
# TRUE if raw data are already clean
# FALSE if raw data are not already clean 
isCleaned<-TRUE

# read raw data 
dataFilePath<-"G:\\My Drive\\NWMSU\\MATH-17639-80SP25-OP2 Data Analysis\\Assigments\\Assignment 2\\Longevity and Treatment Data.csv"
#data_raw<-read_excel(dataFilePath)
data_raw<-read.csv(dataFilePath)

# cleaned data
# data_raw must be cleaned here to conform with the data assumptions listed below
# Assumptions:
# 1) data is a Data Frame
# 2) data contains a column called "id" which is the unique id of each observation
# 3) data contains one column per sample with numeric values

# check if we clean the data
if (isCleaned)
{
  # we don't clean the data so copy raw data to cleaned data
  data_clean<-data_raw
} else {
  # remove outliers from raw data and copy to cleaned data
  data_clean<-remove_outliers_df(data_raw, trialNames, outlierRange[0], outlierRange[1])
}

# create column of ids
data_clean$id<-rownames(data_raw)

# reshape data for different purposes
# check if the data are already melted
if (isMelted)
{
  data_melt<-data_clean
} else {
  # create 3 column data frame of all factors and all observations
  data_melt<-melt(data_clean, 
                  id.vars=idNames, 
                  measure.vars=trialNames, 
                  variable.name=variableName,
                  value.name=valueName)
}

# create list data by factor
data_split<-split(data_melt[valueName], 
                  data_melt[variableName])

# get all combinations of factors for manual pairwise comparisons
# this may be used later
sample_pairs<-arrange(
  subset(
    expand.grid(trialNames,trialNames), 
    as.character(Var1)<as.character(Var2)
  ),
  Var1,Var2
)

#
# get descriptive statistics
#
desc_quantiles<-lapply(data_split,
                       function(x) quantile(x[,valueName],
                                            na.rm=isRemoveNA))
print(summary(desc_quantiles))

desc_medians<-lapply(data_split,
                     function(x) median(x[,valueName],
                                        na.rm=isRemoveNA))
print(summary(desc_medians))

desc_modes<-lapply(data_split,
                   function(x) mlv(x[,valueName], 
                                   method="mfv",
                                   na.rm=isRemoveNA))
print(summary(desc_modes))


#
# tests of normality
#

# shapiro
# does ok with ties
# H0: Data are normally distributed
# H1: Data are not normally distributed
test_shapiro<-lapply(data_split,
                     function(x) shapiro.test(x[,valueName]))
print(summary(test_shapiro))

# anderson
# doesn't do well with ties
# H0: Data are normally distributed
# H1: Data are not normally distributed
test_anderson<-lapply(data_split,
                      function(x) ad.test(x[,valueName]))
print(summary(test_anderson))

#
#  tests of means against 0
#

#t_test=list()
#for(i in 1:length(trialNames)) {
#  result<-t.test(unlist(data_split[trialNames[i]]),
#                  alternative=testMethod,
#                  exact=isExact,
#                  na.rm=isRemoveNA)
#  
#   t_test<-append(t_test,
#                  result,
#                 length(t_test))
#}

#
# pairwise tests of means
#

# two sample t test (parametric)
# normally distributed
# H0: All the means are equal
# H1: At least 2 means are not equal
test_t_pairwise<-pairwise.t.test(data_melt[,valueName], 
                                 data_melt[,variableName],
                                 alternative=testMethod,
                                 p.adjust.method="none",
                                 paired=isPairedData,
                                 exact=isExact,
                                 na.rm=isRemoveNA)
print(summary(test_t_pairwise))

# wilcox test (non-parametric)
# ranked, not normally distributed
# H0: All the means are equal
# H1: At least 2 means are not equal
test_wilcox_pairwise<-pairwise.wilcox.test(data_melt[,valueName], 
                                           data_melt[,variableName], 
                                           alternative=testMethod,
                                           p.adjust.method="none", 
                                           paired=isPairedData, 
                                           exact=isExact,
                                           na.rm=isRemoveNA)
print(summary(test_wilcox_pairwise))

# ANOVA
# need to figure out how to create this formula using variables
# H0: All the means are equal
# H1: At least 2 means are not equal
test_aov<-aov(Longevity~Treatment, 
              data_melt,
              projections=TRUE,
              qr=TRUE,
              na.action=if(isRemoveNA) na.omit else na.fail)
print(summary(test_aov))

#
# tests of variances
#

# f-test (parametric)
# assumes normal (but it's OK with non-normal data, buyer beware)
# H0: All variances are equal
# H1: At least 2 variances are not equal
test_f_pairwise<-pairwise.var.test(data_melt[,valueName], 
                                   data_melt[,variableName],
                                   alternative=testMethod,
                                   p.method="none")
print(summary(test_f_pairwise))

# bartlett test (parametric)
# very sensitive to normality
# H0: All variances are equal
# H1: At least 2 variances are not equal
test_bartlett<-bartlett.test(data_melt[,valueName],
                             data_melt[,variableName],
                             na.rm=isRemoveNA)
print(summary(test_bartlett))


# levene test (parametric)
# very robust to non-normal data
# H0: All variances are equal
# H1: At least 2 variances are not equal
test_levene<-leveneTest(data_melt[,valueName],
                        data_melt[,variableName],
                        na.rm=isRemoveNA)
print(test_levene)

# fligner-killeen test (non-parametric)
# very robust to non-normal data
# uses ranks
# H0: All variances are equal
# H1: At least 2 variances are not equal
test_fligner<-fligner.test(data_melt[,valueName],
                           data_melt[,variableName],
                           na.rm=isRemoveNA)
print(summary(test_fligner))


#
# plots
#

# scatter plot
data_melt %>%
  ggplot(aes(x=!! sym(variableName), 
             y=!! sym(valueName),
             na.rm=isRemoveNA)) +
  geom_point(size=2,
             shape=23,
             na.rm=isRemoveNA) +
  labs(
    title=paste("Scatter plot of",valueName,"by", variableName,sep=" ")
  )

# q-q plot 
data_melt %>%
  ggplot(aes(sample=!! sym(valueName), 
             na.rm=isRemoveNA)) +
  geom_qq(na.rm=isRemoveNA) +
  geom_qq_line(na.rm=isRemoveNA) +
  facet_wrap(vars(!! sym(variableName)), scales="free_y") +
  labs(
    title=paste("Q-Q plot of",valueName,"by", variableName,sep=" ")
  )

# histogram 
data_melt %>%
  ggplot(aes(x=!! sym(valueName), 
             fill=factor(!! sym(variableName)),
             na.rm=isRemoveNA))+
  geom_histogram(na.rm=isRemoveNA) +
  facet_wrap(vars(!! sym(variableName))) +
  labs(
    title=paste("Histogram of",valueName,"by", variableName,sep=" ")
  )


# boxplot 
data_melt %>%
  ggplot(aes(x=!! sym(valueName), 
             fill=factor(!! sym(variableName))),
         na.rm=isRemoveNA)+
  geom_boxplot(na.rm=isRemoveNA) +
  coord_flip()+
  facet_wrap(vars(!! sym(variableName))) +
  labs(
    title=paste("Box plot of",valueName,"by", variableName,sep=" ")
  )





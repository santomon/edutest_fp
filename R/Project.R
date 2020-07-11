## author : Diyar Takak and Quang Anh Hong

## load necessary libraries

library("psy")
library("psych")
library("nFactors")
library("modeest")
library("plyr")
library("Rmisc")

## load and show data set

ds <- read.csv("../merged/merged.csv",TRUE,",")
class(ds)
head(ds)
nums <- Filter(is.numeric, ds) #only numeric attributes

## variables

ATYPE <- ds$assessment_type
SCLICK <- ds$sum_click
SCORE <- ds$score
ID_ASS <- ds$id_assessment
WEIGHT <- ds$weight
STCR <- ds$studied_credits
DASUB <- ds$date_submitted

## some plots

plot(SCLICK, SCORE)
plot(ID_ASS, SCORE)
plot(SCORE, ID_ASS)

## some analyzation

mean(SCORE, na.rm = TRUE) # average of the results
mlv(ATYPE) # frequency of assessment type
cor(nums, SCORE, use="complete.obs", method = "pearson")
sd(SCORE, na.rm = TRUE)
SCORE <- scale(SCORE, center = TRUE, scale = TRUE)
SCORE

## author : Diyar Takak and Quang Anh Hong



## load necessary libraries

library("psy")
library("psych")
library("nFactors")
library("modeest")
library("plyr")
library("dplyr")
library("Rmisc")
library("tidyverse")
library(reshape2)

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


#TODO: not all columns make sense to analyze, like ID and stuff
format(colMeans(nums, na.rm = TRUE), digits = 3, scientific = FALSE) # average of the results
format(apply(nums, FUN=sd, MARGIN=2, na.rm = TRUE), digits = 3, scientific = FALSE) # standard deviations of the results






mlv(ATYPE) # frequency of assessment type
#the most frequently used type of assessment is the TMA

#correlation analysis:
#We use correlation analysis to 
cor(nums, SCORE, use="complete.obs", method = "pearson")


#given an activity type, how often were they used on average and what was the average number of clicks for it?
#doesnt make too much sense thinking about it...
#TODO: unduplicate, multiple occurances bc of assessments
aggregate(x=ds[c('score', 'sum_click')], by=ds[c('activity_type')], FUN=mean, na.rm=TRUE)


#QUERY: for which activity type is Correlation with higher score the best?
#I guess this would mean sum_click gets higher, score gets higher, but for which resource is this the best?
#first normalize
ds_norm <- mutate_if(ds, is.numeric, scale)

result <- ddply(ds_norm, .(activity_type), summarise, click_score = cor(Filter(is.numeric, sum_click), score,use="complete.obs", method = "pearson"))
result

#no correlation for shared subpage?
ds_norm[ds_norm$activity_type=="sharedsubpage",]
#only 2 entries, so let's ignore


#QUERY: Grouped by individual students, what is the correlation between the sum of clicks to the Virtual Learning environments and the average score they received?
#i.e. does more VLE usage correspond with higher scores?
stud_activity <- unique(x=ds[c('id_student', 'activity_type', 'id_site', 'sum_click')])
stud_activity_sum <- aggregate(x=stud_activity[c('sum_click')], by=stud_activity['id_student'], FUN=sum, na.rm=TRUE)
stud_scores <- unique(x=ds[c('id_student', 'id_assessment', 'score')])
stud_avg_score <- aggregate(x=stud_scores[c('score')], by=stud_scores['id_student'], FUN=mean, na.rm=TRUE)
#now the real thing:
stud_activity_score <- join(stud_avg_score, stud_activity_sum)

cor(stud_activity_score['sum_click'], stud_activity_score['score'], use="complete.obs", method="pearson")
plot(stud_activity_score$sum_click, stud_activity_score$score)
#cor of 0.05ish --> the total number of VLE resources used is a bad predictor for the average score achieved by
#albeit slightly positive; this is evident from the plot
#this should be attributed to the fact that increased numbers of clicks correspond with more studying, instead
#of the VLE as a medium in general




#A linear model to predict the score based on all learning resources used by each student
##TODO: probably cant because there is no fking linear relation ship
##TODO: 
stud_activitytype_sum <- aggregate(x=stud_activity[c('sum_click')], by=stud_activity[c('id_student', 'activity_type')], FUN=sum, na.rm=TRUE)
stud_activitytype_sum_xd <- expand(stud_activitytype_sum, id_student, activity_type)
stud_activitytype_sum <- dplyr::right_join(stud_activitytype_sum, stud_activitytype_sum_xd, ) %>% mutate_all(funs(ifelse(is.na(.), 0, .)))

stud_activitytype_sum__ <- dcast(stud_activitytype_sum, id_student~activity_type) %>% join(stud_avg_score) %>% mutate_all(funs(ifelse(is.na(.), 0, .)))

stud_activitytype_sum_PCA <- prcomp(stud_activitytype_sum__[,2:20], scale = TRUE)
summary(stud_activitytype_sum_PCA)


fit <- lm(score~ .
, data=stud_activitytype_sum__[,2:21])
summary(fit)

plot(stud_activitytype_sum__, col="navy", main="Matrix Scatterplot")




#######################Playground and additional stuff for fun#############################################################
#QUERY which sites are the best for learning? Just out of curiousity; not that we gain anything since they are anonymous
res <- ddply(ds, .(id_site), summarise, click_score = cor(Filter(is.numeric, sum_click), score,use="pairwise.complete.obs", method = "pearson"))
res




#######DEBUG of sorts

ds[ds$id_site==883219,]
ds[ds$id_student==113565,]
nrow(unique(x=ds['id_student']))


stud_activity[stud_activity$id_student==368315,]


##################################################################################
######################### By request of our supervisors:
#Query: Is there a relationship between usage of VLE and resignation rate?

studentInfo <- read.csv("../datasets/studentInfo.csv")
studentVLE <- read.csv("../datasets/studentVLE.csv")
VLE <- read.csv("../datasets/VLE.csv")

###################Known Problems##############################
# duplicate contents ---> biases during calculations; some students took more assessments and therefore have way more entries and bias the  calculations towards themselves

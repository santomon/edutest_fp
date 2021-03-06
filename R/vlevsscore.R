## authors: Diyar Takak and Quang Anh Hong
#
# This R code examines, how the use of various virtual learning environments (VLEs)
# influences the score achieved in assessments.

# This code expects you to have downloaded and saved the OULAD data set into ../datsets
# and the saved merged variant, that is generated from our .pynb-file, into ../merged/


#!!!!-----------------------------------!!!###
#the code is designed to be run in RStudio; throughout, there will be remarks on the data
#please refer to the respective global data frames


#configuration

alpha_ = 0.05 #The significance level we are using


##libraries

library("psy")
library("psych")
library("nFactors")
library("modeest")
library("plyr")
library("dplyr")
library("Rmisc")
library("tidyverse")
library("reshape2")
library(ggplot2)

## load and show data set
#the data has generated using the pynb from this repository
#and is now loaded into R

#first an overview of the data

ds <- read.csv("../merged/merged.csv",TRUE,",") %>% 
  subset(select=-c(week_from, week_to, 
                   module_presentation_length, 
                   date_registration, 
                   date_unregistration, 
                   num_of_prev_attempts, 
                   date_submitted, studied_credits, weight, is_banked,
                   gender, region, highest_education, imd_band, age_band, disability)) #drop columns that are not relevant to our question

class(ds)
head(ds)
nums <- Filter(is.numeric, ds) #only the numerical values from the dataset


###################################################################################################################################################################
#QUERY: Grouped by individual students, what is the correlation between the sum of clicks to the Virtual Learning environments and the average score they received?
#i.e. does more VLE usage correspond with higher scores?
stud_activity <- unique(x=ds[c('id_student', 'activity_type', 'id_site', 'sum_click', 'date')])  #there are multiple entries for each student
  #stud_activity now contains a student's the number of clicks on every VLE, where they clicked at least once, as well as the type of the VLE 


stud_activity_sum <- aggregate(x=stud_activity[c('sum_click')], by=stud_activity['id_student'], FUN=sum, na.rm=TRUE)
  #now contains the sum of clicks on all VLEs for any given student  

stud_scores <- unique(x=ds[c('id_student', 'id_assessment', 'score')])
stud_avg_score <- aggregate(x=stud_scores[c('score')], by=stud_scores['id_student'], FUN=mean, na.rm=TRUE) 
  #stud_avg_score now contains the average score of all assessments, any given student has taken so far

#join the tables of average scores and total usage of VLEs, using the student id as the common variable
stud_activity_score <- join(stud_avg_score, stud_activity_sum)

#plot the data:
plot(stud_activity_score$sum_click, stud_activity_score$score)

#It can be seen, that, with higher usage of VLEs, lower scores become less and less common.

#In the following plot, for every number of click, the average score should be plotted
avg_score_given_clicks <- aggregate(x=stud_activity_score['score'], by=stud_activity_score['sum_click'], FUN=mean, na.rm=TRUE)
plot(avg_score_given_clicks)

#Judging from this plot, the total number of clicks on all VLEs is not a good predictor for the average score.
#################################################################################################################################################################


#To the actual question at hand
#To understand, which types of VLEs positively influence the scores of a student best, we use a multi-linear regression model.
#Our idea is as follows: The greater the coefficient of a VLE is, the more positive the influence of that VLE is on the score.
#Even if the score and the usage of VLEs don't share a linear relationship, the coefficient should still be a good indicator to the overall tendency of how 
#the VLE would influence the score, moreover compared with coefficients from the other VLEs.

stud_atype_sum <- aggregate(x=stud_activity[c('sum_click')], by=stud_activity[c('id_student', 'activity_type')], FUN=sum, na.rm=TRUE)
  #for every student, sum all the clicks he did on VLEs, grouped by the type,
  #e.g. for a student: for homepage, count their clicks on all sites, that are homepages

stud_atype <- expand(stud_atype_sum, id_student, activity_type) 
  #a target frame, that is the cartesian product of <student_id> and <activity_type>
  #the goal is to create a table with entry of 0, if a student never used that VLE

stud_atype_sum_expanded <- dplyr::right_join(stud_atype_sum, stud_atype) %>% mutate_all(funs(ifelse(is.na(.), 0, .)))
  #expand the previous table with the VLEs, a student didn't use and set it to 0;

stud_atype_sum_rearranged <- dcast(stud_atype_sum_expanded, id_student~activity_type) %>% join(stud_avg_score) %>% mutate_all(funs(ifelse(is.na(.), 0, .)))
  #rearrange the table, so that the VLEs are now the columns and the student ID are the rows

fit <- lm(score~ ., data=stud_atype_sum_rearranged[,2:21])
summary(fit)
  #The results


lm_results <- summary(fit)$coefficients[-1,] %>%          #drop first row, which is the intercept 
                        as.data.frame() %>%               #the result is a matrix; we turn it into a data frame for convenience
                        arrange(-Estimate) %>%
                        tibble::rownames_to_column("activity_type") #let the row names become the first columns, for convenience

lm_results_significant <- lm_results[lm_results$`Pr(>|t|)` < alpha_, ]

barplot(lm_results$Estimate, 
        names=lm_results$activity_type, 
        las=2,
        main = "Slopes of all activity types",
        col="#2DBAE3",
        #ylim=c(-5,3),
        cex.name=0.6
)


barplot(lm_results_significant$Estimate, 
        names=lm_results_significant$activity_type, 
        las=2,
        main = "Slopes of activity types with p-value < 0.05",
        col="#2DBAE3",
        ylim=c(-1.5,1.5),
        cex.name=0.8
        )


#Judging from the summary, questionnaires and pages have the best coefficients with p-values << 0.05.

#On the other hand, external quizes have the most negative influence on the average score of the student, with p-value << 0.05.
#It stands out, as it has the only significant slope less than -1.
#What this implies, is, that learning with external quizes is actually detrimental with respect to the average score achieved at one's one university.

#These findings are interesting and seem very well explainable.
#This would seem to support the idea, that a student should preferably use ressources provided by their own university. External quizes may use different notations and set diferent priorities, so that the questions in the quiz have little in common
#with what will be in the final exam.

#However, other possible explanations must be explored first.
#######################################################################################################################
##### 
#Validation of results

vle_ <- read.csv("../datasets/Vle.csv")
assessments <- read.csv("../datasets/assessments.csv")

externalquiz_vle <- vle_[vle_$activity_type == "externalquiz",]
  #---> external quizes only exist in code module DDD
  # low results for external quizes only, because module D is harder than the other modules?

#show average score across semester for each module:
assessment_scores <- join(stud_scores, assessments)
assessment_avg_scores <- aggregate(x=assessment_scores['score'], by=assessment_scores['code_module'], FUN='mean', na.rm=TRUE) %>% arrange(score)
mean(assessment_scores$score, na.rm = TRUE)

#----> the negative slope of 'externalquiz' is explained by module D assessments having overall the lowest average score and external quizes
# only being available to this module


questionnaire_vle <- vle_[vle_$activity_type == "questionnaire",]
page_vle <- vle_[vle_$activity_type == "page",]

#---->similar explanations to the result before:
#The questionnaire and page type appear mostly in module FFF, which has a higher average score across students that partook in it.

#####################################################################################
############
#### =====> group the data for the linear regression for each assessment,
#### the Vles used prior, that are part of the module

### refer the second R-file for this

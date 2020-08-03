## authors: Diyar Takak and Quang Anh Hong
#
# This R code is the follow up from vlevsscore.R
# The linear model will be created for the following groups:
# 
#         - for every id_assessment, take only the students that took part in it,
#         - only count the Vles, that have been used prior to the date submitted for that assessment
#         - and only count the Vles, that are frm the same course as the assessment
#
# we then average the slopes for the assessments for every code_module; See if there are any differences

# This code expects you to have downloaded and saved the OULAD data set into ../datsets
# and the saved merged variant, that is generated from our .pynb-file, into ../merged/


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
studentVle <- read.csv("../datasets/studentVle.csv")
Vle <- read.csv("../datasets/vle.csv")
studentRegistration <- read.csv("../datasets/studentRegistration.csv")
assessments <- read.csv("../datasets/assessments.csv")
studentAssessment <- read.csv("../datasets/studentAssessment.csv")
courses <- read.csv("../datasets/courses.csv")

studentInfo <- read.csv("../datasets/studentInfo.csv")
studentRegistration <- read.csv("../datasets/studentRegistration.csv")

##############################
#####commonly used inferred tables:

viable_assessments <- (select(studentAssessment, id_assessment) %>% unique())$id_assessment  #a list of assessments, where at least 1 student was present
code_modules <- (select(courses, code_module) %>% unique())$code_module                      # a list of all modules


####################
####Functions:
####for a given id_assessment:
#####create a linear model, that approximates the relationship between the number of clicks on the Vles by a student and the score he achieved in that Vle

lm_vle_by_id_assessment <- function(id_assessment_) {
    student_scores <- studentAssessment[studentAssessment$id_assessment == id_assessment_,] #get scores for all students on that assessment 
    
    if(nrow(student_scores) == 0) {
      stop("no students partook in this assessment, so no model can be generated")
      return(NA)
    }
    
    code_module_ <- unique(assessments[assessments$id_assessment == id_assessment_,]["code_module"])[1,1]  #find the module, to which the assessment belongs
                                                                      #there can only be 1 module; we extract the value from the 1x1 generated table with [1,1]
    
    vles_prior_assessment <- studentVle[studentVle$code_module == code_module_,] %>% #get all clicks on Vles, that belong to the module of the assessment 
                                                join(student_scores[c("id_student", "date_submitted")]) %>%          #join, to get rows, with date of the click and the date of submission of the assessment
                                                filter(date < date_submitted) %>%     # get all clicks on vles, that have been done prior to the date of submission
                                                left_join(Vle[c("id_site", "activity_type")]) %>%  #join with Vle, to get the activity_type for every site
    
                                                select(id_student, activity_type, sum_click) %>%#take only the necessary columns for the linear model
                                                  group_by(id_student, activity_type) %>%
                                                  summarize(total_clicks = sum(sum_click, na.rm = TRUE)) %>% #aggregate sum of clicks per activity type and student ID
                                                  ungroup() %>%                       
                                                as.data.frame() 
      
    activity_types <- unique(Vle["activity_type"])  #get all activity types; will be used to expand the main data for linear regression
    
    vle_explicit <- expand(vles_prior_assessment, id_student, activity_types) %>% as.data.frame() #cartesian product of all student_ids of this assessment and all activity types
    
    vles_prior_assessment <- right_join(vles_prior_assessment, vle_explicit) %>% #explicitly define the number of clicks on every type of Vle for every student with NA, if he has not used it
                                mutate_all(funs(ifelse(is.na(.), 0, .))) %>% #set total clicks to 0 for every Vle that the student has not clicked on
                                dcast(id_student~activity_type) %>%       #transform the dataframe, so that id_student is rows and every total_click for each activity type becomes a column
                                join(student_scores[c("id_student", "score")])  #join with the score, every student achieved in the assessment


    #some students might never have used a Vle at all;
    # we exclude these students with a regular join, because we only want to evaluate the students, that used Vles
      

    
    vles_prior_assessment <- drop_na(vles_prior_assessment, score)  #do not analyze rows, for which the score is not defined
    fit <- lm(score~., data=vles_prior_assessment[2:22]) #first column is the student id, which is not needed for the regression anymore
    #print(summary(fit))
    summary(fit)$coefficients[-1,] %>% return()  #first row is the intercept, which is of no interest and can be discarded
  
    #student_vles <- stude
  
}






lm_vle_by_code_module <- function(code_module_) {
  #executes the linear regression score ~ vle-use for every assessment, that belongs to the input module, and where at least 1 student participated
  id_assessments <- filter(assessments, code_module == code_module_ & id_assessment %in% viable_assessments)$id_assessment  #get a list of all id_assessment, where
                                              #the code_module matches and where at least 1 student participated
  lm_results <- matrix(ncol = 5, nrow = 0) %>% data.frame()
  colnames(lm_results) <- c("activity_type", "Estimate", "Std. Error", "t value", "Pr(>|t|)")
  
  for (id_ass in id_assessments) {
    
    lm_tmp <- lm_vle_by_id_assessment(id_assessment_ = id_ass)  %>%  #execute the linear regression
        as.data.frame() %>%
        tibble::rownames_to_column("activity_type")
    lm_results <- rbind(lm_results, lm_tmp)  #append the results as new rows
  }
  
  lm_results %>% 
    group_by(activity_type) %>%
    summarize(`mean_estimate` = mean(Estimate), #calculate the mean from the results across all assessments from the module
              `mean_stderr` = mean(`Std. Error`),
              `mean_t` = mean(`t value`),
              `mean_p` = mean(`Pr(>|t|)`)
              ) %>%
  
              #`count%` = count(Estimate[`Pr(>|t|)` < alpha_]) / count(Estimate)) %>%
    ungroup() %>%
    as.data.frame() %>%
    return()
    
} 


#############################
####Execute linear regression for all  modules

results <- lapply(code_modules, lm_vle_by_code_module)
print(results)  #the results are in the order of of code_modules (which should be alphabetical)

# if you browse through the results, the mean p-values are for the most part greater than 0.05
# the number of students partaking in the exams is too small to execute the linear regression with significant results
# however, when the p-value is is less than 0.05, the slope is usually close to 0
#
# ======> We could not prove, that a specific type of Vle is superior to the others

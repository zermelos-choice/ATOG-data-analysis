# Think about how we’re using A to G and AP exams – 
# are these strong predictors of success in intro chem vs. intro math vs. intro BIS 
# (the first course people take here at Davis)? 
#   Should we be using chem AP score as a predictor in all STEM? Or math AP?
# Admissions A-G   requirements

#3 is a medium-size project, bigger than a data request but a lot smaller than the leavers study. 
# It’s exploratory. 
# The basic idea is first get some nice clean data set up
# and then run some regressions with the score 
# in the first course in whichever sequence at UC Davis as the dependent variable and the usual explanatory variables + A to G, 
# number of APs, 
# and specific subject AP test scores.
#------library------
library(tidyverse)
# library(synthpop)
setwd("/Users/bvelasq6/Box Sync/CEE Educational Analytics/ATOG Analysis")
#------Functions------
na.count<-function(df){
  sapply(df, function(y) length(which(is.na(y))))
}


#----FILES----

admissions<-read.csv("Data/admissions.csv",stringsAsFactors = F)


courses<-read.csv("Data/courses.csv",stringsAsFactors = F)
ap_tests<-read.csv("Data/ap_tests.csv",stringsAsFactors = F)
test<-ap_tests[!duplicated(ap_tests$TEST),]
ap_test_sub<-ap_tests[ap_tests$TEST %in% c("APST","APMA","APBI","APMG","APCH","APPM","APCA","A")]

#-----wrangling-----
admissions<-admissions[admissions$PIDM %in% courses$PIDM,]
courses <-courses[courses$TERM<202001,]
test <-	courses[courses$PIDM == 	3351901,]
courses$CLASS<-paste(courses$SUBJ,courses$CRSE, sep=" ")
courses_sub <-select(courses, PIDM, CLASS, GRADE_PT, TERM)
courses_sub <- courses_sub %>% group_by(PIDM) %>%  mutate(grouped_id = row_number()) %>% spread(CLASS,GRADE_PT) %>% filter(TERM==min(TERM)) %>% select(-grouped_id)

courses_sub_mat_21<-courses_sub[!is.na(courses_sub$`MAT 021A`),c("PIDM","MAT 021A")]
table(duplicated(courses_sub_mat_21$PIDM))

courses_sub_mat_16<-courses_sub[!is.na(courses_sub$`MAT 016A`),c("PIDM","MAT 016A")]
table(duplicated(courses_sub_mat_16$PIDM))

courses_sub_bis_2A<-courses_sub[!is.na(courses_sub$`BIS 002A`),c("PIDM","BIS 002A")]
table(duplicated(courses_sub_bis_2A$PIDM))

courses_sub_che_2A<-courses_sub[!is.na(courses_sub$`CHE 002A`) & !duplicated(courses_sub$PIDM),c("PIDM","CHE 002A")]
table(duplicated(courses_sub_che_2A$PIDM))

courses_sub<-merge(courses_sub_mat_21,courses_sub_mat_16,by="PIDM",all=T)
courses_sub<-merge(courses_sub,courses_sub_bis_2A,by="PIDM",all=T)
courses_sub<-merge(courses_sub,courses_sub_che_2A,by="PIDM",all=T)


#ap test wrangling
ap_tests <- ap_tests[,c("PIDM","SCORE","TEST")] %>% group_by(PIDM,TEST) %>% filter(SCORE==max(SCORE)) %>% ungroup() %>%  spread(TEST,SCORE)  


#-----models-----
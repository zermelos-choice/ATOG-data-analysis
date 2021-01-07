#------ATOG Study------
library(tidyverse)
library(abroca)
library(randomForest)
library(rsample)

#-----functions------
na.count<-function(df){
  sapply(df, function(y) length(which(is.na(y))))
}

#---------Import files-----
setwd("/Users/bvelasq6/Box Sync/CEE Educational Analytics/ATOG Analysis")


main_file<-read.csv("Data/clean_file_acorba.csv",as.is=T,stringsAsFactors = F)
predictors <- c(
  "ans1_grade_pt",
  "ant2_grade_pt",
  "bis2a_grade_pt",
  "bis2b_grade_pt",
  "chem2a_grade_pt",
  "chem2b_grade_pt",
  "enl3_grade_pt",
  "his17_grade_pt",
  "mat16a_grade_pt",
  "mat21a_grade_pt",
  "mat21b_grade_pt",
  "phy7a_grade_pt",
  "psc1_grade_pt" ,
  "sta13_grade_pt",
  "sta100_grade_pt")
controls = c('ATOG',
            'ATOGA',
            'ATOGB',
            'ATOGC',
            'ATOGD',
            'ATOGE',
            'ATOGF',
            'ATOGG',
            'APPASSED',
            'GPA',
            #'UNWEIGHTED_GPA',
            'LOWINCOME',
            #             'EDFATHER',
            #             'EDMOTHER',
            'CCCREDIT',
            'SAT_TOTAL_OFFICIAL_SQ',
            #'IS_URM',
            #'ANY_FIRST_TERM_MAJOR_IS_STEM',
            'FIRST_GENERATION',
            'LOW_INCOME',
            'TRIPLE',
            #             'AP_Bio_bool',
            #             'AP_CS_bool',
            #             'AP_Chemistry_bool',
            #             'AP_Calculus_AB_bool',
            #             'AP_Calculus_BC_bool',
            #             'AP_Physics_bool',
            #             'AP_Stats_bool',
            'SEX_F', 
            'ETH',
            # 'ETH_AF',
            # 'ETH_AI', 
            # 'ETH_CH', 
            # 'ETH_EI',
            # 'ETH_FP',
            # 'ETH_JA', 
            # 'ETH_KO',
            # 'ETH_LA',
            # 'ETH_MX', 
            # 'ETH_OA', 
            # 'ETH_PI', 
            # 'ETH_VT', 
            # 'ETH_WH',
            'AP_Bio_0.0',
            'AP_Bio_3.0', 
            'AP_Bio_4.0',
            'AP_Bio_5.0',
            'AP_CS_0.0', 
            'AP_CS_3.0', 
            'AP_CS_4.0', 
            'AP_CS_5.0',
            'AP_Chemistry_0.0',
            'AP_Chemistry_3.0',
            'AP_Chemistry_4.0',
            'AP_Chemistry_5.0', 
            'AP_Calculus_AB_0.0', 
            'AP_Calculus_AB_3.0',
            'AP_Calculus_AB_4.0', 
            'AP_Calculus_AB_5.0',
            'AP_Calculus_BC_0.0',
            'AP_Calculus_BC_3.0', 
            'AP_Calculus_BC_4.0', 
            'AP_Calculus_BC_5.0',
            'AP_Physics_1_0.0', 
            'AP_Physics_1_3.0',
            'AP_Physics_1_4.0',
            'AP_Physics_1_5.0', 
            'AP_Physics_2_0.0',
            'AP_Physics_2_3.0',
            'AP_Physics_2_4.0',
            'AP_Physics_2_5.0',
            'AP_Physics_B_0.0',
            'AP_Physics_B_3.0',
            'AP_Physics_B_4.0', 
            'AP_Physics_B_5.0',
            'AP_Physics_C_EM_0.0',
            'AP_Physics_C_EM_3.0',
            'AP_Physics_C_EM_4.0',
            'AP_Physics_C_EM_5.0',
            'AP_Physics_C_Mech_0.0',
            'AP_Physics_C_Mech_3.0',
            'AP_Physics_C_Mech_4.0',
            'AP_Physics_C_Mech_5.0', 
            'AP_Stats_0.0',
            'AP_Stats_3.0', 
            'AP_Stats_4.0', 
            'AP_Stats_5.0',
            'EDFATHER_0.0',
            'EDFATHER_1.0', 
            'EDFATHER_2.0', 
            'EDFATHER_3.0', 
            'EDFATHER_5.0',
            'EDFATHER_6.0', 
            'EDFATHER_7.0', 
            'EDFATHER_8.0',
            'EDMOTHER_0.0',
            'EDMOTHER_1.0', 
            'EDMOTHER_2.0', 
            'EDMOTHER_3.0', 
            'EDMOTHER_5.0',
            'EDMOTHER_6.0',
            'EDMOTHER_7.0', 
            'EDMOTHER_8.0')
main_file<-main_file[,union(controls,"mat21a_grade_pt")]
main_file<-main_file[!is.na(main_file$mat21a_grade_pt),]
main_file[is.na(main_file)]<-0
main_file$ETH<-factor(main_file$ETH)
# Set random seed to make results reproducible:
set.seed(17)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(main_file)/3)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(main_file), size = data_set_size)
# Assign the data to the correct sets
training <- main_file[-indexes,]
test <- main_file[indexes,]
na.count(main_file)

#--------RANDOM FOREST--------


rf_regression = randomForest(mat21a_grade_pt ~ ., data=training, ntree=100, importance=TRUE)

test$pred = predict(rf_regression, test, type = "response")
test$grade_bool<- (test$mat21a_grade_pt>=2.74)*1
test$pred_bool <- (test$pred>=2.74)*1

abroca <- compute_abroca(test, pred_col = "pred_bool", label_col = "grade_bool", 
                         protected_attr_col = "ETH", majority_protected_attr_val = "WH", plot_slices = F)


print(abroca)

#-----test------
data("recidivism")
recidivism$returned = as.factor(recidivism$Return.Status != "Not Returned")
in_train = caret::createDataPartition(recidivism$returned, p = 0.75, list = FALSE)
traindata = recidivism[in_train,c("Release.Year", "County.of.Indictment", "Gender", "Age.at.Release", "returned")]
testdata = recidivism[-in_train,c("Release.Year", "County.of.Indictment", "Gender", "Age.at.Release", "returned")]
lr = glm(returned ~ ., data=traindata, family="binomial")
testdata$pred = predict(lr, testdata, type = "response")
abroca <- compute_abroca(testdata, pred_col = "pred", label_col = "returned", 
                         protected_attr_col = "Gender", majority_protected_attr_val = "MALE", 
                         plot_slices = F, identifier="recidivism")
print(abroca)
#-------REGRESSION------
# controls = c(
#   'ATOG',
#   'ATOGA',
#   'ATOGB',
#   'ATOGC',
#   'ATOGD',
#   'ATOGE',
#   'ATOGF',
#   'ATOGG',
#   'APTAKEN',
#   'APPASSED',
#   'APCREDIT',
#   'GPA',
#   'LOWINCOME',
#   'FG',
#   'UNWEIGHTED_GPA',
#   'SAT_TOTAL_OFFICIAL',
#   'IS_URM',
#   'ANY_FIRST_TERM_MAJOR_IS_STEM',
#   'STARTED_INTERNATIONAL',
#   'FIRST_GENERATION',
#   'LOW_INCOME',
#   'TRIPLE',
#   'NO_TRIPLE',
#   'AP.Bio',
#   'AP.CS',
#   'AP.Chemistry',
#   'AP.Calculus.AB',
#   'AP.Calculus.BC',
#   'AP.Physics.1',
#   'AP.Physics.2',
#   'AP.Physics.B',
#   'AP.Physics.C:..E&M',
#   'AP.Physics.C:..Mech.',
#   'AP.Stats',
#   'SEX_F')

# colnames(main_file[,191:201]) <-c("AP_Bio",
#                                   "AP_CS",
#                                   "AP_Chem",
#                                   "AP_Calculus_AB",
#                                   "AP_Calculus_BC",
#                                   "AP_Physics_1",
#                                   "AP_Physics_2",
#                                   "AP_Physics_B",
#                                   "AP_Physics_C_EM",
#                                   "AP_Physics_C_Mech",
#                                   "AP_Stats")

# bis_model <- glm(bis2a_grade_pt ~ 
#                      ATOG+
#                      ATOGA+
#                      ATOGB+
#                      ATOGC+
#                      ATOGD+
#                      ATOGE+
#                      ATOGF+
#                      ATOGG+
#                      APTAKEN+
#                      APPASSED+
#                      APCREDIT+
#                      GPA+
#                      LOWINCOME+
#                      FG+
#                      UNWEIGHTED_GPA+
#                      SAT_TOTAL_OFFICIAL+
#                      IS_URM+
#                      ANY_FIRST_TERM_MAJOR_IS_STEM+
#                      STARTED_INTERNATIONAL+
#                      FIRST_GENERATION+
#                      LOW_INCOME+
#                      TRIPLE+
#                      NO_TRIPLE+
#                      AP_Bio_bool+
#                      AP_CS_bool+
#                      AP_Chemistry_bool+
#                      AP_Calculus_AB_bool+
#                      AP_Calculus_BC_bool+
#                      AP_Physics_1_bool+
#                      AP_Physics_2_bool+
#                      AP_Physics_B_bool+
#                      AP_AP_Physics_C_EM_bool+
#                      AP_Physics_C_Mech_bool+
#                      AP_Stats_bool+
#                      SEX_F
#                  
#                  
#   ,data=main_file)
# summary(bis_model)

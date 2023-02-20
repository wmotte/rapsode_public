#  SCRIPT for EXTRACTING AIR POLLUTION  AND SES DATA PER PATIENT

# packages
library( "rgdal" )
library("writexl")
library("dplyr")
library("tidyverse")
library("oddsratio")
library("questionr")


# outdir
outdir <- 'out.extract.data'
dir.create( outdir, showWarnings = FALSE )

# load complete air pollution and SES data
patient_SES_data_complete <- read.csv("SES_data_complete_pc_bu_final.csv")
patient_pollution_data <- read.csv("patient_pollution_data_including_age.csv", row.names=1)

# merge air pollution and SES data
data_complete <- merge (patient_pollution_data, patient_SES_data_complete, by= "id")

# drop unclear epilepsy diagnosis (3) group
data_complete <- subset( data_complete, data_complete$diagnss != 3 ) 

# recode diagnosis (0 = no epilepsy, 1 = epilepsy)
data_complete$diagnosis<- recode( data_complete$diagnosis, "2" = 0, "1" = 1 )

# check dataset and column names
head(data_complete) 

# Reformat the data so that it is easy to use by adding castor column names
colnames(data_complete) <- c(
  "id",
  "postal_code",
  "postal_code6",
  "presentation_date",
  "age",
  "diagnosis2",
  "no2_annual",
  "o3_annual",
  "pm10_annual",
  "pm25_annual",
  "postal_code2",
  "buurt_code",
  "presentation_year",
  "diagnosis",
  "epilepsy_type",
  # 1 = focal
  # 2 = generalized
  # 3 = combination
  # 4 = unknown
  "epilepsy_etiology",
  # 1 = genetic
  # 2 = structural
  # 3 = metabolic
  # 4 = immunological
  # 5 = infectious
  # 6 = unknown
  "genetic_etiology_subtype",
  # 1 = established genetic
  # 2 = presumed genetic
  "sz_load",
  "year_ses_data",
  "part_score_welfare",
  "part_score_employment",
  "seswoa_lower",
  "seswoa_avg",
  "seswoa_upper"
)

# now drop unnecessary or duplicate columns
data_complete_final <- data_complete[,c(
  "id",
  "postal_code",
  "presentation_date",
  "age",
  "no2_annual",
  "o3_annual",
  "pm10_annual",
  "pm25_annual",
  "buurt_code",
  "presentation_year",
  "diagnosis",
  "epilepsy_type",
  # 1 = focal
  # 2 = generalized
  # 3 = combination
  # 4 = unknown
  "epilepsy_etiology",
  # 1 = genetic
  # 2 = structural
  # 3 = metabolic
  # 4 = immunological
  # 5 = infectious
  # 6 = unknown
  "genetic_etiology_subtype",
  # 1 = established genetic
  # 2 = presumed genetic
  "sz_load",
  "year_ses_data",
  "part_score_welfare",
  "part_score_employment",
  "seswoa_lower",
  "seswoa_avg",
  "seswoa_upper"
)]

head(data_complete_final) 
# now we have all data and correct column names

# now check structure of dataset to see which columns contain factors
str(data_complete_final) 

# recode character variables needed for analysis to numbers
data_complete_final$seswoa_avg <- as.numeric(data_complete_final$seswoa_avg)
data_complete_final$age <- as.numeric(data_complete_final$age)

# check if changed correctly
str(data_complete_final)


# now check if all factor levels are represented by people with epilepsy
xtabs(~ diagnosis + epilepsy_type, data=data_complete_final)
xtabs(~ diagnosis + epilepsy_etiology, data=data_complete_final)
xtabs(~ diagnosis + genetic_etiology_subtype, data=data_complete_final)

# now first make unadjusted regression model using 1 air pollutant to predict diagnosis
# EFFECT PER AIR POLLUTANT and OR + 95% confidence intervals and P values
logistic_no2 <- glm(diagnosis ~ no2_annual, data=data_complete_final, family="binomial"(link ="logit" ))
summary(logistic_no2)
odds.ratio(logistic_no2, level=0.95)

logistic_o3 <- glm(diagnosis ~ o3_annual, data=data_complete_final, family="binomial")
summary(logistic_o3)
odds.ratio(logistic_o3, level=0.95)

logistic_pm10 <- glm(diagnosis ~ pm10_annual, data=data_complete_final, family="binomial")
summary(logistic_pm10)
odds.ratio(logistic_pm10, level=0.95)

logistic_pm25 <- glm(diagnosis ~ pm25_annual, data=data_complete_final, family="binomial")
summary(logistic_pm25)
odds.ratio(logistic_pm25, level=0.95)

# make adjusted regression model using  both SES and AGE as possible confounding factors
# EFFECT PER AIR POLLUTANT, SES AND AGE
logistic_no2_ses_age <- glm(diagnosis ~ no2_annual + seswoa_avg + age, data=data_complete_final, family="binomial")
summary(logistic_no2_ses_age)
odds.ratio(logistic_no2_ses_age, level=0.95)

logistic_o3_ses_age <- glm(diagnosis ~ o3_annual + seswoa_avg + age, data=data_complete_final, family="binomial")
summary(logistic_o3_ses_age)
odds.ratio(logistic_o3_ses_age, level=0.95)

logistic_pm10_ses_age <- glm(diagnosis ~ pm10_annual+ seswoa_avg + age, data=data_complete_final, family="binomial")
summary(logistic_pm10_ses_age)
odds.ratio(logistic_pm10_ses_age, level=0.95)

logistic_pm25_ses_age <- glm(diagnosis ~ pm25_annual + seswoa_avg + age, data=data_complete_final, family="binomial")
summary(logistic_pm25_ses_age)
odds.ratio(logistic_pm25_ses_age, level=0.95)



###### more regression models

# regression model with all air pollutants
logistic_all_poll <- glm(diagnosis ~ no2_annual + pm25_annual + pm10_annual + o3_annual, data=data_complete_final, family="binomial")
summary(logistic_all_poll)

# regression model with all air pollutants and all confounders
logistic_all <- glm(diagnosis ~ no2_annual + pm25_annual + pm10_annual + o3_annual + seswoa_avg + age, data=data_complete_final, family="binomial")
summary(logistic_all)


########### regression models with only 1 confounding factor

# now make regression model using 1 air pollutant and SES as confounder
# EFFECT PER AIR POLLUTANT AND POSSIBLE SES CONFOUNDING
logistic_no2_ses <- glm(diagnosis ~ no2_annual + seswoa_avg, data=data_complete_final, family="binomial")
summary(logistic_no2_ses)

logistic_pm25_ses <- glm(diagnosis ~ pm25_annual + seswoa_avg, data=data_complete_final, family="binomial")
summary(logistic_pm25_ses)

logistic_pm10_ses <- glm(diagnosis ~ pm10_annual+ seswoa_avg, data=data_complete, family="binomial")
summary(logistic_pm10_ses)

logistic_o3_ses <- glm(diagnosis ~ o3_annual + seswoa_avg, data=data_complete_final, family="binomial")
summary(logistic_o3_ses)

# now make regression model using 1 air pollutant and age as confounder
# EFFECT PER AIR POLLUTANT AND POSSIBLE AGE CONFOUNDING
logistic_no2_age <- glm(diagnosis ~ no2_annual + age, data=data_complete_final, family="binomial")
summary(logistic_no2_age)

logistic_pm25_age <- glm(diagnosis ~ pm25_annual + age, data=data_complete_final, family="binomial")
summary(logistic_pm25_age)

logistic_pm10_age <- glm(diagnosis ~ pm10_annual+ age, data=data_complete_final, family="binomial")
summary(logistic_pm10_age)

logistic_o3_age <- glm(diagnosis ~ o3_annual + age, data=data_complete_final, family="binomial")
summary(logistic_o3_age)



# Script for sensitivity analysis including only patients from 2014-2019

# packages
library( "rgdal" )
library("writexl")
library("ggplot2")
library("cowplot")
library("sjPlot")
library("dplyr")
library("tidyverse")
library("oddsratio")
library("questionr")


# outdir
outdir <- 'out.sensitivity.analysis'
dir.create( outdir, showWarnings = FALSE )

# load data of only presentation years 2014-2019
# n = 546
sens_data <- read.csv("data_complete_final_sens.csv", sep=";")

# now check if all factor levels are represented by people with epilepsy
xtabs(~ diagnosis + epilepsy_type, data=sens_data)
xtabs(~ diagnosis + epilepsy_etiology, data=sens_data)


# make unadjusted regression model using 1 air pollutant to predict diagnosis
# EFFECT PER AIR POLLUTANT and OR + 95% confidence intervals and P values

sens_glm_no2 <- glm(diagnosis ~ no2_annual, data=sens_data, family="binomial")
summary(sens_glm_no2)
odds.ratio(sens_glm_no2, level=0.95)

sens_glm_o3 <- glm(diagnosis ~ o3_annual, data=sens_data, family="binomial")
summary(sens_glm_o3)
odds.ratio(sens_glm_o3, level=0.95)

sens_glm_pm10 <- glm(diagnosis ~ pm10_annual, data=sens_data,family="binomial")
summary(sens_glm_pm10)
odds.ratio(sens_glm_pm10, level=0.95)

sens_glm_pm25 <- glm(diagnosis ~ pm25_annual, data=sens_data,family="binomial")
summary(sens_glm_pm25 )
odds.ratio(sens_glm_pm25, level=0.95)


# make adjusted regression model using  both SES and AGE as possible confounding factors
# EFFECT PER AIR POLLUTANT, SES AND AGE
sens_glm_no2_ses_age <- glm(diagnosis ~ no2_annual + seswoa_avg + age, data=sens_data,family="binomial")
summary(sens_glm_no2_ses_age)
odds.ratio(sens_glm_no2_ses_age, level=0.95)

sens_glm_o3_ses_age <- glm(diagnosis ~ o3_annual + seswoa_avg + age, data=sens_data,family="binomial")
summary(sens_glm_o3_ses_age)
odds.ratio(sens_glm_o3_ses_age, level=0.95)

sens_glm_pm10_ses_age <- glm(diagnosis ~ pm10_annual + seswoa_avg + age, data=sens_data,family="binomial")
summary(sens_glm_pm10_ses_age)
odds.ratio(sens_glm_pm10_ses_age, level=0.95)

sens_glm_pm25_ses_age <- glm(diagnosis ~ pm25_annual + seswoa_avg + age, data=sens_data,family="binomial")
summary(sens_glm_pm25_ses_age)
odds.ratio(sens_glm_pm25_ses_age, level=0.95)

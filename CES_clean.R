#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [https://usa.ipums.org/usa/index.shtml]
# Author: Rohan Alexander and Sam Caetano [Muhammad Tsany]
# Data: 22 October 2020 [27 October 2020]
# Contact: rohan.alexander@utoronto.ca [muh.tsany@mail.utoronto.ca]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!

# ------ SURVEY CLEANING ------

# devtools::install_github("hodgettsp/cesR") #- only if need to install

library(cesR)
library(haven)
library(tidyverse)

# loading CES data
setwd("C:/Users/irfan/Documents/University/Year 3/Fall 2020/STA304/Problem Set/Final PS/Option B")
raw_data <- read_dta("inputs/2019 Canadian Election Study - Online Survey v1.0.dta")

# Labels
raw_data <- labelled::to_factor(raw_data)

# selecting predictor and dependent variable(s)
reduced_data <-
  raw_data %>%
  select(cps19_citizenship, cps19_yob, cps19_province, 
         cps19_votechoice, cps19_education, cps19_income_cat,
         cps19_income_number, cps19_gender)

# code for logit model - transforming vote to only liberal and conservative (1, 0)
reduced_data <-
  reduced_data %>%
  mutate(vote_liberal = case_when(
    cps19_votechoice == "Liberal Party" ~ 1,
    cps19_votechoice == "Conservative Party" ~ 0
  ))

# Quantifying cps19_yob to age
reduced_data <-
  reduced_data %>%
  mutate(age = case_when(
    TRUE ~ as.numeric(as.character(cps19_yob))
  ))

# creating age
reduced_data$age = 2020 - reduced_data$age

# creating age brackets
reduced_data <-
  reduced_data %>%
  mutate(age_bracket = case_when(
    age >= 19 & age <= 40 ~ 1,
    age > 40 & age <= 60 ~ 2,
    age > 60 & age <= 80 ~ 3,
    age > 80 & age <= 100 ~ 4
  ))

# quantifying province - note: Yukon residence not included in census data (GSS)
reduced_data <-
  reduced_data %>%
  mutate(province_q = case_when(
    cps19_province == "Alberta" ~ 4,
    cps19_province == "British Columbia" ~ 5,
    cps19_province == "Manitoba" ~ 4,
    cps19_province == "New Brunswick" ~ 1,
    cps19_province == "Newfoundland and Labrador" ~ 1,
    # cps19_province == "Northwest Territories" ~ 6,
    cps19_province == "Nova Scotia" ~ 1,
    # cps19_province == "Nunavut" ~ 8,
    cps19_province == "Ontario" ~ 3,
    cps19_province == "Prince Edward Island" ~ 1,
    cps19_province == "Quebec" ~ 2,
    cps19_province == "Saskatchewan" ~ 4
  ))

# quantifying citizenship *note using anymore - does not match up with census data
#reduced_data <-
  #reduced_data %>%
  #mutate(citizenship = case_when(
    #cps19_citizenship == "Canadian citizen" ~ 1,
    #cps19_citizenship == "Permanent resident" ~ 2,
    #cps19_citizenship == "Other" ~ 3
  #))

# quantifying education
reduced_data <-
  reduced_data %>%
  mutate(education_q = case_when(
    cps19_education == "No schooling" ~ 1,
    cps19_education == "Some elementary school" ~ 1,
    cps19_education == "Completed elementary school" ~ 1,
    cps19_education == "Some secondary/ high school" ~ 1,
    cps19_education == "Completed secondary/ high school" ~ 2,
    cps19_education == "Some technical, community college, CEGEP, College Classique" ~ 2,
    cps19_education == "Completed technical, community college, CEGEP, College Classique" ~ 2,
    cps19_education == "Some university" ~ 2,
    cps19_education == "Bachelor's degree" ~ 3,
    cps19_education == "Master's degree" ~ 3,
    cps19_education == "Professional degree or doctorate" ~ 3
    #cps19_education == "Don't know/ Prefer not to answer" ~ 4
  ))

# quantifying cps19_income_num as numeric
reduced_data <-
  reduced_data %>%
  mutate(cps19_income_number = case_when(
    TRUE ~ as.numeric(as.character(cps19_income_number))
  ))

# quantifying income to income brackets
reduced_data <-
  reduced_data %>%
  mutate(income_bracket = case_when(
    cps19_income_number == 0 ~ 0,
    cps19_income_number >= 1 & cps19_income_number <= 30000 ~ 1,
    cps19_income_number > 30000 & cps19_income_number <= 60000 ~ 1,
    cps19_income_number > 60000 & cps19_income_number <= 90000 ~ 2,
    cps19_income_number > 90000 & cps19_income_number <= 110000 ~ 2,
    cps19_income_number > 110000 & cps19_income_number <= 150000 ~ 3,
    cps19_income_number > 150000 & cps19_income_number <= 200000 ~ 3,
    cps19_income_number > 200000 ~ 3,
    cps19_income_cat == "No income" ~ 0,
    cps19_income_cat == "$1 to $30,000" ~ 1,
    cps19_income_cat == "$30,001 to $60,000" ~ 1,
    cps19_income_cat == "$60,001 to $90,000" ~ 2,
    cps19_income_cat == "$90,001 to $110,000" ~ 2,
    cps19_income_cat == "$110,001 to $150,000" ~ 2,
    cps19_income_cat == "$150,001 to $200,000" ~ 3,
    cps19_income_cat == "More than $200,000" ~ 3
    # cps19_income_cat == "Don't know/ Prefer not to answer" ~ 8 * delete?
  ))

# imputing gender as a binary variable
reduced_data <-
  reduced_data %>%
  mutate(sex = case_when(
    cps19_gender == "A man" ~ 1,
    cps19_gender == "A woman" ~ 0,
    cps19_gender == "Other (e.g. Trans, non-binary, two-spirit, gender-queer)" ~ 0
  ))

# removing cps19_income_number and cps19_income_cat so complete.cases works
reduced_data$cps19_income_cat <- NULL
reduced_data$cps19_income_number <- NULL

# reducing reduced_data for complete cases only - such that only votes for liberal and conservative
reduced_data <- reduced_data[complete.cases(reduced_data), ]
# argue using this to be more precise but also losing statistical power since
# sample size decreases***
# reduces observations from 37,822 to 16,518

write_csv(reduced_data, "outputs/survey_data.csv")

# say bias if you only use cps19_votechoice instead of not using cps19_vote_unlike variables -- not using full census (bias)

# maybe use cps19_2nd_choice as second party choice (?) - maybe


# ------ CENSUS CLEANING ------

# reading census data
census_data <- read_csv("inputs/gss.csv")

# selecting the same variables for post-strat
cen_reduced_data <- census_data %>%
  select(citizenship_status, age, province, education, income_family, sex)

# filtering for only >= 19 year olds - reduces observations from 20,602 to 20,048
cen_reduced_data <- cen_reduced_data %>%
  filter(age >= 19)


# quantifying citizenship *not using
#cen_reduced_data <-
  #cen_reduced_data %>%
  #mutate(citizenship = case_when(
    #citizenship_status == "By birth" ~ 1,
    #citizenship_status == "By naturalization" ~ 1,
  #))
  

# quantifying age
cen_reduced_data <-
  cen_reduced_data %>%
  mutate(age_bracket = case_when(
    age >= 19 & age <= 40 ~ 1,
    age > 40 & age <= 60 ~ 2,
    age > 60 & age <= 80 ~ 3,
    age > 80 & age <= 100 ~ 4
  ))

# quantifying province to regions
cen_reduced_data <-
  cen_reduced_data %>%
  mutate(province_q = case_when(
    province == "Alberta" ~ 4,
    province == "British Columbia" ~ 5,
    province == "Manitoba" ~ 4,
    province == "New Brunswick" ~ 1,
    province == "Newfoundland and Labrador" ~ 1,
    # province == "Northwest Territories" ~ 6, *not in census data
    province == "Nova Scotia" ~ 1,
    # province == "Nunavut" ~ 8, *not in census data
    province == "Ontario" ~ 3,
    province == "Prince Edward Island" ~ 1,
    province == "Quebec" ~ 2,
    province == "Saskatchewan" ~ 4
    ))

# quantifying education
cen_reduced_data <-
  cen_reduced_data %>%
  mutate(education_q = case_when(
    education == "Less than high school diploma or its equivalent" ~ 1,
    education == "High school diploma or a high school equivalency certificate" ~ 2,
    education == "Trade certificate or diploma" ~ 2,
    education == "College, CEGEP or other non-university certificate or di..." ~ 2,
    education == "University certificate or diploma below the bachelor's level" ~ 2,
    education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" ~ 3,
    education == "University certificate, diploma or degree above the bach..." ~ 3
  ))

# quantifying income
cen_reduced_data <-
  cen_reduced_data %>%
  mutate(income_bracket = case_when(
    income_family == "Less than $25,000" ~ 1,
    income_family == "$25,000 to $49,999" ~ 1,
    income_family == "$50,000 to $74,999" ~ 1,
    income_family == "$75,000 to $99,999" ~ 2,
    income_family == "$100,000 to $ 124,999" ~ 2,
    income_family == "$125,000 and more" ~ 3
  ))

# quantifying sex (binary variable to gender in CES)
cen_reduced_data <-
  cen_reduced_data %>%
  mutate(sex = case_when(
    sex == "Male" ~ 1,
    sex == "Female" ~ 0
  ))

# deleting N/A cases - reduced observations from 20,602 to 18,687
cen_reduced_data <- cen_reduced_data[complete.cases(cen_reduced_data), ]

write_csv(cen_reduced_data, "outputs/census_data.csv")

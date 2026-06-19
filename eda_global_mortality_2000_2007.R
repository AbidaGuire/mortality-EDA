# ============================================================
# Project: Exploratory Data Analysis of Global Mortality (2000–2007)
# Author: Abida Guire
# Date: 2026-06-01
#
# Description:
# This script performs a full exploratory data analysis (EDA) of 
# global mortality data among older adults. The workflow includes:
#   - Data cleaning and preprocessing
#   - Cause grouping and factor engineering
#   - Outlier detection and removal
#   - Missing data exploration
#   - Statistical testing (Kruskal–Wallis, Chi-square)
#   - Visualizations by cause, region, age group and sex
#
# Purpose:
# To demonstrate data wrangling, statistical reasoning, and 
# visualization skills using R for portfolio and research readiness.
#
# Requirements:
#   - tidyverse
#   - janitor
#   - lubridate
#   - readxl
#
# Notes:
#   - Data covers years 2000–2007
#   - Outliers removed using IQR method within groups
#   - Cause categories aggregated into broader groups for clarity
# ============================================================

#Clean workspace
rm(list = ls())

#Load packages
library(dplyr)
library(janitor)
library(tidyverse)
library(lubridate)
library(readxl)
        
#Load dataset
#Excel
data <-read_excel("~/Exploratory data analysis project/Data 2026-06-01 09-41.xlsx")


#Initial exploration

# First look checklist
str(data)
head(data)
summary(data)


#Remove duplicates
data<-data[!duplicated(data),]


#Variable cleaning

#Convert key variables into the right data types

data <- data |>
  mutate(
    death_rate = as.numeric(`Value Numeric`),
    cause = as.factor(Cause),
    sdg_region = as.factor(`SDG region`),
    age_group = as.factor(`Age group`),
    sex = as.factor(Sex),
    year = as.numeric(Year)
  )

summary(data)

#Cause grouping into broader categories


data <- data |> 
  mutate(Cause_group = case_when(
    
    # Cardiovascular
    Cause %in% c("Ischaemic heart disease", "Stroke", "Hypertensive heart disease",
                 "Cardiomyopathy, myocarditis, endocarditis", "Rheumatic heart disease") 
    ~ "Cardiovascular",
    
    # Cancers
    Cause %in% c("Breast cancer", "Prostate cancer", "Colon and rectum cancers",
                 "Trachea, bronchus, lung cancers", "Liver cancer", "Stomach cancer",
                 "Oesophagus cancer", "Pancreas cancer", "Kidney cancer",
                 "Bladder cancer", "Larynx cancer", "Mouth and oropharynx cancers",
                 "Melanoma and other skin cancers", "Corpus uteri cancer",
                 "Cervix uteri cancer", "Ovary cancer", "Leukaemia",
                 "Lymphomas, multiple myeloma", "Brain and nervous system cancers",
                 "Mesothelioma", "Gallbladder and biliary tract cancer")
    ~ "Cancer",
    
    # Respiratory
    Cause %in% c("Chronic obstructive pulmonary disease", "Asthma",
                 "Lower respiratory infections")
    ~ "Respiratory",
    
    # Infectious diseases
    Cause %in% c("HIV/AIDS", "Tuberculosis", "Malaria", "Meningitis",
                 "Encephalitis", "Diarrhoeal diseases", "Rabies")
    ~ "Infectious",
    
    # Neurological
    Cause %in% c("Alzheimer disease and other dementias", "Parkinson disease")
    ~ "Neurological",
    
    # Digestive & metabolic
    Cause %in% c("Diabetes mellitus", "Cirrhosis of the liver",
                 "Gallbladder and biliary diseases", "Peptic ulcer disease",
                 "Paralytic ileus and intestinal obstruction", "Protein-energy malnutrition")
    ~ "Digestive/Metabolic",
    
    # Injuries & external causes
    Cause %in% c("Falls", "Road injury", "Self-harm", "Interpersonal violence",
                 "Natural disasters")
    ~ "Injuries/External",
    
    # Everything else
    TRUE ~ "Other"
  ))



#Missingness Overview
colSums(is.na(data))


#Outlier Detection and Cleaning

#Clean outliers per group and omit non specific causes

clean_data <- data |>
  group_by(Cause_group) |>
  mutate(
    Q1 = quantile(death_rate, 0.25),
    Q3 = quantile(death_rate, 0.75),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) |>
  filter(death_rate >= lower, death_rate <= upper) |>
  filter(Cause_group != "Other")|>
  ungroup()

#Statistical Test : Kruskal-Wallis
kruskal.test(death_rate ~ Cause_group, data = clean_data)



#Visualization: Boxplot by Cause Group

#Box plot per cause a comparison side by side

ggplot(clean_data, aes(x = reorder(Cause_group, death_rate), y = log(death_rate), color = Cause_group)) +
  geom_boxplot() +
  theme_minimal()+
  labs(
    title = "Death Rate Distribution by Cause Group",
    x = "Cause Group",
    y = "log (Deaths per 100,000)"
  )+
  coord_flip()



#Average mortality rate by Cause Group

plot_data <- clean_data |>
  group_by(Cause_group) |>
  summarise(mean_rate = mean(death_rate, na.rm = TRUE))

#A bar chart
ggplot(plot_data, aes(x = reorder(Cause_group,-mean_rate), y = mean_rate)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Mean Death Rate by Cause Group",
    y = "Deaths per 100,000 population"
  )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



#Outlier Cleaning for Regional data

clean_data_region <- data |>
  group_by(sdg_region) |>
  mutate(
    Q1 = quantile(death_rate, 0.25),
    Q3 = quantile(death_rate, 0.75),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) |>
  filter(death_rate >= lower, death_rate <= upper) |>
  filter(sdg_region != "Other")|>
  ungroup()

#Average mortality by region

#Average by region
mean_by_region <- clean_data_region |>
  group_by(sdg_region) |>
  summarise(mean_rate = mean(death_rate, na.rm = TRUE))

ordered_mean_by_region <- mean_by_region |>
  arrange(desc(mean_rate))


#Bar chart of the average death rate per region ordered from highest peak to lowest

ggplot(mean_by_region, aes(x = reorder(sdg_region, -mean_rate), y = mean_rate)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Mean Death Rate by region",
    y = "Deaths per 100,000 population",
    x = "SDG Region"
  )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#Chi-Square Test: Region by Cause Group

#Conducting a Chi squared test of independence


# contingency table
tab <- table(clean_data$sdg_region, clean_data$Cause_group) 
tab

# chi-square test of independence
chisq.test(tab)


#Faceted bar chart: Causes by Region

new_df<- data|>
  group_by(sdg_region,Cause_group) |>
  summarise(mean_rate = mean(death_rate, na.rm = TRUE)) |>
  filter(
    sdg_region != "NA",
    Cause_group != "Other"
  )


#Facetted bar chart by region

ggplot(new_df, aes(x = reorder(Cause_group,-mean_rate), y = mean_rate)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Mean Death Rate by Cause Group Accross Regions",
    y = "Deaths per 100,000 population"
  )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  facet_wrap(~sdg_region, nrow = 2)


#Mortality by sex


sex_summary <- data |>
  group_by (sex) |>
  summarise(
    mean_rate = mean(death_rate, na.rm = TRUE),
    median_rate = median(death_rate, na.rm = TRUE),
    n = n()
  )

sex_summary

#Visualization
#Remove the outliers

data_s <- data |>
  group_by(sex) |>
  mutate(
    Q1 = quantile(death_rate, 0.25),
    Q3 = quantile(death_rate, 0.75),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) |>
  filter(death_rate >= lower, death_rate <= upper)|>
  ungroup()

#Using the log function to transform the data
ggplot(data_s, aes(x = reorder(sex, death_rate), y =log (death_rate), color = sex)) +
  geom_boxplot() +
  theme_minimal()+
  labs(
    title = "Death Rate Distribution by sex",
    y = "log (Deaths per 100,000)",
    x = "Sex"
  )+
  coord_flip()


#Mortality by age

age_summary <- data |>
  group_by(age_group) |>
  summarise(
    mean_rate = mean(death_rate, na.rm = TRUE),
    median_rate = median(death_rate, na.rm = TRUE),
    n = n()
  )
age_summary

# box plot of age groups side by side

data_a <- data |>
  group_by(age_group) |>
  mutate(
    Q1 = quantile(death_rate, 0.25),
    Q3 = quantile(death_rate, 0.75),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) |>
  filter(death_rate >= lower, death_rate <= upper)|>
  filter(age_group != "60+")
ungroup()

#Using the log function to transform the data
ggplot(data_a, aes(x = reorder(age_group, death_rate), y =log (death_rate), color = age_group)) +
  geom_boxplot() +
  theme_minimal()+
  labs(
    title = "Death Rate Distribution by age group",
    y = "Deaths per 100,000",
    x = "age group"
  )+
  coord_flip()

#Statistical test: Kruskal Wallice
kruskal.test(death_rate ~ age_group, data = data_a)


#Cause and sex interaction

interaction_sex <-data|>
  group_by(Cause_group, sex) |>
  summarise(mean_rate = mean (death_rate, na.rm = TRUE)) |>
  filter(Cause_group!= "Other")

ggplot(interaction_sex, aes(x = Cause_group, y=mean_rate, fill = sex)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Mean Death Rate by Cause Group and Sex",
    y = "Deaths per 100,000"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Cause and age interaction

interaction_age <- data |>
  group_by(Cause_group, age_group) |>
  summarise(mean_rate = mean (death_rate, na.rm = TRUE)) |>
  filter(Cause_group!= "Other")|>
  filter(age_group != "60+")

ggplot(interaction_age, aes(x = Cause_group, y=mean_rate, fill = age_group)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Mean Death Rate by Cause Group and Age Group",
    y = "Deaths per 100,000"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# mortality-EDA

Project Overview
This project explores global mortality patterns among older adults using data from the World Health Organization (WHO) for the years 2000-2007. The goal is to practice full exploratory data analysis (EDA) in R, including data cleaning, cause grouping, demographic comparisons, and visualization. The analysis focuses on how mortality varies by cause of death, SDG region, sex and age group. This project demonstrates skills in data wrangling, statistical reasoning, and public-health analytics.


Data Source & Preparation
The dataset was sourced from the World Health organization website using the following link: https://platform.who.int/data/maternal-newborn-child-adolescent-ageing/indicator-explorer-new/MCA/mortality-rate-in-older-people---top-20-causes-(global-and-regions)
It includes mortality counts and rates per 100,000 population, as well as other factors such as region, sex, age group, and cause of death. The main steps in preparing the data include:
•	Cleaning and standardizing the variable names
•	Converting key variable to numeric or factor types
•	Creating a “Cause_group” variable to aggregate 50+ causes into broader categories
•	Checking for missing values across all variables
•	Removing duplicates 
•	Identifying and removing extreme outliers using IQR method.
•	Applying log transformations in visualizations to address strong right-skewness.


Research Questions
The EDA addresses the following questions:
o	Which cause has the highest mortality rates globally?
o	How do mortality patterns differ across regions?
o	Do males and females differ in mortality rates?
o	How does mortality vary across age groups?
o	Which causes show the largest demographic differences?
o	Is there an association between region and Cause?
o	How do cause patterns interact with sex and age?


Methods Summary
This project uses a combination of data cleaning, outlier detection using IQR, data transformation using the logarithm function, non-parametric tests such as Kruskal-Wallis and Chi-square, grouped summaries, and interaction plots.
All analysis was performed in R using dplyr, janitor, tidyerse, lubridate, and readxl.


Key Findings
•	Cardiovascular and respiratory diseases show the highest average mortality among older adults. (Shown by the bar graph “Mean Death Rate by Cause Group”)

•	Mortality varies significantly across regions, with some regions showing much higher burdens. (Shown by the bar graph “Mean Death Rate by Region”)
                                                                        
•	Males exhibit higher median mortality rated than females. (Shown by the box plot “death rate distribution by sex)

•	Mortality increases with age, with the oldest groups showing the highest rates. (Shown by the box plot. “Death rate distribution by sex”)

•	Cause-specific patterns differ by sex and age group, indicating demographic variation in disease burden. (Shown by bar graphs “mean death rate by cause group and sex” and “mean death rate by cause group and age group)

•	There is a strong association between region and cause group, suggesting that there is a geographic clustering of certain causes. (Shown by chi-square test of independence with a p values extremely low.)


How to Run the Script
1.	Open eda_global_mortality_2000_2007.R
2.	Install the required packages
3.	Copy the raw data file path to replace the original file path in the script
4.	Run the script from top to bottom.




Business Understanding
================
Manmeet Bains
9/27/2019

``` r
#Libraries
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.4.4

    ## -- Attaching packages -------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1       v purrr   0.2.5  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.3.1  
    ## v readr   1.1.1       v forcats 0.3.0

    ## Warning: package 'tibble' was built under R version 3.4.4

    ## Warning: package 'tidyr' was built under R version 3.4.4

    ## Warning: package 'readr' was built under R version 3.4.4

    ## Warning: package 'purrr' was built under R version 3.4.4

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## Warning: package 'stringr' was built under R version 3.4.4

    ## Warning: package 'forcats' was built under R version 3.4.4

    ## -- Conflicts ----------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidyquant)
```

    ## Loading required package: lubridate

    ## Warning: package 'lubridate' was built under R version 3.4.4

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    ## Loading required package: PerformanceAnalytics

    ## Warning: package 'PerformanceAnalytics' was built under R version 3.4.4

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 3.4.4

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 3.4.4

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## 
    ## Attaching package: 'PerformanceAnalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

    ## Loading required package: quantmod

    ## Warning: package 'quantmod' was built under R version 3.4.4

    ## Loading required package: TTR

    ## Warning: package 'TTR' was built under R version 3.4.4

    ## Version 0.4-0 included new data defaults. See ?getSymbols.

    ## == Need to Learn tidyquant? =========================================================================================================
    ## Business Science offers a 1-hour course - Learning Lab #9: Performance Analysis & Portfolio Optimization with tidyquant!
    ## </> Learn more at: https://university.business-science.io/p/learning-labs-pro </>

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 3.4.4

``` r
library(forcats)
library(stringr)
```

``` r
# Load Data
Path_train <- "C:/Users/Manmeet Bains/Desktop/Business Science University/Employee-Attrition/00_Data/telco_train.xlsx"

train_raw_data <- read_excel(Path_train, sheet = 1)
```

``` r
# Subset Data

dept_job_role_tbl <- train_raw_data %>%
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)
```

# 1\. Business Science Problem Framework —-

# 1A. View Business As Machine —-

# BSU’s: Department and Job Role

# Define Objectives: Retrain High Performers

# Assess Outcomes: TBD

``` r
dept_job_role_tbl %>% 
  group_by(Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n))
```

    ## # A tibble: 2 x 3
    ##   Attrition     n   pct
    ##   <chr>     <int> <dbl>
    ## 1 No         1049 0.839
    ## 2 Yes         201 0.161

# 1B. Understand The Drivers —-

# Investigate Objectives: 16% Pct Attrition

# Sythesize Outcomes: High Counts and High percentages

# Hypothesize Drivers: Job Role and Departments

``` r
# Department ----
dept_job_role_tbl %>% 
  
  group_by(Department, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(Department) %>%
  mutate(pct = n / sum(n))
```

    ## # A tibble: 6 x 4
    ## # Groups:   Department [3]
    ##   Department             Attrition     n   pct
    ##   <chr>                  <chr>     <int> <dbl>
    ## 1 Human Resources        No           37 0.755
    ## 2 Human Resources        Yes          12 0.245
    ## 3 Research & Development No          721 0.867
    ## 4 Research & Development Yes         111 0.133
    ## 5 Sales                  No          291 0.789
    ## 6 Sales                  Yes          78 0.211

``` r
# Job Role ----
dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% c("Yes"))
```

    ## # A tibble: 10 x 5
    ##    Department             JobRole                   Attrition     n    pct
    ##    <chr>                  <chr>                     <chr>     <int>  <dbl>
    ##  1 Human Resources        Human Resources           Yes          12 0.308 
    ##  2 Research & Development Healthcare Representative Yes           8 0.0762
    ##  3 Research & Development Laboratory Technician     Yes          49 0.219 
    ##  4 Research & Development Manager                   Yes           2 0.0417
    ##  5 Research & Development Manufacturing Director    Yes           7 0.0569
    ##  6 Research & Development Research Director         Yes           2 0.0274
    ##  7 Research & Development Research Scientist        Yes          43 0.166 
    ##  8 Sales                  Manager                   Yes           2 0.0645
    ##  9 Sales                  Sales Executive           Yes          50 0.183 
    ## 10 Sales                  Sales Representative      Yes          26 0.4

# 1C. Measure The Drivers —-

# Collect Information on Employee Attrition: On going

# Develop KPI’s: Industry KPIs: 8.8%

``` r
dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% c("Yes")) %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  )
```

    ## # A tibble: 10 x 6
    ##    Department       JobRole         Attrition     n    pct above_industry_~
    ##    <chr>            <chr>           <chr>     <int>  <dbl> <chr>           
    ##  1 Sales            Sales Represen~ Yes          26 0.4    Yes             
    ##  2 Human Resources  Human Resources Yes          12 0.308  Yes             
    ##  3 Research & Deve~ Laboratory Tec~ Yes          49 0.219  Yes             
    ##  4 Sales            Sales Executive Yes          50 0.183  Yes             
    ##  5 Research & Deve~ Research Scien~ Yes          43 0.166  Yes             
    ##  6 Research & Deve~ Healthcare Rep~ Yes           8 0.0762 No              
    ##  7 Sales            Manager         Yes           2 0.0645 No              
    ##  8 Research & Deve~ Manufacturing ~ Yes           7 0.0569 No              
    ##  9 Research & Deve~ Manager         Yes           2 0.0417 No              
    ## 10 Research & Deve~ Research Direc~ Yes           2 0.0274 No

``` r
# 1D. Uncover Problems and Opportunities ----

calculate_attrition_cost <- function(
  
  # Employee
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
  
) {
  
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year * 
    (workdays_position_open + workdays_onboarding * onboarding_efficiency) 
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
}
```

``` r
# Calculate Cost By Job Role ---- 

dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% c("Yes")) %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  )
```

    ## # A tibble: 10 x 7
    ##    Department JobRole Attrition     n    pct above_industry_~
    ##    <chr>      <chr>   <chr>     <int>  <dbl> <chr>           
    ##  1 Sales      Sales ~ Yes          26 0.4    Yes             
    ##  2 Human Res~ Human ~ Yes          12 0.308  Yes             
    ##  3 Research ~ Labora~ Yes          49 0.219  Yes             
    ##  4 Sales      Sales ~ Yes          50 0.183  Yes             
    ##  5 Research ~ Resear~ Yes          43 0.166  Yes             
    ##  6 Research ~ Health~ Yes           8 0.0762 No              
    ##  7 Sales      Manager Yes           2 0.0645 No              
    ##  8 Research ~ Manufa~ Yes           7 0.0569 No              
    ##  9 Research ~ Manager Yes           2 0.0417 No              
    ## 10 Research ~ Resear~ Yes           2 0.0274 No              
    ## # ... with 1 more variable: cost_of_attrition <dbl>

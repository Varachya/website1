---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: volunteer.jpg
keywords: ""
slug: finalgroupproject
title: Predicting Credit Card Fraud
---



# The problem: predicting credit card fraud

The goal of the project is to predict fraudulent credit card transactions.

We will be using a dataset with credit card transactions containing legitimate and fraud transactions. Fraud is typically well below 1% of all transactions, so a naive model that predicts that all transactions are legitimate and not fraudulent would have an accuracy of well over 99%-- pretty good, no?

You can read more on credit card fraud on [Credit Card Fraud Detection Using Weighted Support Vector Machine](https://www.scirp.org/journal/paperinformation.aspx?paperid=105944)

The dataset we will use consists of credit card transactions and it includes information about each transaction including customer details, the merchant and category of purchase, and whether or not the transaction was a fraud.

## Obtain the data

The dataset is too large to be hosted on Canvas or Github, so please download it from dropbox <https://www.dropbox.com/sh/q1yk8mmnbbrzavl/AAAxzRtIhag9Nc_hODafGV2ka?dl=0> and save it in your `dsb` repo, under the `data` folder.

As we will be building a classifier model using tidymodels, there's two things we need to do:

1.  Define the outcome variable `is_fraud` as a factor, or categorical, variable, instead of the numerical 0-1 varaibles.
2.  In tidymodels, the first level is the event of interest. If we leave our data as is, `0` is the first level, but we want to find out when we actually did (`1`) have a fraudulent transaction


```
## Rows: 671,028
## Columns: 14
## $ trans_date_trans_time <dttm> 2019-02-22 07:32:58, 2019-02-16 15:07:20, 2019-…
## $ trans_year            <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2020, …
## $ category              <chr> "entertainment", "kids_pets", "personal_care", "…
## $ amt                   <dbl> 7.79, 3.89, 8.43, 40.00, 54.04, 95.61, 64.95, 3.…
## $ city                  <chr> "Veedersburg", "Holloway", "Arnold", "Apison", "…
## $ state                 <chr> "IN", "OH", "MO", "TN", "CO", "GA", "MN", "AL", …
## $ lat                   <dbl> 40.1186, 40.0113, 38.4305, 35.0149, 39.4584, 32.…
## $ long                  <dbl> -87.2602, -80.9701, -90.3870, -85.0164, -106.385…
## $ city_pop              <dbl> 4049, 128, 35439, 3730, 277, 1841, 136, 190178, …
## $ job                   <chr> "Development worker, community", "Child psychoth…
## $ dob                   <date> 1959-10-19, 1946-04-03, 1985-03-31, 1991-01-28,…
## $ merch_lat             <dbl> 39.41679, 39.74585, 37.73078, 34.53277, 39.95244…
## $ merch_long            <dbl> -87.52619, -81.52477, -91.36875, -84.10676, -106…
## $ is_fraud              <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

The data dictionary is as follows

| column(variable)      | description                                 |
|-----------------------|---------------------------------------------|
| trans_date_trans_time | Transaction DateTime                        |
| trans_year            | Transaction year                            |
| category              | category of merchant                        |
| amt                   | amount of transaction                       |
| city                  | City of card holder                         |
| state                 | State of card holder                        |
| lat                   | Latitude location of purchase               |
| long                  | Longitude location of purchase              |
| city_pop              | card holder's city population               |
| job                   | job of card holder                          |
| dob                   | date of birth of card holder                |
| merch_lat             | Latitude Location of Merchant               |
| merch_long            | Longitude Location of Merchant              |
| is_fraud              | Whether Transaction is Fraud (1) or Not (0) |

We also add some of the variables we considered in our EDA for this dataset during homework 2.


```r
card_fraud <- card_fraud %>% 
  mutate( hour = hour(trans_date_trans_time),
          wday = wday(trans_date_trans_time, label = TRUE),
          month_name = month(trans_date_trans_time, label = TRUE),
          age = interval(dob, trans_date_trans_time) / years(1)
) %>% 
  rename(year = trans_year) %>% 
  
  mutate(
    
    # convert latitude/longitude to radians
    lat1_radians = lat / 57.29577951,
    lat2_radians = merch_lat / 57.29577951,
    long1_radians = long / 57.29577951,
    long2_radians = merch_long / 57.29577951,
    
    # calculate distance in miles
    distance_miles = 3963.0 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians)),

    # calculate distance in km
    distance_km = 6377.830272 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians))

  )
```

## Exploratory Data Analysis (EDA)

You have done some EDA and you can pool together your group's expertise in which variables to use as features. You can reuse your EDA from earlier, but we expect at least a few visualisations and/or tables to explore the dataset and identify any useful features.

Distribution of amounts charged to credit card, both for legitimate and fraudulent accounts.


```r
head(card_fraud)
```

```
## # A tibble: 6 × 24
##   trans_date_trans_time  year category     amt city  state   lat   long city_pop
##   <dttm>                <dbl> <chr>      <dbl> <chr> <chr> <dbl>  <dbl>    <dbl>
## 1 2019-02-22 07:32:58    2019 entertain…  7.79 Veed… IN     40.1  -87.3     4049
## 2 2019-02-16 15:07:20    2019 kids_pets   3.89 Holl… OH     40.0  -81.0      128
## 3 2019-12-27 22:25:34    2019 personal_…  8.43 Arno… MO     38.4  -90.4    35439
## 4 2019-03-03 10:11:39    2019 grocery_n… 40    Apis… TN     35.0  -85.0     3730
## 5 2019-02-09 17:14:54    2019 food_dini… 54.0  Red … CO     39.5 -106.       277
## 6 2019-09-09 01:19:59    2019 shopping_… 95.6  Irwi… GA     32.8  -83.2     1841
## # ℹ 15 more variables: job <chr>, dob <date>, merch_lat <dbl>,
## #   merch_long <dbl>, is_fraud <fct>, hour <int>, wday <ord>, month_name <ord>,
## #   age <dbl>, lat1_radians <dbl>, lat2_radians <dbl>, long1_radians <dbl>,
## #   long2_radians <dbl>, distance_miles <dbl>, distance_km <dbl>
```

```r
summary_table <- card_fraud %>%

  group_by(year, is_fraud) %>% # Eugene: changed "trans_year" to "year"

  summarize(  
    total_amt = sum(amt),
    total_count = n() #Eugene: added count statistic
    ) %>% 
  mutate(  #Eugene: count percentages in the same table
    total_percentage_cases = round(100*total_count/sum(total_count),2),
    total_percentage_amt = round(100*total_amt/sum(total_amt),2)
  )
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
summary_table # Eugene: Check the results. IMPORTANT RESULTS - the fraudelent transactions comprise about 0.6% of cases, but account for more than 4% of amount
```

```
## # A tibble: 4 × 6
## # Groups:   year [2]
##    year is_fraud total_amt total_count total_percentage_cases
##   <dbl> <fct>        <dbl>       <int>                  <dbl>
## 1  2019 1         1423140.        2721                   0.57
## 2  2019 0        32182901.      475925                  99.4 
## 3  2020 1          651949.        1215                   0.63
## 4  2020 0        12925914.      191167                  99.4 
## # ℹ 1 more variable: total_percentage_amt <dbl>
```


```r
summary_table %>% select(c('year','is_fraud','total_amt','total_percentage_amt')) %>% 
mutate(is_fraud=ifelse(is_fraud==1,"Fraud","Legit")) %>% 
                   
                  ggplot(aes(x=factor(year), y=total_amt, fill = is_fraud))+
                  geom_bar(position = position_stack(), stat="identity")+
                  scale_y_continuous(labels = scales::comma)+
                  labs(title = "Total Amount per Year", x = "Year", y = "Total Amount") +
                  geom_text(aes(label=total_percentage_amt),
                                size = 3, position = position_stack(vjust = 0.5))+
                  theme(legend.title=element_blank())
```

<img src="/blogs/finalgroupproject_files/figure-html/unnamed-chunk-5-1.png" width="672" />


```r
summary_table %>% select(c('year','is_fraud','total_count','total_percentage_cases')) %>% 
mutate(is_fraud=ifelse(is_fraud==1,"Fraud","Legit")) %>% 
                   
                  ggplot(aes(x=factor(year), y=total_count, fill = is_fraud))+
                  geom_bar(position = position_stack(), stat="identity")+
                  scale_y_continuous(labels = scales::comma)+
                  labs(title = "Total Count per Year", x = "Year", y = "Total Amount") +
                  geom_text(aes(label=total_percentage_cases),
                                size = 3, position = position_stack(vjust = 0.5))+
                  theme(legend.title=element_blank())
```

<img src="/blogs/finalgroupproject_files/figure-html/unnamed-chunk-6-1.png" width="672" />

Types of purchases that are most likely to be instances of fraud.


```r
# Per merchant category

fraud_by_category <- card_fraud %>%

  filter(is_fraud == 1) %>%

  count(category, wt = amt) %>%

  mutate(percentage = (n / sum(n)) * 100) %>%

  arrange(desc(percentage))

 
# Create a bar chart

bar_chart <- ggplot(fraud_by_category, aes(x = reorder(category, percentage), y = percentage)) +

  geom_bar(stat = "identity") +

  labs(title = "Percentage of Fraudulent Transactions by Merchant Category",

       x = "Merchant Category", y = "Percentage of Fraudulent Transactions") +

  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Print the bar chart

bar_chart
```

<img src="/blogs/finalgroupproject_files/figure-html/unnamed-chunk-7-1.png" width="672" />


```r
# distance between card holder's home and transaction
# code adapted from https://www.geeksforgeeks.org/program-distance-two-points-earth/amp/

library(tidyverse)
library(skimr)
library(here)
library(lubridate)

fraud <- card_fraud %>%
  mutate(
    
    # convert latitude/longitude to radians
    lat1_radians = lat / 57.29577951,
    lat2_radians = merch_lat / 57.29577951,
    long1_radians = long / 57.29577951,
    long2_radians = merch_long / 57.29577951,
    
    # calculate distance in miles
    distance_miles = 3963.0 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians)),

    # calculate distance in km
    distance_km = 6377.830272 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians))

  )

fraud %>% 
  ggplot(aes(x = distance_km, y = is_fraud)) +
  geom_boxplot()
```

<img src="/blogs/finalgroupproject_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Group all variables by type and examine each variable class by class. The dataset has the following types of variables:

1.  Strings
2.  Geospatial Data
3.  Dates
4.  Date/Times
5.  Numerical

Strings are usually not a useful format for classification problems. The strings should be converted to factors, dropped, or otherwise transformed.

***Strings to Factors***

-   `category`, Category of Merchant
-   `job`, Job of Credit Card Holder

***Strings to Geospatial Data***

We have plenty of geospatial data as lat/long pairs, so I want to convert city/state to lat/long so I can compare to the other geospatial variables. This will also make it easier to compute new variables like the distance the transaction is from the home location.

-   `city`, City of Credit Card Holder
-   `state`, State of Credit Card Holder

## Exploring factors: how is the compactness of categories?

-   Do we have excessive number of categories? Do we want to combine some?


```r
card_fraud %>% 
  count(category, sort=TRUE)%>% 
  mutate(perc = n/sum(n))
```

```
## # A tibble: 14 × 3
##    category           n   perc
##    <chr>          <int>  <dbl>
##  1 gas_transport  68046 0.101 
##  2 grocery_pos    63791 0.0951
##  3 home           63597 0.0948
##  4 shopping_pos   60416 0.0900
##  5 kids_pets      58772 0.0876
##  6 shopping_net   50743 0.0756
##  7 entertainment  48521 0.0723
##  8 food_dining    47527 0.0708
##  9 personal_care  46843 0.0698
## 10 health_fitness 44341 0.0661
## 11 misc_pos       41244 0.0615
## 12 misc_net       32829 0.0489
## 13 grocery_net    23485 0.0350
## 14 travel         20873 0.0311
```

```r
card_fraud %>% 
  count(job, sort=TRUE) %>% 
  mutate(perc = n/sum(n))
```

```
## # A tibble: 494 × 3
##    job                            n    perc
##    <chr>                      <int>   <dbl>
##  1 Film/video editor           5106 0.00761
##  2 Exhibition designer         4728 0.00705
##  3 Naval architect             4546 0.00677
##  4 Surveyor, land/geomatics    4448 0.00663
##  5 Materials engineer          4292 0.00640
##  6 Designer, ceramics/pottery  4262 0.00635
##  7 IT trainer                  4014 0.00598
##  8 Financial adviser           3959 0.00590
##  9 Systems developer           3948 0.00588
## 10 Environmental consultant    3831 0.00571
## # ℹ 484 more rows
```

#We have 14 categories. We believe that agreggating some of them (e.g. grocery_pos and grocery_net into grocery, shopping_pos and shopping_net into shopping) would minimize number of categories and increase the chance of category being a predictor of new data, as each category would contain more records to train from.

The predictors `category` and `job` are transformed into factors.


```r
card_fraud <- card_fraud %>% 
  mutate(category = factor(category),
         job = factor(job))
```

`category` has 14 unique values, and `job` has 494 unique values. The dataset is quite large, with over 670K records, so these variables don't have an excessive number of levels at first glance. However, it is worth seeing if we can compact the levels to a smaller number.

### Why do we care about the number of categories and whether they are "excessive"?

Consider the extreme case where a dataset had categories that only contained one record each. There is simply insufficient data to make correct predictions using category as a predictor on new data with that category label. Additionally, if your modeling uses dummy variables, having an extremely large number of categories will lead to the production of a huge number of predictors, which can slow down the fitting. This is fine if all the predictors are useful, but if they aren't useful (as in the case of having only one record for a category), trimming them will improve the speed and quality of the data fitting.

If I had subject matter expertise, I could manually combine categories. If you don't have subject matter expertise, or if performing this task would be too labor intensive, then you can use cutoffs based on the amount of data in a category. If the majority of the data exists in only a few categories, then it might be reasonable to keep those categories and lump everything else in an "other" category or perhaps even drop the data points in smaller categories.

## Do all variables have sensible types?

Consider each variable and decide whether to keep, transform, or drop it. This is a mixture of Exploratory Data Analysis and Feature Engineering, but it's helpful to do some simple feature engineering as you explore the data. In this project, we have all data to begin with, so any transformations will be performed on the entire dataset. Ideally, do the transformations as a `recipe_step()` in the tidymodels framework. Then the transformations would be applied to any data the recipe was used on as part of the modeling workflow. There is less chance of data leakage or missing a step when you perform the feature engineering in the recipe.

## Which variables to keep in your model?

You have a number of variables and you have to decide which ones to use in your model. For instance, you have the latitude/lognitude of the customer, that of the merchant, the same data in radians, as well as the `distance_km` and `distance_miles`. Do you need them all?

## Fit your workflows in smaller sample

You will be running a series of different models, along the lines of the California housing example we have seen in class. However, this dataset has 670K rows and if you try various models and run cross validation on them, your computer may slow down or crash.

Thus, we will work with a smaller sample of 10% of the values the original dataset to identify the best model, and once we have the best model we can use the full dataset to train- test our best model.


```r
# select a smaller subset
my_card_fraud <- card_fraud %>% 
  # select a smaller subset, 10% of the entire dataframe 
  slice_sample(prop = 0.10) 
```

## Split the data in training - testing


```r
# **Split the data**

set.seed(123)

data_split <- initial_split(my_card_fraud, # updated data
                           prop = 0.8, 
                           strata = is_fraud)

card_fraud_train <- training(data_split) 
card_fraud_test <- testing(data_split)
```

## Cross Validation

Start with 3 CV folds to quickly get an estimate for the best model and you can increase the number of folds to 5 or 10 later.


```r
set.seed(123)
cv_folds <- vfold_cv(data = card_fraud_train, 
                          v = 5, 
                          strata = is_fraud)
cv_folds 
```

```
## #  5-fold cross-validation using stratification 
## # A tibble: 5 × 2
##   splits                id   
##   <list>                <chr>
## 1 <split [42944/10737]> Fold1
## 2 <split [42945/10736]> Fold2
## 3 <split [42945/10736]> Fold3
## 4 <split [42945/10736]> Fold4
## 5 <split [42945/10736]> Fold5
```

## Define a tidymodels `recipe`

What steps are you going to add to your recipe? Do you need to do any log transformations?

We are doing the following steps:

1.  step_rm(): excludes {job, city, state} variables because they have \>50 different values with no dominating ones. It is impractical to convert this variable to a large number of dummies\
    We also excluded "trans_date_trans_time" and "dob", as R had problems with treating them as numerical variables in the model. Moreover, the date-time was already split into several distinct variables, and age is a new variable that stems from dob.

2.  step_other(): groups infrequent categories (bottom 3) into the "other" value. The others would be split into regular dummy variables

3.  step_novel(): adds a catch-all level to a factor for any new values not encountered in model training. This is especially important because we are only using 10% of data as a sample

4.  step_dummy(): converts nominal disperse data into dummy separate variables

5.  step_zv(): omits zero variance variables (if any)

6.  step_normalize(): standardizes and normalizes the numerical variables to fit (mean = 0, sd = 1). Note - the underlying distribution is not necessarily normal. We believe that transformation just shifts the mean and changes the scale, but does not change the nature of distribution (as would other methods, such as Gaussian copulas)

We did not include step_corr() to remove highly correlated data for the following reasons:

-   There is not many numerical variables across which it is possible to measure correlation

-   We don't expect any correlation among these numerical variables. Possible, a positive correlation exists between amount and age of cardholder (as more mature cardholders are likely to have more money in their account or higher credit card limits).


```r
fraud_rec <- recipe(is_fraud ~ ., data = card_fraud_train) %>% 
  
  step_rm(job, state, city, trans_date_trans_time, dob) %>%  # Excludes the variables with large number of values and no dominant ones.


 step_other(category, threshold = .05) %>% # Groups infrequent categories into the "other" value. 
  
 step_novel(all_nominal(), -all_outcomes()) %>% # Adds a catch-all level to a factor for any new values not encountered in model training.
  

 step_dummy(all_nominal(), -all_outcomes()) %>% # Converts nominal data into dummy variables
  
 step_zv(all_numeric(), -all_outcomes())  %>% # Omits zero variance variables (if any)
  
 step_normalize(all_numeric()) # Eugene: standardizes and normalizes the numerical variables to fit (mean = 0, sd = 1). Note - the underlying distribution is not necessarily normal. We believe that transformation just shifts the mean and changes the scale, but does not change the nature of distribution (as would other methods, such as Gaussian copulas)

fraud_rec
```

```
## 
```

```
## ── Recipe ──────────────────────────────────────────────────────────────────────
```

```
## 
```

```
## ── Inputs
```

```
## Number of variables by role
```

```
## outcome:    1
## predictor: 23
```

```
## 
```

```
## ── Operations
```

```
## • Variables removed: job, state, city, trans_date_trans_time, dob
```

```
## • Collapsing factor levels for: category
```

```
## • Novel factor level assignment for: all_nominal(), -all_outcomes()
```

```
## • Dummy variables from: all_nominal(), -all_outcomes()
```

```
## • Zero variance filter on: all_numeric(), -all_outcomes()
```

```
## • Centering and scaling for: all_numeric()
```

Once you have your recipe, you can check the pre-processed dataframe


```r
prepped_data <- 
  fraud_rec %>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

glimpse(prepped_data)
```

```
## Rows: 53,681
## Columns: 46
## $ year                    <dbl> 1.5959320, 1.5959320, 1.5959320, -0.6265814, -…
## $ amt                     <dbl> 2.469216388, -0.458547019, -0.448034527, -0.30…
## $ lat                     <dbl> 0.61589333, -1.32136984, -1.49713883, -0.82330…
## $ long                    <dbl> -1.381606231, -0.667135721, 0.504062413, 0.712…
## $ city_pop                <dbl> -0.2021976, -0.2751456, -0.2606407, -0.2800523…
## $ merch_lat               <dbl> 0.6996997, -1.3589322, -1.6529111, -1.0121713,…
## $ merch_long              <dbl> -1.448505452, -0.687579671, 0.469294777, 0.739…
## $ hour                    <dbl> -0.2664875, 1.3465851, 0.1734414, 0.7600133, 0…
## $ age                     <dbl> -0.59194589, 0.69139766, -1.37975989, 0.670261…
## $ lat1_radians            <dbl> 0.61589333, -1.32136984, -1.49713883, -0.82330…
## $ lat2_radians            <dbl> 0.6996997, -1.3589322, -1.6529111, -1.0121713,…
## $ long1_radians           <dbl> -1.381606231, -0.667135721, 0.504062413, 0.712…
## $ long2_radians           <dbl> -1.448505452, -0.687579671, 0.469294777, 0.739…
## $ distance_miles          <dbl> 0.54171082, -1.31383037, 0.95836285, 1.3333056…
## $ distance_km             <dbl> 0.54171082, -1.31383037, 0.95836285, 1.3333056…
## $ is_fraud                <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_food_dining    <dbl> -0.275968, -0.275968, 3.623541, -0.275968, -0.…
## $ category_gas_transport  <dbl> -0.3341197, -0.3341197, -0.3341197, -0.3341197…
## $ category_grocery_pos    <dbl> -0.3214335, -0.3214335, -0.3214335, -0.3214335…
## $ category_health_fitness <dbl> -0.2658263, -0.2658263, -0.2658263, -0.2658263…
## $ category_home           <dbl> -0.3248788, -0.3248788, -0.3248788, -0.3248788…
## $ category_kids_pets      <dbl> -0.305082, -0.305082, -0.305082, 3.277746, -0.…
## $ category_misc_pos       <dbl> -0.2581529, -0.2581529, -0.2581529, -0.2581529…
## $ category_personal_care  <dbl> -0.2801666, 3.5692381, -0.2801666, -0.2801666,…
## $ category_shopping_net   <dbl> -0.2893797, -0.2893797, -0.2893797, -0.2893797…
## $ category_shopping_pos   <dbl> -0.3117823, -0.3117823, -0.3117823, -0.3117823…
## $ category_other          <dbl> 2.7800133, -0.3597038, -0.3597038, -0.3597038,…
## $ wday_1                  <dbl> -0.8101822, 1.0632152, 0.5948659, -1.2785315, …
## $ wday_2                  <dbl> 0.2640467, -0.6978025, -1.1787270, 1.7068204, …
## $ wday_3                  <dbl> 1.1128107, -1.0560449, -0.3330931, -1.0560449,…
## $ wday_4                  <dbl> -1.1943111, -0.1060618, 1.1998374, 0.9821876, …
## $ wday_5                  <dbl> 1.3184024, 0.9795884, 0.8666504, -0.3756675, -…
## $ wday_6                  <dbl> -0.7796784, 1.6343008, -0.7796784, 0.2548841, …
## $ wday_7                  <dbl> 0.37629985, 1.08840384, -1.76001213, -0.030616…
## $ month_name_01           <dbl> -0.34174608, -0.34174608, -1.50982548, -0.6337…
## $ month_name_02           <dbl> -0.7610013, -0.7610013, 2.1329196, -0.3088262,…
## $ month_name_03           <dbl> 1.08448339, 1.08448339, -1.89528097, 1.2500258…
## $ month_name_04           <dbl> 0.3107243, 0.3107243, 1.5940442, -0.6371824, -…
## $ month_name_05           <dbl> -0.9784639, -0.9784639, -0.7945671, -0.2888510…
## $ month_name_06           <dbl> 0.7316518, 0.7316518, 0.7316518, 1.3501559, 0.…
## $ month_name_07           <dbl> 0.8395685, 0.8395685, -0.2661291, -0.7399995, …
## $ month_name_08           <dbl> -1.2241882, -1.2241882, 0.2440326, -0.2931214,…
## $ month_name_09           <dbl> 0.34061701, 0.34061701, -0.01028147, 1.4371747…
## $ month_name_10           <dbl> 1.170886784, 1.170886784, 0.004077521, -1.6603…
## $ month_name_11           <dbl> -1.658665696, -1.658665696, -0.006114322, 1.11…
## $ month_name_12           <dbl> 1.11569247, 1.11569247, 0.05461099, -0.4200833…
```

## Define various models

You should define the following classification models:

1.  Logistic regression, using the `glm` engine
2.  Decision tree, using the `C5.0` engine
3.  Random Forest, using the `ranger` engine and setting `importance = "impurity"`)\
4.  A boosted tree using Extreme Gradient Boosting, and the `xgboost` engine
5.  A k-nearest neighbours, using 4 nearest_neighbors and the `kknn` engine


```r
## Model Building 

# 1. Pick a `model type`
# 2. set the `engine`
# 3. Set the `mode`:  classification

# We will use classification mode everywhere, since it is our primary task - to classify (predict) future transactions into Fraudulent/Not Fraudulent buckets

# The engines are used as per instructions above

# Logistic regression
log_spec <-  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

# Show your model specification
log_spec
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
# Decision Tree
tree_spec <- decision_tree() %>%
  set_engine(engine = "C5.0") %>%
  set_mode("classification")

tree_spec
```

```
## Decision Tree Model Specification (classification)
## 
## Computational engine: C5.0
```

```r
# Random Forest
library(ranger)

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")


# Boosted tree (XGBoost)
library(xgboost)
```

```
## 
## Attaching package: 'xgboost'
```

```
## The following object is masked from 'package:dplyr':
## 
##     slice
```

```r
xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

# K-nearest neighbour (k-NN)
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification") 
```

## Bundle recipe and model with `workflows`


```r
## Bundle recipe and model with `workflows`

# Workflow for logistic regression
log_wflow <- # new workflow object
 workflow() %>% # use workflow function
 add_recipe(fraud_rec) %>%   # use the new recipe
 add_model(log_spec)   # add your model spec

# show object
log_wflow
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: logistic_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 6 Recipe Steps
## 
## • step_rm()
## • step_other()
## • step_novel()
## • step_dummy()
## • step_zv()
## • step_normalize()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
# Workflow for decision tree
tree_wflow <-
 workflow() %>%
 add_recipe(fraud_rec) %>% 
 add_model(tree_spec) 

# Workflow for random forest
rf_wflow <-
 workflow() %>%
 add_recipe(fraud_rec) %>% 
 add_model(rf_spec) 

# Workflow for gradient boosting
xgb_wflow <-
 workflow() %>%
 add_recipe(fraud_rec) %>% 
 add_model(xgb_spec)

# Workflow for K-nearest neighbor algorithm
knn_wflow <-
 workflow() %>%
 add_recipe(fraud_rec) %>% 
 add_model(knn_spec)
```

## Fit models

You may want to compare the time it takes to fit each model. `tic()` starts a simple timer and `toc()` stops it


```r
# We have amended the code to be reusable in a function form
time_elapsed <- function(x) {
tic()
log_res <- {{x}} %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
time <- toc()
log_time <- time[[4]]
}

# We can later run the function, changing the argument (workflow name)
log_time <- time_elapsed(log_wflow)
```

```
## → A | warning: prediction from a rank-deficient fit may be misleading
```

```
## There were issues with some computations   A: x1
```

```
## There were issues with some computations   A: x3
```

```
## There were issues with some computations   A: x5
## There were issues with some computations   A: x5
```

```
## 
```

```
## 6.513 sec elapsed
```

```r
tree_time <- time_elapsed(tree_wflow)
```

```
## 25.076 sec elapsed
```

```r
rf_time <- time_elapsed(rf_wflow)
```

```
## 63.119 sec elapsed
```

```r
xgb_time <- time_elapsed(xgb_wflow)
```

```
## 6.183 sec elapsed
```

```r
#knn_time <- time_elapsed(knn_wflow)

log_time
```

```
## [1] "6.513 sec elapsed"
```

```r
tree_time
```

```
## [1] "25.076 sec elapsed"
```

```r
rf_time
```

```
## [1] "63.119 sec elapsed"
```

```r
xgb_time
```

```
## [1] "6.183 sec elapsed"
```

```r
# We have to comment the function call for knn_wflow, as it takes too long (221 sec in our experiments, had to terminate)
#time_elapsed(knn_wflow)
```

Time elapsed (recorded with 3 folds):

-   Logistic model: 3.88 sec

-   Decision tree: 15.893 sec

-   Random forest: 27.505 sec

-   Gradient boosting: 3.198 sec

-   KNN: 221.735 sec

As we can see, most quick models are gradient boosting and logistic (less than 4 seconds). With this number of dummy variables, other models take longer. KNN-model took nearly 4 minutes to complete. (note that this records is run by another team member who got better machine than the one who did the final knitting, and also with only 3 folds, the final results when knitted were different in numbers but similar in the message) -> we remove Knn from further assessment due to its inefficiency

## Compare models


```r
#run the sampling for all workflow
log_res <-
  log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 
```

```
## → A | warning: prediction from a rank-deficient fit may be misleading
```

```
## There were issues with some computations   A: x1
```

```
## There were issues with some computations   A: x2
```

```
## There were issues with some computations   A: x4
```

```
## There were issues with some computations   A: x5
```

```
## 
```

```r
tree_res <-
  tree_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

xgb_res <-
  xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

#knn_res <-
#  knn_wflow %>% 
#  fit_resamples(
#    resamples = cv_folds, 
#    metrics = metric_set(
#      recall, precision, f_meas, 
#      accuracy, kap,
#      roc_auc, sens, spec),
#    control = control_resamples(save_pred = TRUE)
#    ) 
```


```r
## Model Comparison

log_metrics <- 
  log_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "Logistic Regression",
         time = log_time)

tree_metrics <- 
  tree_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "Decision Tree",
         time = tree_time)

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "Random Forest",
         time = rf_time)

xgb_metrics <- 
  xgb_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "XGBoost",
         time = xgb_time)

#knn_metrics <- 
#  knn_res %>% 
#  collect_metrics(summarise = TRUE) %>%
#  # add the name of the model to every row
#  mutate(model = "Knn",
#         time = knn_time)
```



```r
# create dataframe with all models
model_compare <- bind_rows(log_metrics,
                            tree_metrics,
                            rf_metrics,
                           xgb_metrics,
                           #knn_metrics
                      ) %>% 
  # get rid of 'sec elapsed' and turn it into a number
  mutate(time = str_sub(time, end = -13) %>% 
           as.double()
         )

model_compare
```

```
## # A tibble: 32 × 8
##    .metric   .estimator   mean     n  std_err .config              model    time
##    <chr>     <chr>       <dbl> <int>    <dbl> <chr>                <chr>   <dbl>
##  1 accuracy  binary     0.993      5 0.000476 Preprocessor1_Model1 Logist…  6.51
##  2 f_meas    binary     0.0381     5 0.00492  Preprocessor1_Model1 Logist…  6.51
##  3 kap       binary     0.0368     5 0.00488  Preprocessor1_Model1 Logist…  6.51
##  4 precision binary     0.209      5 0.0564   Preprocessor1_Model1 Logist…  6.51
##  5 recall    binary     0.0216     5 0.00277  Preprocessor1_Model1 Logist…  6.51
##  6 roc_auc   binary     0.839      5 0.00789  Preprocessor1_Model1 Logist…  6.51
##  7 sens      binary     0.0216     5 0.00277  Preprocessor1_Model1 Logist…  6.51
##  8 spec      binary     0.999      5 0.000147 Preprocessor1_Model1 Logist…  6.51
##  9 accuracy  binary     0.997      5 0.000370 Preprocessor1_Model1 Decisi… 25.1 
## 10 f_meas    binary     0.717      5 0.0327   Preprocessor1_Model1 Decisi… 25.1 
## # ℹ 22 more rows
```


```r
#pivot longer for graphing
 model_comp <- model_compare %>% 
  select(model, .metric, mean, std_err, time) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

model_comp
```

```
## # A tibble: 4 × 18
##   model       time mean_accuracy mean_f_meas mean_kap mean_precision mean_recall
##   <chr>      <dbl>         <dbl>       <dbl>    <dbl>          <dbl>       <dbl>
## 1 Logistic …  6.51         0.993      0.0381   0.0368          0.209      0.0216
## 2 Decision … 25.1          0.997      0.717    0.716           0.868      0.617 
## 3 Random Fo… 63.1          0.996      0.450    0.449           0.987      0.292 
## 4 XGBoost     6.18         0.997      0.750    0.749           0.903      0.645 
## # ℹ 11 more variables: mean_roc_auc <dbl>, mean_sens <dbl>, mean_spec <dbl>,
## #   std_err_accuracy <dbl>, std_err_f_meas <dbl>, std_err_kap <dbl>,
## #   std_err_precision <dbl>, std_err_recall <dbl>, std_err_roc_auc <dbl>,
## #   std_err_sens <dbl>, std_err_spec <dbl>
```


```r
#product the graph comparing mean of AUC of ROC across methods 
AUC_graph <- model_comp %>% 
                  arrange(mean_roc_auc) %>% 
                  mutate(model = fct_reorder(model, mean_roc_auc)) %>% # order results
                  ggplot(aes(model, mean_roc_auc, fill=model)) +
                  geom_col() +
                  coord_flip() +
                  scale_fill_brewer(palette = "Blues") +
                   geom_text(
                     size = 3,
                     aes(label = round(mean_roc_auc, 2), 
                         y = mean_roc_auc + 0.08),
                     vjust = 1
                  )+
                  theme_light()+
                  theme(legend.position = "none")+
                  labs(y = NULL)
```

```r
time_graph <- model_comp %>% 
                arrange(time) %>% 
                mutate(model = fct_reorder(model, time)) %>% # order results
                ggplot(aes(model, time, fill=model)) +
                geom_col() +
                coord_flip() +
                scale_fill_brewer(palette = "Blues") +
                 geom_text(
                   size = 3,
                   aes(label = round(mean_roc_auc, 2), 
                       y = mean_roc_auc + 0.08),
                   vjust = 1
                )+
                theme_light()+
                theme(legend.position = "none")+
                labs(y = NULL)
```

```r
library(patchwork)

AUC_graph/
time_graph
```

<img src="/blogs/finalgroupproject_files/figure-html/unnamed-chunk-19-1.png" width="672" />
It is clear XGBoost is most correct as well as most effective


## Which metric to use

This is a highly imbalanced data set, as roughly 99.5% of all transactions are ok, and it's only 0.5% of transactions that are fraudulent. A `naive` model, which classifies everything as ok and not-fraud, would have an accuracy of 99.5%, but what about the sensitivity, specificity, the AUC, etc?

## `last_fit()`


```r
## `last_fit()` on test set

# - `last_fit()`  fits a model to the whole training data and evaluates it on the test set. 
# - provide the workflow object of the best model as well as the data split object (not the training data). 
last_fit_xgb <- last_fit(xgb_wflow,split = data_split) 
```

## Get variable importance using `vip` package


```r
last_fit_xgb %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()
```

```
## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
## ℹ Please use `extract_fit_parsnip()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="/blogs/finalgroupproject_files/figure-html/unnamed-chunk-21-1.png" width="672" />

## Plot Final Confusion matrix and ROC curve


```r
## Final Confusion Matrix

last_fit_xgb %>%
  collect_predictions() %>% 
  conf_mat(is_fraud, .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/blogs/finalgroupproject_files/figure-html/unnamed-chunk-22-1.png" width="672" />

```r
## Final ROC curve
last_fit_xgb %>% 
  collect_predictions() %>% 
  roc_curve(is_fraud, .pred_1) %>% 
  autoplot()
```

<img src="/blogs/finalgroupproject_files/figure-html/unnamed-chunk-22-2.png" width="672" />

## Calculating the cost of fraud to the company

-   How much money (in US\$ terms) are fraudulent transactions costing the company? Generate a table that summarizes the total amount of legitimate and fraudulent transactions per year and calculate the % of fraudulent transactions, in US\$ terms. Compare your model vs the naive classification that we do not have any fraudulent transactions.


```r
best_model_preds <- 
  xgb_wflow %>% 
  fit(data = card_fraud_train) %>%  
  
  ## Use `augment()` to get predictions for entire data set
  augment(new_data = card_fraud)

best_model_preds
```

```
## # A tibble: 671,028 × 27
##    trans_date_trans_time  year category    amt city  state   lat   long city_pop
##    <dttm>                <dbl> <fct>     <dbl> <chr> <chr> <dbl>  <dbl>    <dbl>
##  1 2019-02-22 07:32:58    2019 entertai…  7.79 Veed… IN     40.1  -87.3     4049
##  2 2019-02-16 15:07:20    2019 kids_pets  3.89 Holl… OH     40.0  -81.0      128
##  3 2019-12-27 22:25:34    2019 personal…  8.43 Arno… MO     38.4  -90.4    35439
##  4 2019-03-03 10:11:39    2019 grocery_… 40    Apis… TN     35.0  -85.0     3730
##  5 2019-02-09 17:14:54    2019 food_din… 54.0  Red … CO     39.5 -106.       277
##  6 2019-09-09 01:19:59    2019 shopping… 95.6  Irwi… GA     32.8  -83.2     1841
##  7 2019-12-15 09:11:53    2019 gas_tran… 65.0  Rani… MN     48.6  -93.3      136
##  8 2020-06-02 17:31:41    2020 home       3.41 Hunt… AL     34.6  -86.6   190178
##  9 2019-12-28 16:58:06    2019 entertai… 69.1  Lore… TX     33.7 -102.      1571
## 10 2019-12-15 12:14:45    2019 entertai…  7.23 Bowd… ME     44.1  -70.0     3224
## # ℹ 671,018 more rows
## # ℹ 18 more variables: job <fct>, dob <date>, merch_lat <dbl>,
## #   merch_long <dbl>, is_fraud <fct>, hour <int>, wday <ord>, month_name <ord>,
## #   age <dbl>, lat1_radians <dbl>, lat2_radians <dbl>, long1_radians <dbl>,
## #   long2_radians <dbl>, distance_miles <dbl>, distance_km <dbl>,
## #   .pred_class <fct>, .pred_1 <dbl>, .pred_0 <dbl>
```

```r
best_model_preds %>% 
  conf_mat(truth = is_fraud, estimate = .pred_class)
```

```
##           Truth
## Prediction      1      0
##          1   2690    226
##          0   1246 666866
```



```r
cost <- best_model_preds %>%
  select(is_fraud, amt, pred = .pred_class)

cost
```

```
## # A tibble: 671,028 × 3
##    is_fraud   amt pred 
##    <fct>    <dbl> <fct>
##  1 0         7.79 0    
##  2 0         3.89 0    
##  3 0         8.43 0    
##  4 0        40    0    
##  5 0        54.0  0    
##  6 0        95.6  0    
##  7 0        65.0  0    
##  8 0         3.41 0    
##  9 0        69.1  0    
## 10 0         7.23 0    
## # ℹ 671,018 more rows
```



```r
cost <- cost %>%
  mutate(

  # naive false-- we think every single transaction is ok and not fraud
    false_naives=ifelse(is_fraud==1,amt,0),

  # false negatives-- we thought they were not fraud, but they were
    false_negatives=ifelse((is_fraud==1)&(pred==0),amt,0),
  
  
  # false positives-- we thought they were fraud, but they were not
    false_positives=ifelse((is_fraud==0)&(pred==1),amt,0),
  
    
  # true positives-- we thought they were fraud, and they were 
    true_positives=ifelse((is_fraud==1)&(pred==1),amt,0),

  
  # true negatives-- we thought they were ok, and they were 
    true_negatives=ifelse((is_fraud==0)&(pred==0),amt,0),
)
  
# Summarising

cost_summary <- cost %>% 
  summarise(across(starts_with(c("false","true", "amt")), 
            ~ sum(.x, na.rm = TRUE)))

cost_summary
```

```
## # A tibble: 1 × 6
##   false_naives false_negatives false_positives true_positives true_negatives
##          <dbl>           <dbl>           <dbl>          <dbl>          <dbl>
## 1     2075089.         472080.         157422.       1603009.      44951393.
## # ℹ 1 more variable: amt <dbl>
```

-   If we use a naive classifier thinking that all transactions are legitimate and not fraudulent, the cost to the company is 
$2,075,089.

```r
  scales::dollar(cost_summary$false_naives)
```

```
## [1] "$2,075,089"
```

-   With our best model, the total cost of false negatives, namely transactions our classifier thinks are legitimate but which turned out to be fraud, is $472,080.


```r
  scales::dollar(cost_summary$false_negatives)
```

```
## [1] "$472,080"
```


-   Our classifier also has some false positives, $157,422, namely flagging transactions as fraudulent, but which were legitimate. 

```r
  scales::dollar(cost_summary$false_positives)
```

```
## [1] "$157,422"
```

Assuming the card company makes around 2% for each transaction (source: <https://startups.co.uk/payment-processing/credit-card-processing-fees/>), the amount of money lost due to these false positives is $3,148.44

```r
  scales::dollar(cost_summary$false_positives* 0.02)
```

```
## [1] "$3,148.44"
```


-   The \$ improvement over the naive policy is $1,599,861.


```r
  scales::dollar(cost_summary$false_naives - cost_summary$false_negatives - cost_summary$false_positives * 0.02)
```

```
## [1] "$1,599,861"
```


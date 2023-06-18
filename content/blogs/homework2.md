---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: cooking.jpg
keywords: ""
slug: homework2
title: Exploratory Data Analysis
---




# Data Visualisation - Exploration

Now that you've demonstrated your software is setup, and you have the basics of data manipulation, the goal of this assignment is to practice transforming, visualising, and exploring data.

# Mass shootings in the US

In July 2012, in the aftermath of a mass shooting in a movie theater in Aurora, Colorado, [Mother Jones](https://www.motherjones.com/politics/2012/07/mass-shootings-map/) published a report on mass shootings in the United States since 1982. Importantly, they provided the underlying data set as [an open-source database](https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/) for anyone interested in studying and understanding this criminal behavior.

## Obtain the data


```
## Rows: 125
## Columns: 14
## $ case                 <chr> "Oxford High School shooting", "San Jose VTA shoo…
## $ year                 <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2020, 2020, 2…
## $ month                <chr> "Nov", "May", "Apr", "Mar", "Mar", "Mar", "Mar", …
## $ day                  <dbl> 30, 26, 15, 31, 22, 16, 16, 26, 10, 6, 31, 4, 3, …
## $ location             <chr> "Oxford, Michigan", "San Jose, California", "Indi…
## $ summary              <chr> "Ethan Crumbley, a 15-year-old student at Oxford …
## $ fatalities           <dbl> 4, 9, 8, 4, 10, 8, 4, 5, 4, 3, 7, 9, 22, 3, 12, 5…
## $ injured              <dbl> 7, 0, 7, 1, 0, 1, 0, 0, 3, 8, 25, 27, 26, 12, 4, …
## $ total_victims        <dbl> 11, 9, 15, 5, 10, 9, 4, 5, 7, 11, 32, 36, 48, 15,…
## $ location_type        <chr> "School", "Workplace", "Workplace", "Workplace", …
## $ male                 <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
## $ age_of_shooter       <dbl> 15, 57, 19, NA, 21, 21, 31, 51, NA, NA, 36, 24, 2…
## $ race                 <chr> NA, NA, "White", NA, NA, "White", NA, "Black", "B…
## $ prior_mental_illness <chr> NA, "Yes", "Yes", NA, "Yes", NA, NA, NA, NA, NA, …
```

| column(variable)     | description                                                                 |
|--------------------------|----------------------------------------------|
| case                 | short name of incident                                                      |
| year, month, day     | year, month, day in which the shooting occurred                             |
| location             | city and state where the shooting occcurred                                 |
| summary              | brief description of the incident                                           |
| fatalities           | Number of fatalities in the incident, excluding the shooter                 |
| injured              | Number of injured, non-fatal victims in the incident, excluding the shooter |
| total_victims        | number of total victims in the incident, excluding the shooter              |
| location_type        | generic location in which the shooting took place                           |
| male                 | logical value, indicating whether the shooter was male                      |
| age_of_shooter       | age of the shooter when the incident occured                                |
| race                 | race of the shooter                                                         |
| prior_mental_illness | did the shooter show evidence of mental illness prior to the incident?      |

## Explore the data

### Specific questions

-   Generate a data frame that summarizes the number of mass shootings per year.


```r
mass_shootings %>%
  group_by(year) %>%
  summarise(count=n())
```

```
## # A tibble: 37 × 2
##     year count
##    <dbl> <int>
##  1  1982     1
##  2  1984     2
##  3  1986     1
##  4  1987     1
##  5  1988     1
##  6  1989     2
##  7  1990     1
##  8  1991     3
##  9  1992     2
## 10  1993     4
## # ℹ 27 more rows
```

-   Generate a bar chart that identifies the number of mass shooters associated with each race category. The bars should be sorted from highest to lowest and each bar should show its number.


```r
race_mass_shootings<-mass_shootings %>%
  group_by(race) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(race,-count),y=count))+
  geom_col()+ 
  geom_text(aes(label=count),vjust=-0.5) +
  labs(title = "Mass Shootings by race",
       x = "Race", y="Number of Mass Shootings") +
  theme_bw()

race_mass_shootings
```

<img src="/blogs/homework2_files/figure-html/unnamed-chunk-4-1.png" width="672" />

-   Generate a boxplot visualizing the number of total victims, by type of location.


```r
victims_location_mass_shootings<-mass_shootings %>% 
  group_by(location_type) %>% 
  ggplot(aes(x=location_type,y=total_victims))+geom_boxplot()+
labs(title = "Total Victims in Mass Shootings by Location ",
       x = "Location", y="Number of Victims") +
  theme_bw() 

victims_location_mass_shootings
```

<img src="/blogs/homework2_files/figure-html/unnamed-chunk-5-1.png" width="672" />

-   Redraw the same plot, but remove the Las Vegas Strip massacre from the dataset.


```r
victims_location_mass_shootings_excl<-
  mass_shootings %>% 
  filter(case != "Las Vegas Strip massacre") %>% 
  group_by(location_type) %>% 
  ggplot(aes(x=location_type,y=total_victims))+geom_boxplot()+
labs(title = "Total Victims in Mass Shootings by Location ",
       x = "Location", y="Number of Victims") +
  theme_bw()

victims_location_mass_shootings_excl
```

<img src="/blogs/homework2_files/figure-html/unnamed-chunk-6-1.png" width="672" />

### More open-ended questions

Address the following questions. Generate appropriate figures/tables to support your conclusions.

-   How many white males with prior signs of mental illness initiated a mass shooting after 2000?


```r
mass_shootings %>%
  filter(year > 2000, race == "White", prior_mental_illness =="Yes") %>% 
  summarise(count=n())
```

```
## # A tibble: 1 × 1
##   count
##   <int>
## 1    23
```

```r
#The number of white males with prior signs of mental illness initiated a mass shooting after 2000 is 23
```

-   Which month of the year has the most mass shootings? Generate a bar chart sorted in chronological (natural) order (Jan-Feb-Mar- etc) to provide evidence of your answer.


```r
month_order<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

mass_shootings%>% 
  mutate(month = factor(month, levels=month_order)) %>% 
  group_by(month) %>% 
  summarise(count=n()) 
```

```
## # A tibble: 12 × 2
##    month count
##    <fct> <int>
##  1 Jan       7
##  2 Feb      13
##  3 Mar      12
##  4 Apr      11
##  5 May       8
##  6 Jun      12
##  7 Jul      10
##  8 Aug       8
##  9 Sep      10
## 10 Oct      11
## 11 Nov      12
## 12 Dec      11
```

```r
#convert month into factor first which has category and then order
```

-   How does the distribution of mass shooting fatalities differ between White and Black shooters? What about White and Latino shooters?


```r
mass_shootings%>% 
  filter(race %in% c("White","Black","Latino")) %>% 
 group_by(race) %>% 
  ggplot(aes(x=fatalities)) +
  geom_histogram() +
  facet_wrap(~race)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="/blogs/homework2_files/figure-html/unnamed-chunk-9-1.png" width="672" />

### Very open-ended

-   Are mass shootings with shooters suffering from mental illness different from mass shootings with no signs of mental illness in the shooter?


```r
mass_shootings%>% 
 group_by(prior_mental_illness) %>% 
  summarize(count=n())
```

```
## # A tibble: 3 × 2
##   prior_mental_illness count
##   <chr>                <int>
## 1 No                      17
## 2 Yes                     62
## 3 <NA>                    46
```



-   Assess the relationship between mental illness and total victims, mental illness and location type, and the intersection of all three variables.


```r
mass_shootings%>% 
 group_by(prior_mental_illness) %>% 
  drop_na() %>% 
  ggplot(aes(x=prior_mental_illness,y=total_victims))+geom_boxplot()+
labs(title = "Total Victims in Mass Shootings by Prior Mental Illness ",
       x = "Prior Mental Illness", y="Number of Victims") +
  theme_bw()
```

<img src="/blogs/homework2_files/figure-html/unnamed-chunk-11-1.png" width="672" />


```r
mass_shootings%>% 
 group_by(prior_mental_illness) %>% 
  drop_na() %>% 
  ggplot(aes(x=prior_mental_illness,fill=location_type))+geom_bar()+
labs(title = "Mental Illness vs Location Type ",
       x = "Prior Mental Illness", y="Location Type") +
  theme_bw()
```

<img src="/blogs/homework2_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```r
mass_shootings%>% 
 group_by(prior_mental_illness,location_type) %>% 
  drop_na() %>% 
  ggplot(aes(x=location_type,y=total_victims,fill=prior_mental_illness))+geom_bar(stat="identity",position="dodge")+
labs(title = "Relationship between Mental Illness and Location on Total Number of Victims  ",
       x = "Location Type", 
     y="Total Victim", 
     fill="Mental Illness") +
  theme_bw()
```

<img src="/blogs/homework2_files/figure-html/unnamed-chunk-13-1.png" width="672" />


Make sure to provide a couple of sentences of written interpretation of your tables/figures. Graphs and tables alone will not be sufficient to answer this question.

# Exploring credit card fraud

We will be using a dataset with credit card transactions containing legitimate and fraud transactions. Fraud is typically well below 1% of all transactions, so a naive model that predicts that all transactions are legitimate and not fraudulent would have an accuracy of well over 99%-- pretty good, no? (well, not quite as we will see later in the course)

You can read more on credit card fraud on [Credit Card Fraud Detection Using Weighted Support Vector Machine](https://www.scirp.org/journal/paperinformation.aspx?paperid=105944)

The dataset we will use consists of credit card transactions and it includes information about each transaction including customer details, the merchant and category of purchase, and whether or not the transaction was a fraud.

## Obtain the data

The dataset is too large to be hosted on Canvas or Github, so please download it from dropbox <https://www.dropbox.com/sh/q1yk8mmnbbrzavl/AAAxzRtIhag9Nc_hODafGV2ka?dl=0> and save it in your `dsb` repo, under the `data` folder


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
## $ is_fraud              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
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

-   In this dataset, how likely are fraudulent transactions? Generate a table that summarizes the number and frequency of fraudulent transactions per year.


```r
card_fraud %>%
  group_by(trans_year) %>% 
  summarise(count=n())
```

```
## # A tibble: 2 × 2
##   trans_year  count
##        <dbl>  <int>
## 1       2019 478646
## 2       2020 192382
```

-   How much money (in US\$ terms) are fraudulent transactions costing the company? Generate a table that summarizes the total amount of legitimate and fraudulent transactions per year and calculate the % of fraudulent transactions, in US\$ terms.


```r
fraudulent_transaction_cost<-card_fraud %>%
  mutate(is_fraud = case_when(is_fraud == 0 ~ "Legitimate", is_fraud == 1 ~ "Fraudulent")) %>%
  group_by(trans_year, is_fraud) %>%
  summarise(total_amount = sum(amt),.groups='keep') %>%
  pivot_wider(names_from = is_fraud, values_from = total_amount, values_fill = 0) %>%
  mutate(fraud_percentage = Fraudulent / (Legitimate + Fraudulent) * 100)

print(fraudulent_transaction_cost)
```

```
## # A tibble: 2 × 4
## # Groups:   trans_year [2]
##   trans_year Fraudulent Legitimate fraud_percentage
##        <dbl>      <dbl>      <dbl>            <dbl>
## 1       2019   1423140.  32182901.             4.23
## 2       2020    651949.  12925914.             4.80
```

-   Generate a histogram that shows the distribution of amounts charged to credit card, both for legitimate and fraudulent accounts. Also, for both types of transactions, calculate some quick summary statistics.


```r
summary_table <- card_fraud %>%
  group_by(trans_year, is_fraud) %>%
  summarize(total_amt = sum(amt))
```

```
## `summarise()` has grouped output by 'trans_year'. You can override using the
## `.groups` argument.
```

```r
# total fraud
fraud_sum <- summary_table %>%
  filter(is_fraud == 1) %>%
  mutate(fraud_amt = total_amt) %>%
  select(trans_year, fraud_amt)

# total legit
legitimate_sum <- summary_table %>%
  filter(is_fraud == 0) %>%
  mutate(legitimate_amt = total_amt) %>%
  select(trans_year, legitimate_amt)
  
fraud_percentage <- fraud_sum %>%
  left_join(legitimate_sum, by = "trans_year") %>%
  mutate(percentage_fraud = (fraud_amt / (fraud_amt + legitimate_amt)) * 100, percentage_legitimate = (legitimate_amt / (fraud_amt + legitimate_amt)) * 100) %>%
  select(trans_year, fraud_amt, legitimate_amt, percentage_fraud, percentage_legitimate) %>% 
  
ggplot(aes(x = factor(trans_year), y = fraud_amt + legitimate_amt)) +
  geom_col(aes(fill = paste0(sprintf("%.1f", round(percentage_fraud, 1)), "% Fraud\n", sprintf("%.1f", round(percentage_legitimate, 1)), "% Legitimate")), width = 0.8) +
  labs(title = "Total Amount per Year", x = "Year", y = "Total Amount") +
  scale_fill_manual(values = c("grey", "black"), name = "Transaction Type") +
  scale_x_discrete(labels = c("2019", "2020")) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(size = 10, vjust = 0.5))

print(fraud_percentage)
```

<img src="/blogs/homework2_files/figure-html/unnamed-chunk-17-1.png" width="672" />

-   What types of purchases are most likely to be instances of fraud? Consider category of merchants and produce a bar chart that shows % of total fraudulent transactions sorted in order.


```r
# Calculate the percentage of fraudulent transactions by merchant category
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

<img src="/blogs/homework2_files/figure-html/unnamed-chunk-18-1.png" width="672" />

-   When is fraud more prevalent? Which days, months, hours? To create new variables to help you in your analysis, we use the `lubridate` package and the following code

```         
mutate(
  date_only = lubridate::date(trans_date_trans_time),
  month_name = lubridate::month(trans_date_trans_time, label=TRUE),
  hour = lubridate::hour(trans_date_trans_time),
  weekday = lubridate::wday(trans_date_trans_time, label = TRUE)
  )
```

-   Are older customers significantly more likely to be victims of credit card fraud? To calculate a customer's age, we use the `lubridate` package and the following code


```r
card_fraud <- card_fraud %>% 
  mutate(
  date_only = lubridate::date(trans_date_trans_time),
  month_name = lubridate::month(trans_date_trans_time, label=TRUE),
  hour = lubridate::hour(trans_date_trans_time),
  weekday = lubridate::wday(trans_date_trans_time, label = TRUE)
  )

#When is fraud more prevalent? 
#by date #2019-01-18 has the most fraud
card_fraud %>% 
  filter(is_fraud == 1) %>% 
  group_by(date_only) %>%
  select(date_only, amt) %>% 
  arrange(desc(amt))
```

```
## # A tibble: 3,936 × 2
## # Groups:   date_only [493]
##    date_only    amt
##    <date>     <dbl>
##  1 2019-01-18 1334.
##  2 2019-12-08 1313.
##  3 2020-06-07 1313.
##  4 2019-12-20 1295.
##  5 2019-06-03 1288.
##  6 2019-08-31 1282.
##  7 2019-05-20 1262.
##  8 2019-02-15 1259.
##  9 2020-03-10 1258.
## 10 2019-10-05 1246.
## # ℹ 3,926 more rows
```

```r
#by months #Mar and May have the most fraud
card_fraud %>% 
  filter(is_fraud == 1) %>%
  group_by(month_name) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
```

```
## # A tibble: 12 × 2
##    month_name count
##    <ord>      <int>
##  1 Mar          472
##  2 May          472
##  3 Jan          461
##  4 Feb          434
##  5 Jun          387
##  6 Apr          349
##  7 Dec          301
##  8 Nov          226
##  9 Sep          219
## 10 Oct          218
## 11 Aug          213
## 12 Jul          184
```

```r
#by hours #Fraud is likely to happen at hour 23
card_fraud %>% 
  filter(is_fraud == 1) %>%
  group_by(hour) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
```

```
## # A tibble: 24 × 2
##     hour count
##    <int> <int>
##  1    23  1012
##  2    22   981
##  3     0   348
##  4     1   332
##  5     3   326
##  6     2   313
##  7    19    52
##  8    18    49
##  9    17    48
## 10    13    45
## # ℹ 14 more rows
```

```r
#by day #Monday has the most fraud
card_fraud %>% 
  filter(is_fraud == 1) %>%
  group_by(weekday) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
```

```
## # A tibble: 7 × 2
##   weekday count
##   <ord>   <int>
## 1 Mon       639
## 2 Sat       626
## 3 Sun       608
## 4 Fri       557
## 5 Thu       542
## 6 Tue       496
## 7 Wed       468
```
  

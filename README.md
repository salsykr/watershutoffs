# watershutoffs

---
title: 'U6614 Assignment 5: \break Water shutoffs, race, and income in Detroit: \break Which communities have been hardest hit?'
author: "Salma Syakira Widodo"
date: "`r Sys.Date()`"
output:
  pdf_document:
    toc: no
    toc_depth: '3'
    number_sections: yes
  html_document:
    toc: yes
    toc_depth: 3
    toc_float: yes
    number_sections: yes
    highlight: tango
    theme: default
    fig_caption: yes
    df_print: tibble
urlcolor: blue
---

```{r setup, include = FALSE}
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(weights)
library(foreign) 
library(tidyverse)

install.packages("readstata13", repos = "https://cloud.r-project.org/")
library(readstata13)

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

##### *Please submit your knitted .pdf file along with the corresponding R markdown (.rmd) via Courseworks by 11:59pm on Monday, November 1st.* {-}

##### Remember to think carefully about what code you include in your knitted document. Only include code chunks that you need to generate the plots and statistics to answer the questions below. Don't include code from your working R script (that we started in class) that was only used to inspect and validate your results, and isn't necessary to answer the questions. {-}

\medspace

# Load libraries {-}
```{r}

#load ACS data
  MI_acs_tract_10_17 <- readRDS("/Users/salmasyakira/Documents/Spring 2025/Data Analysis using R/Lecture 7/Data/MI_acs_tract_10_17.rds")

#inspect
  str(MI_acs_tract_10_17)
  summary(MI_acs_tract_10_17)
  

#what is the unit of observation?
  table(MI_acs_tract_10_17$tractid, MI_acs_tract_10_17$year) %>%
    head(n = 10)
  #we have a balanced year-tract panel!

  #population represented by the sample? 
    #all tracts in Michigan (MI)
    #tract-level data from ACS is representative of total pop in each tract

  #why the NA values? let's explore 
    MI_acs_tract_10_17 %>% 
      filter(is.na(medianinc) == TRUE) %>% 
      select(year, tractid, pop, medianinc, whiteshare) %>% 
      arrange(desc(pop))
    
    MI_acs_tract_10_17 %>% 
      filter(is.na(whiteshare) == TRUE) %>% 
      select(year, tractid, pop, medianinc, whiteshare) %>% 
      arrange(desc(pop))
  
      
## -----------------------------------------------------------------------------
## Get service interruption (SI) data - i.e. shutoff records (microdata)
## -----------------------------------------------------------------------------
  
#get service interruption data
  input_si <- read.dta13("/Users/salmasyakira/Documents/Spring 2025/Data Analysis using R/Lecture 7/Data/si_1017_cleaned.dta")
  
  #what is the unit of observation? each observation is a shutoff record.
      
    
#focus on key variables to identify period/location of every shutoff
#we'll want to join to demographic data based on tractid and get tract-level obs
  si.clean <- input_si %>% 
    select(si_order_number, census_tract_long, year, month) %>% 
    rename(tractid = census_tract_long) %>% 
    arrange(tractid, year, month)

#aggregate to tract-year/month totals
  si_tract_ym <- si.clean %>% 
    group_by(tractid, year, month) %>% 
    summarise(si_count = n_distinct(si_order_number)) %>% 
    arrange(tractid, year, month)

  #inspect
    summary(si_tract_ym)
    table(si_tract_ym$month, si_tract_ym$year) #what does this tell us?


## -----------------------------------------------------------------------------
## Join shutoff & demographic data: construct tract-year/month panel 
## -----------------------------------------------------------------------------

#join tract-yr demographic data (MI_acs_tract_10_17) to 
  # tract-month shutoff data (si_tract_ym)
#only keep tracts that are in the shutoff data (si_tract_ym)
  #MI_acs_tract_10_17 includes Detroit tracts (Wayne County), 
  #but also tracts outside of Detroit across the state of Michigan
#want to end up with a tract-year-month panel
#new df should include: all columns from two dfs and a new date column
#also filter out observation for 2017-11-01 due to incomplete data
    
#HINT: what column(s) do you want to join on?
#HINT: what kind of join would work here?
    
tract_ym <- left_join(si_tract_ym, MI_acs_tract_10_17,
                      by = c("tractid", "year")) %>% 
  mutate(date = make_date(year, month, 1)) %>% 
  arrange(tractid, year, month) %>% 
  filter(date != "2017-11-01")
  
  #inspect
  summary(tract_ym)
    
  #do we have a balanced panel? 
  table(tract_ym$month, tract_ym$year)
  
  tract_ym %>% 
    group_by(tractid) %>%
    count(tractid) %>% 
    arrange(n)
    #nope, if a tract had 0 shutoffs one month -> no row for that tract-month


#collapse to tract-level totals (summed over all year-months)
#i.e. a single obs per tract with shutoffs summed over all years
#include time-invariant measures of other variables
#NOTE: this allows us to focus on variation between tracts 
  # (not within-tracts over time)
  # will no longer be a panel dataframe
  tract <- tract_ym %>% 
    group_by(tractid) %>% #kills time dimension
    summarise(si_count = sum(si_count),
              pop = mean(pop, na.rm = TRUE),
              blackshare = mean(blackshare, na.rm = TRUE),
              black75 = round(mean(black75, na.rm = TRUE), 0),
              medianinc = mean(medianinc, na.rm = TRUE),
              inc_above_median = round(mean(inc_above_median, 
                                            na.rm = TRUE), 0) ) %>% 
    mutate(si_1000 = si_count / (pop / 1000) ) %>% #shutoffs per 1000 people
    arrange(tractid)
  
  #inspect
  summary(tract)
```

\medspace


# "Cross-sectional" analysis

- In this section we'll explore variation in shutoffs *across* Census tracts (one observation per Census tract, summing shutoffs over the whole time period).

### 1a) Visualize and interpret the relationship between share Black and shutoffs per capita across census tracts in Detroit. {-}
```{r}
# Scatter plot: % Black vs Shutoffs per Capita with Population Weighting and Color Gradient
ggplot(data = tract, 
       aes(x = blackshare, 
           y = si_1000, 
           size = pop,          # Size points based on population
           color = si_1000)) +  # Color points based on shutoff intensity
  geom_point(alpha = 0.5) +   # Adjust transparency for better visibility
  scale_size(range = c(0.1, 6), guide = "none") +  # Size scale for points, no guide
  scale_color_gradient(low = "blue", high = "red") +  # Color gradient for shutoffs
  ggtitle("Shutoffs per Capita on Black Share of Population") +  # Title
  xlab("Share of Black Residents") +  # X-axis label
  ylab("Shutoffs per 1000 People") +  # Y-axis label
  theme_minimal() +   # Minimal theme for cleaner look
  theme(
    axis.text = element_text(size = 10),  # Adjust axis text size
    axis.title = element_text(size = 12), # Adjust axis title size
    plot.title = element_text(size = 14, hjust = 0.5)  # Center title and adjust size
  )

# Calculate correlations for model fit
cor(tract$blackshare, tract$si_1000, use = "pairwise.complete.obs")
wtd.cor(tract$blackshare, tract$si_1000, weight = tract$pop)

```
This result (p = 0.472) suggests a moderate positive relationship between the
percentage of Black residents in a Census tract and the shutoffs per capita.
In other words, as the proportion of Black residents increases in a Census
tract, the shutoffs per capita also tend to increase, but the relationship
is not very strong.The p-value is extremely small (close to zero, 3.92e-21),
indicating that the observed correlation is statistically significant. 
The t-value of 10.22 is also quite large, further supporting the significance
of this correlation. Both results suggest that areas with higher percentages
of Black residents are more likely to experience higher shutoffs per capita.


### 1b) Visualize and interpret the relationship between median income and shutoffs per capita across census tracts in Detroit. {-}
```{r}
# Scatterplot: Median income vs shutoffs per capita
ggplot(data = tract, 
       aes(x = medianinc, 
           y = si_1000, 
           size = pop)) + 
  geom_point(alpha = 0.1) + # alpha adjusts the transparency of points
  scale_size(range = c(0.1, 6), guide = "none") +
  ggtitle("Shutoffs per capita vs Median Income") +
  xlab("Median Income") +
  ylab("Shutoffs per capita") 

cor(tract$medianinc, tract$si_1000, use = "pairwise.complete.obs")

wtd.cor(tract$medianinc, tract$si_1000, weight = tract$pop)

```
The positive correlation (0.1445) suggests that there is a weak positive
relationship between median income and shutoffs per capita. This indicates
that as median income increases, shutoffs per capita tend to slightly increase
as well, but the relationship is quite weak. After accounting for population
size, the correlation remains positive but still weak. The p-value for the
weighted correlation is 0.032, which is statistically significant at 5%
significance level. This means that the positive relationship between median
income and shutoffs per capita is statistically significant, even though the
strength of the relationship is weak.

### 1c) Visualize and interpret how shutoffs per capita relate to both Black share and median income on the ***same*** plot. Does race or income appear to be more salient? {-}
```{r}

ggplot(data = tract, 
       aes(x = blackshare, 
           y = medianinc, 
           size = si_1000,       
           color = si_1000)) +   
  geom_point(alpha = 0.5) +   
  scale_size(range = c(0.5, 10), guide = "none") +  
  scale_color_gradient(low = "blue", high = "red") + 
  ggtitle("Shutoffs per capita by Black share and Median Income") +
  xlab("Black Share of Population") + 
  ylab("Median Income") +
  labs(color = "Shutoffs per capita", size = "Shutoffs per capita") +
  theme_minimal()

```
From the result above, it can be seen that higher black share and lower
median income experienced higher shutoffs per capita. Both race and income
appear to be important factors in the distribution of shutoffs across Detroit,
but from the plot and correlation results, race (Black share) might be a
slightly more salient factor since it shows a clearer pattern of association
with higher shutoffs, even when income is controlled for.

\medspace


# Time-series analysis.

- In this section, we'll explore variation *between* different groups of Census tracts and over time *within* groups (with groups defined based on tract-level income and racial composition).

### 2a) Plot and interpret the shutoffs per capita over time for tracts below/above citywide median housheold income (show two time series on a single plot). {-}
```{r}
detroit_pop_hi_inc <- tract %>%
  filter(inc_above_median == 1) %>%
  summarise(sum(pop)) %>%
  as.numeric()

detroit_pop_lo_inc <- tract %>%
  filter(inc_above_median == 0) %>%
  summarise(sum(pop)) %>%
  as.numeric()


#time series of shutoffs per capita for tracts above/below citywide median inc
#what should the unit of observation be for this new data frame?
ym_inc <- tract_ym %>% 
  group_by(date,inc_above_median) %>% 
  summarise(si_count = sum(si_count)) %>%
  mutate(pop = if_else(inc_above_median == 1, 
                       detroit_pop_hi_inc, 
                       detroit_pop_lo_inc),
         si_1000 = si_count / (pop / 1000)) %>%
  na.omit()

#validation
ym_inc %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  filter(n != 2)

ym_inc <- tract_ym %>% 
  group_by(date,inc_above_median) %>% 
  summarise(si_count = sum(si_count)) %>%
  na.omit() %>% 
  ungroup() %>%
  complete(date, 
           inc_above_median, 
           fill = list(si_count = 0)) %>% #this fills in a new obs for Feb 2016
  mutate(pop = if_else(inc_above_median == 1, 
                       detroit_pop_hi_inc, 
                       detroit_pop_lo_inc),
         si_1000 = si_count / (pop / 1000)) 

#plot time series: separate lines for tracts above/below median income
ggplot(ym_inc, 
       aes(x = date, y = si_1000)) + 
  geom_line(aes(group = inc_above_median, 
                color = inc_above_median)) 

ym_inc$inc_above_median <- factor(ym_inc$inc_above_median,
                                  levels = c(0,1),
                                  labels = c("Below median income",
                                             "Above median income"))


#now plot time series by income group again using a factor for the color arg
ggplot(ym_inc, 
       aes(x = date, y = si_1000, color = inc_above_median)) + 
  geom_line()

#note that here the color aesthetic can be set in ggplot() or geom_line()
ggplot(ym_inc, 
       aes(x = date, y = si_1000)) + 
  geom_line(aes(color = inc_above_median)) +
  ggtitle("Time series plot of shutoffs by tracts above/median below median income")+
  xlab("Year")+
  ylab("Shutoff intensity")+
  labs(color="Tract Income")+
  scale_color_brewer(palette = "Set2")

```
Based on the result above, it seems like those who are on the "Above Median
Income" bracket also experienced higher intensity of shutoffs.

### 2b) Plot and interpret the shutoffs per capita over time for tracts that are at least 75% Black and those that aren't (show two time series on a single plot). {-}
```{r}
  detroit_pop_black <- tract %>%
    filter(black75 == 1) %>%
    summarise(sum(pop)) %>%
    as.numeric()
  
  detroit_pop_nblack <- tract %>%
    filter(black75 == 0) %>%
    summarise(sum(pop)) %>%
    as.numeric()
    
#get time series of shutoffs per capita by tracts above/below 75% black
  ym_race <- tract_ym %>% 
    group_by(date, black75) %>% 
    summarise(si_count = sum(si_count)) %>%
    na.omit() %>% 
    ungroup() %>%
    complete(date, 
             black75, 
             fill = list(si_count = 0)) %>%
    mutate(pop = if_else(black75 == 1, 
                         detroit_pop_black, 
                         detroit_pop_nblack),
           si_1000 = si_count / (pop / 1000))
  
  # Convert black75 to a factor with clear labels for better plot appearance
ym_race$black75 <- factor(ym_race$black75, 
                          levels = c(0, 1), 
                          labels = c("Below 75% Black", "At least 75% Black"))
# Plot time series by tracts above/below 75% Black
ggplot(ym_race, aes(x = date, y = si_1000, color = black75)) + 
  geom_line() +
  ggtitle("Shutoffs per Capita Over Time by Racial Composition (75% Black)") +
  xlab("Date") +
  ylab("Shutoff Intensity (per 1000 people)") +
  labs(color = "Racial Composition") +
  scale_color_manual(values = c("blue", "red")) +  # Blue for Below 75%, Red for At least 75% Black
  theme_minimal()


```
From the result above, it can be seen that tracts with at least 75% black
suffered higher shutoff intensity, except on mid 2014 where tracts with below
75% black suffered higher shutoff intensity.

\medspace

# Conclusion

### 3a) Based on the "cross-sectional" and time series analysis conducted above, does race or income appear to be a more important factor for explaining what type of households are most affected by the public water shutoffs? Explain. {-}
Based on the correlations, time series trends, and overall relationships, race
(higher proportion of Black residents) appears to be a more consistent and 
influential factor in determining which households are most affected by
water shutoffs. On the other hand, income, while related, does not seem to have
as strong or consistent an effect across different time points or neighborhoods
when compared to race.


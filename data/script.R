###########################################################################
## Modeling Life Expectancy in Developing Countries
## Amos Okutse, Monica Colon-Vargas
## Fall 2023
## PHP 2601: Linear Models
############################################################################

## Use the Sage Journal Template: Asia Pacific Journal of Public Health

## packages
library(tidyverse)    # data manipulation
library(lme4)         # linear mixed effects modeling
library(lmerTest)     # tests in linear mixed effects models
library(robustlm)     # robust variable selection with exponential squared loss
library(robustlmm)    # robust estimation of linear mixed-effects models
library(knitr)
library(gtsummary)
library(kableExtra)

library(rstatix)

## load the data frame
df <- read.csv("data/lfe.csv")

## dimensions [2938 rows on 22 variables]
dim(df) 
names(df)

## missing data
sum(is.na(df))
sum(is.na(df))/prod(dim(df))*100 #3.9%

## missing by variable
mf = apply(df, 2, function(x){return(sum(!is.na(x))/length(x))})
kable(mf, type = "pipe", caption = "Percentage missing by variable", digits = 3)

## missing by country


## data structure
str(df)
df = df %>% mutate_if(is.character, as.factor)

## Clean variables
df$Year <- as.factor(df$Year)
table(df$Year) # 15 years from 2000 to 2015: use only 10 latest years
## Table 1: Variable, type, and description



## Table 2: summary table as in Roffia et al 2022

# Table 3: average life expectancy by year [ mean response over time]
tab3 <- group_by(df, Year) %>% 
  rstatix::get_summary_stats(Life.expectancy)

ggplot(df, aes(Year, Life.expectancy, fill = Year)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  guides(fill = "none") +
  labs(x = "Year", y = "Life Expectancy, %")+
  theme_bw()


# mean response by country and year [include this in paper]
group_by(df, Country, Year) %>% 
  get_summary_stats(Life.expectancy, show = c("mean", "sd"))




# Table 4: correlation between all factors and life expectancy; correlation matrix table [group the factors into demographic, social, economic, etc] include only significant correlations in the table



## exploratory regression : assumes everything is a fixed effect
summary(lm(df$Life.expectancy ~., data = df))

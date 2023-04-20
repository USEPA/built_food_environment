###################################################
## Creating 2013 Birth Cohort Demographics Table ##
###################################################

#################
# load packages #
#################

library(haven)
library(tidyverse)
library(tigris)
library(readxl)
library(dplyr)

###############################
# Source 3_merged_data_2013.R #
###############################

source("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/built_food_environment/code/3_merged_data_2013.R")


###################################
# Table Create/Cleaning Functions #
###################################

#create_table is a function for creating the base table 

create_table <- function(df, group_var, EGA, BWT){
  df_summary <- df %>%
    group_by({{group_var}}) %>%
    summarize(count = n(),
              ptm_count = sum(EGA<37, na.rm = TRUE),
              lbw_count = sum(BWT<2500 & EGA>=37, na.rm = TRUE)) %>%
    mutate(percent_total_pop = (count/ sum(count))*100,
           ptm_percent = (ptm_count/sum(ptm_count))*100,
           lbw_percent = (lbw_count/sum(lbw_count))*100)
  return(df_summary)
}

 
#create vector for column drops

drops <- c("n","count", "ptm_count", "lbw_count")

###########################################################################
# Get each demographic statistic and corresponding outcome variable stats #
###########################################################################

########
# Race #
########


data_summary_race<- nc_2013 %>%
  group_by(RACEGP)%>%
  summarize(
    n_total= n(),
    n_lbw = sum(is_LBW),
    n_ptm = sum(is_PTM)) %>%
  mutate (pct_total_pop = n_total / sum(n_total)* 100,
          pct_lbw = n_lbw/ n_total *100,
          pct_ptm = n_ptm/ n_total *100 )


## Rename Race variables

race_legend<- c("1"= "White, non-Hispanic",
                "2"= "Black, non-Hispanic",
                "3"= "Hispanic",
                "4"= "Asian/ Pacific Islander, nH",
                "5"= "American Indian, nH",
                "6"= "other, nH/ unknown")

data_summary_race$RACEGP<- as.character(race_legend[data_summary_race$RACEGP])
#Drop unnecessary columns
data_summary_race <- data_summary_race[ , !(names(data_summary_race) %in% drops)]

#############
# Education #
#############

data_summary_education <- create_table(nc_2013, MOTHED, EGA, BWT)

## Rename Education variables

education_legend<- c("1"= "< HS",
                     "2"= "HS",
                     "3"= "more than HS")


data_summary_education$MOTHED<- as.character(education_legend[data_summary_education$MOTHED])

## Replace NAs with "missing"
data_summary_education$MOTHED <- ifelse(is.na(data_summary_education$MOTHED), 'missing', data_summary_education$MOTHED)
#Drop unnecessary columns
data_summary_education <-data_summary_education[ , !(names(data_summary_education) %in% drops)]


###################
# Medicaid status #
###################

data_summary_medicaid <- create_table(nc_2013, MEDICAID, EGA, BWT)

## Rename Medicaid variables

medicaid_legend<- c("0"= "No",
                     "1"= "Yes")


data_summary_medicaid$MEDICAID<- as.character(medicaid_legend[data_summary_medicaid$MEDICAID])
#Drop unnecessary columns
data_summary_medicaid<- data_summary_medicaid[ , !(names(data_summary_medicaid) %in% drops)]


##########################
# Gestational Parent Age #
##########################

data_summary_age <- create_table(nc_2013, MAGE, EGA, BWT)

## Find Continuous Maternal Age (median, IQR)

age_ptm_iqr<- IQR(data_summary_age$ptm_percent,na.rm = TRUE)
age_lbw_iqr<- IQR(data_summary_age$lbw_percent,na.rm = TRUE)
age_ptm_median<- median(data_summary_age$ptm_percent,na.rm = TRUE)
age_lbw_median<- median(data_summary_age$lbw_percent,na.rm = TRUE)

## Cut age bins

data_summary_age$MAGE<- as.numeric(data_summary_age$MAGE)
data_summary_age <- data_summary_age %>%
  mutate(ages = cut(MAGE, c(0, 17, 24, 34, 39, Inf))) %>%
  group_by(ages, .drop = TRUE) %>%
  summarize(n = n(),
            count = sum(count),
            ptm_count = sum(ptm_count),
            lbw_count = sum(lbw_count)) %>%
  mutate(percent_total_pop = (count/ sum(count))*100,
         ptm_percent = (ptm_count/sum(ptm_count))*100,
         lbw_percent = (lbw_count/sum(lbw_count))*100)

## Rename Age variables

age_legend<- c("(0,17]"= "< 18",
              "(17,24]"= "18-24",
              "(24,34]"= "25-34",
              "(34,39]"= "35-39",
              "(39,Inf]"= "40+")


data_summary_age$ages<- as.character(age_legend[data_summary_age$ages])

## Replace NAs with "missing"
data_summary_age$ages <- ifelse(is.na(data_summary_age$ages), 'missing', data_summary_age$ages)
#Drop unnecessary columns
data_summary_age<-data_summary_age[ , !(names(data_summary_age) %in% drops)]



##################
# Marital Status #
##################

data_summary_married <- create_table(nc_2013, MS, EGA, BWT)

## Rename Marital variables

married_legend<- c("1"= "married",
               "2"= "unmarried")


data_summary_married$MS<- as.character(married_legend[data_summary_married$MS])

# Replace NA with "missing"
data_summary_married$MS <- ifelse(is.na(data_summary_married$MS), 'missing', data_summary_married$MS)
#Drop unnecessary columns
data_summary_married <- data_summary_married[ , !(names(data_summary_married) %in% drops)]


############################
# Urban/ Rural (RUCA Code) #
############################


# Create RUCA Data Summary Table
data_summary_ruca <-create_table(nc_2013_ruca, `Secondary RUCA Code, 2010`, EGA, BWT)

data_summary_ruca<- data_summary_ruca%>%
  mutate(RUCA = cut(`Secondary RUCA Code, 2010`, c(0,3,6,9,Inf))) %>%
  group_by(RUCA, .drop = TRUE) %>%
  summarize(n = n(),
            count = sum(count),
            ptm_count = sum(ptm_count),
            lbw_count = sum(lbw_count)) %>%
  mutate(percent_total_pop = (count/ sum(count))*100,
         ptm_percent = (ptm_count/sum(ptm_count))*100,
         lbw_percent = (lbw_count/sum(lbw_count))*100)

## Rename RUCA variables

ruca_legend<- c("(0,3]"= "Metropolitan",
               "(3,6]"= "Micropolitan",
               "(6,9]"= "Small Town",
               "(9,Inf]"= "Rural")


data_summary_ruca$RUCA<- as.character(ruca_legend[data_summary_ruca$RUCA])

#Drop unnecessary columns

data_summary_ruca<-data_summary_ruca[ , !(names(data_summary_ruca) %in% drops)]


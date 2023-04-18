###################################################
## Creating 2013 Birth Cohort Demographics Table ##
###################################################

###############
# load packages
###############

library(haven)
library(tidyverse)
library(tigris)
library(readxl)


####################################
# Read in 2013 NC Birth Cohort csv #
####################################

nc_2013 <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/clean/base_birth_data/births_2013.csv", NULL)

#################
# Clean nc_2013 #
#################

# Change first row to headers

names(nc_2013) <- nc_2013[1,]
nc_2013 <- nc_2013[-1,]
names(nc_2013)[names(nc_2013) == 'NA'] <- 'Index'

###########################################################################
# Get each demographic statistic and corresponding outcome variable stats #
###########################################################################


########
# Race #
########

data_summary_race <- nc_2013 %>%
  group_by(RACEGP) %>%
  summarize(count = n(),
            ptm_count = sum(EGA<37, na.rm = TRUE),
            lbw_count = sum(BWT<2500 & EGA>=37, na.rm = TRUE)) %>%
  mutate(percent_total_pop = count/ sum(count),
         ptm_percent = ptm_count/sum(ptm_count),
         lbw_percent = lbw_count/sum(lbw_count))

## Rename Race variables

race_legend<- c("1"= "White, non-Hispanic",
                "2"= "Black, non-Hispanic",
                "3"= "Hispanic",
                "4"= "Asian/ Pacific Islander, nH",
                "5"= "American Indian, nH",
                "6"= "other, nH/ unknown")

data_summary_race$RACEGP<- as.character(race_legend[data_summary_race$RACEGP])

#############
# Education #
#############

data_summary_education <- nc_2013 %>%
  group_by(MOTHED) %>%
  summarize(count = n(),
            ptm_count = sum(EGA<37, na.rm = TRUE),
            lbw_count = sum(BWT<2500 & EGA>=37, na.rm = TRUE)) %>%
  mutate(percent_total_pop = count/ sum(count),
         ptm_percent = ptm_count/sum(ptm_count),
         lbw_percent = lbw_count/sum(lbw_count))

## Rename Education variables

education_legend<- c("1"= "< HS",
                     "2"= "HS",
                     "3"= "more than HS")


data_summary_education$MOTHED<- as.character(education_legend[data_summary_education$MOTHED])

## Replace NAs with "missing"
data_summary_education$MOTHED <- ifelse(is.na(data_summary_education$MOTHED), 'missing', data_summary_education$MOTHED)

###################
# Medicaid status #
###################

data_summary_medicaid <- nc_2013 %>%
  group_by(MEDICAID) %>%
  summarize(count = n(),
            ptm_count = sum(EGA<37, na.rm = TRUE),
            lbw_count = sum(BWT<2500 & EGA>=37, na.rm = TRUE)) %>%
  mutate(percent_total_pop = count/ sum(count),
         ptm_percent = ptm_count/sum(ptm_count),
         lbw_percent = lbw_count/sum(lbw_count))

## Rename Medicaid variables

medicaid_legend<- c("0"= "No",
                     "1"= "Yes")


data_summary_medicaid$MEDICAID<- as.character(medicaid_legend[data_summary_medicaid$MEDICAID])

##########################
# Gestational Parent Age #
##########################

data_summary_age <- nc_2013 %>%
  group_by(MAGE) %>%
  summarize(count = n(),
            ptm_count = sum(EGA<37, na.rm = TRUE),
            lbw_count = sum(BWT<2500 & EGA>=37, na.rm = TRUE)) %>%
  mutate(percent_total_pop = count/ sum(count),
         ptm_percent = ptm_count/sum(ptm_count),
         lbw_percent = lbw_count/sum(lbw_count))

## Find Continuous Maternal Age (median, IQR)

ptm_iqr<- IQR(data_summary_age$ptm_percent,na.rm = TRUE)
lbw_iqr<- IQR(data_summary_age$lbw_percent,na.rm = TRUE)
ptm_median<- median(data_summary_age$ptm_percent,na.rm = TRUE)
lbw_median<- median(data_summary_age$lbw_percent,na.rm = TRUE)

## Cut age bins
data_summary_age$MAGE<- as.numeric(data_summary_age$MAGE)
data_summary_age <- data_summary_age %>%
  mutate(ages = cut(MAGE, c(0, 18, 24, 25, 34, 35, 39, 40, Inf))) %>%
  group_by(ages, .drop = TRUE) %>%
  summarize(n = n(),
            count = sum(count),
            ptm_count = sum(ptm_count),
            lbw_count = sum(lbw_count)) %>%
  mutate(percent_total_pop = count/ sum(count),
         ptm_percent = ptm_count/sum(ptm_count),
         lbw_percent = lbw_count/sum(lbw_count))

## Rename Age variables

age_legend<- c("1"= "< HS",
              "2"= "HS",
              "3"= "more than HS")


data_summary_age$MAGE<- as.character(age_legend[data_summary_age$MAGE])


##################
# Marital Status #
##################

data_summary_married <- nc_2013 %>%
  group_by(MS) %>%
  summarize(count = n(),
            ptm_count = sum(EGA<37, na.rm = TRUE),
            lbw_count = sum(BWT<2500 & EGA>=37, na.rm = TRUE)) %>%
  mutate(percent_total_pop = count/ sum(count),
         ptm_percent = ptm_count/sum(ptm_count),
         lbw_percent = lbw_count/sum(lbw_count))

## Rename Marital variables

married_legend<- c("1"= "married",
               "2"= "unmarried")


data_summary_married$MS<- as.character(married_legend[data_summary_married$MS])

# Replace NA with "missing"
data_summary_married$MS <- ifelse(is.na(data_summary_married$MS), 'missing', data_summary_married$MS)

############################
# Urban/ Rural (RUCA Code) #
############################

# Read in RUCA CODE excel sheet
ruca_2010<- read_excel("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/supplementary_information/ruca2010revised.xlsx", sheet= "NC_filter" )

# Clean RUCA CODE sheet

names(ruca_2010)[names(ruca_2010) == 'State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)'] <- 'State-County-Tract_FIPS-Code'
names(ruca_2010)[names(ruca_2010) == 'Secondary RUCA Code, 2010 (see errata)'] <- 'Secondary RUCA Code, 2010'

#Drop unecessary columns
ruca_2010<- subset(ruca_2010, select = -c(2,3,5,8,9))

#Join Ruca table with 2013 births
nc_2013_ruca <- merge( nc_2013, ruca_2010, by.x = ("new_gc_2010CT"), by.y = ('State-County-Tract_FIPS-Code') )

# Create RUCA Data Summary Table
data_summary_ruca <-nc_2013_ruca %>%
  group_by(`Secondary RUCA Code, 2010`) %>%
  summarize(count = n(),
            ptm_count = sum(EGA<37, na.rm = TRUE),
            lbw_count = sum(BWT<2500 & EGA>=37, na.rm = TRUE)) %>%
  mutate(percent_total_pop = count/ sum(count),
         ptm_percent = ptm_count/sum(ptm_count),
         lbw_percent = lbw_count/sum(lbw_count))%>%
  mutate(RUCA = cut(`Secondary RUCA Code, 2010`, c(1,3,4,6,7,9,10,Inf))) %>%
  group_by(RUCA, .drop = TRUE) %>%
  summarize(n = n(),
            count = sum(count),
            ptm_count = sum(ptm_count),
            lbw_count = sum(lbw_count)) %>%
  mutate(percent_total_pop = count/ sum(count),
         ptm_percent = ptm_count/sum(ptm_count),
         lbw_percent = lbw_count/sum(lbw_count))

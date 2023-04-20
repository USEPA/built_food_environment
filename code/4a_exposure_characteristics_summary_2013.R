###############################################################
## Creating 2013 Birth Cohort Exposure Characteristics Table ##
###############################################################

#################
# load packages #
#################

library(haven)
library(tidyverse)
library(tigris)
library(readxl)
library(dplyr)

####################################
# Read in 2013 NC Birth Cohort csv #
####################################

nc_2013 <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/clean/base_birth_data/births_2013.csv", NULL)

# Clean nc_2013
# Change first row to headers

names(nc_2013) <- nc_2013[1,]
nc_2013 <- nc_2013[-1,]
names(nc_2013)[names(nc_2013) == 'NA'] <- 'Index'

# Keep necessary columns

nc_2013 <- subset(nc_2013, select = c('STUDYID_char','EGA', 'BWT', 'new_gc_2010CT'))

###################################
# Read in 2019 USDA FARA 2019 csv #
###################################

fara_2019<- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/supplementary_information/FoodAccessResearchAtlasData2019.csv", NULL)

# Clean fara_2019
# Change first row to headers

names(fara_2019) <- fara_2019[1,]
fara_2019 <-fara_2019[-1,]

# Keep necessary columns

fara_2019 <- subset(fara_2019, select = c('CensusTract','LILATracts_1And10', 'LILATracts_1And20', 'LILATracts_halfAnd10', 'LILATracts_Vehicle'))


####################################
# JOIN FARA table with 2013 births #
####################################

nc_2013_fara <- merge( nc_2013, fara_2019, by.x = ("new_gc_2010CT"), by.y = ('CensusTract') )

#######################
# Read in Buffer CSVs #
#######################

nearest_grocery <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/nearest_supermarket.csv", NULL)
names(nearest_grocery) <- nearest_grocery[1,]
nearest_grocery <-nearest_grocery[-1,]

buffer_2_5_mi <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/two_point_five_mile_linear_tables/two_point_five_linear.csv", NULL)
names(buffer_2_5_mi) <- buffer_2_5_mi[1,]
buffer_2_5_mi <-buffer_2_5_mi[-1,]

buffer_5_mi <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/fivemilelinear.csv", NULL)
names(buffer_5_mi) <- buffer_5_mi[1,]
buffer_5_mi <-buffer_5_mi[-1,]

buffer_5_mi_network <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/five_mile_network_distance.csv", NULL)
names(buffer_5_mi_network) <- buffer_5_mi_network[1,]
buffer_5_mi_network <-buffer_5_mi_network[-1,]

buffer_20min <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/twenty_minute_network_tables/twenty_minute_network_distance.csv", NULL)
names(buffer_20min) <- buffer_20min[1,]
buffer_20min <-buffer_20min[-1,]

buffer_20min_joined <- merge( buffer_20min, nc_2013, by.x =  ('STUDYID_ch') , by.y =  ('STUDYID_char'))


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

#create LILA Legend

LILA_legend<- c("0"= "No",
                "1"= "Yes")

########################################################################
# Get each exposure statistic and corresponding outcome variable stats #
########################################################################

#####################
# LILATracts_1And10 #
#####################

LILATracts_1And10 <- create_table(nc_2013_fara, LILATracts_1And10, EGA, BWT)

#Rename and Drop unnecessary columns

LILATracts_1And10$LILATracts_1And10<- as.character(LILA_legend[LILATracts_1And10$LILATracts_1And10])

LILATracts_1And10 <- LILATracts_1And10[ , !(names(LILATracts_1And10) %in% drops)]

#####################
# LILATracts_1And20 #
#####################

LILATracts_1And20 <- create_table(nc_2013_fara, LILATracts_1And20, EGA, BWT)

#Rename and Drop unnecessary columns

LILATracts_1And20$LILATracts_1And20<- as.character(LILA_legend[LILATracts_1And20$LILATracts_1And20])

LILATracts_1And20 <- LILATracts_1And20[ , !(names(LILATracts_1And20) %in% drops)]

########################
# LILATracts_halfAnd10 #
########################

LILATracts_halfAnd10 <- create_table(nc_2013_fara, LILATracts_halfAnd10, EGA, BWT)

#Rename and Drop unnecessary columns

LILATracts_halfAnd10$LILATracts_halfAnd10<- as.character(LILA_legend[LILATracts_halfAnd10$LILATracts_halfAnd10])

LILATracts_halfAnd10 <- LILATracts_halfAnd10[ , !(names(LILATracts_halfAnd10) %in% drops)]

######################
# LILATracts_Vehicle #
######################

LILATracts_Vehicle <- create_table(nc_2013_fara, LILATracts_Vehicle, EGA, BWT)

#Rename and Drop unnecessary columns

LILATracts_Vehicle$LILATracts_Vehicle<- as.character(LILA_legend[LILATracts_Vehicle$LILATracts_Vehicle])

LILATracts_Vehicle <- LILATracts_Vehicle[ , !(names(LILATracts_Vehicle) %in% drops)]

#########################
# Nearest Grocery Store #
#########################

#is near fid correct variable?

nearest_grocery_tbl <- create_table(nearest_grocery, NEAR_FID, EGA, BWT)
nearest_grocery_tbl <-nearest_grocery_tbl %>% mutate_at('NEAR_FID', as.numeric)



################
# 2.5mi Buffer #
################

# is point count correct?

buffer_2_5_mi_tbl <- create_table(buffer_2_5_mi, Point_Count, EGA, BWT)
buffer_2_5_mi_tbl <-buffer_2_5_mi_tbl %>% mutate_at('Point_Count', as.numeric)


##############
# 5mi Buffer #
##############

buffer_5_mi_tbl <- create_table(buffer_5_mi, five_mile_linear_distance, EGA, BWT)
buffer_5_mi_tbl <- buffer_5_mi_tbl %>% mutate_at('five_mile_linear_distance', as.numeric)


######################
# 5mi Network Buffer #
######################

buffer_5_mi_network_tbl <- create_table(buffer_5_mi_network, five_mile_driving_distance, EGA, BWT)
buffer_5_mi_network_tbl <- buffer_5_mi_network_tbl %>% mutate_at('five_mile_driving_distance', as.numeric)



################
# 20min Buffer #
################

buffer_20min_tbl <- create_table(buffer_20min_joined, twenty_minute_driving_distance, EGA, BWT)

## Find Continuous Maternal Age (median, IQR)

#age_ptm_iqr<- IQR(data_summary_age$ptm_percent,na.rm = TRUE)
#age_lbw_iqr<- IQR(data_summary_age$lbw_percent,na.rm = TRUE)
#age_ptm_median<- median(data_summary_age$ptm_percent,na.rm = TRUE)
#age_lbw_median<- median(data_summary_age$lbw_percent,na.rm = TRUE)





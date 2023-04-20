####################################
# Create 2013 Birth Cohort Dataset #
####################################

### This dataset is the merged combination of the 2013 NC Birth Cohort Data, RUCA 2010 Codes, 2019 USDA FARA Data,
### and 2.5mibuffer, 5mibuffer, 5mibuffernetwork, nearestgrocery, and 20minbuffer

### final output is named: nc_2013

### Notes:
### Can more columns be deleted from buffers? what data do we need from buffer data exactly?

#################
# load packages #
#################

library(tidyverse)
library(readxl)
library(dplyr)

####################################
# Read in 2013 NC Birth Cohort csv #
####################################

old_nc_2013 <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/clean/base_birth_data/births_2013.csv", NULL)

  ## Clean 2013 NC Birth Cohort csv
  ## Change first row to headers

names(old_nc_2013) <- old_nc_2013[1,]
old_nc_2013 <- old_nc_2013[-1,]
names(old_nc_2013)[names(old_nc_2013) == 'NA'] <- 'Index'

  ## Create columns for LBW and PTM

old_nc_2013 <- old_nc_2013%>%
  mutate(
    'is_PTM' = case_when(
      EGA <37 ~ 1,
      TRUE ~ 0
      ),
    'is_LBW' = case_when(
    BWT<2500 & EGA>=37 ~ 1,
    TRUE ~0

    )
  )


#################################
# Read in RUCA CODE excel sheet #
#################################

# Final ruca_2010 includes: State-County FIPS Code, State-County-Tract FIPS Code, Secondary RUCA Code 2010, Tract Population 2010

ruca_2010<- read_excel("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/supplementary_information/ruca2010revised.xlsx", sheet= "NC_filter" )

  ## Clean RUCA CODE sheet
  ## Rename variables

names(ruca_2010)[names(ruca_2010) == 'State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)'] <- 'State-County-Tract_FIPS-Code'
names(ruca_2010)[names(ruca_2010) == 'Secondary RUCA Code, 2010 (see errata)'] <- 'Secondary RUCA Code, 2010'

  ## Drop unnecessary columns in ruca_2010

ruca_2010<- subset(ruca_2010, select = -c(2,3,5,8,9))

###################################
# Read in 2019 USDA FARA 2019 csv #
###################################

# Final fara_2019 includes: CensusTracts and LILATracts 1And10, 1And20, halfAnd10, Vehicle

fara_2019<- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/supplementary_information/FoodAccessResearchAtlasData2019.csv", NULL)

  ## Clean fara_2019
  ## Change first row to headers

names(fara_2019) <- fara_2019[1,]
fara_2019 <-fara_2019[-1,]

  ## Keep necessary columns

fara_2019 <- subset(fara_2019, select = c('CensusTract','LILATracts_1And10', 'LILATracts_1And20', 'LILATracts_halfAnd10', 'LILATracts_Vehicle'))

#######################
# Read in buffer data #
#######################

  ## Change first row to headers
  ## SUbset necessary rows


nearest_grocery <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/nearest_supermarket.csv", NULL)
names(nearest_grocery) <- nearest_grocery[1,]
nearest_grocery <-nearest_grocery[-1,]

## Create list of duplicate column names (cols already in old_nc_2013) to drop
repeat_names<-colnames(nearest_grocery)
list_names<- repeat_names[4:25]

nearest_grocery <- nearest_grocery[ , ! names(nearest_grocery) %in% list_names]


buffer_2_5_mi <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/two_point_five_mile_linear_tables/two_point_five_linear.csv", NULL)
names(buffer_2_5_mi) <- buffer_2_5_mi[1,]
buffer_2_5_mi <-buffer_2_5_mi[-1,]
#buffer_2_5_mi <- buffer_2_5_mi[ , ! names(buffer_2_5_mi) %in% list_names]


buffer_5_mi <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/fivemilelinear.csv", NULL)
names(buffer_5_mi) <- buffer_5_mi[1,]
buffer_5_mi <-buffer_5_mi[-1,]
buffer_5_mi <- buffer_5_mi[ , ! names(buffer_5_mi) %in% list_names]

buffer_5_mi_network <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/five_mile_network_distance.csv", NULL)
names(buffer_5_mi_network) <- buffer_5_mi_network[1,]
buffer_5_mi_network <-buffer_5_mi_network[-1,]
#buffer_5_mi_network <- buffer_5_mi_network[ , ! names(buffer_5_mi_network) %in% list_names]


buffer_20min <- read_csv("O:/PRIV/IRBData/Birth_Outcomes/Food Environment/analysis/data/raw/variable_analysis_files/twenty_minute_network_tables/twenty_minute_network_distance.csv", NULL)
names(buffer_20min) <- buffer_20min[1,]
buffer_20min <-buffer_20min[-1,]



##################
# Joining Tables #
##################

##################################
#Join RUCA table with 2013 births#
##################################
nc_2013_ruca <- merge( old_nc_2013, ruca_2010, by.x = ("new_gc_2010CT"), by.y = ('State-County-Tract_FIPS-Code') )

#########################################
# JOIN FARA table with 2013_ruca births #
#########################################

nc_2013_fara <- merge( nc_2013_ruca, fara_2019, by.x = ("new_gc_2010CT"), by.y = ('CensusTract') )

######################
# Join Buffer Tables #
######################


a<- merge( nc_2013_fara, 
           buffer_20min %>% dplyr::select('twenty_minute_driving_distance', 'STUDYID_ch'),
           by.x =  ('STUDYID_char') , by.y =  ('STUDYID_ch'))

b<-   left_join(a,
                buffer_5_mi %>% dplyr::select('five_mile_linear_distance', 'new_gc_2010CT', 'STUDYID_ch'),
                by = 'new_gc_2010CT')

c<-   left_join(b,
                buffer_5_mi_network %>% dplyr::select('five_mile_driving_distance', 'STUDYID_ch'),
                by =  ('STUDYID_ch'))

d<- left_join(c,
              nearest_grocery %>% dplyr::select('NEAR_FID', 'STUDYID_ch'),
              by =  ('STUDYID_ch'))

# FINAL TABLE #

nc_2013<- left_join(d,
                    buffer_2_5_mi %>% dplyr::select('Point_Count', 'STUDYID_char'),
                    by =  ('STUDYID_char'))





# Built Food Environment
A repository to keep track of code and data related to the Built Food Environment project

_Last updated: 4/13/2023_

## Table of Contents

- [About the Project](#about-the-project)
- [The Data](the-data)
- [The Code](#the-code)
  - [Data Processing Pipeline](#data-processing-pipeline)
    - [1 Cleaning and Filtering](1-cleaning-and-filtering)
    - [2 Creating Exposure Variables](2-creating-exposure-variables)
    - [3 Merging Datasets](3-merging-datasets)
    - [4 Outputs](4-outputs)

## About the Project
The Built Food Environment Team...

## The Data
### Dun & Bradstreet
Dun and Bradstreet description goes here
#### 2013 and 2018
Description of EPA D&B goes here

#### 1989-2012
Description of UNC D&B data Archana provided goes here

### North Carolina Birth Cohort

### More datasets 
Description of each dataset would go here

## The Code

### Data Processing Pipeline
The process of interpreting our data starts with raw data files as input and ends with various outputs- this process is referred to as a "Data Processing Pipeline", highlighted below:
<img src="figs\data_pipeline_4_13_2023.png" width=900 height=450>

#### 1 Cleaning and Filtering
Data is first cleaned by __/code/1_clean_dataset_year.R__

For example, the code that would clean the Dun & Bradstreet dataset from 2013 would be titled __1_clean_db_2013.R__. This will then make it useable for the next step, which is creating our exposure variables. These cleaning files perform functions such as removing NAs, transforming data into the necessary formats for processing, and ensuring long/lats are accurate.

#### 2 Creating Exposure Variables
The cleaned versions of datasets are processed by __/code/2_exposure_year.R__

These files will take all cleaned datasets as input, and create/calculate the various exposure metrics from the "Data Analysis Plan". Note that buffers will be calculated in ArcGIS, and will be read in during this step. For any questions related to buffers, contact Maxwell Hatala.

#### 3 Merging Datasets
These scripts read in both clean data and exposure variable outputs, and create a more final dataframe that is outputted as a csv. These are titled __/code/3_merge_year.R__.

This output has one row per birth for a given year, with each column being a exposure variable, demographic info, or covariate. Each year's output should be in the same format. Outputs from this stage are saved as __/data/output/3_merged_data_year.csv__. 


## Outputs
### 4a: Data Visualizations

### 4b: Summary Reports

### 4c?: ArcGIS maps? Let's ask Max for his thoughts 

##--------------------------------------------
##
## Test Farm-Subsidies Data set
##
## Class: PCE Data Science Methods Class
##
## Purpose: Framework for Homework 3
##
## Datasets located:
##
##  http://www.fsa.usda.gov/FSA/webapp?area=newsroom&subject=landing&topic=foi-er-fri-pfi
##
##   Need:
##
##    -2011 Farm Payment File (27MB) txt file
##    -State and County Code List (374KB) xls file (probably convert this to csv)
##
##--------------------------------------------
setwd('E:/Work/Teaching/PCE_Data_Science/3_Outliers_MissingData_Hypothesis')
##----Import Libraries-----
require(RSQLite)
require(logging)

##----Hypotheses to test-----
#
#  Test these two things:
#
#    1.  Does our sample equally represent all 50 states?
#
#    2.  Does our sample equally represent all 50 states, weighted by number of farms/state?
#
#     Note- you can find the farms per state in census data.
#

trim = function (x) gsub("^\\s+|\\s+$", "", x)

##-----Declare Functions Here-----


##----Run Main Function Here----
if(interactive()){
  
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')
  
  ##-----Read in the data-----
  data = read.csv("CAS.WDC11019.PMT11.FINAL.DT11186.TXT", sep=";",
                  header=FALSE, stringsAsFactors=FALSE)
  
  ##----Trim Whitespaces-----
  data = as.data.frame(apply(data,2,trim), stringsAsFactors=FALSE)
  
  names(data) = c('state_code', 'county_code', 'cust_num', 'program_code', 'program_year',
                  'commodity_code', 'amount', 'date', 'cat_code', 'farm_num',
                  'calendar_year', 'fiscal_year', 'seq_num')
  
  ##------Read State/County File-----
  county_state_codes = read.csv("foia_state_county_codes-1.csv", stringsAsFactors=FALSE)
  county_state_codes$state_code = county_state_codes$Stcd
  county_state_codes$Stcd = NULL
  county_state_codes$county_code = county_state_codes$Cntycd
  county_state_codes$Cntycd = NULL
  
  ##----Merge files together----
  data = merge(data, county_state_codes, by=c("state_code", "county_code"), all.x=TRUE)
  
  ##-----Probably do some data exploration----
  
  ##----Perform a test for equal representation-----
  
  ##----Access the farms/state data-----
  
  ##----Derive the weights for each state----
  
  ##----Perform a test for equal repreentation by farms/state-----
  
  ##----Output Results----
  # Acceptable to print output, log output, save in DB, or write to file. Your choice.
  
}





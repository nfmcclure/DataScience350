##--------------------------------------------
##
## R code review (lecture 1)
##
## Class: PCE Data Science Methods Class
##
## Contains examples of:
##
## -Matrices, Dataframe, data.table, apply functions.
##
## -Reading text data, reading web data.
##
## - Dealing with SQLite DBs.
##
##--------------------------------------------

##----Import Libraries----
library(data.table)
library(logging)
library(RSQLite)

##----Set working directory-----
setwd('E:/Work/Teaching/PCE_Data_Science/1_Intro_Lecture')

##----Matrix Review-----
A_matrix = matrix(4, nrow=4, ncol=3) # Makes use of broadcasting
B_matrix = matrix(1:50, nrow=4, ncol=3)

A_matrix + B_matrix # Elementwise
A_matrix * B_matrix # Elementwise
A_matrix %*% B_matrix # Error in matrix multiplication
A_matrix %*% t(B_matrix)

##----Dataframe Review----
x_values = seq(from=as.Date('2015-01-01'),
               to=as.Date('2015-02-12'),
               by = 3)

df = data.frame('dates' = x_values,
                'x1'    = runif(15,-10,20),
                'x2'    = 1:15,
                'x3'    = strsplit('MississippiMath','')[[1]])

df$x3 = as.character(df$x3)
df$x3 = tolower(df$x3)

str(df)
head(df)
tail(df, n=10)

##----data.table syntax-----
df = as.data.table(df)

# Aggregation
df[,sum(x1)]

df[,c(sum(x1),sd(x2))]

df[,c(Sum = sum(x1),
      StDev = sd(x2))]

# Aggregation by groups
df[,sum(x1), by=x3]

# Aggregation by groups on specific rows
df[1:3, sum(x1), by=x3]

# Creating new columns
df[, x4:= x1+x2]

df[,c('x4', 'x5') := list(x1+x2, x1-x2),]

# keys and merging
df2 = data.frame('label'  = c('m','i','s','p','a','t'),
                 'newval' = 5:10)
df2 = as.data.table(df2)
setkey(df,'x3')
setkey(df2,'label')

df_inner = df[df2]  # no more h! (inner join by default)

# outer join?
df_outer = merge(df, df2, all=TRUE) # not quite
df_outer = merge(df, df2, all.x=TRUE, all.y=TRUE, by.x='x3', by.y='label') # nope!

# Turns out data.table is very picky about column names: they have to match!

setnames(df, 'x3', 'label')
df_outer = merge(df, df2, all=TRUE, by='label')

##----Apply Function Review-----
# Please read the first SO answer of this question:
# http://stackoverflow.com/questions/3505701/r-grouping-functions-sapply-vs-lapply-vs-apply-vs-tapply-vs-by-vs-aggrega

# matrices
apply(B_matrix, 1, median) # Across rows (2 = across columns)

# dataframes
test_function = function(a, b){
  return(ifelse(a>0.5, sin(a),b)) # note: ifelse is the vectorized version of if
}

apply(df, 1, function(x) test_function(as.numeric(x['x1']),
                                       as.numeric(x['x2'])))

lapply(df, as.character)

data.frame(lapply(df, as.character))

##-------------------------------
##-------------------------------
##---Read tab separated files----
medals_data <- read.table("medals.txt", sep="\t", header=TRUE)
str(medals_data)
# medals_data is a data frame that contains 62 countries that have won
#   medals in the winter and summer olympics.
#
# Country:    Country name
# Summer:     # of Summer medals won
# Winter:     # of Winter medals won
# Population: Country Population in millions
# Latitude:   Country Latitude

pairs(medals_data, pch=16)

##----Read websites-----
library(XML)
nfl_site = "http://www.usatoday.com/sports/nfl/arrests/"
nfl_html = readHTMLTable(nfl_site)
nfl_data = nfl_html[[1]]

# Also, the base command 'readLines' will read raw html
#  for an example, see 'weather_retrieval.R'

##-------------------------------------
##-------------------------------------
##------Accessing SQLite in R Demo-----
db_file = 'test_db.db'

# Open connection
conn = dbConnect(dbDriver("SQLite"), dbname=db_file)

# Write data frame to db
dbWriteTable(conn, 'table_name', medals_data, overwrite=TRUE)

# Pull data from SQLite DB
query_string = 'SELECT * FROM table_name WHERE Population >= 100'
dbGetQuery(conn, query_string)

# And because we keep a clean workspace...
dbDisconnect(conn)

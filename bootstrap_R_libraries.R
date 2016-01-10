##--------------------------------------------
##
## Install All R Libraries Needed
##
## Class: PCE Data Science Methods Class 350
##
##--------------------------------------------

library_names = as.character(read.table('requirements.txt')$V1)
install.packages(library_names)

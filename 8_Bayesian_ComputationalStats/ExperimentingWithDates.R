##-------------------------------------------
##
##  Data Science PCE Interview Question
##
##        --Dealing with Dates--
##
##-------------------------------------------

# Today's Date:
today = Sys.Date()

# What is the date tomorrow?
today + 1

# What is the date in 5,000 days?
today + 5000

# How many days old am I?
birthday = as.Date("1983-10-21")
today - birthday

# When was my 10,000th birthDAY?
birthday + 10000

# What day of the week was I born on?
format(birthday, "%A")

# What is day 0?
today - as.numeric(today)
# Why then?

# What day of the week was 0000-01-01?
format(as.Date("0000-01-01"), "%A")


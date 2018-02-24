Sys.setlocale("LC_ALL", "C")


#Problem 1 - Loading the Data


#Load the data
mvt = read.csv("mvtWeek1.csv")

# How many observations are in this data set?
# How many variables are in this data set?
str(mvt)
##Answer: 191641, 11

# What is the maximum value of variable "ID"?
max(mvt$ID)
##Answer:  9181151

# What is the minimum value of variable "Beat"?
min(mvt$Beat)
##Answer: 111

# How many observations are TRUE for variable "Arrest"?
table(mvt$Arrest)
##Answer: 15536

# How many observations have a LocationDescription value of ALLEY?
table(mvt$LocationDescription)
##Answer: 2308


#Problem 2 - Understanding Dates in R
 

# In what format are the entries in the variable Date?
mvt[1,"Date"]
##Answer: m/d/y H:M

# Convert Date into a Date object.
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

# What is the month and year of the median date in our dataset?
median(DateConvert)
##Answer: May, 2006

# Add the month and weekday to mvt.
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)

# Replace Date with DateConvert in mvt.
mvt$Date = DateConvert

# In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)
##Answer: February

# On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)
##Answer: Friday

# Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Month, mvt$Arrest)
##Answer: January


#Problem 3 - Visualizing Crime Trends


# Create histogram of Date with 100 bars.
hist(mvt$Date, breaks=100)

# Does it look like crime increases or decreases from 2002 - 2012?
##Answer: decreases

# Does it look like crime increases or decreases from 2005 - 2008?
##Answer: decreases

# Does it look like crime increases or decreases from 2009 - 2011?
##Answer: increases

# Create a boxplot of the variable "Date", sorted by the variable "Arrest".
boxplot(mvt$Date~mvt$Arrest) 


# Does it look like there were more crimes for which arrests were made in
# the first half of the time period or the second half of the time period?
# The answer comes from looking at the boxplot.
##Answer: first half

# For what proportion of motor vehicle thefts in 2001 was an arrest made?
# For what proportion of motor vehicle thefts in 2007 was an arrest made?
# For what proportion of motor vehicle thefts in 2012 was an arrest made?
table(mvt$Arrest, mvt$Year)
##Answer: (2152)/(18517+2152)= 0.1041173; 1212/(1212+13068) = 0.08487395 ; 
##550/(550+13542) = 0.03902924


#Problem 4 - Popular Locations


# Which locations are the top five locations for motor vehicle thefts, 
# excluding the "Other" category? You should select 5 of the following options.
tail(sort(table(mvt$LocationDescription)))
##Answer: DRIVEWAY - RESIDENTIAL, GAS STATION, ALLEY, STREET,
##PARKING LOT/GARAGE(NON.RESID.)

# Create a subset of your data, only taking observations for which the theft
# happened in one of these five locations, and call this new data set "Top5".
Top5 = subset(mvt, LocationDescription == "DRIVEWAY - RESIDENTIAL" | LocationDescription == "ALLEY" |
LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription == "GAS STATION" | 
LocationDescription == "STREET")


# How many observations are in Top5?
str(Top5)
##Answer: 177510

# Get rid of unused factors in Top5.
Top5$LocationDescription = factor(Top5$LocationDescription)

# Which location has a much higher arrest rate than the others?
sort(tapply(Top5$Arrest, Top5$LocationDescription, mean))
##Answer: GAS STATION

# On which day of the week do the most motor vehicle thefts at gas stations happen?
table(Top5$Weekday, Top5$LocationDescription)
##Answer: Sunday

# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
##Answer: Saturday








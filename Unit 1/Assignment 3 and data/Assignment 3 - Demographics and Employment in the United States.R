Sys.setlocale("LC_ALL", "C")

#Load the data
CPS = read.csv("CPSData.csv")


#Problem 1 - Summarizing the Dataset


# How many interviewees are in the dataset?
str(CPS)
##Answer: 131302

# Among the interviewees with a value reported for the Industry variable, what 
# is the most common industry of employment?
sort(table(CPS$Industry))
##Answer: Educational and health services 

# Which state has the fewest interviewees?
sort(table(CPS$State))
##Answer: New Mexico 

# Which state has the most interviewees?
##Answer: California 

# What proportion of interviewees are citizens of the United States?
summary(CPS$Citizenship)
##Answer: (116639+7073)/nrow(CPS) = 9421943

# We add the values for Citizen, Native and Citizen, Naturalized.

# For which races are there at least 250 interviewees in the CPS dataset of 
# Hispanic ethnicity? (Select all that apply.)
table(CPS$Race, CPS$Hispanic)
##Answer: American Indian, Black, Multiracial, White


# Problem 2 - Evaluating Missing Values


# Which variables have at least one interviewee with a missing (NA) value? 
summary(CPS)
##Answer: MetroAreaCode, Married, Education, EmploymentStatus, Industry

# Between the variables Region, Sex, Age and Citizenship which is most
# related to missing values of the Married variable?
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
##Answer: We see that Age is because marriage is only recorded starting at age 15,
##so all values under the FALSE column are 0 until Age 14 and then all values
##under the TRUE column are 0 starting at 15.

# How many states had all interviewees living in a non-metropolitan area?
# How many states had all interviewees living in a metropolitan area?
table(CPS$State, is.na(CPS$MetroAreaCode))
##Answer: For the first we count how many states have 0 under the FALSE column; for 
##the second we count how many states have 0 under the TRUE column. So the
##answers are 2 and 3 respectively.

# Which region of the United States has the largest proportion of 
# interviewees living in a non-metropolitan area?
tapply(is.na(CPS$MetroAreaCode), CPS$Region, mean)
##Answer: Midwest

# Which state has a proportion of interviewees living in a non-metropolitan 
# area closest to 30%?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))
##Answer: Wisconsin

# Which state has the largest proportion of non-metropolitan interviewees, 
# ignoring states where all interviewees were non-metropolitan?
##Answer: Choose the state with the largest value that isn't 1, which is
##Montana.


# Problem 3 - Integrating Metropolitan Area Data


# Load the data sets that tell us the names that the MetroAreaCode and
# CountryOfBirthCode values correspond to.
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

# How many observations (codes for metropolitan areas) are there in MetroAreaMap?
str(MetroAreaMap)
##Answer: 271

# How many observations (codes for countries) are there in CountryMap?
str(CountryMap)
##Answer: 149

# Merge CPS with MetroAreaMap by variables CPS$MetroAreaCode and MetroAreaMap$Code.
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# What is the name of the variable that was added to the data frame by the merge()
# operation?
str(CPS)
##Answer: MetroArea

# How many interviewees have a missing value for the new metropolitan area variable?
table(is.na(CPS$MetroArea))
##Answer: 34238

# Which  metropolitan area has the largest number of interviewees?
sort(table(CPS$MetroArea))
##Answer: New York-Northern New Jersey-Long Island, NY-NJ-PA 

# Which metropolitan area has the highest proportion of interviewees of Hispanic
# ethnicity? 
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
##Answer: Laredo, TX

# Determine the number of metropolitan areas in the United States from which at 
# least 20% of interviewees are Asian.
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
##Answer: 4

# Which metropolitan area has the smallest proportion of interviewees who have 
# received no high school diploma?
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))
##Answer: Iowa City, IA


# Problem 4 - Integrating Country of Birth Data


# Merge in the country of birth information from the CountryMap data frame.
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

# What is the name of the variable added to the CPS data frame by this merge 
# operation?
str(CPS)
##Answer: Country

# How many interviewees have a missing value for the new country of birth 
# variable?
table(is.na(CPS$Country))
##Answer: 176

# Among all interviewees born outside of North America, which country was the
# most common place of birth?
sort(table(CPS$Country))
##Answer: Look at the last country displayed that isn't a North American
##country, which is Philippines.

# What proportion of the interviewees from the "New York-Northern New Jersey-Long
# Island, NY-NJ-PA" metropolitan area have a country of birth that is not the
# United States? Don't include people from this metropolitan area who have a missing
# country of birth.
table(CPS$Country == "United States", CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
##Answer: 3736/(3736+1668) = 0.6913397

# Which metropolitan area has the largest number of interviewees with a country of 
# birth in India? In Brazil? In Somalia?
sort(tapply(CPS$Country=="India", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country=="Brazil", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country=="Somalia", CPS$MetroArea, sum, na.rm=TRUE))
##Answer: New York-Northern New Jersey-Long Island, NY-NJ-PA; Boston-Cambridge-Quincy, MA-NH 
##Minneapolis-St Paul-Bloomington, MN-WI 




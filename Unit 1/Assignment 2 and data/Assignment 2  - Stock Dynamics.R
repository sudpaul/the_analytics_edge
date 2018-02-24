Sys.setlocale("LC_ALL", "C")

#Load the data
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")


#Problem 1 - Summary Statistics


# Convert the Date variable in each data frame to a format R can read
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

# How many observations are there in each data set? (It is the same for each.)
nrow(IBM)
##Answer: 480

# What is the earliest year in our data sets? (It is the same for each.)
head(GE$Date)
##Answer: 1970

# What is the latest year in our data sets? (It is the same for each.)
tail(GE$Date)
##Answer: 2009

# What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice)
##Answer: 144.375

# What is the minimum stock price of General Electric (GE) over this time period?
min(GE$StockPrice)
##Answer: 9.293636

# What is the maximum stock price of Coca-Cola over this time period?
max(CocaCola$StockPrice)
##Answer: 146.5843

# What is the median stock price of Boeing over this time period?
median(Boeing$StockPrice)
##Answer: 44.8834

# What is the standard deviation of the stock price of Procter & Gamble
# over this time period?
sd(ProcterGamble$StockPrice)
##Answer: 18.19414


#Problem 2 - Visualizing Stock Dynamics


# Plot the Date on the x-axis and the StockPrice on the y-axis for Coca-Cola
# using a red line to connect the data.
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")

# Around what year did Coca-Cola have its highest stock price in this time period?
##Answer: 1973

# Around what year did Coca-Cola have its lowest stock price in this time period?
##Answer: 1980

# Add a blue line to the plot for the Procter & Gamble date.
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")

# Which company's stock dropped more in March 2000?
abline(v=as.Date("2000-03-01"), lwd=2)
##Answer: ProcterGamble

# Around 1983, the stock for one of these companies (Coca-Cola or Procter and 
# Gamble) was going up, while the other was going down. Which one was going up?
abline(v=as.Date("1983-01-01", lwd=2))
##Answer: 

# In the time period shown in the plot, which stock generally has lower values?
##Answer: Coca-Cola


#Problem 3 - Visualizing Stock Dynamics 1995-2005


# Plot the CocaCola stock prices from 1995 through 2005, corresponding to 
# observations numbered from 301 to 432. Connect the points by a red line
# and limit the y axis from 0 to 210.
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

# Add a similar line graph for each of the other four companies, making
# sure to give each a unique color.
lines(GE$Date[301:432], GE$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="purple")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="orange")

# Which stock fell the most right after the technology bubble burst in March 2000?
abline(v=as.Date("2000-03-01", lwd=2))
##Answer: GE

# Which stock reaches the highest value in the time period 1995-2005?
##Answer: IBM

# Comparing September 1997 to November 1997, which companies saw a decreasing trend
# in their stock price?
abline(v=as.Date("1997-09-01", lwd=2))
abline(v=as.Date("1997-11-30", lwd=2))
##Answer: Boeing, ProcterGamble

# In 2004-2005, which stock seems to be performing the best, in terms of increasing 
# stock price?
abline(v=as.Date("2004-01-01", lwd=2))
abline(v=as.Date("2005-12-31", lwd=2))
##Answer: Boeing


#Problem 4 - Monthly Trends


# In which months has IBM historically had a higher stock price (on average)? Select
# all that apply.
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)
##Answer: January, February, March, April, May

# Repeat the tapply function from the previous problem for each of the other four 
# companies
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)

# General Electric and Coca-Cola both have their highest average stock price in the 
# same month. Which month is this?
##Answer: April

# For the months of December and January, every company's average stock is higher 
# in one month and lower in the other. In which month are the stock prices lower?
##Answer: December
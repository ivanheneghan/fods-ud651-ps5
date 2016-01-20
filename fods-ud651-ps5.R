# Install packages
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("alr3")
update.packages("ggplot2")
install.packages("GGally")

# Load relevant libraries
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr, warn.conflicts = FALSE)
library(alr3)
library(reshape2)
library(tidyr)
library(GGally)

# Histogram of diamond prices, faceted by color; cut used to color the bars
glimpse(diamonds)
ggplot(data = diamonds, aes(x = price, fill = cut)) + 
  geom_histogram(binwidth = 1000) +
  facet_wrap( ~ color) +
  scale_fill_brewer(type = "qual") +
  xlab("Price") +
    ggtitle("Diamond Prices")

# Scatterplot of price v table, colored by cut
ggplot(data = diamonds, aes(x = table, y = price)) + 
  geom_point(aes(color = cut)) +
  coord_cartesian(xlim = c(50, 80)) +
  scale_x_discrete(breaks = seq(50, 80, 1)) +
  scale_color_brewer(type = "qual") +
  xlab("Table") +
  ylab("Price") +
  ggtitle("Price vs Table")

# Scatterplot of price v table, colored by cut
# Create volume variable
diamonds$volume = diamonds$x * diamonds$y *diamonds$z

# Scatterplot!
ggplot(data = diamonds, aes(x = volume, y = price)) + 
  geom_point(aes(color = clarity)) +
  coord_cartesian(xlim = c(0, quantile(diamonds$volume, 0.99))) +
  scale_y_log10() +
  scale_color_brewer(type = "div") +
  xlab("Volume") +
  ylab("Price") +
  ggtitle("Price vs Volume")

# Proportion of friendships initiated
# Read data in
pf <- read.csv("pseudo_facebook.tsv", sep = "\t")
glimpse(pf)

# Create prop_initiated (friendships_initiated/friend_count)
pf$prop_initiated <- ifelse(pf$friend_count > 0, c(pf$friendships_initiated/pf$friend_count), 0)

# Line graph of median prop vs tenure; color by year_joined.bucket
# Year joined & year joined bucket
pf$year_joined <- floor(2014 - ceiling(pf$tenure/365))
pf$year_joined.bucket <- cut(pf$year_joined, c(2004, 2009, 2011, 2012, 2014))

# Graphing
ggplot(aes(x = tenure, y = prop_initiated), 
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = median) +
  xlab("Tenure") +
  ylab("Prop Initiated") +
  ggtitle("Prop Initiated v Tenure")

# Smooth the plot
ggplot(aes(x = tenure, y = prop_initiated), 
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = median) +
  geom_smooth() +
  xlab("Tenure") +
  ylab("Prop Initiated") +
  ggtitle("Prop Initiated v Tenure")

# Summary
summary(subset(pf$prop_initiated, pf$year_joined > 2012 & pf$year_joined <= 2014))


# Scatterplot of price/carat ratio
ggplot(data = diamonds, aes(x = cut, y = price/carat)) + 
  geom_point(aes(color = color), position = position_jitter(h = 0)) +
  facet_wrap( ~ clarity) +
  scale_color_brewer(type = "div") +
  xlab("Cut") +
  ylab("Price/Carat Ratio") +
  ggtitle("Price/Carat Ratio vs Cut")

# Gapminder revisited
# Mobile phone ownership & income per person/GDP per capita
# Go to right directory
getwd()
setwd("D:\\Foundations of Data Science\\Projects\\EDA - Facebook\\fods-ud561-ps5")

# Import data
cell <- read.csv("cell phone per 100.csv", 1, stringsAsFactors = FALSE)
gdp <- read.csv("GDPpercapitaconstant2000US.csv", 1, stringsAsFactors = FALSE)
hourly <- read.csv("indicator_hour compensation.csv", 1, stringsAsFactors = FALSE)

# Check data
str(cell)
head(cell)
View(cell)
str(gdp)
head(gdp)
View(gdp)
str(hourly)
head(hourly)
View(hourly)

# Clean up cell data
names(cell)[1] <- c("country")
cell <- cell %>% gather(year, subs, -country)
cell$year <- gsub("X", "", cell$year)

# Clean up gdp data
names(gdp)[1] <- c("country")
gdp <- gdp %>% gather(year, gdp, -country)
gdp$year <- gsub("X", "", gdp$year)

# Clean up hourly data
names(hourly)[1] <- c("country")
hourly <- hourly %>% gather(year, hourly, -country)
hourly$year <- gsub("X", "", hourly$year)

# Subset to a matching timeframe - 1981 to 2006
cell <- subset(cell, year >= 1981 & year <= 2006)
gdp <- subset(gdp, year >= 1981 & year <= 2006)
hourly <- subset(hourly, year >= 1981 & year <= 2006)

# Merge data sets
temp <- merge(cell, gdp, all = TRUE)
merged <- merge(temp, hourly, all = TRUE)

# Clean NAs
merged <- filter(merged, !is.na(merged$subs))
merged <- filter(merged, !is.na(merged$gdp))
merged <- filter(merged, !is.na(merged$hourly))

# Coerce year to numeric
merged$year <- as.numeric(merged$year)

# Scatterplot matrix for initial viewpoints - shows strong correlation between GDP & hourly rate (naturally), but also strong correlations between all three variables
ggpairs(merged)

# Line graph showing evolution of hourly rate over time, and mean/quantiles for 10th/50th/90th
ggplot(aes(x = year, y = hourly), data = merged) +
  geom_point(color = "orange") +
  geom_line(stat = "summary", fun.y = mean) +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = "blue") +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .5), linetype = 2, color = "blue") +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = "blue") +
  xlab("Year") +
  ylab("Hourly Rate") +
  ggtitle("Hourly Rate over Time")

# Line graph showing evolution of subs over time, and mean/quantiles for 10th/50th/90th
ggplot(aes(x = year, y = subs), data = merged) +
  geom_point(color = "orange") +
  geom_line(stat = "summary", fun.y = mean) +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = "blue") +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .5), linetype = 2, color = "blue") +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = "blue") +
  xlab("Year") +
  ylab("Cell Phone Subs per 100 People") +
  ggtitle("Cell Phone Subs per 100 People over Time")

# By country, ratio of subs/gdp evolution - massive uptick in some countries (Philippines, Hungary, Brazil, Poland)
ggplot(aes(x = year, y = subs/gdp), data = merged) + 
  geom_line() +
  facet_wrap( ~ country) +
  xlab("Year") +
  ylab("Subs/GDP Ratio") +
  ggtitle("By-Country Change in Subs/GDP Ratio over Time")

# Look at which countries means are increasing faster than the grand mean of the ratio, zooming in on 1995 to 2006 - 8 in total. Is this the right approach, as removing first mean calculations doesn't change the graph at all?
ggplot(aes(x = year, y = subs/gdp), data = merged) +
  geom_line(aes(color = country, group = country), stat = "summary", fun.y = mean) +
  geom_line(stat = "summary", fun.y = mean, linetype = 2, color = "black") +
  coord_cartesian(xlim = c(1995, 2006)) +
  xlab("Year") +
  ylab("Subs/GDP Ratio") +
  ggtitle("Change in Subs/GDP Ratio over Time")

# Add mean, median, percentile (10, 50, 90), using cut on GDP
summary(merged$gdp)
table(merged$gdp)
merged$gdpbucket <- cut(merged$gdp, breaks = seq(463, 53630, by = 5000))
ggplot(aes(x = gdpbucket, y = subs), data = merged) +
  geom_point(color = "orange") +
  geom_line(stat = "summary", fun.y = mean, group = 1) +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = "blue", group = 1) +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .5), linetype = 2, color = "blue", group = 1) +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = "blue", group = 1) +
  labs(title = "GDP vs Cell Phone Subs",
       x = "GDP per Capita",
       y = "Cell Phones per 100 People")

# Heatmaps
ggplot(aes(x = year, y = country, fill = subs), data = merged) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("blue", "red"))(100))
ggplot(aes(x = year, y = country, fill = gdp), data = merged) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("blue", "red"))(100))
ggplot(aes(x = year, y = country, fill = hourly), data = merged) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("blue", "red"))(100))
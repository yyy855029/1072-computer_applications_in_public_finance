
library(readxl)
library(tidyverse)

# Load the data files
setwd(getwd())

mydata <- read_excel("counties.xlsx", na="-")

# check the data
mydata

# Recode Year as a numeric variable
# All other variables are NOT capitalized, we will change the variable name from "Year" to "year" for consistency

mydata <- rename(mydata, year=Year)
mydata <- mutate (mydata, year=as.numeric(year))

# Area could be coded as a categorical variable, and renamed as area for consistency
mydata <- rename(mydata, area=Area)
mydata <- mutate (mydata, area=factor(area))
levels(mydata$area)

# Create a new data frame. In this new data frame, exclude observations that belong to "Total" and "Taiwan"
sub_data <- filter(mydata, area != "Total" & area != "Taiwan")

# Create another data frame, keep only observations from year 2015
sub_data2015 <- filter(sub_data, year==2015)

plot(sub_data2015$self_funding, sub_data2015$elder_share)

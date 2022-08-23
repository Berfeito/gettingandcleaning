getwd()
install.packages("dplyr")
library(dplyr)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
if(!file.exists("data")){
  dir.create("data")
}
download.file(fileURL, destfile = "./data/microdata.csv", method = "curl")
library(readr)
microdata <- read_csv("data/microdata.csv")

# households on greater than 10 acres who sold more than $10,000 worth of agriculture products
agricultureLogical <- microdata$ACR == 3 & microdata$AGS == 6
which(agricultureLogical)

#install JPEG package
install.packages("jpeg")
library(jpeg)
photo <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
photoPath = 'C:/Users/Bernardo/Documents/Coursera/Developping Data Products/Products_Learning/gettingandcleaning/photo.jpg'
download.file(photo, photoPath, mode = 'wb')
readphoto <- readJPEG(photoPath, native = TRUE)
quantile(readphoto, probs = c(0.3, 0.8))


#Load the Gross Domestic Product data for the 190 ranked countries in this data set

library(data.table)

URLgdp <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
URLgdppath <- 'C:/Users/Bernardo/Documents/Coursera/Developping Data Products/Products_Learning/gettingandcleaning/GDP.csv'
download.file(URLgdp, destfile = URLgdppath, method = "curl")
GDP <- fread(URLgdp, skip = 5, nrows = 190, select = c(1, 2, 4, 5), col.names = c("CountryCode", "Rank", "Economy", "Total"))

# Load the educational data from this data set:
  
URLeduc <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(URLeduc, destfile = "./data/Educ.csv", method = "curl")
Educ <- read_csv("data/Educ.csv")

Merge <- merge(GDP, Educ, by = "CountryCode")
Merge <- Merge %>% arrange(desc(Rank))
head(Merge, 13)


# What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?

aveGDP <- Merge %>% group_by(`Income Group`) %>%
  filter("High income: OECD" %in% `Income Group` | "High income: nonOECD" %in% `Income Group`) %>%
  summarize(Average = mean(Rank, na.rm = TRUE)) %>%
  arrange(desc(`Income Group`))

# Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. How many countries
# are Lower middle income but among the 38 nations with highest GDP?


Merge$RankGroups <- cut(Merge$Rank, breaks = 5)
vs <- table(Merge$RankGroups, Merge$`Income Group`)
vs


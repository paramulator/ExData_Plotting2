# Exploratory Data Analysis
# Course Project 2 - Analysis of PM2.5 dataset------------------------------


# Question 1: Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, 
# and 2008.


# Approach:  A simple bar plot showing the yearly level of total emissions.


# STEP 1: Load required packages and define some utility stuff -------------

# Load required packages...  Won't reload if already loaded.
library(MASS)   # Statistics library
library(dplyr)  # Data manipulation
library(tidyr)  # Data manipulation

# All png plot dimensions
width <- 900
height <- 700

# All bar plots the same color.
barFillColor <- "blue"

prettyNum <- function(num, d=1) {
# A helper function that formats numbers as xxx,xxx,xxx.xxxx where
# the number of decimal places is a parameter.  For example the real number
# num = 4536890.23457 is formatted as 4,536,890.2 if d = 1.
# 
# Inputs:
#   num:  The number to be formatted with commas and decimal places.
#   d:    The number of decimal places to be displayed.
#   
# Outputs:
#   The number formatted with commas and the specified number of decimal places.
#   The number is rounded to achieve the number of decimal places specified.

    formatC(round(num, 1), format = "f", big.mark = ",", digits = d)    
}



# STEP 2: Setting up raw data for analysis --------------------------------

# Define external and local files and folders. 
externalZipURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
localZipFolder <- "FNEIData"
localZipFile <- "___FNEIData___.zip"

# Names of local files once unzipped.
rawPm25Data <- "summarySCC_PM25.rds"
rawSourceClassCode <- "Source_Classification_Code.rds"

# Along the way several data frames get created.  Lets remove these and start
# from scratch so we're sure of the data being plotted.  But, we'll always keep
# the two large frames SCC and NEI since once built these never change.
 
objectList <- c("totalEmissions")
				
for (object in objectList) {
    if (exists(object)) {
        remove(list = object)
        message(paste(object, "removed"))
    }
}

# Download external data if its not already stored in current working directory.  
# There is no point re-downloading this data for each plot we want to make
# since it is so time consuming to do so and the raw data are never modified.
download <- FALSE
if (!file.exists(localZipFolder)) {
    message("Downloading source data")
    download.file(externalZipURL, localZipFile)
    unzip(localZipFile, exdir = localZipFolder) 
    file.remove(localZipFile)
    download <- TRUE
}

# Only build the two big dataframes if they don't already exist, or if raw data 
# has just been downloaded.  We want to avoid repeating this step if we don't
# need to since it is time consuming and these data frames don't get modified
# once they are built.
if (!exists("NEI") | !exists("SCC") | download == TRUE) {
    message("Building NEI and SCC data frames")
    
    # For NEI, specify full path from current working directory to the raw data 
    # file in the local zip folder.  Then, read the data in to a data frame.
    dataPath <- paste0(localZipFolder, "/", rawPm25Data) 
    NEI <- readRDS(dataPath) %>% mutate(SCC = as.character(SCC))
    
    # Repeat above for SCC data.
    dataPath <- paste0(localZipFolder, "/", rawSourceClassCode) 
    SCC <- readRDS(dataPath) %>% mutate(SCC = as.character(SCC))
}



# STEP 3: Prepare data for plotting ---------------------------------------

# Start with NEI dataframe, group it by year, sum the Emissions by year, and
# sort the rows in ascending order by year.
totalEmissions <- NEI %>%
    tbl_df %>%
    filter(Pollutant == "PM25-PRI") %>%
    group_by(year) %>%
    summarize(totalEmissions = sum(Emissions)) %>%
    select(year, totalEmissions) %>%
    arrange(year)



# STEP 4: Generate plot -------------------------------------------------

# Use a simple bar plot to show the yearly levels of total emissions.  From
# here its easy to see how emissions levels have changed over time.

png(width = width, height = height, filename = "plot1.png")
par(mar = c(4,6,5,4))

with(totalEmissions, {mp <- barplot(totalEmissions,  # mp keeps bar midpoints!
                             space = 1.5,
                             xlab = "Year",
                             ylab = "Total PM2.5 Emissions \n (millions of tons)",
                             yaxt = "n",
                             ylim = c(0, 8e6),
                             col = barFillColor)

                    # Notice here that mp can be used to position tick marks.
                    axis(1, at = mp, tick = TRUE, labels = c("1999", 
                                                             "2002", 
                                                             "2005", 
                                                             "2008"))
                    
                    # Using exponential notation to handle large numbers.
                    axis(2, at = seq(0, 8e6, 2e6), labels = seq(0, 8, 2))
                    
                    # Position emissions amount above bars (millions of tons).
                    text(mp, totalEmissions, round(totalEmissions/1e6, 1), 
                         pos = 3)
                    title("Total PM2.5 Emissions (millions of tons) from all sources")})
dev.off()


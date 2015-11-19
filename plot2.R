# Exploratory Data Analysis
# Course Project 2 - Analysis of PM2.5 dataset------------------------------

# Question 2:  Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to 
# make a plot answering this question.


# Approach:  Simple bar plot showing level of total emissions in Baltimore
# for each of the 4 years.



# STEP 1: Load required packages and define some utility stuff -------------

# Load required packages...  Won't reload if already loaded.
library(MASS)   # Statistics library
library(dplyr)  # Data manipulation
library(tidyr)  # Data manipulation

# For ggplot, the common theme we'll use for all plots we build will be 
# theme_bw + some added customization for the title and axis labels.
commonTheme <- theme_bw() +
    theme(plot.title = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          strip.text.x = element_text(size = rel(1.5)))

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
 
objectList <- c("totalEmissionsBalt")
				
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



# STEP 3:  Prepare data for plotting --------------------------------------

# Start with the NEI dataframe and subset to just those rows corresponding to 
# Baltimore.  Then, group by year, sum up the yearly emissions, and order the
# rows by ascending year.

totalEmissionsBalt <- NEI %>%
    tbl_df %>%
    filter(fips == "24510" & Pollutant == "PM25-PRI") %>%
    group_by(year) %>%
    summarize(totalEmissions = sum(Emissions)) %>%
    select(year, totalEmissions) %>%
    arrange(year)



# STEP 4:  Generate plot --------------------------------------------------
# A simple bar plot will show how yearly pollution levels have changed for
# Baltimore.
 
png(width = width, height = height, filename = "plot2.png")
par(mar = c(4,6,5,4)) 

with(totalEmissionsBalt, {mp <- barplot(totalEmissions,  # mp for bar midpoints
                                    space = 1.5,
                                    xlab = "Year",
                                    ylab = "Total PM2.5 Emissions (tons)",
                                    yaxt = "n",
                                    ylim = c(0, 3500),
                                    col = barFillColor)

# Using mp here to position tick marks for bars.
axis(1, at = mp, tick = TRUE, labels = c("1999", "2002", "2005", "2008"))

# Using custom prettyNum function to control display of y-axis tick labels.
axis(2, at = seq(0, 3500, 500), labels = prettyNum(seq(0, 3500, 500), d = 0), 
     cex.axis = 0.8)

# Using custom prettyNum function to control display of bar heights on plot.
text(mp, totalEmissions, prettyNum(totalEmissions), pos = 3)

title("Total PM2.5 Emissions (tons) from all sources \n Baltimore, MD only")})

dev.off()




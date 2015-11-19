# Exploratory Data Analysis
# Course Project 2 - Analysis of PM2.5 dataset------------------------------

# Question 5: How have emissions from motor vehicle sources changed from 
# 1999–2008 in Baltimore City?

# Approach:  We'll use a simple bar plot to show how yearly emissions have
# changed over time for Baltimore. 



# STEP 1: Load required packages and define some utility stuff -------------

# Load required packages...  Won't reload if already loaded.
library(MASS)   # Statistics library
library(dplyr)  # Data manipulation
library(tidyr)  # Data manipulation
library(DT)     # Data display

library(ggplot2) # Graphics
library(scales)  # Graphics
library(grid)    # Graphics
library(gridExtra) # More graphics

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
 
objectList <- c("motorVehicleOnlyBalt", 
				"mvOnlyEmBalt") 

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



# STEP 3: Identify SCC codes for motor vehicle sources --------------------

# First, find the SCC codes that correspond to motor vehicle sources.  An
# analysis of the SCC data suggests to following strategy works.  The end 
# result is a single vector of SCC codes.
motorVehicleOnlyBalt <- SCC %>%
        tbl_df %>%
        filter(grepl("Mobile Sources", SCC.Level.One) == TRUE
             & grepl("Highway Vehicles", SCC.Level.Two) == TRUE) %>%
        select(SCC)



# STEP 4: Organize data for plotting --------------------------------------

mvOnlyEmBalt <- NEI %>%
    tbl_df %>%
    
    # Only keep data for Baltimore, and for PM25 pollution.
    filter(fips == "24510" & Pollutant == "PM25-PRI") %>%
    
    # The semi_join acts as a filter based on the SCC codes found above.
    semi_join(motorVehicleOnlyBalt, by = "SCC") %>%
    
    # Now group the data by year and add up ther emissions across all
    # the motor vehicle SCC codes.
    group_by(year) %>%
    summarize(totalEmissions = sum(Emissions)) %>%
    
    # To simplify plotting convert year to a character value.
    mutate(year = as.character(year))



# STEP 5:  Construct the plot ---------------------------------------------

png(width = width, height = height, filename = "plot5.png")
ggplot(data = mvOnlyEmBalt) +
    
    # Simple bar plots will show how emissions vary over time.
    geom_bar(aes(x = year, y = totalEmissions, width = 0.35), 
             stat = "identity", fill = barFillColor) + 
    
    # Using custom function prettyNum to add nice totals over each bar.
    geom_text(aes(x = year, y = totalEmissions, 
              label = prettyNum(totalEmissions, 1)), 
              vjust = -1.6, color = "black", size = 4.5) +
    
    # Control the range of the y-axis so annotated totals will fit.
    scale_y_continuous(limits = c(0, 400), labels = comma) +
    
    # Title and labels.  Note use of \n for improved spacing.
    xlab("Year") +
    ylab("Total PM2.5 Emissions (tons) \n") +
    ggtitle("Total PM2.5 Emissions (tons) from Vehicle sources \n Baltimore, MD only \n") +
    
    # Adjustments based on the common theme defined above.
    commonTheme 

dev.off()




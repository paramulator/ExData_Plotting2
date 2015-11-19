# Exploratory Data Analysis
# Course Project 2 - Analysis of PM2.5 dataset------------------------------

# Question 6:  Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes over time 
# in motor vehicle emissions? 

# Approach:  Change can be absolute or relative.  Construct two bar plots
# with one comparing total emissions from motor vehicle sources between the two 
# counties and the other showing the relative change in emissions from one year 
# to the next between the two counties.



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
#   A character string representing the number formatted with commas and the 
#   specified number of decimal places.  The number is rounded to achieve the 
#   number of decimal places specified. 

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
 
objectList <- c("mvOnlyEmLACBalt", 
				"motorVehicleOnly") 
				
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



# STEP 3:  Identify the appropriate SCC codes -----------------------------

# First, find the SCC codes that correspond to motor vehicle sources.  An
# analysis of the SCC data suggests to following strategy works.  The end 
# result is a single vector of appropriate SCC codes. 
motorVehicleOnly <- SCC %>%
    tbl_df %>%
    filter(grepl("Mobile Sources", SCC.Level.One) == TRUE
           & grepl("Highway Vehicles", SCC.Level.Two) == TRUE) %>%
    select(SCC)



# STEP 4: Organize data for plotting --------------------------------------

mvOnlyEmLACBalt <- NEI %>%
    tbl_df %>%
    
    # Filter NEI data to specific fips codes and PM25 pollution.
    filter(fips %in% c("06037", "24510") & Pollutant == "PM25-PRI") %>%
    
    # Filter via selected SCC codes identified above.
    semi_join(motorVehicleOnly, by = "SCC") %>% 
    
    # Sum up emissions by county and year.
    group_by(fips, year) %>%
    summarize(totalEmissions = sum(Emissions)) %>%
    ungroup %>%
    
    # Convert year values to valid R variable names.  We'll see why in a moment.
    # Also, convert fips to a factor and give meaningful labels to its levels.
    mutate(prettyYear = paste0("Year", year),
           fips = factor(fips, levels = c("06037", "24510"),
                         labels = c("Los Angeles County, CA", "Baltimore, MD"))) %>%
    select(-year) %>%
    
    # Now transpose the data so that for each fips code we have one column of 
    # total emissions for each year.  From there we can compute absolute and 
    # relative year-over-year changes in polution levels.  When we're done we'll
    # have 2 very wide rows, one for each county.
    spread(prettyYear, totalEmissions) %>%
    mutate(delta1999 = NA,
           delta2002 = Year2002 - Year1999,
           delta2005 = Year2005 - Year2002,
           delta2008 = Year2008 - Year2005,
           pdelta1999 = NA,
           pdelta2002 = (Year2002 - Year1999) / Year1999,
           pdelta2005 = (Year2005 - Year2002) / Year2002,
           pdelta2008 = (Year2008 - Year2005) / Year2005) %>%
    
    # For plotting we want to reorganize the data again so that we have a row
    # for each county and year and a column for total emissions, year over year
    # absolute change in emissions, and year over year relative change in emissions.
    # The following steps complete this while cleaning up and reordering the 
    # columns of the final data frame.  The select statement makes it clear
    # what the new columns are.  Perhaps this can be done in fewer steps? 
    gather(key = "year", value = "value", -fips) %>%
    separate(year, c("varType", "year"), -5) %>%
    spread(varType, value) %>%
    rename(totalEmissions = Year) %>%
    select(fips, year, totalEmissions, delta, pdelta)
    


# STEP 5:  Generate plot --------------------------------------------------

# We're going to stack two unrelated plots vertically so we'll generate two 
# ggplot2 objects and replay them back in a grid.

# First make a plot of total emissions by year, faceted by county.
p1 <- ggplot(data = mvOnlyEmLACBalt) +
    
    # Simple bar plots tell the story.
    geom_bar(aes(x = year, y = totalEmissions, width = 0.35), 
             stat = "identity", fill = barFillColor) + 
 
    scale_y_continuous(limits = c(0, 5000), labels = comma) +
       
    # Annotating bars with the value of total emissions.      
    geom_text(aes(x = year, y = totalEmissions, 
                  label = prettyNum(totalEmissions, 1)), 
              vjust = -1.0, color = "black", size = 4.5) +
    
    # Place county results side-by-side.
    facet_grid(. ~ fips) +
    
    # Title and axis labels.
    xlab("Year") +
    ylab("Total PM2.5 Emissions (tons) \n") +
    ggtitle("Total PM2.5 Emissions (tons) from Vehicle sources \n Los Angeles County, CA vs. Baltimore, MD \n") +
    
    # Apply common theme elements and we're done.
    commonTheme

# Now plot the relative change in emissions year over year using similar code.
p2 <- ggplot(data = mvOnlyEmLACBalt) +
    geom_bar(aes(x = year, y = pdelta, width = 0.35), 
             stat = "identity", fill = barFillColor) + 
    
    # Note we can use the sign of pdelta to help position bar annotations.
    geom_text(aes(x = year, 
                  y = pdelta, 
                  label = paste(prettyNum(100 * pdelta, 1), "%"), 
                  vjust = -1.0 * sign(pdelta)), color = "black", size = 4.5) +
    facet_grid(. ~ fips) +
    scale_y_continuous(limits = c(-0.70, 0.70), breaks = seq(-0.8, 0.8, 0.2), labels = percent) +
    xlab("Year") +
    ylab("Percent relative change \n") +
    ggtitle("Relative change in emissions from Vehicle sources \n Year over previous year \n Los Angeles County, CA vs. Baltimore, MD \n") +
    commonTheme

# Replay the plots stacked in a 2 x 1 grid with p1 on top of p2.
png(width = width, height = height, filename = "plot6.png")
grid.arrange(p1, p2, nrow = 2)
dev.off()


# Exploratory Data Analysis
# Course Project 2 - Analysis of PM2.5 dataset------------------------------


# Question 4: Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?


# Approach: Determine if the pattern of changes in emissions over time depends 
# on a county's overall level of emissions.  We won't look at this by 
# individual county, but instead will look at aggregate behavior by forming 
# three groups of counties: large counties, medium counties and small counties. 
# County size is simply taken to be a county's grand total of emissions,
# summed across all four years of data.

# Note: I would have prefered to do this analysis by grouping counties according 
# to population size, proximity to major cities, or by an metro/urban/rural 
# designation but these variables are not in the source files.  Another approach 
# would have been to base county size on 1999 emissions but it turns out that
# many fips codes were not populated with data for 1999.  So, the grand total of
# emissions across was used for grouping purposes. 



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
 
objectList <- c("coalCombustionOnly",
                "coalCombustionEmissions")
				
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



# STEP 3: Identify SCC codes for coal combustion --------------------------

# First identify the SCC codes that correspond with coal combustion.  This
# was done by looking at the level one and level three variables in the SCC 
# data frame. An examination of the data suggested a simple way to narrow down
# the SCC codes to the appropriate set as can be seen here:
    
coalCombustionOnly <- SCC %>%
    tbl_df %>%
    filter(grepl("[cC]ombustion", SCC.Level.One) == TRUE) %>%
    filter(grepl("[cC]oal", SCC.Level.Three) == TRUE) %>%
    select(SCC) %>%
    distinct(SCC)

# The end result of this step is a list of SCC codes that will be used to
# "filter" the NEI data to the appropriate cases.



# STEP 4:  Prepare data for plotting --------------------------------------

# We'll organize the data by groups of large , medium, and small counties, 
# sum up the emissions by group size and year, and then generate a faceted bar
# plot to show how pollution levels for large, medium, and small counties have 
# changed from 1999 to 2008.  Unfortunately there are a lot of steps required
# just to get the data in shape for this analysis.  Sorry its so long :(

coalCombustionEmissions <- NEI %>%
    
	# First, make sure we are only keeping results for PM25 pollution.  Also,
	# dumps rows with NA fips code.
	tbl_df %>%
	filter(grepl("NA", fips) == FALSE & Pollutant == "PM25-PRI") %>%
    
    # Keep just the rows that correspond with coal combustion.  The semi_join
	# acts as a filter based on the SCC codes we found.
    semi_join(coalCombustionOnly, by = "SCC") %>%
	
	# Convert year to a character string that can be treated as a valid R 
	# variable name.  We'll see why later.
    mutate(prettyYear = paste0("Year", year)) %>%
	
	# Keep only the variables we care about to minimize resources used.
    select(fips, prettyYear, Emissions) %>%
			
	# Group everything by fips and year and sum up the emissions.		
    group_by(fips, prettyYear) %>%
    summarize(totalEmissions = sum(Emissions)) %>%
	ungroup %>%
	
	# Now transpose the data.  We want to create four columns of emissions
	# readings for each county: one column for each year (this is why we needed
	# to turn the values of the year variable into valid R variable names.)
    spread(prettyYear, totalEmissions) %>%
	
	# With one row per county and four emissions readings per county, compute a 
	# county emissions grand total, which will be used to group counties into 
	# large, medium and small.  Also count the number of years with readings
	# for each county.  To aid comparisons we'll only keep those counties with
	# four annual readings.
    rowwise %>%  # For non-vectorized calculations at the county level:
    mutate(grandTotal = sum(Year1999, Year2002, Year2005, Year2008, na.rm = TRUE),
           missingYears = anyNA(c(Year1999, Year2002, Year2005, Year2008))) %>%
	filter(missingYears == FALSE) %>%
	ungroup %>%
	
	# With a grand total of emissions computed for each county, rank the 
	# counties from largest to smallest, and then assign each to a "third" 
	# according to their relative rank.
    arrange(desc(grandTotal)) %>%
    mutate(pRankGT = percent_rank(desc(grandTotal)),
           third = cut(pRankGT, c(-0.1, (1/3), (2/3), 1.0), 
                       labels = c("Top 33% of Counties\n (Highest grand total)", 
                                  "Middle 33% of Counties", 
                                  "Bottom 33% of Counties \n (Lowest grand total)"))) %>%

	# Now that each county has been assigned to a group ("third") lets 
	# aggregate the emissions across counties within each group.
    group_by(third) %>%
    summarize(sumYear1999 = sum(Year1999, na.rm = TRUE), 
              sumYear2002 = sum(Year2002, na.rm = TRUE), 
              sumYear2005 = sum(Year2005, na.rm = TRUE),
              sumYear2008 = sum(Year2008, na.rm = TRUE)) %>%
	ungroup %>%
			  
	# We finally have total emissions by group and year, but to make plotting
	# easier lets transpose the data back to its original form where we have
	# a single column for year and a separate column for total emissions in
	# that year.  We'll also convert year to a nice string for better plots.
    gather(key = year, value = totalEmissions, -third) %>%
    mutate(year = as.character(substr(year, 8, 11)))
    
	# The data are finally in shape for plotting.



# STEP 5: Generate plot.  

png(width = width, height = height, filename = "plot4.png")
ggplot(data = coalCombustionEmissions) +
    # Simple bar plot
    geom_bar(aes(x = year, y = totalEmissions, width = 0.35), 
             stat = "identity", fill = barFillColor) +
    
    # Generate a logarithmic y-axis.  Use logs because the largest counties
    # are 100x bigger than medium size counties in the aggregate, and medium 
    # size counties are 10x bigger than the smallest counties in the 
    # aggregate. 
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10 ^ x),
                  labels = trans_format("log10", math_format(10 ^ .x)),
                  limits = c(1, 2 * 1e6)) +
    
    # Using custom function prettyNum to place nice totals on top of bars.
    geom_text(aes(x = year, y = totalEmissions, 
              label = prettyNum(totalEmissions, 0)), 
              vjust = -1.6, color = "black", size = 4.5) +
    
    # Faceting by large/medium/small county-size groups ("third").
    facet_grid(. ~ third) +
    
    # Titles and axis labels.  Note use of \n for new lines and better spacing.
    xlab("\n Year") +
    ylab("Total PM2.5 Emissions (tons) \n") +
    ggtitle("Total PM2.5 Emissions (tons) from Coal Combustion sources \n Counties grouped by four year grand total of emissions \n") +
    
    # Some common theme adjustments.
    commonTheme

dev.off()



#!/usr/bin/env Rscript

# Author: Elliot Kleiman
# File..: plot3.R
# Desc..: File to analyze course project data
# Date..: Sat Dec 20 18:14:13 EST 2014
# Usage.: $ Rscript plot3.R # execute from a Unix or Linux shell, -or
#           source(plot3.R) # execute from R console 

## 0. Download data ---------------------------

# Data dir exist?
if (!file.exists("data")) {
    dir.create("data")
}

# Set File Url
file.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

# Set data dir
data.directory <- "./data"

# Set data file name
file.name <- "NEI_data.zip"

# Set path/to/data file 
data.file <- file.path(data.directory, file.name)

# Download file
download.file(file.url, destfile = data.file, method = "curl")

# Record date downloaded
date.downloaded <- date()

# Extract zip archive file
unzip(data.file, exdir = data.directory)

## 1. Read data ---------------------------

# Set path to emissions data file
emissions.file <- file.path(data.directory, "summarySCC_PM25.rds")

# Set path to source classification code table data file
classification.file <- file.path(data.directory, "Source_Classification_Code.rds")

# Read emissions data
nei <- readRDS(emissions.file)

# Read source classification table data
classification <- readRDS(classification.file)

## 2. Plot data ---------------------------

# Plots dir exist?
if (!file.exists("plots")) {
    dir.create("plots")
}

# Set plots dir
plot.directory <- "./plots"

# Plot3
# Set plot file name
plot.name <- "plot3.png"

# Set path/to/plot file 
plot.file <- file.path(plot.directory, plot.name)

# Set device
png(file = plot.file)

# Compute total emissions by year for county Baltimore city
require(dplyr)
emissions.balto.type <- nei %>%
                          filter(fips == "24510") %>%
                          group_by(year, type) %>%
                          summarise(total = sum(Emissions))

# Plot
require(ggplot2)
g <- ggplot(emissions.balto.type, aes(year, total))

# Add layers
g +
  geom_line(aes(color = type)) +
  labs(title = expression("Total " * PM[2.5] * " Emissions by Year & Type in Baltimore City")) +
  labs(x = "Year", y = expression("Total " * PM[2.5] * " Emissions (tons)"))

# Close device
dev.off()

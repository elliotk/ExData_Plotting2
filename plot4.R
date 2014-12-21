#!/usr/bin/env Rscript

# Author: Elliot Kleiman
# File..: plot4.R
# Desc..: File to analyze course project data
# Date..: Sat Dec 20 18:14:13 EST 2014
# Usage.: $ Rscript plot4.R # execute from a Unix or Linux shell, -or
#           source(plot4.R) # execute from R console 

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

# Plot4
# Set plot file name
plot.name <- "plot4.png"

# Set path/to/plot file 
plot.file <- file.path(plot.directory, plot.name)

# Set device
png(file = plot.file)

# Search for coal combustion-related sources
coal.sources.index <- grep("coal", classification$Short.Name, ignore.case = TRUE, perl = TRUE)
# Select coal combustion-related sources
scc.coal <- as.character(classification[coal.sources.index, "SCC"])

# Compute total coal combustion-related emissions by year
require(dplyr)
emissions.coal <- nei %>%
                    filter(SCC %in% scc.coal) %>%
                    group_by(year) %>%
                    summarise(total = sum(Emissions))

# Plot
plot(emissions.coal$year, emissions.coal$total, xaxt = "n", type = "b", xlab = "", ylab = "")
axis(1, at = emissions.coal$year, labels = as.character(emissions.coal$year))
title(main = expression("Total PM" [2.5] * " Coal Emissions by Year"),
      xlab = "Year",
      ylab = expression("Total PM" [2.5] * " Emissions (tons)"))

# Close device
dev.off()

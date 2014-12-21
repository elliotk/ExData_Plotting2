#!/usr/bin/env Rscript

# Author: Elliot Kleiman
# File..: plot1.R
# Desc..: File to analyze course project data
# Date..: Sat Dec 20 18:14:13 EST 2014
# Usage.: $ Rscript plot1.R # execute from a Unix or Linux shell, or
# source(run_analysis.R) # execute from R console 

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

# Plot1
# Set plot file name
plot.name <- "plot1.png"

# Set path/to/plot file 
plot.file <- file.path(plot.directory, plot.name)

# Set device
png(file = plot.file)

# Compute total emissions by year
require(dplyr)
emissions <- nei %>%
               group_by(year) %>%
               summarise(total = sum(Emissions))

# Plot
plot(emissions$year, emissions$total, xaxt = "n", type = "b", xlab = "", ylab = "")
axis(1, at = emissions$year, labels = as.character(emissions$year))
title(main = expression("Total PM" [2.5] * " Emissions by Year"),
      xlab = "Year",
      ylab = expression("Total PM" [2.5] * " Emissions (Millions of tons)"))
     
# Close device
dev.off()

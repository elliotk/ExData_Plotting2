#!/usr/bin/env Rscript

# Author: Elliot Kleiman
# File..: plot6.R
# Desc..: File to analyze course project data
# Date..: Sat Dec 20 18:14:13 EST 2014
# Usage.: $ Rscript plot6.R # execute from a Unix or Linux shell, -or
#           source(plot6.R) # execute from R console 

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

# Plot6
# Set plot file name
plot.name <- "plot6.png"

# Set path/to/plot file 
plot.file <- file.path(plot.directory, plot.name)

# Set device
png(file = plot.file)

# Search for motor vehicle sources
motor.vehicle.index.1 <- grep("motor|vehicle", classification$EI.Sector, ignore.case = TRUE, perl = TRUE)
motor.vehicle.index.2 <- grep("motor|vehicle", classification$SCC.Level.Two, ignore.case = TRUE, perl = TRUE)
motor.vehicle.index.3 <- grep("motor|vehicle", classification$SCC.Level.Three, ignore.case = TRUE, perl = TRUE)
motor.vehicle.index.4 <- grep("motor|vehicle", classification$SCC.Level.Four, ignore.case = TRUE, perl = TRUE)
# Get unique set of source indexes
motor.vehicle.sources.index <- unique(c(motor.vehicle.index.1, motor.vehicle.index.2, motor.vehicle.index.3, motor.vehicle.index.4))
# Select motor vehicle sources
scc.motor.vehicles <- as.character(classification[motor.vehicle.sources.index, "SCC"])

# Compute total motor vehicle emissions by year for both Balto and LA
require(dplyr)
emissions.motor.vehicles.balto.la <- nei %>%
                                       filter(SCC %in% scc.motor.vehicles & fips == "06037" | fips == "24510") %>%
                                       mutate(fips = ifelse(fips == "06037", "Los Angeles", "Baltimore City")) %>%
                                       rename(county = fips) %>%
                                       group_by(year, county) %>%
                                       summarise(total = sum(Emissions))

# Define function to scale total between 0 and 1
range01 <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

# Scale balto and la between 0 and 1
balto.scaled.01 <- range01(emissions.motor.vehicles.balto.la$total[emissions.motor.vehicles.balto.la$county == "Baltimore City"])
   la.scaled.01 <- range01(emissions.motor.vehicles.balto.la$total[emissions.motor.vehicles.balto.la$county == "Los Angeles"])

# Update total to scaled value
emissions.motor.vehicles.balto.la$total[emissions.motor.vehicles.balto.la$county == "Baltimore City"] <- balto.scaled.01
emissions.motor.vehicles.balto.la$total[emissions.motor.vehicles.balto.la$county == "Los Angeles"]    <- la.scaled.01

# Plot
require(ggplot2)
g <- ggplot(emissions.motor.vehicles.balto.la, aes(year, total))

# Add layers
g +
  geom_line(aes(color = county)) +
  facet_grid(. ~ county) +
  geom_smooth(method = "lm", se = FALSE, color = "#333333", alpha = 1/2) +
  labs(title = expression("Total " * PM[2.5] * " Motor Vehicle Emissions by Year")) +
  labs(x = "Year", y = expression("Total " * PM[2.5] * " Emissions (tons, rescaled [0, 1])"))

# Close device
dev.off()

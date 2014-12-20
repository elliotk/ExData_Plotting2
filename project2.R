#!/usr/bin/env Rscript

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
#png(file = plot.file)

# Compute total emissions by year
#total.emissions <- aggregate(Emissions ~ year, data = nei, sum)
require(dplyr)
emissions <- nei %.%
               group_by(year) %.%
               summarise(total = sum(Emissions))

# Plot
plot(emissions$year, emissions$total, xaxt = "n", type = "b", xlab = "", ylab = "")
axis(1, at = emissions$year, labels = as.character(emissions$year))
title(main = expression("Total PM" [2.5] * " Emissions by Year"),
      xlab = "Year",
      ylab = expression("Total PM" [2.5] * " Emissions (Millions of tons)"))
     
# Close device
#dev.off()

# Plot2
# Set plot file name
plot.name <- "plot2.png"

# Set path/to/plot file 
plot.file <- file.path(plot.directory, plot.name)

# Set device
#png(file = plot.file)

# Compute total emissions by year for county Baltimore city
require(dplyr)
emissions.balto <- nei %.%
                     filter(fips == "24510") %.%
                     group_by(year) %.%
                     summarise(total = sum(Emissions))
# Plot
plot(emissions.balto$year, emissions.balto$total, xaxt = "n", type = "b", xlab = "", ylab = "")
axis(1, at = emissions.balto$year, labels = as.character(emissions.balto$year))
title(main = expression("Total PM" [2.5] * " Emissions by Year for Baltimore city"),
      xlab = "Year",
      ylab = expression("Total PM" [2.5] * " Emissions (tons)"))

# Close device
dev.off()

# Plot3
# Set plot file name
plot.name <- "plot3.png"

# Set path/to/plot file 
plot.file <- file.path(plot.directory, plot.name)

# Set device
#png(file = plot.file)

# Compute total emissions by year for county Baltimore city
require(dplyr)
emissions.balto.type <- nei %.%
                          filter(fips == "24510") %.%
                          group_by(year, type) %.%
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

# Plot4
# Set plot file name
plot.name <- "plot4.png"

# Set path/to/plot file 
plot.file <- file.path(plot.directory, plot.name)

# Set device
#png(file = plot.file)

# Search for coal combustion-related sources
coal.sources.index <- grep("coal", classification$Short.Name, ignore.case = TRUE, perl = TRUE)
# Select coal combustion-related sources
scc.coal <- as.character(classification[coal.sources.index, "SCC"])

# Compute total coal combustion-related emissions by year
require(dplyr)
emissions.coal <- nei %.%
                    filter(SCC %in% scc.coal) %.%
                    group_by(year) %.%
                    summarise(total = sum(Emissions))

# Plot
plot(emissions.coal$year, emissions.coal$total, xaxt = "n", type = "b", xlab = "", ylab = "")
axis(1, at = emissions.coal$year, labels = as.character(emissions.coal$year))
title(main = expression("Total PM" [2.5] * " Coal Emissions by Year"),
      xlab = "Year",
      ylab = expression("Total PM" [2.5] * " Emissions (tons)"))

# Close device
dev.off()

# Plot5
# Set plot file name
plot.name <- "plot5.png"

# Set path/to/plot file 
plot.file <- file.path(plot.directory, plot.name)

# Set device
#png(file = plot.file)

# Search for motor vehicle sources
motor.vehicle.index.1 <- grep("motor|vehicle", classification$EI.Sector, ignore.case = TRUE, perl = TRUE)
motor.vehicle.index.2 <- grep("motor|vehicle", classification$SCC.Level.Two, ignore.case = TRUE, perl = TRUE)
motor.vehicle.index.3 <- grep("motor|vehicle", classification$SCC.Level.Three, ignore.case = TRUE, perl = TRUE)
motor.vehicle.index.4 <- grep("motor|vehicle", classification$SCC.Level.Four, ignore.case = TRUE, perl = TRUE)
# Get unique set of source indexes
motor.vehicle.sources.index <- unique(c(motor.vehicle.index.1, motor.vehicle.index.2, motor.vehicle.index.3, motor.vehicle.index.4))
# Select motor vehicle sources
scc.motor.vehicles <- as.character(classification[motor.vehicle.sources.index, "SCC"])

# Compute total motor vehicle emissions by year
require(dplyr)
emissions.motor.vehicles.balto <- nei %.%
                                    filter(SCC %in% scc.motor.vehicles & fips == "24510") %.%
                                    group_by(year) %.%
                                    summarise(total = sum(Emissions))

# Plot
plot(emissions.motor.vehicles.balto$year, emissions.motor.vehicles.balto$total, xaxt = "n", type = "b", xlab = "", ylab = "")
axis(1, at = emissions.motor.vehicles.balto$year, labels = as.character(emissions.motor.vehicles.balto$year))
title(main = expression("Total PM" [2.5] * " Motor Vehicle Emissions by Year in Baltimore City"),
      xlab = "Year",
      ylab = expression("Total PM" [2.5] * " Emissions (tons)"))


# Close device
dev.off()

# Plot6
# Set plot file name
plot.name <- "plot6.png"

# Set path/to/plot file 
plot.file <- file.path(plot.directory, plot.name)

# Set device
#png(file = plot.file)

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
# Use rename(county = fips), if upgrade to R version 3.1
## not-run:
# alternatively, mutate(county = ifelse(fips == "24510", "Baltimore City", "Los Angeles")
emissions.motor.vehicles.balto.la <- nei %.%
                                       filter(SCC %in% scc.motor.vehicles & fips == "06037" | fips == "24510") %.%
                                       group_by(year, fips) %.%
                                       summarise(total = sum(Emissions))

# Set fips values to their character string equivalents
emissions.motor.vehicles.balto.la$fips <- rep(c("Los Angeles", "Baltimore City"), 4)

# Rename colname "fips" to "county"
colnames(emissions.motor.vehicles.balto.la)[2] <- "county"

# Plot
require(ggplot2)
g <- ggplot(emissions.motor.vehicles.balto.la, aes(year, total))

# Add layers
g +
  geom_line(aes(color = county)) +
  labs(title = expression("Total " * PM[2.5] * " Motor Vehicle Emissions by Year")) +
  labs(x = "Year", y = expression("Total " * PM[2.5] * " Emissions (tons)"))

# inspect relative percent chnage ((new - old) / old) * 100
# scale total to [0, 1]: xi - min(x) / max(x) - min(x)

# Close device
dev.off()

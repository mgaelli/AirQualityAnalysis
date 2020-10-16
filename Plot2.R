makeplot2 <- function(){
  # load dplyr library
  library(dplyr)
  # check if the data is already loaded. If not, do so
  if(!exists("NEI")) {  NEI <- readRDS("summarySCC_PM25.rds") }
  # extract data for Baltimore, group by year, and calculate the totals by summing emissions per year
  TE <- NEI %>% filter(fips == "24510") %>% group_by(year) %>% summarize(Total = sum(Emissions))
  # create a plot with total emissions vs. year, and save it to a ".png" file. The y axis is adjusted to start at 0. 
  png("Plot2.png")
  with(TE, plot(year,Total,pch=20,xlab="Year",ylab="Total PM2.5 Emissions (tons)",cex=2,main="PM2.5 Emissions in Baltimore by Year",
                ylim=c(0,range(Total)[2])))
  dev.off()
}

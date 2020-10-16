makeplot1 <- function(){
  # load dplyr library
  library(dplyr)
  # check if the data is already loaded. If not, do so
  if(!exists("NEI")) {  NEI <- readRDS("summarySCC_PM25.rds") }
  # group data by year, and calculate total mass by summing emissions by year
  TE <- NEI %>% group_by(year) %>% summarize(Total =sum(Emissions))
  # plot the total emissions versus year, and save as ".png" file. The y-axis set to start at 0.
  png("Plot1.png")
  with (TE, plot(year,Total,pch=20,xlab="Year",ylab="Total PM2.5 Emissions (tons)",
               cex=2,main="PM2.5 Emissions by Year",ylim=c(0,range(Total)[2])))
  dev.off()
}

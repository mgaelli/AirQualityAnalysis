makeplot5 <- function(){
  # load dplyr, ggplot2 libraries
  library(dplyr)
  library(ggplot2)
  # check if the data is already loaded. If not, do so
  if(!exists("NEI")) {  NEI <- readRDS("summarySCC_PM25.rds") }
  # check if the source classification codes are loaded. If not, do so
  if(!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}
  # find all source classification codes that have "vehicle" in the EI.Sector 
  mv <- SCC$SCC[grep("[Vv]ehicle",SCC$EI.Sector)]
  # Extract the vehicle contributions from NEI, group by year, and calculate the sum of these emissions
  mv_results <- NEI %>% filter(SCC %in% mv & fips=="24510") %>% group_by(year) %>% summarize(Total = sum(Emissions))
  # plot the results. The y-axis is set to start at 0
  plot5 <- mv_results %>% ggplot(aes(year,Total)) + geom_point(size=4)  + xlab("Year") +
    ylab("Total PM2.5 Emissions from Vehicle Sources (tons)") + ggtitle("PM2.5 Emissions from Motor Vehicles in Baltimore") + 
    ylim(0,range(mv_results$Total)[2])
  # save the plot as a ".png" file
  png("Plot5.png")
  print(plot5)
  dev.off()
}
makeplot4 <- function(){
  # load dplyr, ggplot2 libraries
  library(dplyr)
  library(ggplot2)
  # check if the data is already loaded. If not, do so
  if(!exists("NEI")) {  NEI <- readRDS("summarySCC_PM25.rds") }
  # check if the source classification codes are loaded. If not, do so
  if(!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}
  # find all source classification codes that have "coal" in the Short.Name 
  coal <- SCC$SCC[grep("[Cc]oal",SCC$Short.Name)]
  # Extract the coal contributions from NEI, group by year, and calculate the sum of these emissions
  coal_results <- NEI %>% filter(SCC %in% coal) %>% group_by(year) %>% summarize(Total = sum(Emissions)) 
  # plot the results. The y-axis is set to start at 0
  plot4 <- coal_results %>% ggplot(aes(year,Total)) + geom_point(size=4) + xlab("Year") +
    ylab("Total PM2.5 Emissions from Coal Sources (tons)") + ggtitle("PM2.5 Emissions from Coal") + 
    ylim(0,range(coal_results$Total)[2])
  # save the plot as a ".png" file
  png("Plot4.png")
  print(plot4)
  dev.off()
}
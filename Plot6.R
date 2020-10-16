makeplot6 <- function(){
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
  mv_results <- NEI %>% filter(SCC %in% mv & (fips == "24510" | fips == "06037")) %>% group_by(year,fips) %>% summarize(Total = sum(Emissions))
  # plot the results. The y-axis is set to start at 0
  plot6 <- mv_results %>% ggplot(aes(year,Total)) + geom_point(aes(color=fips),size=4)  + xlab("Year") +
    ylab("Total PM2.5 Emissions from Vehicle Sources (tons)") + ggtitle("PM2.5 Emissions from Motor Vehicles") + 
    ylim(0,range(mv_results$Total)[2]) + scale_color_discrete(name = "City", labels = c("Los Angeles, CA","Baltimore, MD")) + 
    theme(legend.position = c(0.9, 0.3))
  
  # save the plot as a ".png" file
  png("Plot6.png")
  print(plot6)
  dev.off()
}
makeplot3 <- function(){
  # load dplyr, ggplot2 libraries
  library(dplyr)
  library(ggplot2)
  # check if the data is already loaded. If not, do so
  if(!exists("NEI")) {  NEI <- readRDS("summarySCC_PM25.rds") }
  # extract data from Baltimore, group by year and type, and calculate the totals for each group. 
  # Create plot with data series (type) differentiated by color
    Baltimore <- NEI %>% filter(fips == "24510") %>% group_by(year,type) %>% summarize(Total = sum(Emissions)) %>% 
    ggplot(aes(year,Total))  + geom_point(aes(color=type),size=4) + xlab("Year")+ ylab("Total PM2.5 Emissions (tons)")+
    ggtitle("PM2.5 Emissions in Baltimore") + theme(legend.position = c(0.7, 0.9),legend.direction = "horizontal")
  # print the plot to a ".png" file
  png("Plot3.png")
  print(Baltimore)
  dev.off()
}
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("C:/Users/Hp/Downloads/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("C:/Users/Hp/Downloads/exdata_data_NEI_data/Source_Classification_Code.rds")
names(SCC)

install.packages("dplyr")
require(dplyr)

## PLOT1.R
#1) Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from all 
#sources for each of the years 1999, 2002, 2005, and 2008.

annual <- NEI %>% group_by(year) %>% 
  filter(year == 1999|2002|2005|2008) %>% 
  summarize(Annual.Total = sum(Emissions));

yrs <- c(1999,2002,2005,2008)

plot(annual$year, annual$Annual.Total/1000000, type = "l", lwd = 2, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in the United States"))
axis(1, at = yrs, labels = paste(yrs));
axis(2, at = pts, labels = paste(pts, "M", sep = ""));

# or 

x1<-barplot(height=annual$Annual.Total/1000, names.arg=annual$year,
            xlab="years", ylab=expression('total PM'[2.5]*' emission in kilotons'),ylim=c(0,8000),
            main=expression('Total PM'[2.5]*' emissions at various years in kilotons'))

## Add text at top of bars
text(x = x1, y = round(annual$Annual.Total/1000,2), label = round(annual$Annual.Total/1000,2), pos = 3, cex = 0.8, col = "black")

## PLOT2.R
#2)Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (\color{red}
#{\verb|fips == "24510"|}fips=="24510") from 1999 to 2008? Use the base plotting system
#to make a plot answering this question.

baltimore <- NEI %>% 
  filter(fips == "24510") %>% 
  group_by(year) %>% 
  summarize(Annual.Total = sum(Emissions))

plot(baltimore$year, baltimore$Annual.Total/1000, type = "l", lwd = 2, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in Baltimore"))
axis(1, at = c(1999,2002,2005,2008))
baltimore.pts <- pretty(baltimore$Annual.Total/1000);
axis(2, at = baltimore.pts, labels = paste(baltimore.pts, "K", sep = ""))

#or

x2<-barplot(height=baltimore$Annual.Total/1000, names.arg=baltimore$year,
            xlab="years", ylab=expression('total PM'[2.5]*' emission in kilotons'),ylim=c(0,4),
            main=expression('Total PM'[2.5]*' emissions in Baltimore City-MD in kilotons'),col="green")

## Add text at top of bars
text(x = x2, y = round(baltimore$Annual.Total/1000,2), label = round(baltimore$Annual.Total/1000,2), pos = 3, cex = 0.8, col = "black")

## PLOT3.R
#3)Of the four types of sources indicated by the \color{red}{\verb|type|}type 
#(point, nonpoint, onroad, nonroad) variable, which of these four sources have seen 
#decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases 
#in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer 
#this question.

nei.baltimore <- NEI %>% 
  filter(fips == "24510") %>% 
  group_by(type,year) %>% 
  summarize(Annual.Total = sum(Emissions))

#Re-order factor levels so they plot in the order we wish
nei.baltimore$type <- factor(nei.baltimore$type, levels = c("ON-ROAD", "NON-ROAD", "POINT", "NONPOINT"))

ggplot(nei.baltimore, aes(x = factor(year), y = Annual.Total, fill = type)) + 
  geom_bar(stat = "identity") + 
  facet_grid(. ~ type) + 
  xlab("Year") + 
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression("Total Tons of PM"[2.5]*" Emissions in Baltimore by Source Type")) +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous() +
  guides(fill = FALSE)

#or

ggplot(nei.baltimore, aes(x=factor(year), y=Annual.Total, fill=type,label = round(Annual.Total,2))) +
  geom_bar(stat="identity") +
  #geom_bar(position = 'dodge')+
  facet_grid(. ~ type) +
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emission in tons")) +
  ggtitle(expression("PM"[2.5]*paste(" emissions in Baltimore ",
                                     "City by various source types", sep="")))+
  geom_label(aes(fill = type), colour = "white", fontface = "bold")

## PLOT4.R
#4) Across the United States, how have emissions from coal combustion-related sources 
#changed from 1999–2008?

combustion.coal <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
ssc.coal <- SCC[combustion.coal,]

scc.coal.list <- unique(ssc.coal$SCC);
nei.coal <- subset(NEI, SCC %in% scc.coal.list);
nei.coal <- nei.coal %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions))
nei.coal.total <- nei.coal %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(type = "TOTAL");
nei.coal <- nei.coal %>% select(Annual.Total, type, year);
nei.coal <- bind_rows(nei.coal, nei.coal.total);
nei.coal$type <- factor(nei.coal$type, levels = c("TOTAL", "ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")); # Re-order factor levels to they plot in the order we wish
ggplot(nei.coal, aes(x = factor(year), y = Annual.Total, fill = type)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ type) +
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions in the United States", paste("from Coal Combustion-Related Sources")))) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = FALSE)


## PLOT5.R
#5) How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

ssc.vehical <- grepl("Mobile.*Vehicles", SCC$EI.Sector)
ssc.vehical <- SCC[ssc.vehical,]


scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; # Pattern match mobile vehicles in SCC description
scc.vehicles.list <- unique(scc.vehicles$SCC); # Create motor vehicle lookup list by SCC
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list); # Filter for motor vehicle sources
nei.vehicles <- nei.vehicles %>% filter(fips == "24510") # Filter for Baltimore
nei.vehicles <- merge(x = nei.vehicles, y = scc.vehicles[, c("SCC", "SCC.Level.Two", "SCC.Level.Three")], by = "SCC") # Join in descriptive data on SCC codes
nei.vehicles <- nei.vehicles %>% group_by(year, SCC.Level.Two, SCC.Level.Three) %>% summarize(Annual.Total = sum(Emissions))
nei.vehicles.total <- nei.vehicles %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total")
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, levels = c("Total", "Highway Vehicles - Diesel", "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ SCC.Level.Two) +
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions in Baltimore City", paste("from Motor Vehicle Sources")))) +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
  theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)

## PLOT6.R
#6) Compare emissions from motor vehicle sources in Baltimore City with emissions from 
#motor vehicle sources in Los Angeles County, California 
#(𝚏𝚒𝚙𝚜 == “𝟶𝟼𝟶𝟹𝟽”. Which city has seen greater changes over time in motor
#vehicle emissions?

require(dplyr)
baltcitymary.emissions<-summarise(group_by(filter(NEI, fips == "24510"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))
losangelscal.emissions<-summarise(group_by(filter(NEI, fips == "06037"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))

baltcitymary.emissions$County <- "Baltimore City, MD"
losangelscal.emissions$County <- "Los Angeles County, CA"
both.emissions <- rbind(baltcitymary.emissions, losangelscal.emissions)

require(ggplot2)
ggplot(both.emissions, aes(x=factor(year), y=Emissions, fill=County,label = round(Emissions,2))) +
  geom_bar(stat="identity") + 
  facet_grid(County~., scales="free") +
  ylab(expression("total PM"[2.5]*" emissions in tons")) + 
  xlab("year") +
  ggtitle(expression("Motor vehicle emission variation in Baltimore and Los Angeles in tons"))+
  geom_label(aes(fill = County),colour = "white", fontface = "bold")


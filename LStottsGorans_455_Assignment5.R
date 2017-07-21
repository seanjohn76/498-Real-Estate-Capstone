# run choropleth map jump-start using R package googleViz

# program prepared by Tom Miller

# alternative to building a map from scratch using ggplot2 

library(googleVis)  # for working with Google Chart Tools APIA

make.state.abbreviation <- function(x) {
  switch(x, 
    "Alaska" = "AK", "Alabama" = "AL", "Arkansas" = "AR", 
    "Arizona" = "AZ",  "California" = "CA",
    "Colorado" = "CO", "Connecticut" = "CT", "District of Columbia" = "DC", 
    "Delaware" = "DE", "Florida" = "FL",
    "Georgia" =  "GA", "Hawaii" = "HI", "Iowa" = "IA", 
    "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN",
    "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", 
    "Massachusetts" = "MA", "Maryland" = "MD", "Maine" = "ME",
    "Michigan" = "MI", "Minnesota" = "MN", "Missouri" = "MO",  
    "Mississippi" = "MS", "Montana" = "MT", 
    "North Carolina" = "NC", "North Dakota" = "ND",
    "Nebraska" = "NE", "New Hampshire" = "NH", "New Jersey" = "NJ",
    "New Mexico" = "NM", "Nevada" = "NV", "New York" = "NY", 
    "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", 
    "Pennsylvania" = "PA", 
    "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD",
    "Tennessee" = "TN", "Texas" = "TX",
    "Utah" = "UT",  "Virginia" = "VA", "Vermont" = "VT", 
    "Washington" = "WA", "Wisconsin" = "WI", 
    "West Virginia" = "WV", "Wyoming" = "WY", "")    
    }


my.data.frame <- read.csv("State_PctOfListingsWithPriceReductions_AllHomes.csv", header = TRUE)
# character string for state... not factor
my.data.frame$State <- as.character(my.data.frame$State)  

my.data.frame$state <- rep("", length = nrow(my.data.frame))
for(index.for.state in seq(along = my.data.frame$State)) 
  my.data.frame$state[index.for.state] <- make.state.abbreviation(my.data.frame$State[index.for.state])
  
# check the coding of states
print(my.data.frame[,c("State", "state")])

# set up color gradient color codes and values to be used in the JSON
my.value.gradient <- c(min(my.data.frame$X2017.01), 
                       median(my.data.frame$X2017.01),
                       max(my.data.frame$X2017.01)) 
print(my.value.gradient)  # min, med, max to be entered into JSON format                       

# create the map object by using googleVis function to 
# generate a javascript program for running the choropleth map
javascript.us.map.object <-  gvisGeoChart(my.data.frame, "state", "X2017.01",
  options=list(region="US", 
  displayMode="regions",
  resolution="provinces", 
  colorAxis = "{values: [5.882353, 11.928474, 18.206434], colors: [\'lightgreen', \'lightgray', \'blue']}", 
  width=700, height=500,
  jsHeader="Percent of Home Listings with Price Reductions: January 2017"))

# what kind of class is this javascript program in R?
#   class(javascript.us.map.object)
#   [1] "gvis" "list"

# using plot on a gvis list object sends the program to the default browser application
plot(javascript.us.map.object)  # plots in a new browser window

############################################################################################################
#trying zipmap in choroplethr
library(dplyr)
library(choroplethr)
library(choroplethrMaps)

zipcode <- read.csv("Zip_MedianSoldPrice_AllHomes_2016.csv", header = TRUE)
zipcode <- data.frame(zipcode)
zipcode <- subset(zipcode,State=="MN")
zipcode$RegionName <- as.character(zipcode$RegionName) 


#looking at data for MN only, then removing rows (counties) where we are missing data for Jun 2009 or Jun 2015
attach(zipcode)
zipcode_mn <- subset(zipcode, !is.na(zipcode$X2016.01))
detach(zipcode)
attach(zipcode_mn)

############################################################################################################
# June 2015 Median Sale Price
############################################################################################################
mn.df <- data.frame(zipcode_mn$RegionName, zipcode_mn$X2016.01)
colnames(mn.df) <- c('region','value')

pdf(file = "Jan 2016 Sales Zipcode Map.pdf") 
zip2016 <- zip_map(mn.df,
        title="Chart 3: MN Median Sales Price by Zipcode-January 2016",
        legend="Median Sales Price USD",
        buckets=3,
        zoom=c("minnesota"))
print(zip2016)
dev.off()
############################################################################################################

zipcode2 <- read.csv("Zip_MedianListingPrice_AllHomes_2016.csv", header = TRUE)
zipcode2 <- data.frame(zipcode2)
zipcode2 <- subset(zipcode2,State=="MN")
zipcode2$RegionName <- as.character(zipcode2$RegionName) 

#looking at data for MN only, then removing rows (counties) where we are missing data for Jun 2009 or Jun 2015
attach(zipcode2)
zipcode2_mn <- subset(zipcode2, !is.na(zipcode2$X2016.01))
detach(zipcode2)
attach(zipcode2_mn)

############################################################################################################
# June 2015 Median List Price
############################################################################################################
mn2.df <- data.frame(zipcode2_mn$RegionName, zipcode2_mn$X2016.01)
colnames(mn2.df) <- c('region','value')

pdf(file = "Jan 2016 Listing Zipcode Map.pdf") 
zip2016.2 <- zip_map(mn2.df,
                   title="Chart 2: MN Median Listing Price by Zipcode-January 2016",
                   legend="Median Listing Price USD",
                   buckets=3,
                   zoom=c("minnesota"))
print(zip2016.2)
dev.off()


#####################################################################################################################################
#try zip data with googlevis
#I can't get this to look good in a static visualization...

zipcode <- read.csv("Zip_MedianSoldPrice_AllHomes.csv", header = TRUE)
zipcode <- data.frame(zipcode)
zipcode$RegionName <- as.character(zipcode$RegionName)  

#looking at data for MN only, then removing rows (counties) where we are missing data for Jun 2009 or Jun 2015
attach(zipcode)
zipcode <- subset(zipcode, !is.na(zipcode$X2009.06))
zipcode_mn <- subset(zipcode, !is.na(zipcode$X2015.06))
detach(zipcode)
attach(zipcode_mn)
#getting min, max, med values for sales prices in Jun 2009
zipcode.gradient <- c(min(zipcode_mn$X2009.06), 
                      median(zipcode_mn$X2009.06),
                      max(zipcode_mn$X2009.06)) 
print(zipcode.gradient)  # min, med, max to be entered into JSON format 

zipcode.gvis <- gvisGeoChart(zipcode_mn, locationvar='latlong', colorvar='X2009.06', hovervar='RegionName', sizevar='SizeRank',
                             options=list(region='US-MN',width=1050, height=1050, keepAspectRatio=TRUE,
                                          displayMode='Markers', resolution="provinces", markerOpacity=1.0,
                                          colorAxis="{values: [76235, 194000, 367000], colors: [\'lightgreen', \'lightblue', \'coral']}")
) 
plot(zipcode.gvis)
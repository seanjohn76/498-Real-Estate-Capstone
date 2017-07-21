# Visualizing Time Jump-Start Code for Financial Time Series

# begin by installing the packages quantmod, lubridate, latticeExtra, and zoo 

library(quantmod) # use for gathering and charting economic data
library(lubridate) # date functions
library(zoo)  # utilities for working with time series
library(reshape)
library(grid)
library(scales)

#retrieving France, Iceland, Portugal data
country.df <- 
  data.frame(read.csv("Country_Data.csv"))

#subsetting to narrow down to some indicators of interest
country.dfsub <- data.frame(country.df[country.df$Indicator.Code %in% c(
"NY.GNS.ICTR.ZS",
"ST.INT.ARVL",
"ST.INT.TVLR.CD",
"NY.GDP.MKTP.KD.ZG",
"BX.TRF.CURR.CD",
"BX.KLT.DINV.WD.GD.ZS",
"ST.INT.TVLR.CD"
),])

#further subsets to separate country data

#subset per country and remove extra labels
Iceland.sub <- country.dfsub[country.dfsub$Country.Name=='Iceland',4:61]
France.sub <- country.dfsub[country.dfsub$Country.Name=='France',4:61]
Portugal.sub <- country.dfsub[country.dfsub$Country.Name=='Portugal',4:61]

#then transpose and make sure column is labeled with Indicator Code
Iceland.sub <- setNames(data.frame(t(Iceland.sub[,-1])),Iceland.sub[,1])
France.sub <- setNames(data.frame(t(France.sub[,-1])),France.sub[,1])
Portugal.sub <- setNames(data.frame(t(Portugal.sub[,-1])),Portugal.sub[,1])

#remove the 'X' from the year in the rowname
row.names(Iceland.sub) <- gsub('X', '', rownames(Iceland.sub))
row.names(France.sub) <- gsub('X', '', rownames(France.sub))
row.names(Portugal.sub) <- gsub('X', '', rownames(Portugal.sub))

#and we'll break these down so we can create single series ts and xts objects for use of ease
#(I'm sure I could have done this with a loop or in some better way, but...)

# .gdpGrowth = GDP Growth (annual %)
# .intTour   = International Tourism Receipts
# .scndInc   = Secondary Income Receipts
# .intArriv  = International Tourism Arrivals
# .frnInv    = Foreign Direct Investment


#first Iceland
Iceland.gdpGrowth.ts <- ts(Iceland.sub$NY.GDP.MKTP.KD.ZG, start=c(1960), end=c(2016),frequency = 1)
Iceland.gdpGrowth.xts <- as.xts(Iceland.gdpGrowth.ts)
                         
Iceland.intTour.ts <- ts(Iceland.sub$ST.INT.TVLR.CD, start=c(1960), end=c(2016),frequency = 1)
Iceland.intTour.xts <- as.xts(Iceland.intTour.ts)

Iceland.scndInc.ts <- ts(Iceland.sub$BX.TRF.CURR.CD, start=c(1960), end=c(2016),frequency = 1)
Iceland.scndInc.xts <- as.xts(Iceland.intTour.ts)

Iceland.intArriv.ts <- ts(Iceland.sub$ST.INT.ARVL, start=c(1960), end=c(2016),frequency = 1)
Iceland.intArriv.xts <- as.xts(Iceland.intArriv.ts)

Iceland.frnInv.ts <- ts(Iceland.sub$BX.KLT.DINV.WD.GD.ZS, start=c(1960), end=c(2016),frequency = 1)
Iceland.frnInv.xts <- as.xts(Iceland.frnInv.ts)

#next France
France.gdpGrowth.ts <- ts(France.sub$NY.GDP.MKTP.KD.ZG, start=c(1960), end=c(2016),frequency = 1)
France.gdpGrowth.xts <- as.xts(France.gdpGrowth.ts)

France.intTour.ts <- ts(France.sub$ST.INT.TVLR.CD, start=c(1960), end=c(2016),frequency = 1)
France.intTour.xts <- as.xts(France.intTour.ts)

France.scndInc.ts <- ts(France.sub$BX.TRF.CURR.CD, start=c(1960), end=c(2016),frequency = 1)
France.scndInc.xts <- as.xts(France.scndInc.ts)

France.intArriv.ts <- ts(France.sub$ST.INT.ARVL, start=c(1960), end=c(2016),frequency = 1)
France.intArriv.xts <- as.xts(France.intArriv.ts)

France.frnInv.ts <- ts(France.sub$BX.KLT.DINV.WD.GD.ZS, start=c(1960), end=c(2016),frequency = 1)
France.frnInv.xts <- as.xts(France.frnInv.ts)

#finally Portugal
Portugal.gdpGrowth.ts <- ts(Portugal.sub$NY.GDP.MKTP.KD.ZG, start=c(1960), end=c(2016),frequency = 1)
Portugal.gdpGrowth.xts <- as.xts(Portugal.gdpGrowth.ts)

Portugal.intTour.ts <- ts(Portugal.sub$ST.INT.TVLR.CD, start=c(1960), end=c(2016),frequency = 1)
Portugal.intTour.xts <- as.xts(Portugal.intTour.ts)

Portugal.scndInc.ts <- ts(Portugal.sub$BX.TRF.CURR.CD, start=c(1960), end=c(2016),frequency = 1)
Portugal.scndInc.xts <- as.xts(Portugal.scndInc.ts)

Portugal.intArriv.ts <- ts(Portugal.sub$ST.INT.ARVL, start=c(1960), end=c(2016),frequency = 1)
Portugal.intArriv.xts <- as.xts(Portugal.intArriv.ts)

Portugal.frnInv.ts <- ts(Portugal.sub$BX.KLT.DINV.WD.GD.ZS, start=c(1960), end=c(2016),frequency = 1)
Portugal.frnInv.xts <- as.xts(Portugal.frnInv.ts)

#**********************************************************************************************************
#Data is ready(ish)!  Time to plot.
#**********************************************************************************************************

# ---------------------------------------
# Single time series plots
# ---------------------------------------

#Let's look at GDP growth first

chartSeries(Portugal.gdpGrowth.xts,theme="white", major.ticks="years")
chartSeries(France.gdpGrowth.xts,theme="white", major.ticks="years")
chartSeries(Iceland.gdpGrowth.xts,theme="white", major.ticks="years")

# define multiple time series object
GDPGrowth.mts <- cbind(Portugal.gdpGrowth.ts,
                       France.gdpGrowth.ts,
                       Iceland.gdpGrowth.ts) 
dimnames(GDPGrowth.mts)[[2]] <- c("Portugal","France", "Iceland") # keep simple names 
GDPGrowth.mts <- na.omit(GDPGrowth.mts) # keep overlapping time intervals only 

# plot GDP series using standard R graphics 
plot(GDPGrowth.mts,main="GDP Annual Growth%", col="blue", xlab="Year")


#######################################################################################

#International Tourism Receipts

chartSeries(Portugal.intTour.ts,theme="white", major.ticks="years")
chartSeries(France.intTour.ts,theme="white", major.ticks="years")
chartSeries(Iceland.intTour.ts,theme="white", major.ticks="years")

# define multiple time series object
intTour.mts <- cbind(Portugal.intTour.ts,
                     France.intTour.ts,
                     Iceland.intTour.ts) 
dimnames(intTour.mts)[[2]] <- c("Portugal","France", "Iceland")
intTour.mts <- na.omit(intTour.mts) # keep overlapping time intervals only 

# plot International Tourism Receipts series using standard R graphics 
plot(intTour.mts,main="International Tourism Receipts in USD", col="blue", xlab="Year")


#######################################################################################

#Foreign Investment

chartSeries(Portugal.frnInv.ts,theme="white", major.ticks="years")
chartSeries(France.frnInv.ts,theme="white", major.ticks="years")
chartSeries(Iceland.frnInv.ts,theme="white", major.ticks="years")

# define multiple time series object
foreignInv.mts <- cbind(Portugal.frnInv.ts,
                     France.frnInv.ts,
                     Iceland.frnInv.ts) 
dimnames(foreignInv.mts)[[2]] <- c("Portugal","France", "Iceland")
intTour.mts <- na.omit(foreignInv.mts) # keep overlapping time intervals only 

# plot Foreign Investment series using standard R graphics 
plot.ts(foreignInv.mts,type="b", main="Foreign Investment", col="blue", xlab="Year")

#######################################################################################

#International Arrivals - CHART 1

chartSeries(Portugal.intArriv.ts,theme="white", major.ticks="years")
chartSeries(France.intArriv.ts,theme="white", major.ticks="years")
chartSeries(Iceland.intArriv.ts,theme="white", major.ticks="years")

# define multiple time series object
intArrival.mts <- cbind(Iceland.intArriv.ts,France.intArriv.ts, Portugal.intArriv.ts)

dimnames(intArrival.mts)[[2]] <- c("Iceland","France","Portugal")
intArrival.mts <- na.omit(intArrival.mts) # keep overlapping time intervals only 

# plot Foreign Investment series using standard R graphics 
pdf(file="InternationalArrival_ts_plot.pdf",width = 11,height = 8.5)
plot.ts(intArrival.mts,plot.type="multiple", main="Chart 1: International Arrivals", col="blue", xlab="Year", yax.flip=TRUE) 
dev.off()

#######################################################################################

#Secondary Income (USD)

chartSeries(Portugal.scndInc.ts,theme="white", major.ticks="years")
chartSeries(France.scndInc.ts,theme="white", major.ticks="years")
chartSeries(Iceland.scndInc.ts,theme="white", major.ticks="years")

# define multiple time series object
scndInc.mts <- cbind(Iceland.scndInc.ts, 
                        France.scndInc.ts,
                        Portugal.scndInc.ts) 
dimnames(scndInc.mts)[[2]] <- c("Iceland", "France","Portugal")
scndInc.mts <- na.omit(scndInc.mts) # keep overlapping time intervals only 

par(las=1)
# plot Foreign Investment series using standard R graphics 
pdf(file="SecondaryIncome_ts_plot.pdf",width = 11,height = 8.5)
plot.ts(scndInc.mts, plot.type="multiple", main="Chart 2: Secondary Income in USD", col="blue", xlab="Year", yax.flip=TRUE)
dev.off()

###########################################################################################################
#Horizon Plotting
detach("package:ggplot2", unload=TRUE)
library(latticeExtra) # package used for horizon plot

#All Country Summary - Chart 4

# define multiple time series object
All.mts <- cbind(Iceland.intArriv.ts,Iceland.scndInc.ts,
                 France.intArriv.ts,France.scndInc.ts,
                 Portugal.intArriv.ts,Portugal.scndInc.ts) 
dimnames(All.mts)[[2]] <- c("Iceland International Arrivals","Iceland Secondary Income", 
                            "France International Arrivals","France Secondary Income",
                            "Portugal International Arrivals","Portugal Secondary Income") 
All.mts <- na.omit(All.mts) # keep overlapping time intervals only 

# plot Iceland series using horizonplot from latticeExtra - Chart 3
pdf(file="AllCountry_horizon_plot.pdf",width = 11,height = 8.5)
print(horizonplot(All.mts, colorkey = TRUE,
                  layout = c(1,6), strip.left = FALSE,  
                  ylab = list(rev(colnames(All.mts)), rot = 0, cex = 0.7), main="Chart 4: Trends in International Arrivals vs Secondary Income (USD)")) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white"))
dev.off()

#Iceland Summary
# define multiple time series object
Iceland.mts <- cbind(Iceland.scndInc.ts,
                     Iceland.intArriv.ts) 
dimnames(Iceland.mts)[[2]] <- c("Secondary Income (USD)","International Arrivals") 
Iceland.mts <- na.omit(Iceland.mts) # keep overlapping time intervals only 

# plot Iceland series using horizonplot from latticeExtra
pdf(file="Iceland_horizon_plot.pdf",width = 11,height = 8.5)
print(horizonplot(Iceland.mts, colorkey = TRUE,
                  layout = c(1,2), strip.left = FALSE,  
                  ylab = list(rev(colnames(Iceland.mts)), rot = 0, cex = 0.7), main="Chart 3: Iceland Comparison: 1995-2015")) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white"))
dev.off()

#France Summary
# define multiple time series object
France.mts <- cbind(France.scndInc.ts,
                     France.intArriv.ts,
                     France.intTour.ts) 
dimnames(France.mts)[[2]] <- c("Secondary Income (USD)","International Arrivals", "International Tourist Receipts (USD)") 
France.mts <- na.omit(France.mts) # keep overlapping time intervals only 

# plot Iceland series using horizonplot from latticeExtra
print(horizonplot(France.mts, colorkey = TRUE,
                  layout = c(1,3), strip.left = FALSE,  
                  ylab = list(rev(colnames(France.mts)), rot = 0, cex = 0.7), main="France Overview")) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white"))

#Portugal Summary
# define multiple time series object
Portugal.mts <- cbind(Portugal.scndInc.ts,
                      Portugal.intArriv.ts,
                      Portugal.intTour.ts) 
dimnames(Portugal.mts)[[2]] <- c("Secondary Income (USD)","International Arrivals", "International Tourist Receipts (USD)") # keep simple names 
Portugal.mts <- na.omit(Portugal.mts) # keep overlapping time intervals only 

# plot Portugal series using horizonplot from latticeExtra
print(horizonplot(Portugal.mts, colorkey = TRUE,
                  layout = c(1,3), strip.left = FALSE,  
                  ylab = list(rev(colnames(Portugal.mts)), rot = 0, cex = 0.7), main="Portugal Overview")) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white"))


# plot GDP series using horizonplot from latticeExtra
print(horizonplot(GDPGrowth.mts, colorkey = TRUE,
                  layout = c(1,3), strip.left = FALSE,  
                  ylab = list(rev(colnames(GDPGrowth.mts)), rot = 0, cex = 0.7), main="GDP: Annual Growth %")) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white"))

# plot International Tourism Receipts series using horizonplot from latticeExtra
print(
  horizonplot(intTour.mts, colorkey = TRUE,
              layout = c(1,3), strip.left = FALSE,  
              ylab = list(rev(colnames(intTour.mts)), rot = 0, cex = 0.7), main="International Tourism Receipts in USD")) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white") )


# plot International Tourism Receipts series using horizonplot from latticeExtra
print(horizonplot(foreignInv.mts, colorkey = TRUE,
                  layout = c(1,3), strip.left = FALSE,  
                  ylab = list(rev(colnames(foreignInv.mts)), rot = 0, cex = 0.7), main="Foreign Investment")) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white"))

# plot International Tourism Receipts series using horizonplot from latticeExtra
print(horizonplot(intArrival.mts, colorkey = TRUE,
                  layout = c(1,3), strip.left = FALSE,  
                  ylab = list(rev(colnames(intArrival.mts)), rot = 0, cex = 0.7), main="International Arrivals")) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white"))

# plot Secondary Income series using horizonplot from latticeExtra
print(horizonplot(scndInc.mts, colorkey = TRUE,
                  layout = c(1,3), strip.left = FALSE,  
                  ylab = list(rev(colnames(scndInc.mts)), rot = 0, cex = 0.7), main="Secondary Income in USD")) +
  layer_(panel.fill(col = "gray90") , panel.xblocks(..., col = "white"))

#######################################################################################
# -----------------------------------------
# Prepare data frame for ggplot2 work
# -----------------------------------------
library(ggplot2)
modeling_data_frame <- as.data.frame(intArrival.mts)
modeling_data_frame$Year <- as.numeric(time(intArrival.mts))

# examine the structure of the data frame object
# notice an intentional shift to underline in the data frame name
# this is just to make sure we keep our object names distinct
# also you will note that programming practice for database work
# and for work with Python is to utilize underlines in variable names
# so it is a good idea to use underlines generally
print(str(modeling_data_frame)) 
print(head(modeling_data_frame))

# -----------------------------------------
# ggplot2 time series plot
# -----------------------------------------
# with a data frame object in hand... we can go on to use ggplot2 
# and methods described in Chang (2013) R Graphics Cookbook
# note. attepting to use ggplot2 and latticeExtra in the same program
# can cause problems... due to problems with the layer() function
# so we will not use lattice after this point in our code
library(ggplot2)

# highlighting 2008 to 2016
plotting_object <- ggplot(modeling_data_frame, aes(x = Year, y = Portugal)) +
    geom_line() + scale_y_continuous(labels = comma) +
    annotate("rect", xmin = 2008, xmax = 2016, 
        ymin = min(modeling_data_frame$Portugal), ymax = max(modeling_data_frame$Portugal), 
        alpha = 0.3, fill = "blue") +
    ylab("") +
    ggtitle("Portugal International Tourism Receipts") +
    annotate("segment", x=2008, xend=2012, y=7500000000, yend=8000000000, arrow=arrow())
print(plotting_object)    

    
#*****************************************************************************************************************
#Create PDF files

# dump this plot to a pdf file in landscape orientation
pdf(file="fig_economic_time_series_horizon_plot.pdf",width = 11,height = 8.5)

dev.off()

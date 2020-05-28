rm(list=ls())
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(data.table) 

setwd('your directory with downloaded nc files') 

## read the intersection table gridMet polygon by county maps
fips_gridmet <- fread('tabu_fips_gridmet.csv',header=TRUE,stringsAsFactors = FALSE)  
summary(fips_gridmet) 
    OBJECTID           fips             Id              AREA         PERCENTAGE     
 Min.   :     1   Min.   : 1001   Min.   :   711   Min.   : 0.00   Min.   : 0.0000  
 1st Qu.:141534   1st Qu.:19003   1st Qu.:193539   1st Qu.:13.52   1st Qu.: 0.1366  
 Median :283067   Median :31065   Median :331730   Median :15.75   Median : 0.3627  
 Mean   :283067   Mean   :30726   Mean   :337325   Mean   :13.79   Mean   : 0.5489  
 3rd Qu.:424600   3rd Qu.:46019   3rd Qu.:475088   3rd Qu.:17.11   3rd Qu.: 0.8143  
 Max.   :566133   Max.   :56045   Max.   :810489   Max.   :19.39   Max.   :89.5675 
# Key variables are fips, Id, and AREA. 

##
# the daily data of pr (precipitation in mm), tmmn (min temperature in K), tmmx (max temperature in K), vs (surface wind in m/s) 
# is 585(lat) by 1386 (lon) and the top left corner is the first cell with largest lat (49.4) and smallest lon (-124.7666666) and saved 
# row by row. 
# the daily data of pdsi (palmer_drought_severity_index) is 585 by 1386, but the top left corner is the cell with smallest lat (25.06666) 
# and smallest lon (-124.7666666). Thus, daily pdsi data should be flipped along x axis, i.e., the first row become the last row. 

fl <- 'pr_1980.nc' #example nc file 

tryCatch(
  { 
    ncdata <- nc_open(fl) 
  },warning =function(war) {
    print(war)  
  },error   =function(err) {
    print(err)
  }
)
print(ncdata) 

## get the lon, lat, and dates (time dimension) 
lon <- ncvar_get(ncdata,'lon') 
lat <- ncvar_get(ncdata,'lat') 
dates <- ncvar_get(ncdata,'day')  #365/366  

## read one day data, transfor to raster file with date 
i <- 1 
today_pr   <- ncvar_get(ncdata,'precipitation_amount',start=c(1,1,i),count=c(1386,585,1)) # data for first day 

# make a raster dataframe for mapping 
r <- raster(t(today_pr), xmn=min(lon), xmx=max(lon), ymn=min(lat),
                ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) 
plot(r) # see the heatmap of the raster file 

#
pr <- as.data.frame(r)  
pr$Id <- c(1:dim(pr)[1]) #Id matched with fips_gridmet Id variable. 

## match weather variable with fips_gridmet table 
tmp <- merge(fips_gridmet,pr,by='Id',all.x=1) 
setnames(tmp,'layer','pr') 

tmp <- tmp[is.na(pr)==FALSE,] # remove record without weather values 
tmp$year  <- as.numeric(format(as.Date(dates[i],origin='1900-01-01'),'%Y'))
tmp$month <- as.numeric(format(as.Date(dates[i],origin='1900-01-01'),'%m'))
tmp$day   <- as.numeric(format(as.Date(dates[i],origin='1900-01-01'),'%d')) 

    
    
## aggregate 
tmp[,totarea:=sum(AREA),by=c('fips','year','month','day')] 
tmp$wi <- tmp$AREA/tmp$totarea  # area based weights  
xdata <- tmp[,.(pr=sum(pr*wi)),by=c('fips','year','month','day')] #data.table functions 
# xdata is the daily precipitation aggregated at county levels 
# the weighting scheme is area based, and different from other distance-based aggregation. As such, the aggregation results can be 
# different from results from those aggregation. The choice of area based method is from the observation that if you know precipitation
# at every square meter in a region, the average of all these square meter based precipitation should be average precipation in this region, 
# you can know the total amount of rainfall by multiplying this new average with total area of the region. This logic is straightforward in 
# the case of precipitation. For other weather variables, we still use this logic though the same logic is not obvious for these variables. 

# one extra step for PDSI data 
fl <- 'pdsi_1980.nc' 
tryCatch(
  { 
    ncdata <- nc_open(fl) 
  },warning =function(war) {
    print(war)  
  },error   =function(err) {
    print(err)
  }
)

print(ncdata) 
dates <- ncvar_get(ncdata,'day')  # 36 3 dates [1,10,20] at each month  
i <- 1 
today_pdsi   <- ncvar_get(ncdata,'precipitation_amount',start=c(1,1,i),count=c(1386,585,1)) # data for first day 

# make a raster dataframe for mapping 
r <- raster(t(today_pdsi), xmn=min(lon), xmx=max(lon), ymn=min(lat),
                ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) 
plot(r) # see the heatmap of the raster file (inversed) 
rr <- flip(r,direction='y')  # flip the data 
plot(rr) # north is in the north, south is in the south, west is in the west, and east is in the east 
pdsi <- as.data.frame(rr)   
pdsi$Id <- c(1:dim(pdsi)[1]) 

# merge and aggregate 
tmp <- merge(fips_gridmet,pdsi,by='Id',all.x=1) 
setnames(tmp,'layer','pdsi') 
tmp <- tmp[is.na(pdsi)==FALSE,] # remove record without weather values 
tmp$year  <- as.numeric(format(as.Date(dates[i],origin='1900-01-01'),'%Y'))
tmp$month <- as.numeric(format(as.Date(dates[i],origin='1900-01-01'),'%m'))
tmp$day   <- as.numeric(format(as.Date(dates[i],origin='1900-01-01'),'%d')) 
     
## aggregate 
tmp[,totarea:=sum(AREA),by=c('fips','year','month','day')] 
tmp$wi <- tmp$AREA/tmp$totarea  # area based weights  
xdata <- tmp[,.(pdsi=sum(pdsi*wi)),by=c('fips','year','month','day')] #data.table functions  

# match 10-day PDSI to daily precipitation and others 
# assume xdata1: daily weather variables other than PDSI for the year of 1980 (366 days since 1980 is a leap year) 
#        xdata2: 10-day PDSI for the year of 1980 (36 days) 










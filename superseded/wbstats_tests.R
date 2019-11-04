library(countrycode)
library(wbstats)
library(OECD)

new_cache <- wbcache()

unemploy_vars <- wbsearch(pattern = "unemployment")
gdp_vars <- wbsearch(pattern = "GDP per capita", cache = new_cache)
gini_vars <- wbsearch(pattern = "Gini", cache = new_cache)

# Population, total 
pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2000, enddate = 2002)

gdp_data <- wb(indicator = "NY.GDP.PCAP.KD", startdate = 2008, enddate = 2017)

gdp_data <- wb(indicator = "NY.GDP.PCAP.KD", mrv = 1)

gini_data <- wb(indicator = "SI.POV.GINI", startdate = 2008, enddate = 2017)

gini_data <- wb(indicator = "SI.POV.GINI", mrv = 10)

gini_data <- wb(indicator = "SI.POV.GINI", mrv = 1)

gini_data <- wb(indicator = "SI.POV.GINI", mrv = 1, gapfill = T)

gini_data <- wb(indicator = "SI.POV.GINI", startdate = 2008, enddate = 2019)
gini_data
unique(gini_data$iso3c)


### OECD data
 
dataset_list <- get_datasets()
search_dataset("unemployment", data = dataset_list)
search_dataset("income distribution", data = dataset_list)
dstruc <- get_data_structure("IDD")
str(dstruc, max.level = 1)

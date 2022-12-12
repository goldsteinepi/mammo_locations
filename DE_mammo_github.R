#################
# Delaware mammography screening
# Citation: Webster JL, Goldstein ND, Rowland JR, Siegel SD. A Catchment and Location-Allocation Analysis of Mammography Access in Delaware, US: Implications for disparities in geographic access to breast cancer screening. Manuscript in preparation.
# 6/7/22 -- Neal Goldstein
#################

### FUNCTIONS ###

library(tidycensus) #retrieve ACS data, note if error installing on MacOS see: https://github.com/r-quantities/units/issues/1
library(rgdal) #shapefile read
library(sf) #shapefile support
library(psych) #PCA
library(sp) #census tract and spatial points
library(spdep) #census tract and spatial points


### READ DATA ###

#shapefile: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
de_ct = readOGR("tl_2017_10_tract/","tl_2017_10_tract")

#ncc census tracts: https://www.census.gov/geographies/reference-maps/2020/geo/2020pl-maps/2020-census-tract.html
ncc_ct = read.csv("DC10CT_C10003_CT2MS.txt", sep=";", stringsAsFactors=F)

#retrieve census variables of interest: using tidycensus but could manually obtain from FactFinder
census_api_key("<paste api key here>")
population = get_acs(geography="tract", state="DE", table="B01003", year=2017, output="wide")
income = get_acs(geography="tract", state="DE", table="B19013", year=2017, output="wide")
fertility = get_acs(geography="tract", state="DE", variables="B13016_001", year=2017, output="wide")
race = get_acs(geography="tract", state="DE", table="B02001", year=2017, output="wide")
insurance_employment = get_acs(geography="tract", state="DE", table="B27011", year=2017, output="wide")
age_sex = get_acs(geography="tract", state="DE", table="S0101", year=2017, output="wide")
age_race = get_acs(geography="tract", state="DE", table="B01001B", year=2017, output="wide")
vehicles = get_acs(geography="tract", state="DE", table="B08201", year=2017, output="wide")

#census-based deprivation index: details provided here https://towardsdatascience.com/a-census-based-deprivation-index-using-r-7aa738da697c, https://www.ncbi.nlm.nih.gov/pubmed/17031568
deprivation = get_acs(geography="tract", state="DE", variables=c("B17001_002", "B17001_001", "B06009_002" , "B06009_001","B09008_011","B09008_001","B08124_002", "B08124_001", "B25014_005","B25014_006",  "B25014_007","B25014_011", "B25014_012", "B25014_013","B25014_001", "B19058_002", "B19058_001","C23002C_021", "C23002D_008","C23002C_017", "C23002D_003","B19001_002", "B19001_003", "B19001_004","B19001_005", "B19001_006", "B19001_001"), output="wide", year=2017)

#alcohol retailers per tract, from Maddie Brooks via https://data.delaware.gov/Licenses-and-Certifications/Delaware-Business-Licenses/5zy2-grhr
#alcohol = read.csv("DE_Tract_AlcoholRetailerCounts_04172019.csv", stringsAsFactors=F)

#mammography locations, compiled from FDA and ACR
#FDA https://www.accessdata.fda.gov/scripts/cdrh/cfdocs/cfMQSA/mqsa.cfm (download all facilities here: https://www.fda.gov/radiation-emitting-products/consumer-information-mqsa/search-certified-facility)
#ACR https://www.acraccreditation.org/accredited-facility-search (download all facilities here: https://accreditationfacilitylist.acr.org/)
mammo = read.csv("Facilities/Mammo_locations.csv", stringsAsFactors=F)

#bus stops: https://opendata.firstmap.delaware.gov/datasets/delaware::delaware-bus-stops-2-0-1/
busstops = read.csv("Delaware_Bus_Stops_2.0.csv", stringsAsFactors=F)


### CODE and LINK DATA ###

#recode census variables
#subject variable description see: https://api.census.gov/data/2017/acs/acs5/subject/variables/<variable>.json
#detailed variable description: https://api.census.gov/data/2017/acs/acs5/variables/<variable>.json
#all variables: https://api.census.gov/data/2017/acs/acs5/variables.html
population$census_population = population$B01003_001E
income$census_income = income$B19013_001E
fertility$census_fertility = fertility$B13016_001E
race$census_white_percent = race$B02001_002E/race$B02001_001E*100
race$census_black_percent = race$B02001_003E/race$B02001_001E*100
race$census_other_percent = 100 - (race$census_white_percent + race$census_black_percent)
insurance_employment$census_employed_percent = insurance_employment$B27011_003E/insurance_employment$B27011_002E*100
insurance_employment$census_insured_percent = (insurance_employment$B27011_004E+insurance_employment$B27011_009E+insurance_employment$B27011_014E)/insurance_employment$B27011_001E*100
insurance_employment$census_insured_private_percent = (insurance_employment$B27011_005E+insurance_employment$B27011_010E+insurance_employment$B27011_015E)/insurance_employment$B27011_001E*100
insurance_employment$census_insured_govt_percent = (insurance_employment$B27011_006E+insurance_employment$B27011_011E+insurance_employment$B27011_016E)/insurance_employment$B27011_001E*100
age_sex$women_40_49 = age_sex$S0101_C05_010E + age_sex$S0101_C05_011E
age_sex$women_50_74 = age_sex$S0101_C05_012E + age_sex$S0101_C05_013E + age_sex$S0101_C05_014E + age_sex$S0101_C05_015E + age_sex$S0101_C05_016E
age_sex$women_75_plus = age_sex$S0101_C05_017E + age_sex$S0101_C05_018E + age_sex$S0101_C05_019E
age_race$women_35_44_black = age_race$B01001B_026E
age_race$women_45_54_black = age_race$B01001B_027E
age_race$women_55_64_black = age_race$B01001B_028E
age_race$women_65_74_black = age_race$B01001B_029E
age_race$women_75_plus_black = age_race$B01001B_030E + age_race$B01001B_031E
age_race$women_black_percent = age_race$B01001B_017E / age_sex$S0101_C05_001E[match(age_race$GEOID,age_sex$GEOID)] * 100
vehicles$percent_vehicle = (1 - (vehicles$B08201_002E / vehicles$B08201_001E)) * 100

#create deprivation index: https://towardsdatascience.com/a-census-based-deprivation-index-using-r-7aa738da697c, https://www.ncbi.nlm.nih.gov/pubmed/17031568
deprivation$pct_poverty = deprivation$B17001_002E / deprivation$B17001_001E
deprivation$pct_noHS = deprivation$B06009_002E / deprivation$B06009_001E
deprivation$pct_FHH = deprivation$B09008_011E / deprivation$B09008_001E
deprivation$pct_mgmt = deprivation$B08124_002E / deprivation$B08124_001E 
deprivation$pct_crowd = (deprivation$B25014_005E + deprivation$B25014_006E + deprivation$B25014_007E + deprivation$B25014_011E + deprivation$B25014_012E + deprivation$B25014_013E) / deprivation$B25014_001E
deprivation$pct_pubassist = deprivation$B19058_002E / deprivation$B19058_001E
deprivation$pct_unempl = (deprivation$C23002C_021E + deprivation$C23002D_008E) / (deprivation$C23002C_017E + deprivation$C23002D_003E)
deprivation$pct_under30K = ((deprivation$B19001_002E + deprivation$B19001_003E + deprivation$B19001_004E + deprivation$B19001_005E + deprivation$B19001_006E) / deprivation$B19001_001E)
deprivation_matrix = as.matrix(deprivation[, c("pct_poverty","pct_noHS","pct_FHH","pct_mgmt","pct_crowd","pct_pubassist", "pct_unempl","pct_under30K")])
deprivation_matrix[is.nan(deprivation_matrix)] = 0
deprivation$census_NDI = principal(deprivation_matrix,nfactors = 1)$scores 

de_census = data.frame("Tract"=de_ct$GEOID)

#merge census data to individual data
de_census = merge(de_census, population[,c("GEOID","census_population")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)
de_census = merge(de_census, income[,c("GEOID","census_income")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)
de_census = merge(de_census, fertility[,c("GEOID","census_fertility")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)
de_census = merge(de_census, race[,c("GEOID","census_white_percent","census_black_percent","census_other_percent")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)
de_census = merge(de_census, insurance_employment[,c("GEOID","census_employed_percent","census_insured_percent","census_insured_private_percent","census_insured_govt_percent")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)
de_census = merge(de_census, age_sex[,c("GEOID","women_40_49","women_50_74","women_75_plus")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)
de_census = merge(de_census, age_race[,c("GEOID","women_35_44_black","women_45_54_black","women_55_64_black","women_65_74_black","women_75_plus_black","women_black_percent")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)
de_census = merge(de_census, deprivation[,c("GEOID","census_NDI")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)
#de_census = merge(de_census, alcohol[,c("GEOID","AlcOffsite","AlcOnsite")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)
de_census = merge(de_census, vehicles[,c("GEOID","percent_vehicle")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)

#count mammography locations by census tract
de_census$Mammo = 0
de_census$Mammo_units = 0
de_census$BICOE = 0
de_census$BICOE_units = 0
for (i in 1:nrow(mammo)) {
  #create a spatial point from lat/lon
  spatial_pt = SpatialPoints(data.frame(x = as.numeric(mammo$Longitude[i]), y = as.numeric(mammo$Latitude[i])))
  
  #set CRS of spatial point using DE census tract
  proj4string(spatial_pt) = proj4string(de_ct)
  
  #map point into tract
  spatial_ct = over(spatial_pt, de_ct)
  
  #increment counters in this census tract  
  de_census$Mammo[which(de_census$Tract==spatial_ct$GEOID)] = de_census$Mammo[which(de_census$Tract==spatial_ct$GEOID)] + 1
  de_census$Mammo_units[which(de_census$Tract==spatial_ct$GEOID)] = de_census$Mammo_units[which(de_census$Tract==spatial_ct$GEOID)] + mammo$Units[i]
  de_census$BICOE[which(de_census$Tract==spatial_ct$GEOID)] = de_census$BICOE[which(de_census$Tract==spatial_ct$GEOID)] + ifelse(mammo$BICOE[i]=="Y", 1, 0)
  de_census$BICOE_units[which(de_census$Tract==spatial_ct$GEOID)] =de_census$BICOE_units[which(de_census$Tract==spatial_ct$GEOID)] + ifelse(mammo$BICOE[i]=="Y", mammo$Units[i], 0)
}
rm(i, spatial_ct, spatial_pt)

#count bus stops by census tract
de_census$busstops = 0
for (i in 1:nrow(busstops)) {
  #create a spatial point from projection: https://gis.stackexchange.com/questions/18940/converting-point-to-lat-lon and https://epsg.io/3857
  spatial_pt = SpatialPoints(project(matrix(as.numeric(busstops[i,c("X","Y")]),ncol=2), "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", inv=T))
  
  #set CRS of spatial point using DE census tract
  proj4string(spatial_pt) = proj4string(de_ct)
  
  #map point into tract
  spatial_ct = over(spatial_pt, de_ct)
  
  #increment counters in this census tract  
  de_census$busstops[which(de_census$Tract==spatial_ct$GEOID)] = de_census$busstops[which(de_census$Tract==spatial_ct$GEOID)] + 1
}
rm(i, spatial_ct, spatial_pt)

#save
rm(age_sex,age_race,vehicles,deprivation,deprivation_matrix,fertility,income,insurance_employment,population,race,busstops)
save.image("DE_data.RData")


### FUNCTIONS ###

library("psych") #describe, describeBy
library(proj4) #projection to coords
library(raster) #area function
library(sf) #shapefile support
#library(pscl) #zero inflated models


### LOAD DATA ###

load("DE_data.RData")


### SPATIAL ANALYSIS ###

#recast as an sf object
#de_ct = st_as_sf(de_ct)

#trim water areas
de_ct = de_ct[-which(de_ct$NAME %in% c(9900,9901)), ]

#area of census tracts
de_ct$AREA = area(de_ct)
de_census = merge(de_census, de_ct[,c("GEOID","AREA")], by.x="Tract", by.y="GEOID", all.x=T, all.y=F)

#create ncc only
ncc_ct = de_ct[which(de_ct$GEOID %in% ncc_ct$CODE), ]

plot(de_ct$geometry)
plot(ncc_ct$geometry)

#merge map data
map_proj_merged = merge(x=de_ct, y=de_census, by.y="Tract", by.x="GEOID", all.x=T, duplicateGeoms=T)

#check for spatial dependency
nb_list = poly2nb(de_ct)
wt_list = nb2listw(nb_list)
moran.mc(map_proj_merged$Mammo, wt_list, nsim=1000)
moran.mc(map_proj_merged$BICOE, wt_list, nsim=1000)
moran.mc(map_proj_merged$Mammo_units, wt_list, nsim=1000)
moran.mc(map_proj_merged$BICOE_units, wt_list, nsim=1000)


### ECOLOGICAL DESCRIPTIVES ###

de_census = de_census[de_census$census_population>0, ]

sum(de_census$census_population)
sum(de_census$Mammo)
sum(de_census$Mammo_units)
sum(de_census$BICOE)
sum(de_census$BICOE_units)

describe(de_census$census_NDI)
describe(de_census$women_40_49)
describe(de_census$women_50_74)
describe(de_census$women_75_plus)
describe(de_census$women_black_percent)
describe(de_census$percent_vehicle)
describe(de_census$busstops)
describe(de_census$Mammo)
describe(de_census$Mammo_units)
describe(de_census$BICOE)
describe(de_census$BICOE_units)

#NCC only
ncc_census = de_census[which(de_census$Tract %in% ncc_ct$GEOID), ]

sum(ncc_census$census_population)
sum(ncc_census$Mammo)
sum(ncc_census$Mammo_units)
sum(ncc_census$BICOE)
sum(ncc_census$BICOE_units)

describe(ncc_census$census_NDI)
describe(ncc_census$women_40_49)
describe(ncc_census$women_50_74)
describe(ncc_census$women_75_plus)
describe(ncc_census$women_black_percent)
describe(ncc_census$percent_vehicle)
describe(ncc_census$busstops)
describe(ncc_census$Mammo)
describe(ncc_census$Mammo_units)
describe(ncc_census$BICOE)
describe(ncc_census$BICOE_units)

#outside NCC
kentsussex_census = de_census[which(!(de_census$Tract %in% ncc_ct$GEOID)), ]

sum(kentsussex_census$census_population)
sum(kentsussex_census$Mammo)
sum(kentsussex_census$Mammo_units)
sum(kentsussex_census$BICOE)
sum(kentsussex_census$BICOE_units)

describe(kentsussex_census$census_NDI)
describe(kentsussex_census$women_40_49)
describe(kentsussex_census$women_50_74)
describe(kentsussex_census$women_75_plus)
describe(kentsussex_census$women_black_percent)
describe(kentsussex_census$percent_vehicle)
describe(kentsussex_census$busstops)
describe(kentsussex_census$Mammo)
describe(kentsussex_census$Mammo_units)
describe(kentsussex_census$BICOE)
describe(kentsussex_census$BICOE_units)


### ECOLOGICAL MODELS ###

#remove unpopulated tracts
de_census = de_census[de_census$census_population>0, ]

#dichotomous indicators
de_census$Mammo_YN = ifelse(de_census$Mammo>0, 1, 0)
de_census$BICOE_YN = ifelse(de_census$BICOE>0, 1, 0)

#quasibinomial glm for robust errors, weighted by population
#mammo_glm = glm(Mammo_YN ~ census_NDI + women_40_49 + women_50_74 + women_75_plus + women_black_percent + AlcOffsite, weights=log(census_population), data=de_census, family=quasibinomial())
#bicoe_glm = glm(BICOE_YN ~ census_NDI + women_40_49 + women_50_74 + women_75_plus + women_black_percent + AlcOffsite, weights=log(census_population), data=de_census, family=quasibinomial())

#summary(mammo_glm)
#summary(bicoe_glm)

#number of sites
mammo_sites = glm(Mammo ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=de_census, family=poisson())
bicoe_sites = glm(BICOE ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=de_census, family=poisson())

summary(mammo_sites)
summary(bicoe_sites)

round(exp(coef(mammo_sites)),2)
round(exp(coef(bicoe_sites)),2)
round(exp(confint(mammo_sites)),2)
round(exp(confint(bicoe_sites)),2)

#number of units
mammo_units = glm(Mammo_units ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=de_census, family=poisson())
bicoe_units = glm(BICOE_units ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=de_census, family=poisson())

summary(mammo_units)
summary(bicoe_units)

round(exp(coef(mammo_units)),2)
round(exp(coef(bicoe_units)),2)
round(exp(confint(mammo_units)),2)
round(exp(confint(bicoe_units)),2)

#NCC only
ncc_census = de_census[which(de_census$Tract %in% ncc_ct$GEOID), ]

#number of sites
mammo_sites = glm(Mammo ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=ncc_census, family=poisson())
bicoe_sites = glm(BICOE ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=ncc_census, family=poisson())

summary(mammo_sites)
summary(bicoe_sites)

round(exp(coef(mammo_sites)),2)
round(exp(coef(bicoe_sites)),2)
round(exp(confint(mammo_sites)),2)
round(exp(confint(bicoe_sites)),2)

#number of units
mammo_units = glm(Mammo_units ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=ncc_census, family=poisson())
bicoe_units = glm(BICOE_units ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=ncc_census, family=poisson())

summary(mammo_units)
summary(bicoe_units)

round(exp(coef(mammo_units)),2)
round(exp(coef(bicoe_units)),2)
round(exp(confint(mammo_units)),2)
round(exp(confint(bicoe_units)),2)

#outside NCC
kentsussex_census = de_census[which(!(de_census$Tract %in% ncc_ct$GEOID)), ]

#number of sites
mammo_sites = glm(Mammo ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=kentsussex_census, family=poisson())
#bicoe_sites = zeroinfl(BICOE ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(AlcOffsite) + scale(busstops), offset=log(census_population), data=kentsussex_census, family="poisson")
#bicoe_sites = zeroinfl(BICOE ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(AlcOffsite) + scale(busstops), offset=log(census_population), data=kentsussex_census, family="negbin")

summary(mammo_sites)
summary(bicoe_sites)

round(exp(coef(mammo_sites)),2)
round(exp(coef(bicoe_sites)),2)
round(exp(confint(mammo_sites)),2)
round(exp(confint(bicoe_sites)),2)

#number of units
mammo_units = glm(Mammo_units ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(percent_vehicle) + scale(busstops), offset=log(census_population), data=kentsussex_census, family=poisson())
#bicoe_units = zeroinfl(BICOE_units ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(AlcOffsite) + scale(busstops), offset=log(census_population), data=kentsussex_census, family="poisson")
#bicoe_units = zeroinfl(BICOE_units ~ census_NDI + scale(women_40_49) + scale(women_50_74) + scale(women_75_plus) + scale(women_black_percent) + scale(AlcOffsite) + scale(busstops), offset=log(census_population), data=kentsussex_census, family="negbin")

summary(mammo_units)
summary(bicoe_units)

round(exp(coef(mammo_units)),2)
round(exp(coef(bicoe_units)),2)
round(exp(confint(mammo_units)),2)
round(exp(confint(bicoe_units)),2)

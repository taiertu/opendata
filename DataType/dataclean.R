library(jsonlite)
library(dplyr)
library(googleVis)

######################################
#### json to dataframe processing ####

opendata <- fromJSON("./OpenData/final/arcgis_flatfile.json")

####### 
#######inspect the data
opendata$results$createdAt[[1]]
#[1] "2015-08-18T01:55:30.743Z"
opendata$results$org_hq_country[[1]]
#[1] "Australia"
opendata$results$org_hq_country_income_code[[300]]
#[1] "HIC"

length(opendata$results$org_hq_country_income_code)
#[1] 4184

table(opendata$results$org_hq_country_income_code)
#HIC  LIC  LMC  UMC 
#2094   52  170  656 

sum(table(opendata$results$org_hq_country_income_code))
#[1] 2972
## 1212 records are NAs.

# take a look at which countries are missing income code
for (i in 1:length(opendata$results$org_hq_country_income_code)){
  if(is.na(opendata$results$org_hq_country_income_code[[i]])){
    print(i)
    print(opendata$results$org_hq_country[[i]])
  } 
}

table(opendata$results$org_type)
#Developer group      For-profit       Nonprofit           Other 
#           300            2709             953             221  

table(opendata$results$org_type_other)
sum(table(opendata$results$org_type_other))

table(opendata$results$org_greatest_impact)
#Economic 1280, Environmental 421, Governance 850, Other 189, Social 1441, NULL (2)

table(opendata$results$org_size_id)
# 1-10    1000+    11-50 201-1000   51-200 
#1330      347      832       98      274 

####### 
####### convert to dataframe (individual record as row)
org_name = c()
industry_id = c()
latitude = c()
longitude = c()
org_greatest_impact = c()
org_hq_city = c()
org_hq_country = c()
org_hq_country_code = c()
org_type = c()
org_size_id = c()
org_year_founded = c()
data_type = c()
profileid = c()
objectid = c()
for (i in 1:length(opendata$results$org_name)){
  org_name = c(org_name, opendata$result$org_name[[i]]) 
  industry_id = c(industry_id, opendata$results$industry_id[[i]])
  latitude = c(latitude, opendata$results$latitude[[i]])
  longitude = c(longitude, opendata$results$longitude[[i]])
  org_greatest_impact = c(org_greatest_impact, opendata$results$org_greatest_impact[[i]])
  org_hq_city = c(org_hq_city, opendata$results$org_hq_city[[i]])
  org_hq_country = c(org_hq_country, opendata$results$org_hq_country[[i]])
  org_hq_country_code = c(org_hq_country_code, opendata$results$org_hq_country_locode[[i]])
  org_type = c(org_type, opendata$results$org_type[[i]])
  org_size_id = c(org_size_id, opendata$results$org_size_id[[i]])
  org_year_founded = c(org_year_founded, opendata$results$org_year_founded[[i]])
  data_type = c(data_type, opendata$results$data_type[[i]])
  profileid = c(profileid, opendata$results$profile_id[[i]])
  objectid = c(objectid, opendata$results$objectId[[i]])
}

od.df = data.frame(org_name,industry_id,latitude,longitude,
                   org_type, org_size_id, org_year_founded, org_greatest_impact, data_type, 
                   org_hq_city, org_hq_country, org_hq_country_code, objectid, stringsAsFactors=FALSE)

# Clean the data: AU --> Australia, México --> Mexico, Hong Kong --> Hong Kong SAR, China
# 
table(od.df$org_hq_country)
od.df$org_hq_country[od.df$org_hq_country == "AU"] <- "Australia"
od.df$org_hq_country[od.df$org_hq_country == "México"] <- "Mexico"
od.df$org_hq_country[od.df$org_hq_country == "Hong Kong"] <- "Hong Kong SAR, China"
table(od.df$org_hq_country)
od.df$org_hq_country2 <- od.df$org_hq_country

#### Convert some countries in od.df to align with the names in countrygroup
od.df$org_hq_country2[od.df$org_hq_country2 == "Egypt"] <- "Egypt, Arab Rep."
od.df$org_hq_country2[od.df$org_hq_country2 == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
od.df$org_hq_country2[od.df$org_hq_country2 == "Venezuela"] <- "Venezuela, RB"

table(od.df$org_hq_country2)

##### save to Rds and load country income group data from world bank
save(od.df, file = "./OpenData/final/od.df.Rds")
load("/Users/yichun/OpenData/final/countrygroup.Rds")

which(is.na(countrygroup$Country))
#integer(0)

##### Merge open data frame(od.df) with income group data

od.df_ctrygroup = merge(x = od.df, y = countrygroup, by.x = "org_hq_country2", by.y = "Country", all.x = TRUE)
save(od.df_ctrygroup, file = "./OpenData/final/od.df_ctrygroup.Rds")
length(which(is.na(od.df_ctrygroup$Income_Code)))
#[1] 32

## Examine the number of open data use cases across countries and country groups
table(od.df_ctrygroup$Income_Code) 
#HIC  LIC  LMC  UMC 
#2853  34  231  1034  

## remove the duplicated records
od.final = unique(od.df_ctrygroup)
nrow(od.final)
#[1] 4184

save(od.final, file = "./OpenData/final/od.final.Rds")

################## 
#####Examine ODB rankings
ODB_2014_Rankings <- read.csv("~/OpenData/final/ODB_2014_Rankings.csv")
summary(ODB_2014_Rankings$ODB.Rank)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   22.00   41.00   42.86   64.00   86.00 
summary(ODB_2014_Rankings$ODB.Score)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.42710 -0.24750 -0.03953  0.01311  0.21790  0.83400
hist(ODB_2014_Rankings$ODB.Score)
#needs to do transformation
summary(ODB_2014_Rankings$Readiness)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.64700 -0.69240 -0.19510 -0.07621  0.53440  1.44000 


# merge opendata and odb tables
load("/Users/yichun/OpenData/final/od.final.Rds")
nrow(od.final)
length(which(is.na(od.final$Code)))
#[1] 32
od.odb = merge(x = od.final, y = ODB_2014_Rankings, by.x = "Code", by.y = "ISO3", all.x = TRUE)
save(od.odb, file = "./OpenData/final/od.odb.Rds")
load("/Users/yichun/OpenData/final/od.odb.Rds")

# examine the association b/w the number of cases and the ODB ranking across countries
length(which(is.na(od.odb$Code)))
#[1] 32
which(is.na(od.odb$org_hq_country))
#integer(0)

#####################
###### Create a dataframe based on country level
cty_cases =arrange(summarise(group_by(od.odb, org_hq_country2), count = n()), desc(count))
cty_cases = merge(x = cty_cases, y = countrygroup, by.x = "org_hq_country2", by.y = "Country", all.x  = TRUE)
View(cty_cases)
save(cty_cases, file = "./OpenData/final/cty_cases.Rds")

###### visualization
geo = gvisGeoChart(cty_cases, locationvar = "org_hq_country2", colorvar = "count", options = list(projection="kavrayskiy-vii", colorAxis="{colors:['#f1cddb','#ba064e','#a70546']}"))
plot(geo)


###### examine open data
table(od.final$Income_Code)
#HIC  LIC  LMC  UMC 
#2853   34  231 1034  

prop.table(table(od.final$Income_Code))
#HIC         LIC         LMC         UMC 
#0.687138728 0.008188825 0.055635838 0.249036609 

table(od.final$org_type)
#Developer group      For-profit       Nonprofit           Other 
#           300            2709             953             221 

org.type.income.table = with(od.final, table(org_type, Income_Code))
org.type.income.table
#org_type           HIC  LIC  LMC  UMC
#Developer group  186    7   45    54
#For-profit      2017    0   86    589
#Nonprofit        500   27   84    335
#Other            150    0   15    56

chisq.test(org.type.income.table, simulate.p.value = TRUE)
#	Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
#data:  org.type.income.table
#X-squared = 285.78, df = NA, p-value = 0.0004998

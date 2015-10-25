library(jsonlite)
library(dplyr)
library(googleVis)

######################################
#### json to dataframe processing ####

orgdata <- fromJSON("./OpenData/final/org_profile.json")

####### 
#######inspect the data
orgdata$results$createdAt[[1]]
#[1] "2015-05-28T03:13:40.809Z"
orgdata$results$org_hq_country[[1]]
#[1] "France"
orgdata$results$org_hq_country_income_code[[300]]
#[1] "UMC"

length(orgdata$results$org_hq_country_income_code)
#[1] 1259

table(orgdata$results$org_hq_country_income_code)
#HIC  LIC  LMC  UMC 
#623   19  65   139

sum(table(orgdata$results$org_hq_country_income_code))
#[1] 846
# 413 records are missing

table(orgdata$results$org_type)
#Developer group      For-profit       Nonprofit           Other 
#            121             771             300              66 

table(orgdata$results$org_greatest_impact)
#Economic 338, Environmental 132, Governance 302, Other 50, Social 434, NULL (2)

table(orgdata$results$org_size_id)
# 1-10    1000+    11-50 201-1000   51-200 
# 289      101      222       41      92

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
profileid = c()
objectid = c()
for (i in 1:length(orgdata$results$org_name)){
  org_name = c(org_name, orgdata$result$org_name[[i]]) 
  industry_id = c(industry_id, orgdata$results$industry_id[[i]])
  latitude = c(latitude, orgdata$results$latitude[[i]])
  longitude = c(longitude, orgdata$results$longitude[[i]])
  org_greatest_impact = c(org_greatest_impact, orgdata$results$org_greatest_impact[[i]])
  org_hq_city = c(org_hq_city, orgdata$results$org_hq_city[[i]])
  org_hq_country = c(org_hq_country, orgdata$results$org_hq_country[[i]])
  org_hq_country_code = c(org_hq_country_code, orgdata$results$org_hq_country_locode[[i]])
  org_type = c(org_type, orgdata$results$org_type[[i]])
  org_size_id = c(org_size_id, orgdata$results$org_size_id[[i]])
  org_year_founded = c(org_year_founded, orgdata$results$org_year_founded[[i]])
  profileid = c(profileid, orgdata$results$profile_id[[i]])
  objectid = c(objectid, orgdata$results$objectId[[i]])
}

org.df = data.frame(org_name,industry_id,latitude,longitude,
                   org_type, org_size_id, org_year_founded, org_greatest_impact,  
                   org_hq_city, org_hq_country, org_hq_country_code, objectid, stringsAsFactors=FALSE)

table(org.df$org_hq_country)

# Clean the data: AU --> Australia, México --> Mexico, Hong Kong --> Hong Kong SAR, China
# 
table(org.df$org_hq_country)
org.df$org_hq_country[org.df$org_hq_country == "AU"] <- "Australia"
org.df$org_hq_country[org.df$org_hq_country == "México"] <- "Mexico"
org.df$org_hq_country[org.df$org_hq_country == "Hong Kong"] <- "Hong Kong SAR, China"
table(org.df$org_hq_country)
org.df$org_hq_country2 <- org.df$org_hq_country

#### Convert some countries in od.df to align with the names in countrygroup
org.df$org_hq_country2[org.df$org_hq_country2 == "Egypt"] <- "Egypt, Arab Rep."
org.df$org_hq_country2[org.df$org_hq_country2 == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
org.df$org_hq_country2[org.df$org_hq_country2 == "Venezuela"] <- "Venezuela, RB"

table(org.df$org_hq_country2)

##### save to Rds and load country income group data from world bank
save(org.df, file = "./OpenData/final/org.df.Rds")
load("/Users/yichun/OpenData/final/countrygroup.Rds")

which(is.na(countrygroup$Country))
#integer(0)

##### Merge open data frame(od.df) with income group data

org.df_ctrygroup = merge(x = org.df, y = countrygroup, by.x = "org_hq_country2", by.y = "Country", all.x = TRUE)
save(org.df_ctrygroup, file = "./OpenData/final/org.df_ctrygroup.Rds")
length(which(is.na(org.df_ctrygroup$Income_Code)))
#[1] 20

## Examine the number of open data use cases across countries and country groups
table(org.df_ctrygroup$Income_Code) 
#HIC  LIC  LMC  UMC 
#944  16    89  190   

## remove the duplicated records
org.final = unique(org.df_ctrygroup)
nrow(org.final)
#[1] 1259

save(org.final, file = "./OpenData/final/org.final.Rds")

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
load("/Users/yichun/OpenData/final/org.final.Rds")
nrow(org.final)
length(which(is.na(org.final$Code)))
#[1] 20
org.odb = merge(x = org.final, y = ODB_2014_Rankings, by.x = "Code", by.y = "ISO3", all.x = TRUE)
save(org.odb, file = "./OpenData/final/org.odb.Rds")
load("/Users/yichun/OpenData/final/org.odb.Rds")

# examine the association b/w the number of cases and the ODB ranking across countries
length(which(is.na(org.odb$Code)))
#[1] 20
which(is.na(org.odb$org_hq_country))
#integer(0)


#####################
###### Create a dataframe based on country level
cty_org =arrange(summarise(group_by(org.odb, org_hq_country2), count = n()), desc(count))
cty_org = merge(x = cty_org, y = countrygroup, by.x = "org_hq_country2", by.y = "Country", all.x  = TRUE)
View(cty_org)
save(cty_org, file = "./OpenData/final/cty_org.Rds")


###### visualization
geo = gvisGeoChart(cty_org, locationvar = "org_hq_country2", colorvar = "count", options = list(projection="kavrayskiy-vii", colorAxis="{colors:['#f1cddb','#ba064e','#a70546']}"))
plot(geo)

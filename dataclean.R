library(jsonlite)
library(dplyr)

######################################
#### json to dataframe processing ####

opendata <- fromJSON("./OpenData/arcgis_flatfile (26AUG15).json")

####### 
#######inspect the data
opendata$results$createdAt[[1]]
#[1] "2015-08-18T01:55:30.743Z"
opendata$results$org_hq_country[[1]]
#[1] "Australia"
opendata$results$org_hq_country_income_code[[300]]
#[1] "NA"

length(opendata$results$org_hq_country_income_code)
#[1] 3464

table(opendata$results$org_hq_country_income_code)
#HIC  LIC  LMC  UMC 
#2013   52  167  614 

sum(table(opendata$results$org_hq_country_income_code))
#[1] 2846
## 618 records are NAs.

# take a look at which countries are missing income code
for (i in 1:length(opendata$results$org_hq_country_income_code)){
  if(is.na(opendata$results$org_hq_country_income_code[[i]])){
    print(i)
    print(opendata$results$org_hq_country[[i]])
  } 
}

table(opendata$results$org_type)
#Developer group      For-profit       Nonprofit           Other 
#           279            2235             762             178  

table(opendata$results$org_greatest_impact)
#Economic 1045, Environmental 349, Governance 758, Other 152, Social 1134

table(opendata$results$org_size_id)
# 1-10    1000+    11-50 201-1000   51-200 
#1103      220      689       87      212 

####### 
####### convert to dataframe (individual record as row)
remove(i)
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
  data_type = c(data_type, opendata$results$data_type[[i]])
  profileid = c(profileid, opendata$results$profile_id[[i]])
  objectid = c(objectid, opendata$results$objectId[[i]])
}

od.df = data.frame(org_name,industry_id,latitude,longitude,
                   org_type, org_size_id, org_greatest_impact, data_type, 
                   org_hq_city, org_hq_country, org_hq_country_code, objectid)

# Clean the data: country name AU stand for Australia
od.df$org_hq_country[od.df$org_hq_country == "AU"] <- "Australia"
summary(od.df$org_hq_country)

##### save to Rds and load country income group data from world bank
save(od.df, file = "./OpenData/od.df.Rds")
load("/Users/yichun/OpenData/countrygroup.Rds")

which(is.na(countrygroup$Country))
#integer(0)

##### Merge open data frame(od.df) with income group data
od.df_ctrygroup = merge(x = od.df, y = countrygroup, by.x = "org_hq_country", by.y = "Country", all.x = TRUE)
save(od.df_ctrygroup, file = "./OpenData/od.df_ctrygroup.Rds")

## Examine the number of open data use cases across countries and country groups
table(od.df_ctrygroup$Income_Code) 
#HIC  LIC  LMC  UMC 
#2645   19  205  570  
which(is.na(od.df_ctrygroup$Income_Code))

## remove the duplicated records
od.final = unique(od.df_ctrygroup)
nrow(od.final)
#[1] 3464

save(od.final, file = "./OpenData/od.final.Rds")

################## 
#####Examine ODB rankings
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
load("/Users/yichun/OpenData/od.final.Rds")
nrow(od.final)
which(is.na(od.final$Code))
#[1]    1    2    3    4    5    6    7    8    9   10   11   12   13   14  709 2957 2958 2959
od.odb = merge(x = od.final, y = ODB_2014_Rankings, by.x = "Code", by.y = "ISO3", all.x = TRUE)
save(od.odb, file = "./OpenData/od.odb.Rds")
load("/Users/yichun/OpenData/od.odb.Rds")


# examine the association b/w the number of cases and the ODB ranking across countries
which(is.na(od.odb$Code))
#[1] 2300 2301 2302 2303 2304 2305 2306
which(is.na(od.odb$org_hq_country))
#integer(0)

#####################
###### Create a dataframe based on country level
cty_cases =arrange(summarise(group_by(od.odb, org_hq_country), count = n()), desc(count))
cty_cases = merge(x = cty_cases, y = countrygroup, by.x = "org_hq_country", by.y = "Country", all.x  = TRUE)
View(cty_cases)
save(cty_cases, file = "./OpenData/cty_cases.Rds")

###### visualization
geo = gvisGeoChart(cty_cases, locationvar = "org_hq_country", colorvar = "count", options = list(projection="kavrayskiy-vii", colorAxis="{colors:['#f1cddb','#ba064e','#a70546']}"))
plot(geo)


###### examine open data
table(od.final$Income_Code)
#HIC  LIC  LMC  UMC 
#2645   19  205  570  

prop.table(table(od.final$Income_Code))
#HIC         LIC         LMC         UMC 
#0.769118930 0.005524862 0.059610352 0.165745856 

table(od.final$org_type)
#Developer group      For-profit       Nonprofit           Other 
#           279            2235             762             178 

org.type.income.table = with(od.final, table(org_type, Income_Code))
org.type.income.table
#org_type           HIC  LIC  LMC  UMC
#Developer group  186    6   45   39
#For-profit      1852    0   74  296
#Nonprofit        460   13   77  203
#Other            138    0    8   32

chisq.test(org.type.income.table, simulate.p.value = TRUE)
#	Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
#data:  org.type.income.table
#X-squared = 248.14, df = NA, p-value = 0.0004998

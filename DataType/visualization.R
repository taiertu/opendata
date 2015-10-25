install.packages("viridis") # dependency
install.packages("devtools")
devtools::install_github("ropensci/plotly")

library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(googleVis)

load("./OpenData/final/od.final.Rds")
load("./OpenData/final/od.odb.Rds")
#places <- read.csv("~/OpenData/opendataindex_data_2015-01-12 2/places.csv")

Sys.setenv("plotly_username"="taiertu")
Sys.setenv("plotly_api_key"="xab4cngnaz")

data002 = od.odb[,which(names(od.odb) %in% c("Code", "org_hq_country","Readiness","Impact","Income_Code","Region.x", "ISO2"))]
data003 = data002 %>%
  group_by(Code,org_hq_country,Readiness,Impact,Income_Code, Region.x, ISO2) %>%
  summarise(cases =length(Code))
data003$logcases = log(data003$cases+1)

#data003$ISO2 = tolower(as.character(data003$ISO2))
#places$id = as.character(places$id)

#data004 = merge(x=data003, y = places, by.x = "ISO2", by.y = "id", all.x = TRUE)
  
#data002.s = od.odb[,which(names(od.odb) %in% c("Code", "org_hq_country","Readiness.Scaled","Impact.Scaled","Income_Code"))]
#data003.s = data002.s %>%
#  group_by(Code,org_hq_country,Readiness.Scaled,Impact.Scaled,Income_Code) %>%
#  summarise(cases =length(Code))
#data003.s$logcases = log(data003$cases+1)

#scatter.plotly <- plot_ly(data003.s, x = Readiness.Scaled, y = Impact.Scaled, text = paste(paste("Country:", data003.s$org_hq_country), paste("Cases:", data003.s$cases)),
#                          mode = "markers", color = Income_Code, size = cases)

################
#### ggplot2 chart showing the relationship between ODB readiness, ODB impact, income groups, and number of cases.
#### can be carried on ployly server.
scatter.ggplot <- ggplot(data003, aes(x=Readiness, 
                                      y=Impact, 
                                      colour=Income_Code, 
                                      size=cases, 
                                      text= paste(paste("Country:", data003$org_hq_country), paste("Cases:", data003$cases)))) + 
  geom_point(aes(size = cases), alpha=0.7) + 
  scale_size_continuous(range = c(3, 20)) + 
  theme_bw() + scale_fill_few()

(gg <- ggplotly(scatter.ggplot))


###################
#### plotly plot showing the relationship between ODB readiness, ODB impact, income groups, and number of cases.
data004 = data003
data004$Income_Code <- factor(data004$Income_Code, levels = c("HIC", "UMC", "LMC", "LIC"))
levels(data004$Income_Code)[levels(data004$Income_Code)=="HIC"] <- "High-income countries"
levels(data004$Income_Code)[levels(data004$Income_Code)=="UMC"] <- "Upper-middle-income countries"
levels(data004$Income_Code)[levels(data004$Income_Code)=="LMC"] <- "Lower-middle-income countries"
levels(data004$Income_Code)[levels(data004$Income_Code)=="LIC"] <- "Low-income countries"

scatter.plotly.1 <- plot_ly(data004, x = Readiness, y = Impact, text = paste0("Country:", data004$org_hq_country),
                          mode = "markers", color = Income_Code, size = cases) %>% 
                    layout(title = "Number of open data cases against Open Data Barometer Indexes by income groups")

scatter.plotly.2 <- plot_ly(data003, x = Readiness, y = Impact, text = paste0("Country:", data003$org_hq_country),
                            mode = "markers", color = Region.x, size = cases) %>%
                    layout(title = "Number of open data cases against Open Data Barometer Indexes by regions")


################
##### online map showing the number of cases
load("/Users/yichun/OpenData/final/cty_cases.Rds")
View(cty_cases)

country.map = merge(x = countrygroup, y = cty_cases, by.x = "Country", by.y = "org_hq_country2", all.x = TRUE)
country.map$count[is.na(country.map$count)] <- 0

l <- list(color = toRGB("grey"), width = 0.2)
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'kavrayskiy-vii')
)

map.plotly = plot_ly(country.map, z = count, 
          text = paste0(paste0("\nCountry: ", country.map$Country), paste0("\nCases: ", country.map$count), paste0("\nIncome Group: ", country.map$Income_Code.x)), 
           locations = Code.x, type = 'choropleth', color = count, 
          colorscale= list(list(0,"#F7FBFF"), list(0.1,"#5AA1CF"), list(0.4,"#3987C0"), list(0.6,"#1D6AAF"), list(0.8,"#084D96"), list(1,"#08306B")), marker = list(line = l),
           colorbar = list(title = 'Mapping of open data use cases'), 
           filename="r-docs/world-choropleth") %>%
  layout(title = 'Mapping of open data use cases', geo = g)

################
#### Bar plot showing organization types by income groups
od.final$Income_Code <- factor(od.final$Income_Code, levels = c("HIC", "UMC", "LMC", "LIC"))
od.final2 <- od.final
od.final2 <- od.final2[!(is.na(od.final2$org_type)),]
od.final2 <- od.final2[!(is.na(od.final2$Income_Code)),]
levels(od.final2$Income_Code)[levels(od.final2$Income_Code)=="HIC"] <- "High income"
levels(od.final2$Income_Code)[levels(od.final2$Income_Code)=="UMC"] <- "Upper middle income"
levels(od.final2$Income_Code)[levels(od.final2$Income_Code)=="LMC"] <- "Lower middle income"
levels(od.final2$Income_Code)[levels(od.final2$Income_Code)=="LIC"] <- "Low income"
od.final2$org_type <- factor(od.final2$org_type, levels = c("Other", "Developer group", "Nonprofit", "For-profit"))
od.final2$Income_Code <- factor(od.final2$Income_Code, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))

p1.x = list(title = "Income Groups")
p1.y = list(title = "Case Counts")

p1 = od.final2 %>% count(Income_Code, org_type) %>% 
  plot_ly(x = Income_Code, y = n, type = "bar", 
          color = org_type, 
          colors = c("#FFDF00","#B1D91B","#187A5C", "#003347"), 
          filename="r-docs/bar-color-map") %>%
          layout(title = "Number of open data cases by organization types across income groups", xaxis = p1.x, yaxis = p1.y)

data.t = od.final2 %>% group_by(Income_Code, org_type) %>%
  summarise(total.type = length(Income_Code)) 
View(data.t)

data.t2 = data.t %>% summarise(total = sum(total.type))

data.t = merge(x = data.t, y = data.t2, by = "Income_Code")
data.t$perc = data.t$total.type / data.t$total

data.t$Income_Code <- factor(data.t$Income_Code, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))
data.t <- data.t[order(data.t$Income_Code),]

perc.p1.x = list(title = "Income Groups")
perc.p1.y = list(title = "Percentage")

perc.p1 = data.t %>% plot_ly(x = Income_Code, y = perc, type = "bar",
          color = org_type, 
          colors = c("#FFDF00","#B1D91B","#187A5C", "#003347"),
          filename="r-docs/bar-color-map") %>%
          layout(barmode = "stack", barnorm = "percent", title = "The share of organization types across income groups", xaxis = perc.p1.x, yaxis = perc.p1.y)

###############
#### Bar plot showing organization greatest impact by income groups
#replace the white space in org_greatest_impact to "Other"
od.final2$org_greatest_impact[od.final2$org_greatest_impact== ""] <- "Other"
od.final2$org_greatest_impact <- factor(od.final2$org_greatest_impact, levels = c("Other", "Environmental", "Governance", "Economic", "Social"))

p2.x = list(title = "Income Groups")
p2.y = list(title = "Case Counts")

p2 = od.final2 %>% count(Income_Code, org_greatest_impact) %>% 
  plot_ly(x = Income_Code, y = n, type = "bar", 
          color = org_greatest_impact, 
          colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"), 
          filename="r-docs/bar-color-map") %>%
          layout(title = "Number of open data cases by organization impacts across income groups", xaxis = p2.x, yaxis = p2.y)

###############
##### the number of data type 
table(od.final2$data_type)

datatype.df = od.final2 %>% group_by(data_type) %>% summarise(total = length(data_type))
datatype.df = datatype.df[!(is.na(datatype.df$data_type) | datatype.df$data_type == ""), ]
datatype.df = datatype.df[order(-datatype.df$total),]

p3.x = list(title = "Data Type")
p3.y = list(title = "Case Counts")

p3 = datatype.df %>% plot_ly(x = data_type, y = total, type = "bar", marker = list(color = toRGB("blue")),
          filename = "r-docs/bar-custom-colors") %>%
          layout(title = "Number of open data cases against open data types", xaxis = p3.x, yaxis = p3.y)

dataindustry = od.final2 %>% group_by(industry_id) %>% summarise(total = length(industry_id))
dataindustry = dataindustry[!(is.na(dataindustry$industry_id) | dataindustry$industry_id == ""), ]
dataindustry = dataindustry[order(-dataindustry$total),]

p4.x = list(title = "Industry ID")
p4.y = list(title = "Case Counts")

p4 = dataindustry %>% plot_ly(x = industry_id, y = total, type = "bar", marker = list(color = toRGB("orange")),
          filename = "r-docs/bar-custom-colors") %>%
  layout(title = "Number of open data cases across industry Id", xaxis = p4.x, yaxis = p4.y)


###############
###### Organization size relative to income groups, greatest impact, organization type
od.final2$org_size_id <- factor(od.final2$org_size_id, levels = c("1-10", "11-50", "51-200", "201-1000", "1000+"))
od.final3 <- od.final2[!(is.na(od.final2$org_size_id)),] 

p.size.x = list(title = "Organization Size")
p.size.y = list(title = "Case Counts")

p.size = od.final3 %>% count(org_size_id) %>%
  plot_ly(x = org_size_id, y = n, type = "bar", marker = list(color = "rgb(44, 160, 44)"),
          filename = "r-docs/bar-custom=colors") %>%
  layout(title = "Number of open data cases against organization size", xaxis = p.size.x, yaxis = p.size.y)

#p5 = od.final3 %>% count(Income_Code, org_size_id) %>% 
  #plot_ly(x = Income_Code, y = n, type = "bar", 
   #       color = org_size_id, 
    #      colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"), 
    #      filename="r-docs/bar-color-map") %>%
  #layout(title = "The size of organization against income groups")

data.orgsize = od.final3 %>% group_by(Income_Code, org_size_id) %>%
  summarise(total.type = length(Income_Code)) 
View(data.orgsize)

data.orgsize2 = data.orgsize %>% summarise(total = sum(total.type))

data.orgsize = merge(x = data.orgsize, y = data.orgsize2, by = "Income_Code")
data.orgsize$perc = with(data.orgsize, total.type / total)

data.orgsize$Income_Code <- factor(data.orgsize$Income_Code, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))
data.orgsize <- data.orgsize[order(data.orgsize$Income_Code),]

perc.p5.x = list(title = "Income Groups")
perc.p5.y = list(title = "Percentage")

perc.p5 = data.orgsize %>% plot_ly(x = Income_Code, y = perc, type = "bar",
                             color = org_size_id, 
                             colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"),
                             filename="r-docs/bar-color-map") %>%
  layout(barmode = "stack", barnorm = "percent", title = "The share of different organization sizes across income groups", xaxis = perc.p5.x, yaxis = perc.p5.y)

data.typesize = od.final3 %>% group_by(org_type, org_size_id) %>%
  summarise(total.type = length(org_type))
View(data.typesize)

data.typesize2 = data.typesize %>% summarise(total = sum(total.type))  

data.typesize = merge(x = data.typesize, y = data.typesize2, by = "org_type")
data.typesize$perc = with(data.typesize, total.type / total)

perc.p6.x = list(title = "Organization Type")
perc.p6.y = list(title = "Percentage")

perc.p6 = data.typesize %>% plot_ly(x = org_type, y = perc, type = "bar",
          color = org_size_id,
          colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"),
          filename = "r-docs/bar-color-map") %>%
  layout(barmode = "stack", barnorm = "percent", title = "The share of different organization sizes against organization types", xaxis = perc.p6.x, yaxis = perc.p6.y)

data.sizeimpact = od.final3 %>% group_by(org_size_id, org_greatest_impact) %>%
  summarise(total.type = length(org_size_id))
data.sizeimpact = data.sizeimpact[!(is.na(data.sizeimpact$org_greatest_impact)),]
View(data.sizeimpact)

data.sizeimpact2 = data.sizeimpact %>% summarise(total = sum(total.type))  

data.sizeimpact = merge(x = data.sizeimpact, y = data.sizeimpact2, by = "org_size_id")
data.sizeimpact$perc = with(data.sizeimpact, total.type / total)

perc.p7.x = list(title = "Organization Size")
perc.p7.y = list(title = "Percentage")

data.sizeimpact$org_size_id <- factor(data.sizeimpact$org_size_id, levels = c("1-10", "11-50", "51-200", "201-1000", "1000+"))
data.sizeimpact <- data.sizeimpact[order(data.sizeimpact$org_size_id),]

perc.p7 = data.sizeimpact %>%  plot_ly(x = org_size_id, y = perc, type = "bar",
          color = org_greatest_impact,
          colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"),
          filename = "r-docs/bar-color-map") %>%
  layout(barmode = "stack", barnorm = "percent", title = "The share of organization greatest impacts across different organization sizes", xaxis = perc.p7.x, yaxis = perc.p7.y)

#### year founded
table(od.final3$org_year_founded)
od.final3$year <- cut(od.final3$org_year_founded, c(0, 1960, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020), right = FALSE)

data.year.1 <- od.final3 %>% group_by(org_name, year, org_greatest_impact) %>% summarise(total.cases = length(org_name))
data.year.1 <- data.year.1[!(is.na(data.year.1$year) | (data.year.1$org_greatest_impact == "")),]
data.year.2 <- data.year.1 %>% group_by(year,org_greatest_impact) %>% summarise(total = length(org_greatest_impact))
data.year.2$org_greatest_impact <- factor(data.year.2$org_greatest_impact, levels = c("Other", "Environmental", "Governance", "Economic", "Social"))

#data.year.3 <- od.final3 %>% group_by(org_name, year, Income_Code) %>% summarise(total.cases = length(org_name))
#data.year.3 <- data.year.3[!(is.na(data.year.3$year)),]
#data.year.4 <- data.year.3 %>% group_by(year, Income_Code) %>% summarise(total = length(Income_Code))

#perc.impactyear = data.year.2 %>% plot_ly(x = year, y = total, type = "bar",
 #                                   color = org_greatest_impact,
  #                                  colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"),
   #                                 filename = "r-docs/bar-color-map") %>%
  #layout(barmode = "stack", barnorm = "percent", title = "Share of organization impacts by founded year", xaxis = list(title = "Year"), yaxis = list(title = "Percentage"))

impactyear = data.year.2 %>% plot_ly(x = year, y = total, type = "bar",
                                     color = org_greatest_impact,
                                     colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"),
                                     filename = "r-docs/bar-color-map") %>%
  layout(barmode = "stack", title = "Number of organizations founded by years and impacts", xaxis = list(title = "Year"), yaxis = list(title = "Number of Organizations Founded"))

###############
##### another online map showing the number of cases
load("/Users/yichun/OpenData/final/cty_cases.Rds")
View(cty_cases)
geo = gvisGeoChart(cty_cases, locationvar = "org_hq_country2", 
                   colorvar = "count",  
                   options = list(projection="kavrayskiy-vii", colorAxis="{colors:['#f1cddb','#ba064e','#a70546']}"))
plot(geo)

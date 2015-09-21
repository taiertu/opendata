install.packages("viridis") # dependency
install.packages("devtools")
devtools::install_github("ropensci/plotly")

library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(googleVis)

load("./OpenData/od.final.Rds")
load("./OpenData/od.odb.Rds")
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
data003$Income_Code <- factor(data003$Income_Code, levels = c("HIC", "UMC", "LMC", "LIC"))

scatter.plotly.1 <- plot_ly(data003, x = Readiness, y = Impact, text = paste0("Country:", data003$org_hq_country),
                          mode = "markers", color = Income_Code, size = cases) 

scatter.plotly.2 <- plot_ly(data003, x = Readiness, y = Impact, text = paste0("Country:", data003$org_hq_country),
                            mode = "markers", color = Region.x, size = cases)


################
##### online map showing the number of cases
l <- list(color = toRGB("grey"), width = 0.2)
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'kavrayskiy-vii')
)

map.plotly = plot_ly(data003, z = cases, 
          text = paste0(paste0("\nCountry:", data003$org_hq_country), paste0("\nCases:", data003$cases), paste0("\nIncome Group:", data003$Income_Code)), 
           locations = Code, type = 'choropleth', color = cases, colors = 'Blues', marker = list(line = l),
           colorbar = list(title = 'Total Number of Open Data Use Cases'), 
           filename="r-docs/world-choropleth") %>%
  layout(title = 'Total Number of Open Data Use Cases',
         geo = g)


################
#### Bar plot showing organization types by income groups
od.final$Income_Code <- factor(od.final$Income_Code, levels = c("HIC", "UMC", "LMC", "LIC"))
od.final$org_type <- factor(od.final$org_type, levels = c("Other", "Developer group", "Nonprofit", "For-profit"))

p1 = od.final %>% count(Income_Code, org_type) %>% 
  plot_ly(x = Income_Code, y = n, type = "bar", 
          color = org_type, 
          colors = c("#FFDF00","#B1D91B","#187A5C", "#003347"), 
          filename="r-docs/bar-color-map") %>%
          layout(title = "The Number of Open Data Cases by Organization Types across Income Groups")

###############
#### Bar plot showing organization greatest impact by income groups
#replace the white space in org_greatest_impact to "Other"
od.final$org_greatest_impact[od.final$org_greatest_impact== ""] <- "Other"
od.final$org_greatest_impact <- factor(od.final$org_greatest_impact, levels = c("Other", "Environmental", "Governance", "Economic", "Social"))

p2 = od.final %>% count(Income_Code, org_greatest_impact) %>% 
  plot_ly(x = Income_Code, y = n, type = "bar", 
          color = org_greatest_impact, 
          colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"), 
          filename="r-docs/bar-color-map") %>%
          layout(title = "The Number of Open Data Cases by Organization Impacts across Income Groups")

###############
##### the number of data type 
table(od.final$data_type)

p3 = od.final %>% count(data_type) %>%
  plot_ly(x = data_type, y = n, type = "bar", marker = list(color = toRGB("blue")),
          filename = "r-docs/bar-custom-colors") %>%
          layout(title = "The Number of Open Data Cases against Open Data Types")

p4 = od.final %>% count(industry_id) %>%
  plot_ly(x = industry_id, y = n, type = "bar", marker = list(color = toRGB("blue")),
          filename = "r-docs/bar-custom-colors") %>%
  layout(title = "The Number of Open Data Cases across Industry Id")


###############
###### Organization size relative to income groups, greatest impact, organization type
od.final$Income_Code <- factor(od.final$Income_Code, levels = c("HIC", "UMC", "LMC", "LIC"))
od.final$org_size_id <- factor(od.final$org_size_id, levels = c("1-10", "11-50", "51-200", "201-1000", "1000+"))

p.size = od.final %>% count(org_size_id) %>%
  plot_ly(x = org_size_id, y = n, type = "bar", marker = list(color = toRGB("orange")),
          filename = "r-docs/bar-custom=colors") %>%
  layout(title = "The Number of Open Data Cases against Organization Size")

p5 = od.final %>% count(Income_Code, org_size_id) %>% 
  plot_ly(x = Income_Code, y = n, type = "bar", 
          color = org_size_id, 
          colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"), 
          filename="r-docs/bar-color-map") %>%
  layout(title = "The Size of Organziations against Income Group")

p6 = od.final %>% count(org_type, org_size_id) %>% 
  plot_ly(x = org_type, y = n, type = "bar",
          color = org_size_id,
          colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"),
          filename = "r-docs/bar-color-map") %>%
  layout(title = "The Size of Organziations against Organization Type")

p7 = od.final %>% count(org_size_id, org_greatest_impact) %>% 
  plot_ly(x = org_size_id, y = n, type = "bar",
          color = org_greatest_impact,
          colors = c("#880A00", "#FD5E00","#FFDF00","#B1D91B","#187A5C", "#003347"),
          filename = "r-docs/bar-color-map") %>%
  layout(title = "The Organization Greatest Impact by Different Organization Sizes")
  

###############
##### another online map showing the number of cases
load("/Users/yichun/OpenData/cty_cases.Rds")
#View(cty_cases)
geo = gvisGeoChart(cty_cases, locationvar = "org_hq_country", 
                   colorvar = "count",  
                   options = list(projection="kavrayskiy-vii", colorAxis="{colors:['#f1cddb','#ba064e','#a70546']}"))
plot(geo)








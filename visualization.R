install.packages("viridis") # dependency
install.packages("devtools")
devtools::install_github("ropensci/plotly")

library(googleVis)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)

load("./OpenData/od.final.Rds")
load("./OpenData/od.odb.Rds")

data002 = od.odb[,which(names(od.odb) %in% c("Code", "org_hq_country","Readiness","Impact","Income_Code"))]
data003 = data002 %>%
  group_by(Code,org_hq_country,Readiness,Impact,Income_Code) %>%
  summarise(cases =length(Code))
data003$logcases = log(data003$cases+1)

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

Sys.setenv("plotly_username"="taiertu")
Sys.setenv("plotly_api_key"="xab4cngnaz")
(gg <- ggplotly(scatter.ggplot))


###################
#### plotly plot showing the relationship between ODB readiness, ODB impact, income groups, and number of cases.
scatter.plotly <- plot_ly(data003, x = Readiness, y = Impact, text = paste0("Country:", data003$org_hq_country),
        mode = "markers", color = Income_Code, size = cases)


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
#### Bar plot showing the income groups by organization types
p1 = od.final %>% count(Income_Code, org_type) %>% 
  plot_ly(x = Income_Code, y = n, type = "bar", 
          color = org_type, 
          colors = c("#003347", "#187A5C", "#B1D91B", "#FFDF00"), 
          filename="r-docs/bar-color-map")

###############
##### another online map showing the number of cases
load("/Users/yichun/OpenData/cty_cases.Rds")
View(cty_cases)
geo = gvisGeoChart(cty_cases, locationvar = "org_hq_country", 
                   colorvar = "count",  
                   options = list(projection="kavrayskiy-vii", colorAxis="{colors:['#f1cddb','#ba064e','#a70546']}"))
plot(geo)








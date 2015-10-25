# Open Data Enterprise (ODE) Project
External data sources:
<p>countrygroup.Rds contains country name, code, and income economies information. It was obtained from 
World Bank at http://data.worldbank.org/about/country-and-lending-groups</p>
<p> ODB_2014_Rankings.csv was obtained from Open Data Barometer organization via http://www.opendatabarometer.org/.</p>

Files in "OrganizationLevel" folder:
1. org_profile.json
<p>Raw datasets provided by ODE, stored in json file. org_profile keeps organization profile information, such as organization location, the size of organization, organization greatest impact, industrial type, etc. The total number of records is 1,259. </p>

2. Intermediate Datasets
</p> org.df.Rds is the dataset converted from org_profile.json file with data cleaning. The measurements we used to clean
data was addressed in org_dataclean.R.</p>
</p> org.df_ctrygroup.Rds was created by joining org.df.Rds with countrygroup.Rds. The dataset, therefore, contains the ODE and
income economies information.</p>
<p> org.final.Rds was built upon org.df_ctrygroup.Rds. We used "unique" function to ensure no duplicated records in the
dataset. org.final.Rds is the main dataset we used for the analysis and visualization.</p>
<p>org.odb.Rds was created by joining org.final and ODB_2014_Rankings.csv. By doing so we got a dataset that contains all information we need from ODE, Open Data Barometer, and income economies.</p>
<p> cty_org.Rds was created by grouping organization cases at the country level. It was mainly used to create map visualization.</p>

3. Scripts
<p> org_dataclean.R contains all scripts for cleaning data, creating datasets, and making explotary analysis. </p>
<p> org_visualization.R contains all scripts for making visualizations. The main tool used is plotly. It is an open source visualization

Files in "DataType" folder
1. arcgis_flatfile.json & org_profile.json
<p>Raw datasets provided by ODE, stored in json file. arcgis_flatfile includes the data use information (e.g. what type of 
data is used,from what country, etc.). The total number of record is 4184. </p>

2. Intermediate Datasets
</p> od.df.Rds is the dataset converted from arcgis_flatfile.json file with data cleaning. The measurements we used to clean
data was addressed in dataclean.R.</p>
</p> od.df_ctrygroup.Rds was created by joining od.df.Rds with countrygroup.Rds. The dataset, therefore, contains the ODE and
income economies information.</p>
<p> od.final.Rds was built upon od.df_ctrygroup.Rds. We used "unique" function to ensure there are no duplicated records in the
dataset. od.final.Rds is the main dataset we used for the analysis and visualization.</p>
<p>od.odb.Rds was created by joining od.final and ODB_2014_Rankings.csv. By doing so we got a dataset that contains all information
we need from ODE, Open Data Barometer, and income economies.</p>
<p> cty_cases.Rds was created by grouping data use cases at the country level. It was mainly used to create map visualization.</p>

3. Scripts
<p> dataclean.R contains all scripts for cleaning data, creating datasets, and making explotary analysis. </p>
<p> visualization.R contains all scripts for making visualizations. The main tool used is plotly. It is an open source visualization
platform but requires registration first. You can run the code using my API, which was listed in the scripts. </p>

<p>Though we have explored data and conducted analyses using both of the datasets provided by Open Data Enterprise, most of conclusions were drawn based on organization level. </p>

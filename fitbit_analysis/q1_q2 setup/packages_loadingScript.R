#############################
# Loading Script

## install all packages before running script
library (DBI)
library (odbc)

library(vctrs)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(ggthemes)
library(naniar)
library(janitor)
library(tibble)
library(plotly)
library(dygraphs)
library(rstatix)
library(gridExtra)
library(grid)
library(png)
library(gghighlight)
library(viridis)

# creates database entry point
con <- dbConnect(odbc::odbc(), dsn="Uzima")

# fetching tables from dbo schema
dbListTables(con, schema ="dbo")
dbListTables(con, schema = "DW")

###########################################

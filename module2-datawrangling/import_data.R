# 1.0 Load libraries ----

library(tidyverse)

# Excel Connection
library(readxl)
library(writexl)

# Database Connection
library(odbc)
library(RSQLite)

# 2.0 readr ----

## 2.1 CSV ----

folder_path <- fs::fs_path('data/bike_sales/data_wrangled/')

bike_order_csv_tbl <- read_csv(folder_path / 'bike_order_line.csv')

readr::problems(bike_order_csv_tbl) ## No problems no works


## 2.2 RDS  ----
##

bike_order_rds_tbl <- read_rds(folder_path / 'bike_order_line.rds')

bike_order_rds_tbl

# 3 EXCEL ----
##

bike_order_excel_tbl <- read_excel(folder_path / 'bike_order_line.xlsx')
excel_sheets(folder_path / 'bike_order_line.xlsx')

# 4 ODBC ----
#
#

con <- RSQLite::dbConnect(
  drv = SQLite(),
  dbname = 'data/chinook/Chinook_Sqlite.sqlite'
)
dbListTables(con)


album_tbl <- tbl(con, "Album") |>
  collect()

artist_tbl <- tbl(con, 'Artist') |>
  collect()

dbDisconnect(con)


con

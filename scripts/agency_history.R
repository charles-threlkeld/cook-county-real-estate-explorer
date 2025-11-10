library(data.table)
library(dplyr)
library(here)
library(ggplot2)
library(ptaxsim)

ptaxsim_db_conn <- DBI::dbConnect(
                            RSQLite::SQLite(),
                            here("data/ptaxsim.db")
                        )
pin = "13132310180000"
tax_code <- lookup_tax_code( 2006:2023, pin )


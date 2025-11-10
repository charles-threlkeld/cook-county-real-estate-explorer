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
taxes <- tax_bill(year_vec = 2006:2023,
                  pin_vec = c(pin))

#DBI::DBDisconnect(ptaxsim_db_conn)

ggplot(taxes, aes(x=year, y=final_tax, fill=agency_name)) +
    geom_area() +
    scale_fill_brewer("Agency", type = "qual", palette = "Set3") +
    labs(
        x = "Year",
        y = "$ Taxes",
        title = "Tax Rates Through the Years") +
    theme(legend.position = "bottom")

library(bslib)
library(here)
library(ptaxsim)
library(shiny)
library(sf)
library(tidyverse)

###############
## Constants ##
###############

## See https://prodassets.cookcountyassessoril.gov/s3fs-public/form_documents/Class_codes_definitions_12.16.24.pdf
residential_regression <- c(200, 201, 202, 203, 204, 205, 206, 207,
                            208, 209, 210, 211, 212, 234, 278, 295)
residential_nonregression <- c(200, 201, 213, 218, 219, 225)
multi_family <- c(313, 314, 315, 318, 391, 396, 399)

################
## Parameters ##
################

year = 2023

##################
## Data Sources ##
##################

## See the Assessor's Office documentation above
## This is a mapping of classes to descriptions
classes <- read_csv(here("data/class-descriptions.csv"))

## We grab the ward bounds from the City Data Portal
ward_bounds <- st_read("https://data.cityofchicago.org/resource/k9yb-bpqx.geojson")

## And finally, our database, also supplied by the CCAO
ptaxsim_db_conn <- DBI::dbConnect(
                            RSQLite::SQLite(),
                            here("data/ptaxsim.db")
                        )

############
## Script ##
############

ward_47_bound <-
    ward_bounds |>
    filter(ward == 47)

residential_pins <- DBI::dbGetQuery(
         ptaxsim_db_conn, str_glue("
SELECT p.year, p.pin, p.class, p.av_certified, p.tax_code_num, pg.longitude, pg.latitude
FROM pin p
INNER JOIN pin_geometry pg
ON substr(p.pin, 1, 10) = pg.pin10
AND p.year = pg.year
WHERE substr(p.tax_code_num, 1, 1) = '7'
AND p.year = {year}
")
) |>
    ## Map lat/lon to point
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    ## Include only residental buildings
    filter(class %in% residential_regression |  
           class %in% residential_nonregression |
           class %in% multi_family) |>
    ## Apply residential multiplier
    mutate(cash_value = av_certified * 10)

ward_47 <-
    residential_pins |>
    ## Subset to just properties in the ward
    filter(as.logical(st_within(geometry, ward_47_bound))) |>
    mutate(ward = 47)

ward_40_bound <-
    ward_bounds |>
    filter(ward == 40)

ward_40 <-
    residential_pins |>
    filter(as.logical(st_within(geometry, ward_40_bound))) |>
    mutate(ward = 40)

combined_df <-
    rows_append(ward_47, ward_40) |>
    st_drop_geometry() |>
    mutate_at("ward", as.factor) |>
    group_by(class, ward) |>
    summarize(count = n()) |>
    pivot_wider(names_from = ward,
                names_prefix = "ward ",
                values_from = count,
                values_fill = 0) |>
    merge(classes) |>
    arrange(desc(pick(starts_with("ward"))))

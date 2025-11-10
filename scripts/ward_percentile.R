library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(sf)
library(here)

###############
## Constants ##
###############
## See https://prodassets.cookcountyassessoril.gov/s3fs-public/form_documents/Class_codes_definitions_12.16.24.pdf
residential_regression <- c(200, 202, 203, 204, 205, 206, 207, 208,
                                    209, 210, 201, 212, 234, 278, 295)
residential_nonregression <- c(200, 201, 213, 218, 219, 225)
multi_family <- c(313, 314, 315, 318, 391, 396, 399)

######################
## Helper Functions ##
######################

## Given all Cook County, a ward number, and the geojson ward data
## return a subset of the property data pertaining only to that ward
get_ward <- function(all_pins, ward_num) {
    ward_bound <- st_read("https://data.cityofchicago.org/resource/k9yb-bpqx.geojson") |>
        filter(ward == as.character(ward_num))
    all_pins |>
        filter(as.logical(st_within(geometry, ward_bound)))
}

## Get a tibble of all property data in the given year
query_db <- function(year=2023) {
    ## Set up the DB connection
    ptaxsim_db_conn <- DBI::dbConnect(
                                RSQLite::SQLite(),
                                here("data/ptaxsim.db")
                            )

    all_pins <- DBI::dbGetQuery(
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
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  # Map lat/lon to point

    DBI::dbDisconnect(ptaxsim_db_conn)

    return(all_pins)
}

## Graph a density plot of the ward assessments
make_residential_plot <- function(pin_data) {
    pin_data |>
        filter(class %in% residential_regression |
               class %in% residential_nonregression |
               class %in% multi_family) |>
        filter(av_certified < quantile(av_certified, 0.99)[[1]]) |>
        ggplot(aes(av_certified * 10)) +
        geom_histogram() +
        geom_density(colour == "red") +
        scale_x_continuous(labels = label_currency(prefix="$")) +
        labs(
            x = "Certified Assessed Value",
            y = "Number of PINs"
        )
}

############
## Script ##
############

query_db() |>
    get_ward(47) |>
    make_residential_plot()

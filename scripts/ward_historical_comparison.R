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

## Get a tibble of all property data in the given year
query_db <- function() {
    ## Set up the DB connection
    ptaxsim_db_conn <- DBI::dbConnect(
                                RSQLite::SQLite(),
                                here("data/ptaxsim.db")
                            )

    all_pins <- DBI::dbGetQuery(
                         ptaxsim_db_conn, "
SELECT p.year, p.pin, p.class, p.av_certified, p.tax_code_num, pg.longitude, pg.latitude
FROM pin p
INNER JOIN pin_geometry pg
ON substr(p.pin, 1, 10) = pg.pin10
AND p.year = pg.year
WHERE substr(p.tax_code_num, 1, 1) = '7'
"
) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>  # Map lat/lon to point
    filter(class %in% residential_regression |
           class %in% residential_nonregression |
           class %in% multi_family) |>
    mutate(cash_value = av_certified * 10)

    DBI::dbDisconnect(ptaxsim_db_conn)

    return(all_pins)
}

## Given all Cook County, a ward number, and the geojson ward data
## return a subset of the property data pertaining only to that ward
get_ward <- function(all_pins, ward_num) {
    ## This could be improved by taking the pins (or pin10s) and geometry
    ## Then matching to ward and re-joining the data.
    ## This would take 0.05 as much time doing subset geometry
    ward_bound <- st_read("https://data.cityofchicago.org/resource/k9yb-bpqx.geojson") |>
        filter(ward == as.character(ward_num))
    all_pins |>
        filter(as.logical(st_within(geometry, ward_bound)))
}

## Graph a density plot of the ward assessments
make_ward_history_comparison_plot <- function(ward1_df, ward2_df,
                                              ward1, ward2) {
    bind_rows(list(ward_1 = ward1_df,
                   ward_2 = ward2_df),
              .id = "Ward") |>
        group_by(year, Ward) |>
        summarise(cash_value_sum = sum(cash_value),
                  .groups = "drop") |>
        ggplot() +
        geom_line(aes(x = year, y = cash_value_sum, color=Ward)) +
        scale_y_continuous(labels = label_currency(prefix="$")) +
        scale_color_manual(values = c("red", "blue"),
                           labels = c(ward1, ward2)) +
        labs(
            title = str_glue("Total value of residential property in Ward {ward1} and Ward {ward2}"),
            x = "Total Cash Value",
            y = "Year"
        )
}

############
## Script ##
############

ward1 = 47
ward2 = 46
year = 2023

pin_df <- query_db()

ward1_df <- get_ward(pin_df, ward1)
ward2_df <- get_ward(pin_df, ward2)

make_ward_history_comparison_plot(ward1_df, ward2_df, ward1, ward2)

library(bslib)
library(here)
library(ptaxsim)
library(shiny)
library(tidyverse)

###############
## Constants ##
###############

ptaxsim_db_conn <- DBI::dbConnect(
                            RSQLite::SQLite(),
                            here("data/ptaxsim.db")
                        )

######################
## Helper Functions ##
######################



#####################
## Shiny Functions ##
#####################

ui <- page_sidebar(
    title = "Chicago Real Estate Explorer",

    sidebar = sidebar(
        helpText("See where a certain PIN falls within a ward."),
        ## selectInput(
        ##     "wardNum",
        ##     label = "Choose a ward in the city",
        ##     choices = 1:50,
        ##     selected = 47),
        textInput("pin", label = "Input PIN", value = "13132310180000")
    ),

    navset_card_tab(
        nav_panel(
            card_header("Assessed Valuation History"),
            plotOutput("avHistory")
        ),
    
        nav_panel(
            card_header("Tax History of Property"),
            plotOutput("taxHistory")
        ),
        nav_panel(
            card_header("Taxing Agency History"),
            plotOutput("agencyHistory")
        ),
        nav_panel(
            card_header("Tax Rate History"),
            plotOutput("taxRateHistory")
        )
    )
)

server <- function(input, output) {

    # all_pins <- query_db()
    
    # ward_bounds <- st_read("https://data.cityofchicago.org/resource/k9yb-bpqx.geojson") |>

    ## ward_pins <- reactive({
    ##     filter(all_pins, ward == as.character(input$wardNum))
    ## all_pins |>
    ##     filter(as.logical(st_within(geometry, ward_bound)))
        
    ## })

    pin_history <- reactive({
        lookup_pin(2006:2023,
                   c(input$pin))
    })

    tax_code_vec <- reactive({
        lookup_tax_code(2006:2023,
                        input$pin)
    })

    agency_dt <- reactive({
        agency <- lookup_agency(2006:2023,
                      tax_code = tax_code_vec())
        baseline <- filter(agency, year == 2006)

        left_join(agency, baseline,
                  by = c("agency_name", "agency_major_type",
                         "agency_minor_type", "tax_code", "agency_num"),
                  suffix = c(".float", ".baseline")) |>
            mutate(fraction = agency_total_ext.float / agency_total_ext.baseline)
    })

    taxes <- reactive({
        tax_bill(year_vec = 2006:2023,
                 pin_vec = c(input$pin))
    })

    tax_rate_history <- reactive({
        ## mean of AV is a little hack to get around
        ## the limits of summarize.
        ## AV will be equal for the year groups,
        ## So this shouldn't be an issue.
        taxes() |>
            group_by(year) |>
            summarize(total_tax = sum(final_tax),
                      tax_rate = total_tax / mean(av * 10))
    })

    output$avHistory <- renderPlot({
        ggplot(pin_history(), aes(x = year, y = av * 10)) +
            geom_line() +
            scale_y_continuous(labels=scales::dollar_format()) +
            labs(
                x = "Year",
                y = "$ Valuation",
                title = "Fair Market Value by Year")
    })

    output$taxHistory <- renderPlot({
        ggplot(taxes(), aes(x=year, y=final_tax, fill=agency_name)) +
            geom_area() +
            scale_y_continuous(labels=scales::dollar_format()) +
            scale_fill_brewer("Agency", type="qual", palette = "Set3") +
            labs(
                x = "Year",
                y = "$ Taxes",
                title = "$ Tax Levied by Year") +
            theme(legend.position = "bottom")
    })

    output$agencyHistory <- renderPlot({
        ggplot(agency_dt(),
               aes(x = year.float, y = fraction, color = agency_name)) +
            geom_line() +
            scale_y_continuous(label = scales::label_percent()) +
            scale_fill_brewer("Agency", type="qual", palette = "Set3") +
            labs(
                x = "Year",
                y = "% Change",
                title = "% of Total Tax Dollars Collected vs 2006") +
            theme(legend.position = "bottom")
    })

    output$taxRateHistory <- renderPlot({
        ggplot(tax_rate_history(),
               aes(x = year, y = tax_rate)) +
            geom_line() +
            scale_y_continuous(label = scales::label_percent()) +
            labs(
                x = "Year",
                y = "% Levied",
                title = "Tax Collected as Percent of Fair Market Value") +
            theme(legend.position = "bottom")
    })        
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 4044))

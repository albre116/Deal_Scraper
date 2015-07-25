
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage
        (fluidRow(column(3,
                         sidebarPanel(width = 12,
                                      textInput(inputId = "AWSAccessKeyId", label = "AWSAccessKeyId"),
                                      textInput(inputId = "MarketplaceId", label = "MarketplaceId"),
                                      textInput(inputId = "SellerId", label = "SellerId"),
                                      textInput(inputId = "AWSSecretKey", label = "AWSSecretKey"),
                                      textInput(inputId = "UPSrate", label = "UPSrate"),
                                      textInput(inputId = "MarginAlert", label = "MarginAlert", value = 0.3),
                                      textInput(inputId = "ModelMatchAlert", label = "ModelMatchAlert", value = 0.9),
                                      textInput(inputId = "WeightedMatchAlert", label = "WeightedMatchAlert", value = 6),
                                      textInput(inputId = "alertemail", label = "alertemail"),
                                      fileInput(inputId = "FBAFeeTable", label = "CSV Table of FBA Commissions with Header", multiple = FALSE, accept = c('text/csv', 'text/comma-separated-values', 'text/tab-separated-values', 'text/plain', '.csv', '.tsv')),
                                      fileInput(inputId = "ShipFeeTable", label = "CSV Table of FBA Shipping Fees with Header", multiple = FALSE, accept = c('text/csv', 'text/comma-separated-values', 'text/tab-separated-values', 'text/plain', '.csv', '.tsv')),
                                      actionButton("deals_start", "Start Deal Tracking"),
                                      actionButton("deals_stop", "Pause Deal Tracking")
                         )
        ),
        column(9,navbarPage("Live Deal Table",
                            tabPanel("Deal Table Sortable",
                                     h4("Deal Table Sort"),
                                     dataTableOutput("dealtable")
                            )
        ))
        ))
)
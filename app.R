#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

# Get data
data <-
    read_csv("https://data.virginia.gov/api/views/uktn-mwig/rows.csv?accessType=DOWNLOAD")

#### Clean and format data ####

data$`Health District` <- replace(
    x = data$`Health District`,
    list = which(data$`Health District` == "Thomas Jefferson"),
    values =  "Blue Ridge")

data <- data %>%
    mutate(`Report Date` = mdy(`Report Date`)) %>%
    arrange(`Report Date`) %>%
    filter(`Report Date` >= mdy("04-30-2020")) %>%
    filter(!is.na(`Health District`)) %>%
    group_by(`Age Group`, `Health District`) %>%
    mutate(`New Daily Cases` = `Number of Cases` -
               lag(`Number of Cases`,
                   order_by = `Report Date`)) %>%
    mutate(`New Weekly Cases` = `Number of Cases` -
               lag(`Number of Cases`, n = 7,
                   order_by = `Report Date`)) %>%
    drop_na()

#### UI ####
ui <- fluidPage(
    # Application title
    titlePanel("Virginia Department of Health Covid-19 Surveillance Data"),
    helpText(
        tags$a(href = "https://www.vdh.virginia.gov/coronavirus/",
               "Using the most recently available data from VDH, here.")
    ),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "DateRange",
                label = "Select date range to view",
                min = min(data$`Report Date`),
                max = max(data$`Report Date`),
                value = c(min(data$`Report Date`), max(data$`Report Date`))
            ),
            radioButtons(
                inputId = "sum",
                label = "Plot age groups and health districts individually or 
                    combined totals for selected groups?",
                choices = c("Plot individually", "Sum total"),
                selected = "Plot individually",
                inline = TRUE
            ),
            
            ##### Sum total ####
            conditionalPanel(
                condition = "input.sum == `Sum total`",
                numericInput(
                    inputId = "groups",
                    label = "Number of groups to define",
                    value = 1,
                    min = 1,
                    max = 3
                ),
                selectInput(
                    inputId = "AgeGroup1",
                    label = "Age Groups in Group 1",
                    choices = unique(data$`Age Group`),
                    multiple = TRUE,
                    unique(data$`Age Group`)
                ),
                selectInput(
                    inputId = "HealthDist1",
                    label = "Health Districts in Group 1",
                    choices = sort(unique(data$`Health District`)),
                    multiple = TRUE,
                    "New River"
                ),
                conditionalPanel(
                    condition = "input.groups > 1",
                    selectInput(
                        inputId = "AgeGroup2",
                        label = "Age Groups in Group 2",
                        choices = unique(data$`Age Group`),
                        multiple = TRUE,
                        unique(data$`Age Group`)
                    ),
                    selectInput(
                        inputId = "HealthDist2",
                        label = "Health Districts in Group 2",
                        choices = sort(unique(data$`Health District`)),
                        multiple = TRUE,
                        "New River"
                    ),
                    conditionalPanel(
                        condition = "input.groups > 2",
                        selectInput(
                            inputId = "AgeGroup3",
                            label = "Age Groups in Group 3",
                            choices = unique(data$`Age Group`),
                            multiple = TRUE,
                            unique(data$`Age Group`)
                        ),
                        selectInput(
                            inputId = "HealthDist3",
                            label = "Health Districts in Group 3",
                            choices = sort(unique(data$`Health District`)),
                            multiple = TRUE,
                            "New River"
                        )
                    )
                )
            ),
            ##### Plot individually ####
            conditionalPanel(
                condition = "input.sum != `Sum total`",
                selectInput(
                    inputId = "AgeGroup",
                    label = "Age Group",
                    choices = unique(data$`Age Group`),
                    multiple = TRUE,
                    unique(data$`Age Group`)
                ),
                selectInput(
                    inputId = "HealthDist",
                    label = "Health District",
                    choices = sort(unique(data$`Health District`)),
                    multiple = TRUE,
                    "New River"
                )
            )
        ),
        
        #Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            downloadButton("downloadData", "Download data"),
            downloadButton("downloadPlot", "Download plot pdf")
        )
    )
)

#### Server ####
server <- function(input, output) {
    filtered_data <- reactive({
        data_plot <- data %>%
            filter(`Report Date` %within%
                       interval(input$DateRange[[1]],
                                input$DateRange[[2]]))
        if (input$sum != "Sum total") {
            data_plot <- data_plot %>%
                filter(`Health District` %in% input$HealthDist) %>%
                filter(`Age Group` %in% input$AgeGroup) %>%
                filter(wday(`Report Date`) == wday(today()))
        }
        else {
            ##### 1 Group ####
            if (input$groups == 1) {
                data_plot <- data_plot %>%
                    filter(`Health District` %in%
                               c(input$HealthDist1)) %>%
                    filter(`Age Group` %in%
                               c(input$AgeGroup1)) %>%
                    mutate(
                        group = case_when(
                            `Health District` %in% input$HealthDist1 &
                                `Age Group` %in% input$AgeGroup1 ~
                                paste(
                                    paste(input$HealthDist1, collapse = ", "),
                                    paste(input$AgeGroup1, collapse = ", "),
                                    sep = " & "
                                )
                        )
                    ) %>%
                    group_by(`Report Date`, group) %>%
                    summarise(`New Weekly Cases` =
                                  sum(`New Weekly Cases`))
            } else
                ##### 2 Groups ####
            if (input$groups == 2) {
                data_plot <- data_plot %>%
                    filter(`Health District` %in%
                               c(input$HealthDist1, input$HealthDist2)) %>%
                    filter(`Age Group` %in%
                               c(input$AgeGroup1, input$AgeGroup2)) %>%
                    mutate(
                        group = case_when(
                            `Health District` %in% input$HealthDist1 &
                                `Age Group` %in% input$AgeGroup1 ~
                                paste(
                                    paste(input$HealthDist1, collapse = ", "),
                                    paste(input$AgeGroup1, collapse = ", "),
                                    sep = " & "
                                ),
                            `Health District` %in% input$HealthDist2 &
                                `Age Group` %in% input$AgeGroup2 ~
                                paste(
                                    paste(input$HealthDist2, collapse = ", "),
                                    paste(input$AgeGroup2, collapse = ", "),
                                    sep = " & "
                                )
                        )
                    ) %>%
                    group_by(`Report Date`, group) %>%
                    summarise(`New Weekly Cases` =
                                  sum(`New Weekly Cases`))
            } else
                ##### 3 Groups ####
            if (input$groups == 3) {
                data_plot <- data_plot %>%
                    filter(
                        `Health District` %in%
                            c(
                                input$HealthDist1,
                                input$HealthDist2,
                                input$HealthDist3
                            )
                    ) %>%
                    filter(
                        `Age Group` %in%
                            c(
                                input$AgeGroup1,
                                input$AgeGroup2,
                                input$AgeGroup3
                            )
                    ) %>%
                    mutate(
                        group = case_when(
                            `Health District` %in% input$HealthDist1 &
                                `Age Group` %in% input$AgeGroup1 ~
                                paste(
                                    paste(input$HealthDist1, collapse = ", "),
                                    paste(input$AgeGroup1, collapse = ", "),
                                    sep = " & "
                                ),
                            `Health District` %in% input$HealthDist2 &
                                `Age Group` %in% input$AgeGroup2 ~
                                paste(
                                    paste(input$HealthDist2, collapse = ", "),
                                    paste(input$AgeGroup2, collapse = ", "),
                                    sep = " & "
                                ),
                            `Health District` %in% input$HealthDist3 &
                                `Age Group` %in% input$AgeGroup3 ~
                                paste(
                                    paste(input$HealthDist3, collapse = ", "),
                                    paste(input$AgeGroup3, collapse = ", "),
                                    sep = " & "
                                )
                        )
                    ) %>%
                    group_by(`Report Date`, group) %>%
                    summarise(`New Weekly Cases` =
                                  sum(`New Weekly Cases`))
            }
        }
    })
    
    output$downloadData <- downloadHandler(
        filename = "VDH-Covid-data-subset.csv",
        content = function(file) {
            if (input$sum == "Sum total") {
                write.csv(
                    filtered_data() %>%
                        pivot_wider(names_from = group,
                                    values_from = `New Weekly Cases`, 
                                    values_fill = 0, 
                                    names_prefix = "New Weekly Cases "),
                    file,
                    row.names = FALSE
                )
            } else
                write.csv(
                    filtered_data() %>%
                        pivot_wider(
                            names_from = c(`Health District`, `Age Group`),
                            values_from = `New Weekly Cases`,
                            names_glue = "{`Health District`}_{`Age Group`}", 
                            values_fill = 0, 
                            names_prefix = "New Weekly Cases ")
                        ,
                    file,
                    row.names = FALSE
                )
        }
    )
    distPlot <- reactive({
        if (input$sum != "Sum total") {
            ggplot(
                data = filtered_data(),
                mapping = aes(
                    x = `Report Date`,
                    y = `New Weekly Cases`,
                    color = `Age Group`,
                    linetype = `Health District`,
                    group = interaction(`Age Group`,
                                        `Health District`)
                )
            ) +
                geom_line(alpha = 0.7) +
                scale_color_viridis_d(option = "C", end = 0.7) +
                scale_x_date(date_breaks = "months", date_labels = "%b %y") +
                expand_limits(y = 0) +
                theme_classic()
            
        } else {
            ggplot(
                data = filtered_data(),
                mapping = aes(
                    x = `Report Date`,
                    y = `New Weekly Cases`,
                    color = group,
                    group = group
                )
            ) +
                geom_line(alpha = 0.7) +
                scale_color_viridis_d(option = "C", end = 0.7) +
                scale_x_date(date_breaks = "months", date_labels = "%b %y") +
                theme_classic() +
                theme(legend.position = "top")
        }
    })
    
    output$distPlot <- renderPlot(print(distPlot()))
    
    output$downloadPlot <- downloadHandler(
        filename = "VDH-Covid-data-subset-plot.pdf",
        content = function(file) {
            ggsave(file, distPlot(), device = "pdf")
        }
    )
}


# Run the application
shinyApp(ui = ui, server = server)

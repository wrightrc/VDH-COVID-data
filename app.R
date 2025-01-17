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
library(ggthemes)
library(fontawesome)
library(plotly)

# Get data
data <-
    read_csv("https://data.virginia.gov/api/views/uktn-mwig/rows.csv?accessType=DOWNLOAD")

#### Clean and format data ####

data$`Health District` <- replace(
    x = data$`Health District`,
    list = which(data$`Health District` == "Thomas Jefferson"),
    values =  "Blue Ridge"
)

data$`Age Group` <- replace(
  x = data$`Age Group`,
  list = which(data$`Age Group` == "19-Oct"),
  values =  "10-19"
)

data$`Age Group` <- replace(
  x = data$`Age Group`,
  list = which(data$`Age Group` == "Missing" & data$`Age Group Type`== "Case Age Group" ),
  values =  "Missing Case Age Group"
)

data$`Age Group` <- replace(
  x = data$`Age Group`,
  list = which(data$`Age Group` == "Missing" & data$`Age Group Type`== "Vaccine Age Group" ),
  values =  "Missing Vaccine Age Group"
)

data <- data %>%
    mutate(`Report Date` = mdy(`Report Date`)) %>%
    arrange(`Report Date`) %>%
    filter(`Report Date` >= mdy("04-30-2020")) %>%
    filter(!is.na(`Health District`)) %>%
      arrange(`Health District`) %>%
      arrange(`Age Group Type`)
      

####(ask clay how to do this): need to combine and replace vaccine age-groups 16-17 + 18-24 for Dates prior to Oct 1, 2021. Keep separate after that date. VDH shifted a bunch from 16-17 to the 18-24 age group.
#make dataset to combine pre Oct, 2021 data for those age groups that VDH shifted cases across
data.fix <- data %>%
  select(-`Number of Hospitalizations`, -`Number of Deaths`) %>% 
  filter(`Age Group`%in%c('16-17 Years','18-24 Years')) %>%
  filter(`Report Date` < mdy("10-8-2021")) %>%
    pivot_wider(names_from =`Age Group`, values_from = `Number of Cases`) %>% 
      mutate(`Number of Cases` = `16-17 Years` + `18-24 Years`, 
             `Age Group` = '16-24 Years') %>% 
        select(-`16-17 Years`,-`18-24 Years`) 

data <- data %>%
  filter(!(`Age Group`%in%c('16-17 Years','18-24 Years') & `Report Date` < mdy("10-01-2021"))) 

data <- bind_rows(data,data.fix) %>% 
  select(-`Number of Hospitalizations`, -`Number of Deaths`) %>% 
  arrange(`Health District`,`Report Date`,`Age Group Type`,`Age Group`) %>% 
    mutate(`Age Group` = fct_inorder(`Age Group`)) %>% 
    mutate(`Age Group` = fct_relevel(`Age Group`,"5-11 Years", after = 11)) %>% 
    mutate(`Age Group` = fct_relevel(`Age Group`,"16-17 Years", after = 14)) %>% 
    mutate(`Age Group` = fct_relevel(`Age Group`,"18-24 Years", after = 15))
#### end fix
  
  
  
data <- data %>%
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
    titlePanel("Virginia Department of Health COVID-19 Surveillance Data"),
    helpText(
        "Using the most recent publicly available data from VDH.",
        tags$a(href = "https://www.vdh.virginia.gov/coronavirus/",
               "Raw data is available here."),
        p(
            "Authors are not affiliated with Virginia Department of Health. See important notes below about this data."
        )
    ),
    sidebarLayout(
        # Sidebar -----------------------------------------------------------------
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
                    sum totals for selected groups:",
                choices = c("Plot individually", "Sum total"),
                selected = "Plot individually",
                inline = TRUE
            ),
            radioButtons(
              inputId = "type",
              label = "Select Age Group Type:",
              choiceValues = c("Case Age Group", "Vaccine Age Group"),
                choiceNames = c("Case Age Group (10 years)", "Vaccine Age Group"),
              selected = "Case Age Group",
              inline = TRUE
            ),
            ##### Sum total ####
            conditionalPanel(
                condition = "input.sum == `Sum total`",
                numericInput(
                    inputId = "groups",
                    label = "Number of groups to define (max = 3 at this time)",
                    value = 1,
                    min = 1,
                    max = 3
                ),
                selectInput(
                    inputId = "AgeGroup1",
                    label = "Age Groups in Group 1",
                    choices = levels(data$`Age Group`),
                    multiple = TRUE,
                    levels(data$`Age Group`)
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
                        choices = levels(data$`Age Group`),
                        multiple = TRUE,
                        levels(data$`Age Group`)
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
                            choices = levels(data$`Age Group`),
                            multiple = TRUE,
                            levels(data$`Age Group`)
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
                    choices = levels(data$`Age Group`),
                    multiple = TRUE,
                    levels(data$`Age Group`)
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
        
        # Main Panel --------------------------------------------------------------
        
        
        #Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            downloadButton("downloadData", "Download data"),
            downloadButton("downloadPlot", "Download plot pdf")
        )
    ),
    p("Notes:",
      tags$ul(
          tags$li(
              "Report date may not reflect infection or symptom onset date and could be impacted by variations in test availability, test seeking behaviors, and reporting lag time."
          ),
          tags$li(
              "Cases are assigned to location based on residence and may not reflect where transmission occurred."
          ),
          tags$li(
              "On 08/1/2021, VDH added Vaccine Age Group categories. Current settings allow for viewing age groups from only one group type at a time, depending on which Age Group Type is selected above."
          ),
          tags$li(
              "On 10/1/2021, VDH shifted some data from Vaccine Age Group 16-17 Years to 18-24 Years. To account for this, we added these two age groups together to create a 16-24 Years group for the time period prior to this shift. The groups are kept separate after this data shift."
          )
      )),
    tags$footer(
        align = "center",
        "Concept and design by ",
        tags$a(href = "https://publichealth.vt.edu/people/facultystaff/rachelsilverman.html", "Rachel A. Silverman"),
        tags$br(),
        "Code and design by ",
        tags$a(href = "https://www.bse.vt.edu/about/people/faculty/clay-wright.html", "R. Clay Wright"),
        tags$br(),
        fa(name = "github", fill = "black"),
        tags$a(href = "https://github.com/wrightrc/VDH-COVID-data",
               "View the source code and make suggestions"),
        tags$br(),
        HTML(
            'This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a><br /><a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a>'
        )
    )
)



#### Server ####
server <- function(input, output) {
    ## Groupings
    # output$groupSelectors <- renderUI({
    #     cities <-
    #     checkboxGroupInput("cities", "Choose Cities", cities)
    # })
    ##filter data
    filtered_data <- reactive({
        data_plot <- data %>%
            filter(`Report Date` %within%
                       interval(input$DateRange[[1]],
                                input$DateRange[[2]])) %>%
          filter(wday(`Report Date`) == wday(max(`Report Date`))) %>%
            filter(`Age Group Type` == input$type)
          
        ##### Plot Individual ####
        if (input$sum != "Sum total") {
            data_plot <- data_plot %>%
                filter(`Health District` %in% input$HealthDist) %>%
                filter(`Age Group` %in% input$AgeGroup)
        }
        else {
            #### Sum total ####
            # make table of groups for selecting
            
            ##### 1 Group ####
            if (input$groups >= 1) {
                data_plot1 <- data_plot %>%
                    filter(`Health District` %in%
                               c(input$HealthDist1)) %>%
                    filter(`Age Group` %in%
                               c(input$AgeGroup1)) %>%
                    mutate(group = paste(
                        paste(input$HealthDist1, collapse = ", "),
                        paste(input$AgeGroup1, collapse = ", "),
                        sep = " & "
                    )) %>%
                    group_by(`Report Date`, group) %>%
                    summarise(`New Weekly Cases` =
                                  sum(`New Weekly Cases`))
            }
            ##### 2 Groups ####
            if (input$groups >= 2) {
                data_plot2 <- data_plot %>%
                    filter(`Health District` %in% input$HealthDist2) %>%
                    filter(`Age Group` %in% input$AgeGroup2) %>%
                    mutate(group = paste(
                        paste(input$HealthDist2, collapse = ", "),
                        paste(input$AgeGroup2, collapse = ", "),
                        sep = " & "
                    )) %>%
                    group_by(`Report Date`, group) %>%
                    summarise(`New Weekly Cases` =
                                  sum(`New Weekly Cases`))
            }
            ##### 3 Groups ####
            if (input$groups >= 3) {
                data_plot3 <- data_plot %>%
                    filter(`Health District` %in% input$HealthDist3) %>%
                    filter(`Age Group` %in% input$AgeGroup3) %>%
                    mutate(group =
                               paste(
                                   paste(input$HealthDist3, collapse = ", "),
                                   paste(input$AgeGroup3, collapse = ", "),
                                   sep = " & "
                               )) %>%
                    group_by(`Report Date`, group) %>%
                    summarise(`New Weekly Cases` =
                                  sum(`New Weekly Cases`))
            }
            # bind datasets 
            data_plot <- data_plot1
            if (exists("data_plot2")) {
                data_plot <- data_plot %>% bind_rows(data_plot2)
            }
            if (exists("data_plot3")) {
                data_plot <- data_plot %>% bind_rows(data_plot3)
            }
        }
        data_plot
    })
    
    #### Download ####
    output$downloadData <- downloadHandler(
        filename = "VDH-COVID-data-subset.csv",
        content = function(file) {
            if (input$sum == "Sum total") {
                write.csv(
                    filtered_data() %>%
                        pivot_wider(
                            names_from = group,
                            values_from = `New Weekly Cases`,
                            values_fill = 0,
                            names_prefix = "New Weekly Cases in "
                        ),
                    file,
                    row.names = FALSE
                )
            } else
                write.csv(
                    filtered_data() %>%
                        select(
                            -c(
                                `New Daily Cases`,
                                `Number of Cases`
                                                            )
                        ) %>%
                        pivot_wider(
                            names_from = c(`Health District`, `Age Group`),
                            values_from = `New Weekly Cases`,
                            names_glue = "New Weekly Cases in {`Health District`} & {`Age Group`}",
                            values_fill = 0
                        ),
                    file,
                    row.names = FALSE
                )
        }
    )
    #### Plot ####
    distPlot <- reactive({
        if (input$sum != "Sum total") {
            p <- ggplot(
                data = filtered_data(),
                mapping = aes(
                    x = `Report Date`,
                    y = `New Weekly Cases`,
                    color = `Age Group`,
                    linetype = `Health District`,
                    group = interaction(`Age Group`,
                                        `Health District`)
                )
            )
            
        } else {
            # Formatting --------------------------------------------------------------
            p <- ggplot(
                data = filtered_data(),
                mapping = aes(
                    x = `Report Date`,
                    y = `New Weekly Cases`,
                    color = group,
                    group = group
                )
            )
        }
        p <- p +
            geom_line(alpha = 0.7, size = 1) +
            scale_color_brewer(palette = "Paired") +
            scale_x_date(date_breaks = "months", date_labels = "%b %y") +
            expand_limits(y = 0) +
            theme_excel_new(base_size = 14) +
            theme(axis.title.x = element_text(),
                  axis.title.y = element_text()) +
            labs(y = "New Cases Reported in Prior Week")
        if(input$sum == "Sum total") p + theme(legend.direction = "vertical") else
            p
    })
    
    output$distPlot <- renderPlot(distPlot()) 
    
    output$downloadPlot <- downloadHandler(
        filename = "VDH-COVID-data-subset-plot.pdf",
        content = function(file) {
            ggsave(file, distPlot(), device = "pdf")
        }
    )
}


# Run the application
shinyApp(ui = ui, server = server)


#make more colors:
#color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
#n=15
#pie(rep(1,n), col=sample(color, n))








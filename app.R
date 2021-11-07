
library(shiny)
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)

saveData <- function(data) {
    #unlink("./data/old_data.csv")
    write.csv(data,file = "data/old_data.csv", row.names = FALSE)
    #save(data, file = "data/old_data.Rdata")
}

loadData <- function() {
    dat <- read_csv("data/old_data.csv",col_types = cols(Date = col_date(format = "%Y-%m-%d")))
    return(dat)
}

#  if (file.exists("./data/old_data.Rdata")) {
#      load("data/old_data.Rdata")
# }
# colnames(old_data)[3] <- "Glucose"


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Diabetes Measurements"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout( 
        sidebarPanel(
            fileInput("file", "Upload measurements", multiple = TRUE),
            dateRangeInput(inputId = "date", "Range of data",
                           start = "2020-03-14",end = "2020-09-10"),
            h3("New measurement"),
            dateInput(inputId = "m_date", 
                      label = "Date measurement taken", value = Sys.Date()),
            textInput("glucose", label = "Glucose"),
            selectInput("units", "units", choices = c("mg/dl", "mmoles/lt")),
            selectInput("time", "Time", choices = c("Before Breakfast", "After Breakfast","Before Lunch", "After Lunch", 
                                                    "Before Dinner", "After Dinner", "Before Sleep")),
            actionButton("go", "Add measurement")
        ),
        mainPanel( 
            plotOutput("glucose_graph"),
            tableOutput("statistics"),
            plotOutput("before_after")
        ))
    
)

server <- function(input, output) {
    filedata <- reactive({
        inFile <- input$file 
        if (is.null(inFile) & file.exists("./data/old_data.csv")) {
            dat <- read_csv("data/old_data.csv",col_types = cols(Date = col_date(format = "%Y-%m-%d")))
            colnames(dat)[3] <- "Glucose"
            return(dat)
        }
        df <- read_csv(inFile$datapath, col_types = cols(Date = col_date(format = "%d/%m/%Y")))
        if (file.exists("./data/old_data.Rdata")) {
            load("data/old_data.Rdata")
            df <- rbind(data, df)
            colnames(data)[3] <- "Glucose"
        }
        colnames(df)[3] <- "Glucose"
        df <- df[!duplicated(df),]
        return(df)
    })
    new_measurement <- reactive({
        new_data <- data.frame(Date = input$m_date, Time = NA,
                               Glucose = as.numeric(input$glucose), Period =  input$time,Note = NA)
        return(new_data)
    })

    dat <- reactive({
        if (is.na(new_measurement()[['Glucose']])) {
            return(filedata())
        }
        return(rbind.data.frame(filedata(), new_measurement()))
    })
    observeEvent(input$go, {
        saveData(dat())
    })
    # output$date <- renderTable({ 
    #     input$go
    #     loadData()
    #     #tail(dat(), 10)
    #     })
    # 
    output$glucose_graph <- renderPlot({
        input$go
        dat <- read_csv("data/old_data.csv",col_types = cols(Date = col_date(format = "%Y-%m-%d")))
        if (input$date[2] > max(dat[['Date']])) {
            input$date[2] <- max(dat[['Date']])
        }
        ggplot(data = dat, aes(x = Date, y = Glucose)) +
            geom_line() +
            scale_x_date(date_labels = "%Y %b %d",date_breaks = "2 week",
                         limit = c(input$date[1],input$date[2])) +
            ylab("Glucose") +
            theme(axis.text.x=element_text(angle=60, hjust=1)) +
            facet_wrap(~Period, scales = "free") })
    output$statistics <- renderTable({
        input$go
        dat <- read_csv("data/old_data.csv",col_types = cols(Date = col_date(format = "%Y-%m-%d")))        
        colnames(dat)[3] <- "Glucose"
        dat %>%
            group_by(Period) %>%
            filter(Date >= input$date[1] & Date <= input$date[2]) %>%
            summarise(count = n(), mean = mean(Glucose),
                      median = median(Glucose), std = sd(Glucose))
    })
    output$before_after <- renderPlot({
        input$go
        dat <- read_csv("data/old_data.csv",col_types = cols(Date = col_date(format = "%Y-%m-%d")))
        if (input$date[2] > max(dat[['Date']])) {
            input$date[2] <- max(dat[['Date']])
        }
        dupls_dates <- as.Date(names(which(table(dat$Date)>1)), format = "%Y-%m-%d")
        dat_ba <- dat[dat$Date %in% dupls_dates,]
        dat_ba$session <- ifelse(grepl("Breakfast", dat_ba$Period), "Breakfast",
                                 ifelse(grepl("Lunch", dat_ba$Period), "Lunch",
                                        "Dinner"))
        dat_ba$Period <- ifelse(grepl("After", dat_ba$Period), "2After", "1Before")
        
        ggplot(dat_ba, aes(x = Period, y = Glucose)) +
            geom_point() +
            geom_line(aes(group = Date)) +
            scale_x_discrete(breaks=c("1Before","2After"),
                             labels=c("Before ", "After")) +
            facet_wrap(~session)    })
}

# Run the application 
shinyApp(ui = ui, server = server)




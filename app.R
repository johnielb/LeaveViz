library(shiny)
library(bizdays) # business days
library(tidyverse) # data wrangling
library(lubridate) # date shift
library(grid) # labels on graph

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Entitlement Leave Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("date", "Report date", choices="2019-11-30", selected="2019-11-30"),
      radioButtons("leaveType", "Leave type", choices = list("Entitled leave"=0, "Entitled minus booked leave"=1), selected=0),
      checkboxGroupInput("bizGroup", "Business department", choices = list("NA")
      ),
      actionButton("allGroups", label="Select all groups"),
      actionButton("noGroups", label="Deselect all groups"),
      hr(),
      # change conditional in output$plot if modifying this
      radioButtons("years", "Years employed", 
                   choices = list("First year"=0, "Second year"=1,
                                  "Third year"=2, "Fourth year"=3,
                                  "Fifth year onwards"=4, "All"=5),
                   selected = 5)
    ),
    
    mainPanel(
      width = 10,
      plotOutput("plot", width="100%")
    )
  )
)

server <- function(input, output, session) {
  nzHolidays <- c("2018-01-01", "2018-01-02", "2018-02-06", "2018-03-30", "2018-04-02", "2018-04-25", "2018-06-04", "2018-10-22", "2018-12-25", "2018-12-26",
                  "2019-01-01", "2019-01-02", "2019-02-06", "2019-04-19", "2019-04-22", "2019-04-25", "2019-06-03", "2019-10-28", "2019-12-25", "2019-12-26",
                  "2020-01-01", "2020-01-02", "2020-02-06", "2020-04-10", "2020-04-13", "2020-04-27", "2020-06-01", "2020-10-26", "2020-12-25", "2020-12-28",
                  "2021-01-01", "2021-01-04", "2021-02-08", "2021-04-02", "2021-04-05", "2021-04-26", "2021-06-07", "2021-10-25", "2021-12-27", "2021-12-28")
  wlgHolidays <- c(nzHolidays, "2018-01-22", "2019-01-21", "2020-01-20", "2021-01-25")
  aklHolidays <- c(nzHolidays, "2018-01-29", "2019-01-28", "2020-01-27", "2021-02-01")
  chcHolidays <- c(nzHolidays, "2018-11-16", "2019-11-15", "2020-11-13", "2021-11-12")
  
  create.calendar('wlg', holidays=wlgHolidays, weekdays = c('sunday', 'saturday'), 
                  adjust.from = adjust.next, adjust.to = adjust.previous)
  
  # days to add on an anniversary
  BASELINE_DAYS <- 30
  # minimum days that can be carried over on an anniversary
  CARRYOVER <- 5
  # maximum days someone has on an anniversary
  MAX_DAYS <- BASELINE_DAYS + CARRYOVER
  
  createData <- reactive({
    sampleSize <- 400 # should always be a multiple of 10
    balance <- data.frame(BusinessGroup = integer(sampleSize))
    balance$BusinessGroup <- as.factor(sample(c(
      "Design", "Marketing", "Human Resources", "Production", "Financial"
    ), sampleSize, replace = TRUE))
    
    reportDate1 <- as.Date("2019-11-30")
    balance$ReportDate <- replicate(sampleSize, reportDate1) %>%
      as.Date(origin="1970-01-01")
    
    # creates a sequence of days ranging across the two inputs,
    # "start" or "end" days since the 1st report date
    getDateSequence <- function(start, end) {
      seq(reportDate1-end, reportDate1-start, by="day")
    }
    
    # ensures more people are employed for 5-20 years
    # guarantees 10% are employed for every other age category
    balance$EmployeeStartDate <- c(
      sample(getDateSequence(0,365), sampleSize*0.1),
      sample(getDateSequence(365,730), sampleSize*0.1),
      sample(getDateSequence(730,1095), sampleSize*0.1),
      sample(getDateSequence(1095,1461), sampleSize*0.1),
      sample(getDateSequence(1461,7305), sampleSize*0.5),
      sample(getDateSequence(7305,18262), sampleSize*0.1)
    )
    
    getYearsEmployed <- function(x) {
      daysEmployed <- as.numeric(difftime(x$ReportDate, x$EmployeeStartDate, units="days"))
      
      # 365th day of non leap year should be floor()'d to 1 but will show up as 0 because it's
      # only 0.999 of a year, difftime() doesn't deal in years because of inconsistency
      yearsEmployed <- daysEmployed / 365.25
      if (month(x$ReportDate) == month(x$EmployeeStartDate) && day(x$ReportDate) == day(x$EmployeeStartDate)) {
        yearsEmployed <- yearsEmployed + (floor(yearsEmployed)+1)*0.0007
        # 1/1461 = 0.25/365.25 (the error) multiplied by actual integer years employed
      }
      # only round after adjusting
      floor(yearsEmployed)
    }
    
    balance$YearsEmployed <- getYearsEmployed(balance)
    
    getDSA <- function(x) {
      # allows shifting start date to relevant year's anniversary
      annivDate <- x$EmployeeStartDate
      # love people with leap day anniversaries
      if (day(annivDate) == 29 && month(annivDate) == 2 && x$YearsEmployed %% 4 != 0) {
        day(annivDate) <- 1
        month(annivDate) <- 3
      } 
      # shift start date to last anniversary since sim date
      year(annivDate) <- year(annivDate) + x$YearsEmployed
      
      as.numeric(bizdays(annivDate,x$ReportDate,'wlg') + 1)
    }
    
    balance$DaysSinceAnniversary <- getDSA(balance)
    
    # random amount of leap days, more for those employed longer
    balance$EntitledDays <- c(
      rnorm(sampleSize*0.1, mean=0, sd = 7),
      rnorm(sampleSize*0.1, mean=4, sd = 7),
      rnorm(sampleSize*0.1, mean=14, sd = 14),
      rnorm(sampleSize*0.1, mean=20, sd = 21),
      rnorm(sampleSize*0.6, mean=25, sd = 21)
    ) - (balance$DaysSinceAnniversary - 100) * 0.1
    
    # first years must comply with policy of having no entitled leave
    balance <- balance %>%
      mutate(EntitledDays = replace(EntitledDays, YearsEmployed == 0 & EntitledDays > 0, 0))
    
    # skewed towards booking less leave hence beta dist
    balance$FutureBookedLeave <- as.integer(rbeta(sampleSize,0.5,5)*30)
    balance$NetLeave <- balance$EntitledDays-balance$FutureBookedLeave
    
    # update checkbox list from df
    bizGroupList <- unique(balance$BusinessGroup)
    updateCheckboxGroupInput(session,"bizGroup", "Business group", choices=bizGroupList, selected=bizGroupList)
    
    # update report date selection from df
    dateList <- unique(balance$ReportDate)
    updateSelectInput(session, "date", "Report date", choices=dateList, selected=dateList[1])
    
    balance
  })
  
  # handle Select all groups being clicked, only evaluated if $allGroups changes
  observe({
    if (input$allGroups > 0) {
      bizGroupList <- unique(createData()$BusinessGroup)
      updateCheckboxGroupInput(session, "bizGroup", choices=bizGroupList, selected=bizGroupList)
    }
  })
  
  # handle Deselect all groups being clicked
  observe({
    if (input$noGroups > 0) {
      bizGroupList <- unique(createData()$BusinessGroup)
      updateCheckboxGroupInput(session, "bizGroup", choices=bizGroupList)
    }
  })
  
  output$plot <- renderPlot({
    balance <- createData() %>%
      subset(BusinessGroup %in% input$bizGroup) %>% 
      # filter by years employed
      filter(
        if (input$years == 4) { # fifth year onwards
          YearsEmployed >= as.numeric(input$years)
        } else if (input$years < 4) {
          YearsEmployed == as.numeric(input$years)
        } else { # all
          TRUE
        }
      ) %>% 
      filter(
        ReportDate == input$date
      )
    
    if (input$leaveType == 0) {
      balance <- balance %>% rename(SelectedLeave = EntitledDays)
    } else if (input$leaveType == 1) {
      balance <- balance %>% rename(SelectedLeave = NetLeave)
    }
    
    # do input processing before this line
    validate(
      need(nrow(balance)>0, "Please select settings with data points")
    )
    
    reportDate <- as.Date(input$date)
    yearAfterReport <- reportDate
    # you'll get an NA here if your report date is 29 February, just don't
    year(yearAfterReport) <- year(reportDate) + 1
    bizdaysInYear <- bizdays(reportDate, yearAfterReport, 'wlg')
    
    plot <- ggplot(data = balance, aes(x=DaysSinceAnniversary)) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_line(colour="#f5f5f5"),
            panel.grid.major = element_line(colour="#ffffff"),
            text=element_text(size=18)) +
      # scatter
      geom_point(aes(y = SelectedLeave, colour = BusinessGroup), size=3) + 
      # envelope for spender
      geom_segment(x=0, xend=bizdaysInYear-BASELINE_DAYS, y=MAX_DAYS, yend=MAX_DAYS, color = "#ff0000", size = 1) +
      geom_segment(x=bizdaysInYear-BASELINE_DAYS, xend=bizdaysInYear, y=MAX_DAYS, yend=CARRYOVER, color = "#ff0000", size = 1) +
      # envelope for saver
      geom_segment(x=0, xend=BASELINE_DAYS, y=BASELINE_DAYS, yend=0, color = "#007700", size = 1) +
      geom_segment(x=BASELINE_DAYS, xend=bizdaysInYear, y=0, yend=0, color = "#007700", size = 1) +
      # labels
      labs(x = "Business Days Since Anniversary", y = "Entitlement Days") +
      # show minor gridlines for every 5 days
      scale_x_continuous(minor_breaks = seq(0,260,5), breaks = seq(0,260,20)) +
      scale_y_continuous(minor_breaks = seq(-40,100,10), breaks = seq(-40,100,20)) +
      # legend title
      scale_color_discrete(name = "Business department") +
      # ensure all of envelope seen every time
      expand_limits(x = c(0,270), y=c(-40,100)) +
      annotation_custom(grob=textGrob("Upper policy bound",gp=gpar(col="#ff0000")), xmin=268, xmax=268, ymin=27, ymax=27) +
      annotation_custom(grob=textGrob("Lower policy bound",gp=gpar(col="#007700")), xmin=268, xmax=268, ymin=0, ymax=0) +
      annotation_custom(grob=textGrob(paste("Points on plot =",nrow(balance)),gp=gpar(col="#000000")), xmin=268, xmax=268, ymin=-10, ymax=-10)
    if (nrow(balance) >= 15) {
      # smooth line from scatterplot
      plot <- plot + 
        geom_smooth(data = balance, aes(y = SelectedLeave), method='loess', size=1.2) + 
        annotation_custom(grob=textGrob("Trend + CI",gp=gpar(col="#0000ff")), xmin=268, xmax=268, ymin=14, ymax=14)
    }
    plot
  }, height=800)
}

shinyApp(ui, server)

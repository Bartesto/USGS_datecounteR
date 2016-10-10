library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
    img(src = "DPaW_logo.png", height = 72, width = 180)
    ),
  
  sidebarLayout(position = "right",
    sidebarPanel(
      textInput("pathrow", label = h3("Input Path/Row"), 
                value = "112082")
    ),
    mainPanel(
      h1("Date Counter for USGS path/rows"),
      p("This App will check a USGS path row for numbers of scenes downloaded 
        and processed per year and plot the result. This will show where gaps in 
        download chronology exist."),
      br(),
      br(),
      br(),
      h3("Happy Checking!"),
      textOutput("error"),
      plotOutput("plot1")
      
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$error <- renderText({
    USGSdir <- "W:\\usgs"
    PRdir <- paste0(USGSdir, "\\", input$pathrow)
    validate(
      need(dir.exists(PRdir), "Sorry we have no data for that path")
    )
    setwd(PRdir)
    folds <- list.files()
    if(length(folds) == 0){
      print("Empty path/row. Happy downloading!")
    } else {
      print("")
  }})
  output$plot1 <- renderPlot({
    USGSdir <- "W:\\usgs"
    PRdir <- paste0(USGSdir, "\\", input$pathrow)
    validate(
      need(dir.exists(PRdir), "Sorry we have no data for that path")
    )
    setwd(PRdir)
    folds <- list.files()
    if(length(folds) == 0){
      print("Empty path/row. Happy downloading!")
    } else {
    dates <- as.Date(folds, "%Y%m%d")
    dates <- data.frame(date = dates[!is.na(dates)])
    df <- dates%>%
      mutate(year = as.character(year(date)))%>%
      group_by(year)%>%
      summarise(number = n())
    fullyears <- as.character(seq(year(dates[1,1]), 
                                  year(dates[length(dates$date),1])))
    df2 <- data.frame(year = fullyears, stringsAsFactors = FALSE)
    df3 <- full_join(df2, df, by = "year")
    ggplot(df3)+
      geom_bar(aes(x = year, y = number), stat = "identity")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))+
      ylab("Number of Scenes")+
      xlab("Years")+
      ggtitle(paste0(input$pathrow, " downloads"))
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
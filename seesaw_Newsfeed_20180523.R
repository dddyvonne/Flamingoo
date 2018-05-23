library(shinydashboard)
library(data.table)
library(highcharter)
library(shinythemes)
library(reshape2)
library(ggplot2)
library(shinyjs)
library(plotly)
library(scales)
library(shiny)
library(readr)
library(dplyr)
library(plyr)
library(DT)
library(V8)
library(readxl)
#cat("\014")

news <- read_excel("data/dailyNews.xlsx")
twitter <- read_excel("data/dailyTwitter.xlsx")

twitter <- data.frame(twitter)

b <- gsub(".*www.", "", news$Link, perl = TRUE)
test <- urltools::url_parse(b)$domain
#news$Link <- paste0("<a href='",news$Link,"' target='_blank'>Source</a>")
news$Link <- paste0("<a href='",news$Link,"' target='_blank'>",test,"</a>")
news$Date <- as.Date(news$Date)

twitter$Link <- paste0("<a href='",twitter$Link,"' target='_blank'>Source</a>")
twitter$Date <- as.Date(twitter$Date)
#sample <- fread("~/Documents/- tools/- r/seesaw/sample.csv")
#setnames(twitter, old = c("Number of Retweets"), new = c("Retweets"))
#sample$Date <- as.Date(as.character(sample$Date),"%Y-%m-%d")
h <-c("SUMMARY","MEDIA","WEB","SENTIMENT","GEOGRAPHY")
newsfeed_array <- "['General','Gubernatorial','LCV','Planned Parenthood','Trump']"
social_array <- "['LCV','Planned Parenthood','Progress Michigan','Michigan Gubernatorial Race']"


jsCode <- paste("shinyjs.dateRefresh = function(params){
for(i=0;i<document.querySelectorAll('.input-daterange').length;i++){
                document.querySelectorAll('.input-daterange')[i].querySelectorAll('.input-sm')[0].value = params[0][0];
                document.querySelectorAll('.input-daterange')[i].querySelectorAll('.input-sm')[1].value = params[0][1];
                console.log(params[0][0]);console.log(params[0][1]);}};
                shinyjs.init = function(){var newsfeed_array = ",newsfeed_array,";var social_array = ",social_array,";
                document.querySelector('.input-social').setAttribute('style', 'display:none !important');                
                setInterval(function(){
                try{document.body.removeEventListener(",'"click"',", myInteractionFunc, false);}catch(err){};
                document.body.addEventListener(",'"click"',", myInteractionFunc, false);
                },250);
                var myInteractionFunc = function(e){
                for (var target=e.target; target && target!=this; target=target.parentNode){
                if(target.matches(","'",'a[data-value="Newsfeed"]',"'",")){
                document.querySelector('.input-news').setAttribute('style', 'display:block !important');
                document.querySelector('.input-social').setAttribute('style', 'display:none !important');
                }
                else if(target.matches(","'",'a[data-value="Social Listening"]',"'",")){
                document.querySelector('.input-news').setAttribute('style', 'display:none !important');
                document.querySelector('.input-social').setAttribute('style', 'display:block !important');
                }
                }
                }}")




#jsCode <- "shinyjs.dateRefresh = function(params){
#for(i=0;i<document.querySelectorAll('.input-daterange').length;i++){
#document.querySelectorAll('.input-daterange')[i].querySelectorAll('.input-sm')[0].value = params[0][0];
#document.querySelectorAll('.input-daterange')[i].querySelectorAll('.input-sm')[1].value = params[0][1];
#console.log(params[0][0]);
#console.log(params[0][1]);
#}}"
#setInterval(function(){var a = window.pageYOffset;window.scrollTo(0,a+75);},25);


header <- fluidRow(
  tags$h1("MICHIGAN ISSUE CAMPAIGN"),
  tags$img(src = 'https://watermelonrouge.github.io/seesaw/michigan2.png', class = 'map'),
  tags$h5('Powered by Alchemy', class = 'power'),
  tags$img(src = 'https://watermelonrouge.github.io/seesaw/alchemy.png', class = 'alchemy')
)

categories <- sort(unique(news$Category))
categories_social <- sort(unique(twitter$Category))


themes <- c('Healthcare',
            'Hold Government Officials Accountable',
            'Infrastructure',
            'Guns',
            'Keep Lakes Clean',
            'Our Freedoms',
            'Education',
            'Criminal Justice Reform',
            'Women’s Rights',
            'Marijuana Legalization',
            'Economy & Jobs',
            'Clean Water / Flint')

news_view<- tabPanel(
  "Newsfeed",
  mainPanel(
    header,
    fluidRow(
      tags$h2("Newsfeed Review"),
      tags$div(class='below_title')
    ),
    fluidRow(
      tags$h3("Publications")
    ),
    fluidRow(
      tags$div(class = "news_table",DT::dataTableOutput("table"))
    )
    
  )
)

social_view<- tabPanel(
  "Social Listening",
  mainPanel(
    header,
    fluidRow(
      tags$h2("Social Listening Review"),
      tags$div(class='below_title')
    ),
    fluidRow(
      tags$h3("Trending Tweets (Over Last 3 Days)")
    ),
    fluidRow(
      tags$div(class = "social_table",DT::dataTableOutput("table2"))
    )
    
  )
)

media_view <- tabPanel(
  h[2],
  mainPanel(
    header,
    fluidRow(
      tags$h2("Media Performance Detail"),
      tags$div(class='below_title')
    ),
    fluidRow(tags$h3("Section 1")),
    fluidRow(tags$h3("Section 2")),
    fluidRow(tags$h3("Section 3"))
  )
)

web_view <- tabPanel(
  h[3],
  mainPanel(
    header,
    fluidRow(
      tags$h2("Interest Group Website Performance Detail"),
      tags$div(class='below_title')
    ),
    fluidRow(
      tags$h3("Website Traffic")
    ),
    fluidRow(
      tags$h3("Website Engagement")
    ),
    fluidRow(
      tags$h3("Popular Content")
    )
  )
)

sentiment_view <- tabPanel(
  h[4],
  mainPanel(
    header,
    fluidRow(
      tags$h2("Audience Sentiment Analysis"),
      tags$div(class='below_title')
    ),
    fluidRow(tags$h3("Section 1")),
    fluidRow(tags$h3("Section 2")),
    fluidRow(tags$h3("Section 3"))
  )
)

geo_view <- tabPanel(
  h[5],
  mainPanel(
    header,
    fluidRow(
      tags$h2("Location-Specific Analysis"),
      tags$div(class='below_title')
    ),
    fluidRow(tags$h3("Section A")),
    fluidRow(tags$h3("Section B")),
    fluidRow(tags$h3("Section C"))
  )
)

testarea <- tabPanel("TEST AREA",
   mainPanel(
     header,
     fluidRow(
       tags$h2("A tab to just try things out!"),
       tags$div(class='below_title')
     ),
     fluidRow(
       tags$h3("Plotly Test - Channel response and all filters, by Date")
     ),
     fluidRow(
       plotlyOutput("test1")
     ),
     fluidRow(
       tags$h3("Highctarts test - stacked bar (not currently working!)")
     ),
     fluidRow(
       highchartOutput("test_chart1")       
     ),
     fluidRow(
       tags$h3("Highcharts Test 2")
     ),
     fluidRow(
       highchartOutput("test2")
     )
   )
)


ui <- fluidPage(
  titlePanel("Michigan: Powered by Alchemy"),
  useShinyjs(),
  extendShinyjs(text = jsCode),
  theme = "style.css",
  column(3,
      #tags$div(class='cbx interest',checkboxInput('interestCheck','Show',TRUE)),
     tags$div(class = "input-news",selectInput("categories","Category:",c("All Categories" = "all_categories", categories))),
     tags$div(class = "input-social",selectInput("categories_social","Category:",c("All Categories" = "all_categories", categories_social))),
      #tags$div(class='cbx date',checkboxInput('dateCheck','Show',FALSE)),
      dateRangeInput('dateRange', label = 'Select Date Range:',start = min(news$Date, twitter$Date), end = max(news$Date,twitter$Date))
  ),
  column(9,
    navbarPage("",position = "fixed-top",
      news_view,
      social_view
     
    )
  )
)

colorChoices <- list(
  'rgb(124, 181, 236)',
  'rgb(224, 157, 157)',
  'rgb(124, 236, 181)',
  'rgb(255, 245, 165)'
)

####FUNCTION TO MAKE PLOTLY BAR CHARTS WITH DYNAMIC INPUT
makePlotlyChart <- function(dt,dates,channel,interest,theme) {
    s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
    if (channel != "all_channels"){s <- s[s$Channel == channel,]}
    if (interest != "all_groups"){s <- s[s$`Interest Group` == interest,]}
    if (theme != "all_themes"){s <- s[s$Theme == theme,]}
    s <- s[,c('Date','Channel','Reach')][,j=list(Reach=sum(Reach)),by=c('Date','Channel')]
    s <- dcast(s, Date ~ Channel, fun=sum)
    p <- plot_ly(s, x = ~Date, y = ~s[,2], type = 'bar', name = colnames(s)[2], marker = list(color = 'rgb(124, 181, 236)')) %>%
    layout(p, yaxis = list(title = 'Reach'), barmode = 'stack')
    if(ncol(s) > 2){p <- add_trace(p, y = ~s[,3], name = colnames(s)[3], marker = list(color = 'rgb(224, 157, 157)'))}
    if(ncol(s) > 3){p <- add_trace(p, y = ~s[,4], name = colnames(s)[4], marker = list(color = 'rgb(124, 236, 181)'))}
    if(ncol(s) > 4){p <- add_trace(p, y = ~s[,5], name = colnames(s)[5], marker = list(color = 'rgb(255, 245, 165)'))}
    p
}

makeHighChart <- function(){
    #add function here
}

####FUNCTION TO MAKE TABLES WITH DYNAMIC INPUT
makeTable <- function(dt,dims,dates,cats){
  DT::datatable({
    s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
    if (cats != "all_categories"){s <- s[s$Category == cats,]}
   # if (interest != "all_groups"){s <- s[s$`Interest Group` == interest,]}
  #  if (theme != "all_themes"){s <- s[s$Theme == theme,]}
  #  if(checks[1]==FALSE){dims <- dims[!dims %in% 'Theme']}
   # if(checks[2]==FALSE){dims <- dims[!dims %in% 'Channel']}
  #  if(checks[3]==FALSE){dims <- dims[!dims %in% 'Date']}
  #  if(checks[4]==FALSE){dims <- dims[!dims %in% 'Interest Group']}
   # s <- s[,c(dims,metrics),with=FALSE]  #[,j=list(Reach=sum(Reach)),by=dims]
    #s <- s[,lapply(.SD,sum),by=dims]
   # s$`Amount Spent` <- dollar_format()(s$`Amount Spent`)
    s
  }, escape = FALSE)  
}



server <- function(input, output){
  output$table <- renderDT(
    makeTable(news,c('Date','Category','Summary','Link'),input$dateRange,input$categories), 
    escape = FALSE)
  output$table2 <- renderDT(
    makeTable(twitter,c('Date','Category','Content','Link', "Retweets"),input$dateRange,input$categories_social), 
    escape = FALSE)
 # output$chart1 <- renderPlotly(
  #  makePlotlyChart(sample,input$dateRange,input$channel,input$interest,input$theme)
  #)
  #observeEvent(input$dateRange1, {
  #  #js$dateRefresh(input$dateRange1)
  #})
  #observeEvent(input$dateRange2, {
    #js$dateRefresh(input$dateRange1)
  }
 # output$test_chart1 <- renderHighchart({
#    group1 <- sample[,j=list(`Amount Spent`=sum(`Amount Spent`)), by = c('Date','Channel')]
#    group1 <- group1[(group1$Date >= input$dateRange[1]) & (group1$Date <= input$dateRange[2]),]
    #hc <- group1 %>%
#    hc <- hchart(group1, "column", hcaes(x = 'Date', y = `Amount Spent`)) %>%
#      hc_plotOptions(series = list(stacking = "normal"))
#    hc
  
  
#  #..........TEST CHARTS..........#
  
#  output$test1 <- renderPlotly({
#    makePlotlyChart(sample,input$dateRange,input$channel,input$interest,input$theme)
#  })
#  output$test2 <- renderHighchart({
#    s <- sample[,c('Date','Reach'),with=FALSE]
#    s <- s[,lapply(.SD,sum),by='Date']
#    hchart(s[(s$Date >= input$dateRange[1]) & (s$Date <= input$dateRange[2]),], "column", hcaes(x = Date, y = Reach))
#  })
#}


shinyApp(ui = ui, server = server)

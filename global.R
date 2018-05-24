library(googlesheets)
library(dplyr)
library(highcharter)
gs_auth(token = NULL, new_user = TRUE,
        key = getOption("googlesheets.client_id"),
        secret = getOption("googlesheets.client_secret"),
        cache = getOption("googlesheets.httr_oauth_cache"), verbose = TRUE)

gs_gap() %>% 
  gs_copy(to = "Seesaw Daily_R")
gap <- gs_title("Seesaw Daily_R")

LCV_media <- data.frame(gs_read(gap, "LEVEL 2", range = "B4:R30"))
PP_media <- data.frame(gs_read(gap, "LEVEL 2", range = "T4:AJ30"))
PM_media <- data.frame(gs_read(gap, "LEVEL 2", range = "AL4:BB30"))



LCV_Rate_Chart <-(
  highchart() %>% 
  hc_chart(type = "line") %>% 
  hc_title(text = "League of Conservation Voters Action Rate") %>% 
  hc_subtitle(text = "Sources: FB Ad Manager, Twitter Ad Mananger, Google Ad Manager") %>% 
  hc_xAxis(categories = c(LCV_media$Date)) %>% 
  hc_yAxis(title = list(text = "Action Rate (%)")) %>% 
  hc_plotOptions(line = list(
    dataLabels = list(enabled = FALSE),
    enableMouseTracking = TRUE)
  ) %>% 
  hc_series(
    list(
      name = "Faebook",
      data = as.numeric(sub("%" , "", LCV_media$Facebook.Action))
    ),
    list(
      name = "Twiter",
      data = as.numeric(sub("%" , "", LCV_media$Twitter.Action))
    ),
      list(
        name = "Google",
        data = as.numeric(sub("%" , "", LCV_media$Google.Action))
      )
    ))

PP_Rate_Chart <-(
  highchart() %>% 
    hc_chart(type = "line") %>% 
    hc_title(text = "Planned Parenthood Action Rate") %>% 
    hc_subtitle(text = "Sources: FB Ad Manager, Twitter Ad Mananger, Google Ad Manager") %>% 
    hc_xAxis(categories = c(PP_media$Date)) %>% 
    hc_yAxis(title = list(text = "Action Rate (%)")) %>% 
    hc_plotOptions(line = list(
      dataLabels = list(enabled = FALSE),
      enableMouseTracking = TRUE)
    ) %>% 
    hc_series(
      list(
        name = "Faebook",
        data = as.numeric(sub("%" , "", PP_media$Facebook.Action))
      ),
      list(
        name = "Twiter",
        data = as.numeric(sub("%" , "", PP_media$Twitter.Action))
      ),
      list(
        name = "Google",
        data = as.numeric(sub("%" , "", PP_media$Google.Action))
      )
    ))

PM_Rate_Chart <-(
  highchart() %>% 
    hc_chart(type = "line") %>% 
    hc_title(text = "Progress Michigan Action Rate") %>% 
    hc_subtitle(text = "Sources: FB Ad Manager, Twitter Ad Mananger, Google Ad Manager") %>% 
    hc_xAxis(categories = c(PM_media$Date)) %>% 
    hc_yAxis(title = list(text = "Action Rate (%)")) %>% 
    hc_plotOptions(line = list(
      dataLabels = list(enabled = FALSE),
      enableMouseTracking = TRUE)
    ) %>% 
    hc_series(
      list(
        name = "Faebook",
        data = as.numeric(sub("%" , "", PM_media$Facebook.Action))
      ),
      list(
        name = "Twiter",
        data = as.numeric(sub("%" , "", PM_media$Twitter.Action))
      ),
      list(
        name = "Google",
        data = as.numeric(sub("%" , "", PM_media$Google.Action))
      )
    ))

####################################PLOTLY############################################
LCV_Spend_Chart <-(
LCV_media %>% 
  plot_ly(x = ~Date) %>% 
  add_bars(y = ~Facebook.Impressions,
           name = "Facebook",
           marker = list(color = 'rgb(124,181,236)')) %>% 
  add_bars(y = ~Twitter.Impressions,
           name = "Twitter",
           marker = list(color = 'rgb(67,67,72)')) %>%
  add_bars(y = ~Google.Impressions,
           name = "Google",
           marker = list(color = 'rgb(144,237,125)')) %>%
  add_line(y = ~as.numeric(gsub('\\$|,', '',LCV_media$Spend)),
            name = "Spend",
            yaxis = "y2",
            marker = list(color = 'rgb(255,225,103)'),
            line = list(color = 'rgb(245,180,1)')) %>% 
  layout(title = "League of Conservation Voters Media Performance",
         yaxis = list(
           title = "Impression",
           titlefont = list(
             size = 16,
             color =  'rgb(107,107,107)'
           )
         ),
         barmode = "stack",
         yaxis2 = list(
           title = "Spend",
           titlefont = list(
             size = 16,
             color = 'rgb(107,107,107)'
           ),
           overlaying = "y",
          side = "right",
          tickprefix = "$"),
         barmode = "stack",
         legend = list(orientation = 'h'),
         autosize = F, width = 500, height = 500, margin = m)
)
LCV_Spend_Chart

LCV_media_p <- (
  LCV_media %>%
  plot_ly(x = ~Date) %>%
  add_lines(y = ~as.numeric(sub("%" , "", LCV_media$Facebook.Action)), name = "Facebook", mode = 'lines + markers',
           line = list(color = 'rgb(139,157,195)'),
           marker = list(color = 'rgb(139,157,195)')) %>%
  add_lines(y = ~as.numeric(sub("%" , "", LCV_media$Twitter.Action)), name = "Twitter", mode = 'lines + markers',
            line = list(color = 'rgb(29,161,243)'),
            marker = list(color = 'rgb(29,161,243)'))%>%
  add_lines(y =~as.numeric(sub("%" , "", LCV_media$Google.Action)), name = "Google", mode = 'lines + markers',
            line = list(color = 'rgb(228,116,105)'),
            marker = list(color = 'rgb(228,116,105)')) %>%
layout(title = "League of Conservation Voters Action Rate Per Channel",
       yaxis = list(
         title = 'Action Rate',
         titlefont = list(
           size = 16,
           color = 'rgb(107,107,107)'),
         ticksuffix = "%"
         ),
       legend = list(orientation = 'h')
       ))

LCV_media_p


PP_Spend_Chart <-(
  PP_media %>% 
    plot_ly(x = ~Date) %>% 
    add_bars(y = ~Facebook.Impressions,
             name = "Facebook",
             marker = list(color = 'rgb(124,181,236)')) %>% 
    add_bars(y = ~Twitter.Impressions,
             name = "Twitter",
             marker = list(color = 'rgb(67,67,72)')) %>%
    add_bars(y = ~Google.Impressions,
             name = "Google",
             marker = list(color = 'rgb(144,237,125)')) %>%
    add_lines(y = ~as.numeric(gsub('\\$|,', '',PP_media$Spend)),
              name = "Spend",
              yaxis = "y2",
              marker = list(color = 'rgb(255,222,39)'),
              line = list(color = 'rgb(245,180,1)')) %>% 
    layout(title = "Planned Parenthood Media Performance",
           autosize = F, width = 500, height = 500, margin = m,
           yaxis = list(
             title = "Impression",
             titlefont = list(
               size = 16,
               color =  'rgb(107,107,107)'
             )
           ),
           barmode = "stack",
           yaxis2 = list(
             title = "Spend",
             titlefont = list(
               size = 16,
               color = 'rgb(107,107,107)'
             ),
             overlaying = "y",
             side = "right",
             tickprefix = "$"),
           barmode = "stack",
           legend = list(
             orientation = "h"
           ) )
)
PP_Spend_Chart

PP_media_p <- (
  PP_media %>%
    plot_ly(x = ~Date) %>%
    add_lines(y = ~as.numeric(sub("%" , "", PP_media$Facebook.Action)), name = "Facebook", mode = 'lines + markers',
              line = list(color = 'rgb(139,157,195)'),
              marker = list(color = 'rgb(139,157,195)')) %>%
    add_lines(y = ~as.numeric(sub("%" , "", PP_media$Twitter.Action)), name = "Twitter", mode = 'lines + markers',
              line = list(color = 'rgb(29,161,243)'),
              marker = list(color = 'rgb(29,161,243)'))%>%
    add_lines(y =~as.numeric(sub("%" , "", PP_media$Google.Action)), name = "Google", mode = 'lines + markers',
              line = list(color = 'rgb(228,116,105)'),
              marker = list(color = 'rgb(228,116,105)')) %>%
    layout(title = "Planned Parenthood Action Rate Per Channel",
           yaxis = list(
             title = 'Action Rate',
             titlefont = list(
               size = 16,
               color = 'rgb(107,107,107)'),
             ticksuffix = "%"
           ),
           legend = list(orientation = 'h')
    ))
PP_media_p

PM_Spend_Chart <-(
  PM_media %>% 
    plot_ly(x = ~Date) %>% 
    add_bars(y = ~Facebook.Impressions,
             name = "Facebook",
             marker = list(color = 'rgb(139,157,195)')) %>% 
    add_bars( y = ~Twitter.Impressions,
             name = "Twitter",
             marker = list(color = 'rgb(29,161,243)')) %>%
    add_bars(y = ~Google.Impressions,
             name = "Google",
             marker = list(color = 'rgb(228,116,105)')) %>%
    add_lines( y = ~as.numeric(gsub('\\$|,', '',PM_media$Spend)),
              name = "Spend",
              yaxis = "y2",
              marker = list(color = 'rgb(255,225,103)'),
              line = list(color = 'rgb(245,180,1)')) %>% 
    layout(title = "Progress Michigan Media Performance",
           autosize = F, width = 500, height = 500, margin = m,
           yaxis = list(
             title = "Impression",
             titlefont = list(
               size = 16,
               color =  'rgb(107,107,107)'
             )
           ),
           barmode = "stack",
           yaxis2 = list(
             title = "Spend",
             titlefont = list(
               size = 16,
               color = 'rgb(107,107,107)',
               side = "right"
               
             ),
            overlaying = "y",
             side = "right",
             tickprefix = "$",
            range = c(-2,2)),
           barmode = "stack",
           legend = list(orientation = 'h'),
           autosize = F, width = 500, height = 500, margin = m)
)
PM_Spend_Chart

PM_media_p <- (
  PM_media %>%
    plot_ly(x = ~Date) %>%
    add_lines(y = ~as.numeric(sub("%" , "", PM_media$Facebook.Action)), name = "Facebook", mode = 'lines + markers',
              line = list(color = 'rgb(139,157,195)'),
              marker = list(color = 'rgb(139,157,195)')) %>%
    add_lines(y = ~as.numeric(sub("%" , "", PM_media$Twitter.Action)), name = "Twitter", mode = 'lines + markers',
              line = list(color = 'rgb(29,161,243)'),
              marker = list(color = 'rgb(29,161,243)'))%>%
    add_lines(y =~as.numeric(sub("%" , "", PM_media$Google.Action)), name = "Google", mode = 'lines + markers',
              line = list(color = 'rgb(228,116,105)'),
              marker = list(color = 'rgb(228,116,105)')) %>%
    layout(title = "Progress Michigan Action Rate Per Channel",
           yaxis = list(
             title = 'Action Rate',
             titlefont = list(
               size = 16,
               color = 'rgb(107,107,107)'),
             ticksuffix = "%"
           ),
           legend = list(orientation = 'h'),
           autosize = TRUE
    ))
PM_media_p









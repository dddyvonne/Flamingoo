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



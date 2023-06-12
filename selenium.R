
# https://adstransparency.google.com/advertiser/AR09355418985304162305?political&region=NL&preset-date=Last%207%20days

library(tidyverse)
library(netstat)
library(RSelenium)
# port <- netstat::free_port()
podf <- sample(4000L:5000L,1)
rD <- rsDriver(browser = "firefox"
                    ,chromever=NULL
                ,check = F
                ,port = podf
                ,verbose = T
)


library(rvest)

remDr <- rD$client

# remDr$navigate("https://adstransparency.google.com/political?political&region=FI&preset-date=Last%2030%20days")

# thth <- remDr$getPageSource() %>% .[[1]] %>% read_html()
# 
# 
# tb <- thth %>% 
#   html_nodes(xpath = "/html/body/div[5]/root/political-page/insights-grid/div/div/top-advertisers/widget/div[4]/div/div") %>% 
#   html_children() 
# 
# advertiser_name <- tb %>% 
#   html_nodes(".left-column") %>% 
#   html_text()
# 
# spend <- tb %>% 
#   html_nodes(".right-column") %>% 
#   html_text()
# 
# top30spenders<-tibble(advertiser_name, spend)
# 
# saveRDS(top30spenders, file="data/top30spenders.rds")
# 
# chatfin<-read_csv("data/chatfin.csv") 
# chatfin %>% 
#   count(likely_political_party)
# 
#     select(advertiser_name) %>% clipr::write_clip()
#   dput()
# ggl_spend

retrieve_spend <- function(id, days = 30) {

    # id <- "AR18091944865565769729"
    url <- glue::glue("https://adstransparency.google.com/advertiser/{id}?political&region=GR&preset-date=Last%20{days}%20days")
    remDr$navigate(url)

    Sys.sleep(1)

    thth <- remDr$getPageSource() %>% .[[1]] %>% read_html()

    Sys.sleep(3)
    
    root5 <- "/html/body/div[3]" 
    root3 <- "/html/body/div[5]" 
    ending <- "/root/advertiser-page/political-tabs/div/material-tab-strip/div/tab-button[2]/material-ripple"

    try({
      insights <<- remDr$findElement(value = paste0(root5, ending))
      it_worked <- T
    })
    
    if(!exists("it_worked")){
      
      print("throwed an error")
      
      try({
        insights <<- remDr$findElement(value = paste0(root3, ending))
        
      })
      
      root <- root3
      
    } else {
      root <- root5
    }
    
    print("click now")
    insights$clickElement()

    Sys.sleep(3)

    pp <- remDr$getPageSource() %>% .[[1]] %>% read_html()
    
    ending_eur <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[1]/div"
    ending_ads <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[3]/div"
    
    print("retrieve numbers")
    # try({
    eur_amount <- pp %>%
        html_elements(xpath = paste0(root, ending_eur)) %>%
        html_text()
    
    num_ads <- pp %>%
        html_elements(xpath = paste0(root, ending_ads)) %>%
        html_text()
    
    # })
    
    fin <- tibble(advertiser_id = id, eur_amount, num_ads)
    
    print(fin)

    return(fin)

}

ggl_spend <- readRDS("data/ggl_spend.rds")

# retrieve_spend(unique(ggl_spend$Advertiser_ID)[1])
# fvd <- retrieve_spend("AR03397262231409262593")



ggl_sel_sp <- unique(ggl_spend$Advertiser_ID) %>%
    map_dfr_progress(retrieve_spend)

# ggl_sel_sp %>% 
  # filter(advertiser_id %in% "AR09355418985304162305")
# 
# # ggl_spend %>% 
#   # filter(Advertiser_ID %in% "AR09355418985304162305")
# 
ggl_sel_sp$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .)

  # filter(!(advertiser_id %in% unique(ggl_spend$Advertiser_ID)))

# ggl_sel_sp <- ggl_sel_sp %>%
# bind_rows(fvd) %>%
# distinct(advertiser_id, .keep_all = T)


saveRDS(ggl_sel_sp, file = "data/ggl_sel_sp.rds")

ggl_sel_sp7 <- unique(ggl_spend$Advertiser_ID) %>%
  map_dfr_progress(retrieve_spend, 7)

ggl_sel_sp7$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .)
# missssings <- ggl_sel_sp7$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .) %>%
#   map_dfr_progress(retrieve_spend, 7)

# ggl_sel_sp7 <- ggl_sel_sp7 %>%
#   bind_rows(missssings) %>%
#   distinct(advertiser_id, .keep_all = T)


saveRDS(ggl_sel_sp7, file = "data/ggl_sel_sp7.rds")



retrieve_spend_daily <- function(id, the_date) {

  # id <- "AR18091944865565769729"
  url <- glue::glue("https://adstransparency.google.com/advertiser/{id}??political&region=GR&start-date={the_date}&end-date={the_date}&topic=political")
  remDr$navigate(url)

  Sys.sleep(1)

  thth <- remDr$getPageSource() %>% .[[1]] %>% read_html()

  Sys.sleep(3)

  root3 <- "/html/body/div[3]"
  root5 <- "/html/body/div[5]"
  ending <- "/root/advertiser-page/political-tabs/div/material-tab-strip/div/tab-button[2]/material-ripple"

  try({
    insights <<- remDr$findElement(value = paste0(root5, ending))
    it_worked <- T
  })

  if(!exists("it_worked")){

    print("throwed an error")

    try({
      insights <<- remDr$findElement(value = paste0(root3, ending))

    })

    root <- root3

  } else {
    root <- root5
  }

  print("click now")
  insights$clickElement()

  Sys.sleep(3)

  pp <- remDr$getPageSource() %>% .[[1]] %>% read_html()

  ending_eur <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[1]/div"
  ending_ads <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[3]/div"

  print("retrieve numbers")
  # try({
  eur_amount <- pp %>%
    html_elements(xpath = paste0(root, ending_eur)) %>%
    html_text()

  num_ads <- pp %>%
    html_elements(xpath = paste0(root, ending_ads)) %>%
    html_text()

  # })

  fin <- tibble(advertiser_id = id, eur_amount, num_ads, date = the_date)

  print(fin)

  return(fin)

}

# daily_spending <- readRDS("data/daily_spending.rds")
# Apr 17, 2023 - May 16, 2023
  # 13 February 2023
  timelines <- seq.Date(as.Date("2023-05-19"), as.Date("2023-05-19"), by = "day")
  
  daily_spending <- expand_grid(unique(ggl_spend$Advertiser_ID), timelines) %>%
    set_names(c("advertiser_id", "timelines")) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(~{retrieve_spend_daily(.x$advertiser_id, .x$timelines)})
  # 
# daily_spending <- daily_spending %>%
#   bind_rows(missings) %>%
#   distinct(advertiser_id, date, .keep_all = T)
  # daily_spending2
saveRDS(daily_spending %>% bind_rows(daily_spending2), file = "data/daily_spending.rds")

# retrieve_spend_daily("AR09355418985304162305", "2023-03-01")

# missings <- expand_grid(unique(ggl_spend$Advertiser_ID), timelines) %>%
#   set_names(c("advertiser_id", "timelines")) %>%
#   anti_join(daily_spending %>% rename(timelines = date))  %>%
#   split(1:nrow(.)) %>%
#   map_dfr_progress(~{retrieve_spend_daily(.x$advertiser_id, .x$timelines)})

# retrieve_spend_daily("AR18177962546424709121", "2023-03-14")


timelines <- seq.Date(as.Date("2023-04-17"), as.Date("2023-05-16"), by = "day")

daily_spending <- expand_grid(unique(ggl_spend$Advertiser_ID), timelines) %>%
  set_names(c("advertiser_id", "timelines")) %>%
  split(1:nrow(.)) %>%
  map_dfr_progress(~{retrieve_spend_daily(.x$advertiser_id, .x$timelines)})
# 
# daily_spending <- daily_spending %>%
#   bind_rows(missings) %>%
#   distinct(advertiser_id, date, .keep_all = T)

saveRDS(daily_spending, file = "data/daily_spending.rds")

retrieve_spend_custom <- function(id, from, to) {
  
  # id <- "AR18091944865565769729"
  url <- glue::glue("https://adstransparency.google.com/advertiser/{id}?political&region=GR&start-date={from}&end-date={to}")
  remDr$navigate(url)
  
  Sys.sleep(1)
  
  thth <- remDr$getPageSource() %>% .[[1]] %>% read_html()
  
  Sys.sleep(3)
  
  root3 <- "/html/body/div[3]"
  root5 <- "/html/body/div[5]"
  ending <- "/root/advertiser-page/political-tabs/div/material-tab-strip/div/tab-button[2]/material-ripple"
  
  try({
    insights <<- remDr$findElement(value = paste0(root5, ending))
    it_worked <- T
  })
  
  if(!exists("it_worked")){
    
    print("throwed an error")
    
    try({
      insights <<- remDr$findElement(value = paste0(root3, ending))
      
    })
    
    root <- root3
    
  } else {
    root <- root5
  }
  
  print("click now")
  insights$clickElement()
  
  Sys.sleep(3)
  
  pp <- remDr$getPageSource() %>% .[[1]] %>% read_html()
  
  ending_eur <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[1]/div"
  ending_ads <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[3]/div"
  
  print("retrieve numbers")
  # try({
  eur_amount <- pp %>%
    html_elements(xpath = paste0(root, ending_eur)) %>%
    html_text()
  
  num_ads <- pp %>%
    html_elements(xpath = paste0(root, ending_ads)) %>%
    html_text()
  
  # })
  
  
  ending_type <- "/root/advertiser-page/insights-grid/div/div/ad-formats/widget/div[4]"
  
  
  type_spend <<- pp %>%
    html_elements(xpath = paste0(root, ending_type)) %>%
    html_children() %>%
    html_text() %>%
    tibble(raww = .) %>%
    mutate(type = str_to_lower(str_extract(raww, "Video|Text|Image"))) %>%
    mutate(raww = str_remove_all(raww, "Video|Text|Image") %>% str_remove_all("%|\\(.*\\)") %>% as.numeric) %>%
    pivot_wider(names_from = type, values_from = raww)
  
  
  fin <- tibble(advertiser_id = id, eur_amount, num_ads, from, to)
  
  if(nrow(type_spend)!=0){
    fin <- fin %>%
      bind_cols(type_spend)
  }
  
  
  
  print(fin)
  
  return(fin %>% mutate_all(as.character))
  
}



ggl_sel_sp <- unique(ggl_spend$Advertiser_ID) %>%
  # .[22] %>%
  map_dfr_progress(~{retrieve_spend_custom(.x, "2023-04-22", "2023-05-21")})

# ggl_sel_sp %>%
# filter(advertiser_id %in% "AR09355418985304162305")
#
# # ggl_spend %>%
#   # filter(Advertiser_ID %in% "AR09355418985304162305")
#
misssss <- ggl_sel_sp$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .)
# filter(!(advertiser_id %in% unique(ggl_spend$Advertiser_ID)))

# ggl_sel_sp <- ggl_sel_sp %>%
# bind_rows(fvd) %>%
# distinct(advertiser_id, .keep_all = T)

# fvd <- retrieve_spend("AR03397262231409262593")
fvd <- misssss %>%
  # .[22] %>%
  map_dfr_progress(~{retrieve_spend_custom(.x, "2023-04-20", "2023-05-19")})

ggl_sel_sp <- ggl_sel_sp %>%
  bind_rows(fvd) %>%
  distinct(advertiser_id, .keep_all = T)


saveRDS(ggl_sel_sp, file = "data/ggl_sel_sp.rds")


ggl_sel_sp7 <- unique(ggl_spend$Advertiser_ID) %>%
  # .[22] %>%
  map_dfr_progress(~{retrieve_spend_custom(.x, "2023-05-14", "2023-05-21")})

misssss7 <- ggl_sel_sp7$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .)

misss <- retrieve_spend_custom("AR14725485108811268097", "2023-05-11", "2023-05-17")

misss <- misssss7 %>%
  # .[22] %>%
  map_dfr_progress(~{retrieve_spend_custom(.x, "2023-05-11", "2023-05-17")})


saveRDS(ggl_sel_sp7 %>% bind_rows(misss)%>%
          distinct(advertiser_id, .keep_all = T), file = "data/ggl_sel_sp7.rds")




# https://adstransparency.google.com/advertiser/AR09355418985304162305?political&region=NL&preset-date=Last%207%20days

library(tidyverse)
library(netstat)
library(RSelenium)
# port <- netstat::free_port()
podf <- sample(4000L:5000L,1)
rD <- rsDriver(browser = "firefox"
               ,chromever=NULL
               ,check = F
               ,port = podf
               ,verbose = T
)


library(rvest)

remDr <- rD$client


remDr$navigate("https://www.facebook.com/ads/library/report")


current_url <- remDr$getCurrentUrl()

library(httr)
# Send a GET request to the current URL
response <- GET(current_url[[1]])

# Check the response status code
status_code <- status_code(response)
print(status_code)

country_sel <- remDr$findElement(using = "css",'#ReportDownload div a')

country_sel$click()



library(RSelenium)
library(httr)

startBackfillingFacebook <- function(date, country) {
  # Start the Selenium server and create a remote driver
  driver <- rsDriver(browser = "firefox", port = 4567L)
  remote <- driver$client
  
  # Visit the Facebook Ads Library Report page
  remote$navigate("https://www.facebook.com/ads/library/report")
  
  # Wait for the page to load
  Sys.sleep(5)
  
  # Generate the download URL for the report from yesterday
  downloadURL <- paste0("https://www.facebook.com/ads/library/report/v2/download/?report_ds=", date, "&country=", country, "&time_preset=yesterday")
  
  # Click on the download link to initiate the download
  downloadLink <- remDr$findElement(using = "css", value = "#ReportDownload div a")
  downloadLink$clickElement()
  
  # Wait for the download to complete
  Sys.sleep(10)
  
  # Get the downloaded file name
  file_name <- downloadLink$getElementAttribute("download")[[1]]
  
  # Get the file URL
  file_url <- remote$getCurrentUrl()[[1]]
  
  # Download the file
  download_path <- paste0(getwd(), "/", file_name)
  GET(file_url, write_disk(download_path, overwrite = TRUE))
  
  # Close the remote driver and stop the Selenium server
  remote$close()
  driver$server$stop()
  
  # Return the path to the downloaded file
  return(download_path)
}

# Usage example
date <- "2023-01-19"
country <- "US"
downloadedFile <- startBackfillingFacebook(date, country)


date_range_element <- remDr$findElement(using = "css selector", "#js_9q") # Replace with the actual CSS selector or XPath for the date range dropdown
date_range_element$clickElement()


# Find the end date dropdown menu and select the desired end date
end_date_element <- remDr$findElement(using = "css selector", "#js_fc") # Replace with the actual CSS selector or XPath for the end date dropdown
end_date_element$clickElement()


# Click the "Download Report" button
# Click the "Download Report" button
download_button <- remDr$findElement(using = "xpath", "//a[contains(., 'Download Report')]")
download_button$clickElement()

library(httr)
library(jsonlite)

# Define the form data and headers
form_data <- list(
  "__user" = "0",
  "__a" = "1",
  "__dyn" = "",
  "__req" = "1",
  "__be" = "1",
  "__pc" = "PHASED:DEFAULT",
  "dpr" = "1",
  "__rev" = "",
  "__s" = "",
  "lsd" = "",
  "jazoest" = ""
)

headers <- c(
  'accept' = '*/*',
  'accept-encoding' = '',
  'accept-language' = 'en-US,en;q=0.9,fr;q=0.8',
  'content-length' = as.character(length(form_data)+1),
  'content-type' = 'application/x-www-form-urlencoded',
  'cookie' = 'datr=; fr=; wd=',
  'origin' = 'https://www.facebook.com',
  'referer' = 'https://www.facebook.com/ads/library/report/?source=archive-landing-page&country=FR',
  'user-agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/74.0.3729.169 Chrome/74.0.3729.169 Safari/537.36'
)

# Define the fetch function
fetch_endpoint <- function(endpoint, date, country_code, time_preset = NULL) {
  # Construct the URL based on the endpoint
  if (endpoint == 'lifetime_data') {
    url <- paste0("https://www.facebook.com/ads/library/report/async/lifetime_data/?report_ds=", date, "&country=", country_code)
  } else if (endpoint == 'location_data') {
    url <- paste0("https://www.facebook.com/ads/library/report/async/location_data/?report_ds=", date, "&country=", country_code, "&time_preset=", time_preset)
  } else if (endpoint == 'advertiser_data') {
    url <- paste0("https://www.facebook.com/ads/library/report/async/advertiser_data/?report_ds=", date, "&country=", country_code, "&time_preset=", time_preset, "&sort_column=spend&sort_descending=true&q=")
  } else if (endpoint == 'download') {
    asdd <<- paste0("https://www.facebook.com/ads/library/report/v2/download/?report_ds=", date, "&country=", country_code, "&time_preset=", time_preset)
  } else {
    stop('Unknown endpoint')
  }
  
  # Make the HTTP POST request
  response <- POST(url, body = form_data, add_headers(headers))
  
  # Parse the JSON response
  data <- fromJSON(content(response, "text"))
  
  return(data)
}

asd <- fetch_endpoint("download", "2023-01-20", "US")

response <- POST(asdd, body = form_data, add_headers(headers))
data <- fromJSON(content(response, "text"))
headers(response)


sd <- httr::POST("https://www.facebook.com/ads/library/report/v2/download/?report_ds=2019-10-14&country=US&time_preset=yesterday")
content(sd) %>% as.character()

remDr$navigate("https://www.facebook.com/ads/library/report/v2/download/?report_ds=2019-10-14&country=US&time_preset=yesterday")

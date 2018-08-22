
install.packages("data.table")
install.packages("stringi")
install.packages("stringr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("VIM")
library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyselect)
library(tidyr)
library(VIM)


if (!exists("crime_raw")) {
  tryCatch(crime_raw <- read.csv("Crimes_-_2001_to_present.csv", stringsAsFactors = FALSE), 
           error = function(x) {
             print("File not in WD. Downloading from the source.")
             #crime_url <- "https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv?accessType=DOWNLOAD"
             crime_raw <- read.csv("crime_url")
             write.csv(crime_raw, "crime_raw.csv")
           }
  )
}



community_url <- "https://data.cityofchicago.org/api/views/igwz-8jzy/rows.csv?accessType=DOWNLOAD"
if (!exists("community_area_df")){community_area_df <- read.csv(community_url)}
community_area_key <- dplyr::select(community_area_df, c(AREA_NUMBE, COMMUNITY))

#munge data
if (!exists("crime_df")){
  crime_df <- 
    crime_raw %>% 
    data.table() %>% 
    filter(Year < 2018) %>% 
    #sample_n(500000) %>% 
    left_join(community_area_key, 
              by = c("Community.Area" = "AREA_NUMBE")) %>% 
    mutate(
      DateTime = lubridate::mdy_hms(as.character(Date)),
      Date_rw = as.Date.POSIXct(DateTime),
      Year = as.ordered(Year),
      Month = month(DateTime, label = T),
      DayOfMonth = as.ordered(day(DateTime)),
      DayOfWeek = wday(DateTime, label = T),
      Hour = as.ordered(hour(DateTime)),
      Crime.Type = as.factor(
        ifelse(Primary.Type %in% c("ROBBERY", "BURGLARY", 
                                   "CRIM SEXUAL ASSAULT", "HOMICIDE", "HUMAN TRAFFICKING",
                                   "BATTERY", "SEX OFFENSE", "KIDNAPPING", 
                                   "DOMESTIC VIOLENCE", "ASSAULT", "ARSON", "INTIMIDATION"), 
               "Violent", "Nonviolent"))
    ) %>% 
    dplyr::select(
      c(Crime = Primary.Type, Crime.Type, Crime.Description = Description, Arrest, Domestic, 
        DateTime, Date = Date_rw, Year, Month, DayOfMonth, DayOfWeek, Hour,
        Community.Area = COMMUNITY, Location.Description))  
} 

missing_plot <- VIM::aggr(crime_df,  
                          numbers = T, 
                          sortVars = T,
                          col = c("lightgreen", "darkred", "orange"),
                          labels=str_sub(names(crime_df), 1, 8), 
                          ylab=c("Missing Value Counts", "Pattern"))

#the gather function is used when there are columns that are not variables.
# cat_value_freq <-  
#   crime_df %>% 
#   select_if(is.factor) %>% 
#   select_if(function(x) !is.ordered(x)) %>% 
#   gather("var", "value") %>% 
#   group_by(var) %>% 
#   count(var, value) %>%
#   mutate(prop = prop.table(n)) %>% 
#   filter(prop > .02)
# 
# cat_plot1 <-
#   ggplot(data = cat_value_freq,
#          aes(x = reorder(stringr::str_wrap(value, 20), prop),
#              y = prop)) +
#   geom_bar(stat = "identity", fill = "tomato3") +
#   coord_flip() +
#   facet_wrap(~var, ncol = 3, scales = "free") +
#   ggthemes::theme_fivethirtyeight()
# 
# cat_plot1

# treemap_df <-
#   crime_df %>%
#   dplyr::filter(Crime.Type == "Violent") %>% 
#   group_by(Crime, Crime.Description) %>%
#   summarize(n = n())
# 
# treemap(treemap_df, 
#         index=c("Crime","Crime.Description"), 
#         vSize="n", 
#         type="index",
#         fontsize.labels=c(15,12),
#         fontcolor.labels=c("white","orange"),
#         fontface.labels=c(2,1), 
#         bg.labels=c("transparent"),
#         align.labels=list(
#           c("center", "center"), 
#           c("center", "top")
#         ),                                 
#         overlap.labels=0.2,                     
#         inflate.labels=F
# )

total_crime_17years <-
  crime_df %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(Year = as.integer(as.character(Year))) %>% 
  group_by(Crime.Type, Year) %>% 
  summarize(reported_incidents = n()) 

trend_plot <- 
  ggplot(data = total_crime_17years,
         aes(x = Year, y = reported_incidents, fill = Crime.Type)) +
  geom_area() +
  scale_y_continuous(name = "Reported Incidents", labels = scales::comma)

trend_plot

ggplotly(trend_plot)


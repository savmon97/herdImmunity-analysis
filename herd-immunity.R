#----------------------------------------
# U.S. Herd-immunity Prediction by County
# By: Salomon Villatoro
# Date 4/26/2021
# GAFL 531
#----------------------------------------

# Packages Necessary
  library(tidyverse)
  library(stargazer)
  library(rgdal)
  library(leaflet)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(tidycensus)
  library(ggthemes)
  library(splines)
  library(modelr)
  library(leaflet.providers)

#-------------------------------
# R naught by state calculation
#-------------------------------
  
  # Global R0 as calculated by Billah et. al. 2020
  # NOTE: Not every county will have the same R0 as this
  #       number is calculated through a variety of measures
  r0 <- 2.87
  herdImmunity <- (1-(1/r0))*100
  
#--------------------------------------------------
# Read in necessary Data Sets (UPDATED: 4.25.2021)
#--------------------------------------------------
  # obtaining Texas population estimates (jan 1, 2020)
  pop_tx <- read.csv("county2019.csv") %>% 
    select(county, "pop" = jan1_2020_pop_est) %>% 
    filter(county != "State of Texas") 
  pop_tx$county[pop_tx$county =="De Witt"] <- "DeWitt"
    
  txPop <- sum(pop_tx$pop)
  # Texas recovered population percentage
  recovered <- (2722233/txPop)*100
  
  # preparing vaccination data
  # compiling list of excel sheets to be used
  temp <- list.files(path = "AccessibleVaccineDashboardData" , pattern = '/*.xlsx')
  
  #compiling dates of data collection
  dates <- substr(temp, 1,10)
  
  #creating vaccine table
  vaccine <- read_excel(paste0("AccessibleVaccineDashboardData/", temp[1]),
                        sheet = 2, na = "--") %>% 
    filter(`County Name` != 'Federal Long-Term Care Vaccination Program' & 
             `County Name` != 'Federal Pharmacy Retail Vaccination Program') %>% 
    select(1) %>% filter(`County Name` != "Texas")
  
  #looping through 100 files to obtain vaccine data 
  # obtaining number of people who are fully vaccinated
  for (i in seq(length(temp))) {
    x <- read_excel(paste0("AccessibleVaccineDashboardData/", temp[i]),
                          sheet = 2, na = "--") %>% 
      filter(`County Name` != 'Federal Long-Term Care Vaccination Program' & 
               `County Name` != 'Federal Pharmacy Retail Vaccination Program') %>%
      select(`County Name`, `People Fully Vaccinated`)
    vaccine <- merge(vaccine, x, by = "County Name")
  }
  colnames(vaccine) <- c("county", dates)
  
  #tidying vaccine data for county
  vaccine <- pivot_longer(vaccine,!county, names_to = "date", values_to = "fully")%>% 
    mutate(date = ymd(date)) %>% 
    mutate(fully = as.integer(fully)) %>% 
    left_join(pop_tx, by = "county") %>% 
    mutate("full_pct" = (fully/pop)*100)
  
  #obtaining currently vaccinated table
  full_vac <- vaccine %>% 
    filter(date == ymd("2021-04-25")) %>% 
    select(county,full_pct)
  
  # preparing Covid Data
  txCovCases_county <- read_excel("Texas COVID-19 Case Count Data by County.xlsx", 
                                  col_names = FALSE)
  txCovCases_county <- txCovCases_county[-c(1,2,258,259,260, 261,262),]
  getDate <- function (x) {
    for (i in seq(ncol(x))) {
      if (x[1,i] != "County Name") {
        x[1,i] <- substr(x[1,i], 7, 16)
      } else {
        x[1,i] <- 'county'
      }
    }
    return(x)
  }
  cases <- getDate(txCovCases_county) %>% 
    row_to_names(row_number = 1) %>% 
    pivot_longer(!county, names_to = "date", values_to = 'cases') %>% 
    mutate(date = mdy(date)) %>% 
    mutate(cases = as.integer(cases)) %>% 
    left_join(pop_tx, by = "county") %>% 
    mutate("full_pct" = (cases/pop)*100)
  
#--------------------------------------------------
# Making the Plot
#--------------------------------------------------  
  #test variables
  hco <- cases %>%  filter(grepl('^Presidio$', county))
  hvac <- vaccine %>% filter(grepl('^Presidio$', county))
  hvac1 <- hvac %>% tail(14)
  range <-c(as.Date("2021-1-01"), as.Date("2022-12-31"))
  
  #modeling if the current rate of vaccination continues increasing
  m1 <- lm(full_pct ~ date, data = hvac1)
  
  
  
  #predicted data set for binomial model
  hvac2 <-hvac1 %>% 
    data_grid(date = seq.Date(as.Date("2021-04-25"),as.Date("2150-12-31"), 1)) %>% 
    add_predictions(m1, "full_pct")
  #getting percent fix from model to make predicted data transition smoothly from regression
  fix <- hvac$full_pct[111]-hvac2$full_pct[1]
  hvac2 <- hvac2 %>%
    mutate(pct_fix = full_pct + fix)
  
  #obtaining date of herd immmunity
  immDay <- hvac2[[which(abs(hvac2[3]-herdImmunity)== min(abs(hvac2[3]-herdImmunity))),1]]
    
  immunity<- ggplot(hvac, mapping = aes(x = date, y = full_pct)) +
    geom_smooth(method = "lm", formula = y~bs(x,3), size = 1.5)+
    geom_line(data = hvac2,
              mapping = aes(x = date, y = pct_fix),
              color = "red", 
              size = 1.5) +
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
    scale_x_date(limits = range, 
                 date_breaks = "3 month",
                 date_label = "%b %Y",
                 expand = c(0,0))+
    theme_fivethirtyeight()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    geom_hline(yintercept=herdImmunity, linetype='dotted', col = 'red', size = 1) +
    annotate("text", x = as.Date("2022-10-15"), 
             y = herdImmunity, 
             label = "Herd Immunity", 
             vjust = -0.5) +
    geom_vline(xintercept=as.Date("2021-04-25"), 
               linetype='dotted', col = 'red', size = 1) +
    annotate("text", y = 80, 
             x = as.Date("2021-02-28"), 
             label = "Current Date", 
             vjust = -0.5) +
    annotate("text", y = herdImmunity, x = immDay,
             label = paste0("Est. Date: ",immDay),
             hjust = -0.25,
             vjust = -4)
  immunity
  

  
#--------------------------------------------------
# Making the Texas map
#--------------------------------------------------
  # make a table with predicted date for herd immunity
  counties_all <- unique(vaccine$county)
  herd_table <- tibble(county = counties_all)
  cell1 = 0
  cellT = c()
  for (i in counties_all) {
    cell1 <- cell1 + 1
    gstr <- paste0("^",i,"$")
    vac <- vaccine %>% 
      filter(grepl(gstr, county))
    vac <- vac %>% tail(14)
    model1 <- lm(full_pct ~ date, data = vac)
    vac1 <- vac %>% 
      data_grid(date = seq.Date(as.Date("2021-04-25"),as.Date("2150-12-31"), 1)) %>% 
      add_predictions(model1, "full_pct")
    fix1 <- vac$full_pct[14]-vac1$full_pct[1]
    vac1 <- vac1 %>%
      mutate(pct_fix = full_pct + fix1)
    cell <- vac1[[which(abs(vac1[3]-herdImmunity)== min(abs(vac1[3]-herdImmunity))),1]]
    cellT<- append(cellT,cell)
  }
  herd_table <- herd_table %>% 
    mutate(date = cellT) %>% 
    mutate(day_num = as.double(difftime(ymd(cellT),
                              ymd(Sys.Date()),
                              units = 'days'))) %>% 
    mutate(day_num = if_else(day_num < 0, 0, day_num)) %>%
    mutate(day_num = na_if(day_num,0))
    
  
  # read shape files onto object 
  texas <- readOGR("texascounties", layer = "County", encoding = "UTF-8")
  names(texas@data)
  
  #create palette to use
  pal <- colorNumeric(
    palette = colorRampPalette(c('green','yellow', 'orange','red'), bias = 2.5)(length(counties_all)), 
    domain = herd_table$day_num)
  
  #fixing missing county
  texas@data$CNTY_NM[texas@data$CNTY_NM=="De Witt"] <- "DeWitt"
  
  #match data to shape file
  texas@data <- data.frame(texas@data, 
                           herd_table[match(texas@data$CNTY_NM, 
                                            herd_table$county),]) %>%
    left_join(pop_tx, by = "county") %>% 
    left_join(full_vac, by = "county")
  
  tx_popup <- paste("<b style='font-weight: 900;'>",texas@data$CNTY_NM,
                    "County </b>",
                    "<br>",
                    "Population: ", texas@data$pop,
                    "<br>",
                    "% Vaccinated (as of 4-25-2021): ", round(texas@data$full_pct,2)," %",
                    "<br>",
                    "<b style='font-weight: 900;'>"
                    "Days from 04-25-2021 until herd immunity",
                    "<br>",texas@data$day_num,"days",
                    "</b>"
                    )
  # set up texas leaflet
  texas_map <- leaflet(texas) %>% 
    clearBounds() %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    addPolygons(stroke = TRUE, 
                color = "#c4c4c4", 
                weight = 1.5, 
                smoothFactor = 0.1, #
                fillOpacity = 1, #### the opacity (how see through) the shape is
                fillColor = ~pal(day_num), ####### how to fill which zipcode by what information
                popup = tx_popup)# %>% ######## allow pop ups to show up
    # addLegend("bottomright", ####location of legend in leaflet app
    #           colors = c("#ffe4a8","#ffa45e","#eb7373","#bf3041"), #### legend colors to match zip code colors
    #           labels = c("0 - 24", "25 - 49", "50 - 74", "75 - 98"),#### Bin percentage ranges
    #           title = "Risk: Lowest to Highest", ###### legend title
    #           opacity = 1) #### whether the legend will be see through
  texas_map
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
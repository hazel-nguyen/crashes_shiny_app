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
library(ggthemes)
library(patchwork)
library(scales)
library(lubridate)
library(gridExtra)
library(maps)
library(RColorBrewer)
library(plotly)
library(stringr)
library(stringi)
library(ggpubr)
library(ggmosaic)
library(shinyWidgets) 

########## Importing Datasets from remote server ###################################
load(url("https://santoshrajkumar.com/files_public/sta504/santosh_tab.RData"))
load(url("https://santoshrajkumar.com/files_public/sta504/accident_data_filtered.RData"))
load(url("https://santoshrajkumar.com/files_public/sta504/hosp_pop.RData"))
load(url("https://santoshrajkumar.com/files_public/sta504/crash_time_data.RData"))
load(url("https://santoshrajkumar.com/files_public/sta504/vehicle.RData"))
############ Data Import ends ###########################################################

#################################### Pre-processing of data begins here ###########################
###################################################################################################

########## Arranging day of week & time as factors of accident_data ##################
accident_data_filtered <- accident_data_filtered %>% 
  mutate(day_weekname = factor(day_weekname, 
                               levels= c("Monday", "Tuesday",
                                         "Wednesday","Thursday","Friday",
                                         "Saturday", "Sunday"))) %>% 
  mutate(hourname = factor(hourname, levels= c(
                          "6:00pm-6:59pm","7:00pm-7:59pm","8:00pm-8:59pm",
                           "9:00pm-9:59pm","10:00pm-10:59pm","11:00pm-11:59pm",
                           "0:00am-0:59am","1:00am-1:59am","2:00am-2:59am",
                           "3:00am-3:59am", "4:00am-4:59am","5:00am-5:59am",
                           "6:00am-6:59am","7:00am-7:59am","8:00am-8:59am",
                           "9:00am-9:59am","10:00am-10:59am","11:00am-11:59am",
                           "12:00pm-12:59pm","1:00pm-1:59pm","2:00pm-2:59pm",
                           "3:00pm-3:59pm","4:00pm-4:59pm","5:00pm-5:59pm"
                           
                           )))


############# Filtering necessary parts of vehicle data ############################
vehicle2 <- vehicle2 %>% 
  # create date element
  mutate(ev_date = ymd(paste0(caseyear, "-", month, "-", day))) %>%
  select(ev_date, caseyear, month, monthname, day, hourname, statename, harm_ev, harm_evname, 
         make, makename, model, modelname, mak_mod, mak_modname, 
         body_typ, body_typname, hit_run, hit_runname, trav_sp, trav_spname) 
## extracting varibale choices for vehicles tab
vyear <- vehicle2$caseyear %>% unique() 
vstate <- vehicle2$statename %>% unique() 
vbody <- vehicle2$body_typname %>% unique() 
vmake <- vehicle2$makename %>% unique() 

#################################### Pre-processing of data ends here ###########################
###################################################################################################


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Macadamias : US Road Crash & Fatalities Data"),
    fluidRow(
      ## The variable selectors
      column(12, 
            
             ##First tab sidebar 
            conditionalPanel(condition = "input.tabs == 'Country Wide'",
                  column(3,       
                    selectInput(inputId="state", label="Select State(s) to Highlight",
                                          choices=state_pop$statename, multiple=TRUE))
                  ,
                  column(3,            
                  dateRangeInput(inputId = "dates_states", label="Pick date range",
                                          start=mdy("01/01/2015"), end=mdy("12/31/2019")))
                  ,
                  
             ),
            
            ##2nd tab sidebar 
             conditionalPanel(condition = "input.tabs == 'Factors'",
                  column(3,            
                  selectInput(inputId="statef", label="Select State(s) to Highlight",
                                          choices=state_pop$statename, multiple=TRUE))
                  ,
                  column(3,              
                  dateRangeInput(inputId = "dates_statesf", label="Pick date range",
                                          start=mdy("01/01/2015"), end=mdy("12/31/2019")))
                  ,
             ),
            
            ##3rd tab sidebar
            conditionalPanel(condition = "input.tabs == 'Time Plots'",
                  column(3,
                  selectInput(inputId="stateg", label="Filter By State",
                                         choices=c("none", unique(crash_datetime_data$statename)), multiple=FALSE))
                  ,
                  column(3,      
                  selectInput(inputId="year", label="Filter By Year",
                                         choices=c("2015", "2016", "2017", "2018", "2019"), multiple=FALSE))
                  ,
                             
                             
            ),
            # vehicle tabs using pickerInput to select/ deselect multiple choices at the same time
            conditionalPanel(condition = "input.tabs == 'Vehicle Info'",
                             column(3,
                                    pickerInput("body", h4("Select car body type:"), 
                                                choices = vbody, options = list(`actions-box` = TRUE), multiple = T))
                             ,
                             column(3,
                                    pickerInput("make", h4("Select car make type:"), 
                                                choices = vmake, options = list(`actions-box` = TRUE), multiple = T))
                             ,
                             column(3,
                                    pickerInput("vInfoyear", h4("Select year:"), 
                                                choices = vyear, options = list(`actions-box` = TRUE), multiple = T))
                             ,
                             column(3,
                                    pickerInput("vInfostate", h4("Select state:"), 
                                                choices = vstate, options = list(`actions-box` = TRUE), multiple = T))
                             ,
                             
                             
            ),
    )),
    
    mainPanel(
        tabsetPanel(id="tabs",
                    tabPanel("Country Wide", plotOutput(outputId="countryWide",width = "1500px", height = "800px")),
                    tabPanel("Factors", plotOutput(outputId="factors",width = "1650px", height = "880px")),
                    tabPanel("Time Plots", plotOutput(outputId="timePlot", width = "1500px", height = "700px")),
                    tabPanel("Vehicle Info", plotOutput(outputId="vehiclePlots", width = "1500px", height = "800px")),
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ############### Rendering plot of Santosh First two tabs#######################
  ############### #######################  ########################
    output$countryWide <- renderPlot({
        # taking the input dates in  a variable
        ipdates <- input$dates_states
        
        # filtering crash data by input date range
        crash_state <- crash_list_smr %>% 
          filter(Date >= ipdates[1] & Date <= ipdates[2])
        
        # counting state wise fatalities
        crash_state_choro <- crash_state %>% 
          group_by(statename) %>% 
          summarize(fatals=sum(fatals))
        
        # calculating rate of fatalities
        crash_state_rate <- left_join(crash_state_choro,state_pop,by="statename") %>% 
          mutate(ratefatal = fatals/Population *100000)
        ## extracting top10 & bottom 10 states by fatality rate
        top10_rate <- crash_state_rate %>% 
          top_n(n=10,wt=ratefatal)
        bottom10_rate <- crash_state_rate %>% 
          top_n(n=-10,wt=ratefatal)
        
        ## If input state is null
        if (is.null(input$state)) {
           
          # Calculate daily fatalities of all states
          fatal_date <- crash_state %>% 
          group_by(Date) %>% 
          summarize(fatals=sum(fatals))
          sel = "all"
        ## If input state is not null
        }else{
          # Calculate daily fatalities of selected states
          fatal_date <- crash_state %>% 
            filter(statename %in% input$state) %>% 
            group_by(Date) %>% 
            summarize(fatals=sum(fatals))
          
          sel = paste0(length(input$state)," selected")
        }
        
        
        # join fatality rate of States with US map data
        us_map_fatal <- left_join(crash_state_rate,us_states,by=c("statename"="region"))
        
        # selected states for highlighting
        highlighted_states <- us_map_fatal %>% 
          filter(statename %in% input$state)
        
        # The choropleth of US by fatality rate
        pal <- colorRampPalette(brewer.pal(9, 'Reds'))(length(us_map_fatal$ratefatal))
        choro <- ggplot(us_map_fatal) + 
          geom_polygon(aes(x=long, y=lat, group=group, fill=ratefatal), color="gray40")+
          coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
          theme_map() +
          scale_fill_gradient2(low = pal[1],
                               mid = pal[length(pal)/2],
                               high = pal[length(pal)],
                               midpoint = (max(us_map_fatal$ratefatal) + min(us_map_fatal$ratefatal)) / 2,
                               #breaks=seq(10,120,10),
                               limits=c(min(us_map_fatal$ratefatal),max(us_map_fatal$ratefatal)), 
                               name="fatality / 100K People",
                               guide = guide_colorbar(barwidth = 35, 
                                                      barheight = 1.5,
                                                      title.position="top",
                                                      title.hjust = 0.5))+
          theme(legend.position = "bottom",
                legend.justification = "center",
                text = element_text(size=15),
                legend.title = element_text(size=15),
                legend.text=element_text(size=15))+
          labs(
            title = "US Choropleth Map Showing Statewise Rate of Fatality in Motor Crash",
            subtitle = paste0(ipdates[1]," to ",ipdates[2]),
            caption="Source:NHTSA & US Census Bureau"
            
          )+
          geom_path(data=highlighted_states, aes(x=long,y=lat,group=group),size=0.8, color="blue")
        
        ## plotting daily fatalities of selected states/ all states
        fatals_state <- ggplot(fatal_date)+
          geom_col(aes(x=Date,y=fatals),fill="steelblue")+
          theme_bw()+
          scale_y_continuous(label=comma, breaks = seq(0,max(fatal_date$fatals),ceiling(max(fatal_date$fatals)/12)))+
          scale_x_date(date_labels = "%b-%y", breaks = "3 months",limits = c(ipdates[1],ipdates[2]) )+
          theme(
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(angle = 30),
            text = element_text(size=15)
          )+
          annotate("text", x = ipdates[1]+(ipdates[2]-ipdates[1])/2, 
                   y = max(fatal_date$fatals)+max(fatal_date$fatals)/10, 
                   label = paste0(prettyNum(sum(fatal_date$fatals),big.mark=",",scientific=FALSE),
                                  " Fatalities"), size=6,color="steelblue")+
          labs(
            title = paste0("Daily fatalities in motor crash in US of ",sel,
                           " State(s)","\n(",ipdates[1]," to ",ipdates[2],")"),
            caption="Source:NHTSA & US Census Bureau"
          )
        
        ## bar graphs of top 10 & bottom 10 states as per fatality rate
        top10 <- ggplot(top10_rate)+
          geom_col(aes(x=reorder(statename,ratefatal),y=ratefatal),width=0.5,fill="brown")+
          scale_y_continuous(breaks = seq(0,round(max(us_map_fatal$ratefatal)),round(max(us_map_fatal$ratefatal)/10)), 
                             limits = c(0,max(us_map_fatal$ratefatal)))+
          coord_flip()+
          theme_bw()+
          theme(
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks = element_blank(),
            text = element_text(size=15)
          )+
          labs(
            x="",
            y="fatality / 100K Residents",
            title = "Top 10 US States by motor crash fatality rate",
            subtitle =paste0(ipdates[1]," to ",ipdates[2]),
            caption="Source:NHTSA & US Census Bureau"
          )
        
        bottom10 <- ggplot(bottom10_rate)+
          geom_col(aes(x=reorder(statename,-ratefatal),y=ratefatal),width=0.5,fill="green4")+
          scale_y_continuous(breaks = seq(0,round(max(bottom10_rate$ratefatal)),
                                          round(max(bottom10_rate$ratefatal)/10)), 
                             limits = c(0,max(bottom10_rate$ratefatal)))+
          coord_flip()+
          theme_bw()+
          theme(
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks = element_blank(),
            text = element_text(size=15)
          )+
          labs(
            x="",
            y="fatality / 100K Residents",
            title = "Bottom 10 US States by motor crash fatality rate",
            subtitle = paste0(ipdates[1]," to ",ipdates[2]),
            caption="Source:NHTSA & US Census Bureau"
          )
        
        ## arranging the plots of first tab
        row1 <- ggarrange(choro, fatals_state, nrow = 1)
        row2 <- ggarrange(top10, bottom10, nrow = 1)
        ggarrange(row1, row2,ncol=1)
        
    })
    
    #### rendering of 2nd tab plot begins here
    output$factors <- renderPlot({
      
      # taking the input dates in  a variable
      ipdates <- input$dates_statesf
      
      # cleaning accident data & filter by input date
      week_hours <- accident_data_filtered %>% 
        filter(Date >= ipdates[1] & Date <= ipdates[2]) %>% 
        filter(hourname != "Unknown Hours")
      
      # cleaning accident data & filter by input date
      acc_mosaic <- accident_data_filtered %>%
        filter(Date >= ipdates[1] & Date <= ipdates[2])
      
      # Calculating % drunk in accidents of all US states
      state_drunk <- accident_data_filtered %>%
        filter(Date >= ipdates[1] & Date <= ipdates[2]) %>%
        group_by(statename) %>% 
        mutate(acc = n()) %>% 
        ungroup() %>% 
        group_by(statename,acc) %>% 
        summarise(drunk=sum(drunk)) %>% 
        mutate(drpc = round(drunk/acc *100))
      
      # calculating accidents in county and state roads of all US states
      route_acc <- accident_data_filtered %>%
        filter(Date >= ipdates[1] & Date <= ipdates[2]) %>%
        filter(routename == "County Road"  | routename=="State Highway") %>% 
        group_by(statename) %>% 
        summarise(acc=n())
      
      ## joining the above data with US population data to calculate accident rate
      route_acc_pop <- left_join(route_acc,state_pop, by="statename") %>% 
        mutate(acc= round(acc/Population * 100000))
      
      ## joining the above data with rural hospital data
      route_acc_pop_hr <- left_join(route_acc_pop,hosp_pop, by=c("statename"="STATE")) %>% 
        top_n(n=10,wt=ruralPc)
      
      # selected states for highlight
      highlighted_states <- us_states %>% 
        filter(region %in% input$statef)
      
      # if input state is null
      if (is.null(input$statef)) {
        #calculate accidents by week_hours and days for all states
        week_hours <- week_hours%>% 
          group_by(hourname,day_weekname) %>% 
          summarise(x=n()) 
        # claculate accidents with drunk drivers
        acc_mosaic <- acc_mosaic %>% 
          group_by(day_weekname,drunk) %>% 
          summarize(acc = n())
        
        # calculate fatality rate by different routes for all states
        route <- accident_data_filtered %>% 
          group_by(routename) %>% 
          summarise(fatals=sum(fatals) / sum(state_pop$Population) * 1000000) 
        
        sel = "all"
        routerate = "1M"
        
      }else{
        #calculate accidents by week_hours and days for selected states
        week_hours <- week_hours %>% 
          filter(statename %in% input$statef) %>% 
          group_by(hourname,day_weekname) %>% 
          summarise(x=n()) 
        # calculate accidents with drunk drivers for slected states
        acc_mosaic <- acc_mosaic %>% 
          filter(statename %in% input$statef) %>% 
          group_by(day_weekname,drunk) %>% 
          summarize(acc = n())
        
        sel = paste0(length(input$statef)," selected")
        
        # filter populations of the selected states
        state_popr <- state_pop %>% 
          filter(statename %in% input$statef)
        ## calculate fatality rate by route in the selected states
        route <- accident_data_filtered %>% 
          filter(statename %in% input$statef) %>% 
          group_by(routename) %>% 
          summarise(fatals=sum(fatals) / sum(state_popr$Population) * 100000) 
        
        sel = paste0(length(input$statef), " selected")
        routerate = "100K"
      }
      
      ## plotting the heat map of week days & hour wise accidents
      pal <- colorRampPalette(brewer.pal(9, "YlGnBu"))(length(week_hours$x))
      
      tiles <- ggplot(week_hours, aes(day_weekname, hourname, fill= x)) + 
        geom_tile(color="grey80")+
        scale_fill_gradient2(low = pal[1],
                             mid = pal[length(pal)/2],
                             high = pal[length(pal)],
                             midpoint = (max(week_hours$x) + min(week_hours$x)) / 2,
                             #breaks=seq(min(week_hours$x),max(week_hours$x),
                             #round(max(week_hours$x)/(min(week_hours$x)+max(week_hours$x)/2))),
                             #limits=c(10,120), 
                             name="# of accidents",
                             guide = guide_colorbar(barwidth = 25, 
                                                    barheight = 1.5,
                                                    title.position="top",
                                                    title.hjust = 0.5))+
        theme_bw()+
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          text = element_text(size = 14)
        )+
        labs(x="",y="", 
             title=paste0("# of road accidents in ",sel, " US states \nby time  & week days"),
             subtitle = paste0(ipdates[1]," to ", ipdates[2]),
             caption = "Source: NHTSA FARS Data"
             )
      
      mosaic <- ggplot(acc_mosaic) +
        geom_mosaic(aes(weight=acc, x=product(day_weekname), fill=drunk))+
        theme_bw()+
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          text = element_text(size = 15),
          axis.text.y = element_blank()
        )+
        labs(x="",y="", 
             title="Fractions of drunk drivers in road accidents in USA",
             subtitle = paste0("For ",sel," states","(",
                               ipdates[1]," to ", ipdates[2],")"),
             caption = "Source: NHTSA FARS Data",
             fill="Drunk Driver ?: ")
      
      ## Join state % drunk data with us map data
      drunk_map <- left_join(state_drunk,us_states,by=c("statename"="region"))
      ## choropleth map of % drunk
      pal <- colorRampPalette(brewer.pal(9, "YlGnBu"))(length(drunk_map$drpc))
      drmap <- ggplot(drunk_map) + 
        geom_polygon(aes(x=long, y=lat, group=group, fill=drpc), color="gray40")+
        coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
        theme_map()+
        scale_fill_gradient2(low = pal[1],
                             mid = pal[length(pal)/2],
                             high = pal[length(pal)],
                             midpoint = (max(drunk_map$drpc) + min(drunk_map$drpc)) / 2,
                             breaks=seq(min(drunk_map$drpc),max(drunk_map$drpc),
                                        round(max(drunk_map$drpc)/10)),
                             #limits=c(10,120), 
                             name="% drunk",
                             guide = guide_colorbar(barwidth = 30, 
                                                    barheight = 1.5,
                                                    title.position="top",
                                                    title.hjust = 0.5))+
        theme(legend.position = "bottom",
              legend.justification = "center",
              text = element_text(size=15),
              legend.title = element_text(size=15),
              legend.text=element_text(size=15))+
        labs(
          title = "% of road accidents involving drunk drivers in USA",
          subtitle = paste0(ipdates[1]," to ", ipdates[2]),
          caption = "Source: NHTSA FARS Data"
        )+
        geom_path(data=highlighted_states, aes(x=long,y=lat,group=group),size=0.8, color="red")
      
    ## plotting bar graph with fatalities by route type
     routec <- ggplot(route)+
        geom_col(aes(x=reorder(routename,fatals),y=fatals),width=0.5,fill="brown")+
        scale_y_continuous(breaks = seq(0,round(max(route$fatals)),
                                        round(max(route$fatals)/10)), 
                           limits = c(0,max(route$fatals)))+
        coord_flip()+
        theme_bw()+
        theme(
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size=15)
        )+
        labs(
          x="",
          y=paste0("fatality /",routerate, " People"),
          title = paste0("Fatality rate by road type\n(",sel," US states)" ),
          subtitle =paste0(ipdates[1]," to ",ipdates[2]),
          caption = "Source: NHTSA FARS Data & US Census Bureau"
        )
     
     ## Plotting bar graph of rural hospital & rate of accidents 
     pal <- colorRampPalette(brewer.pal(9, 'Greens'))(1300)
     pal <- pal[seq(300,1300,100)]
     hosp <- ggplot(route_acc_pop_hr) +
       geom_col(aes(x=reorder(statename,acc),y=acc,fill=hr))+
       scale_y_continuous(breaks=seq(0,round(max(route_acc_pop_hr$acc)),
                              round(max(route_acc_pop_hr$acc)/5)))+
       scale_fill_gradient2(low = pal[1],
                            mid = pal[length(pal)/2],
                            high = pal[length(pal)],
                            midpoint = (max(route_acc_pop_hr$hr) + min(route_acc_pop_hr$hr)) / 2,
                            breaks=seq(10,120,10),
                            #limits=c(10,120), 
                            name="# of rural hospitals/1M rural residents",
                            guide = guide_colorbar(barwidth = 25, 
                                                   barheight = 1.5,
                                                   title.position="top",
                                                   title.hjust = 0.5))+
       coord_flip()+
       theme_bw()+
       theme(
         legend.position = "bottom",
         panel.border = element_blank(),
         panel.grid.minor = element_blank(),
         panel.grid.major.y = element_blank(),
         axis.ticks = element_blank(),
         text = element_text(size=15)
       )+
       labs(
         x="",
         y="# of accidents / 100K residents",
         title = paste0("Rate of accidents in State Highways & County Roads\n(Top 10 Rural States in 2010 Census)" ),
         subtitle =paste0(ipdates[1]," to ",ipdates[2]),
         caption = "Source: NHTSA FARS Data,UNC Center for Health Services Research,\nwww.nationalpopularvote.com,US Census Bureau"
       )
     
     ## plotting choropleth map of rural hosptal rate
     hosp_map <- hosp_pop %>% 
       drop_na() %>% 
       left_join(us_states, by=c("STATE"="region"))
     
     pal <- colorRampPalette(brewer.pal(9, "YlGn"))(length(hosp_map$hr))
     hmap <- ggplot(hosp_map) + 
       geom_polygon(aes(x=long, y=lat, group=group, fill=hr), color="gray60")+
       coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
       theme_map()+
       scale_fill_gradient2(low = pal[1],
                            mid = pal[length(pal)/2],
                            high = pal[length(pal)],
                            midpoint = (max(hosp_map$hr) + min(hosp_map$hr)) / 2,
                            breaks=seq(min(hosp_map$hr),max(hosp_map$hr),
                                       round(max(hosp_map$hr)/10)),
                            #limits=c(10,120), 
                            name="# of hospitals / million residents",
                            guide = guide_colorbar(barwidth = 30, 
                                                   barheight = 1.5,
                                                   title.position="top",
                                                   title.hjust = 0.5))+
       theme(legend.position = "bottom",
             legend.justification = "center",
             text = element_text(size=15),
             legend.title = element_text(size=15),
             legend.text=element_text(size=15))+
       labs(
         title = "Rural hospital / million rural population in US States",
         subtitle = "(In the year 2020)",
         #subtitle = paste0(ipdates[1]," to ", ipdates[2]),
         caption = "Source: UNC Center for Health Services Research,www.nationalpopularvote.com"
       )+
       geom_path(data=highlighted_states, aes(x=long,y=lat,group=group),size=0.8, color="red")
     
    ## arranging the plots
     arr1 <- ggarrange(tiles, drmap,hmap,nrow=1)
     arr2 <- ggarrange(mosaic,hosp,routec,nrow=1)
     ggarrange(arr1,arr2,ncol=1)
      
      
    })
    
    ############### Rendering plot of Santoshc ends here#######################
    ############### #######################  ########################
    
    
    
    ########### Gina's plots begins here ###########
    
    output$timePlot <- renderPlot({
      
      if(input$stateg != "none")
        crash_datetime_data <- crash_datetime_data %>%
          filter(statename == input$stateg)
      
      date_min <- as.Date(paste0(input$year, "-01-01"), "%Y-%m-%d")
      date_max <-as.Date(paste0(input$year, "-12-31"), "%Y-%m-%d")
      
      ## Crash Count filtering
      crash_datetime_filtered <- crash_datetime_data %>%
        mutate(crashdate = as.Date(crashdate, '%m/%d/%Y', tz="UTC")) %>%
        filter(crashdate >= date_min &
                 crashdate <= date_max) 
      
      # Crash Count per Day
      crash_count_day <- crash_datetime_filtered %>%
        group_by(crashdate)%>%
        summarize(count = sum(fatals))
      
      ## Crash Count per Hour
      crash_average_hour <- crash_datetime_filtered %>%
        group_by(crashtime) %>%
        summarize(average = mean(fatals))
      
      
      ## Plot of Crashes per Day
      p1 <- ggplot(crash_count_day) +
        geom_line(aes(x=crashdate, y=count))+
        geom_smooth(aes(x=crashdate, y=count),size=2)+
        scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
        labs(y="Fatalities", x="",
             title=paste("Amount of Fatalities due to Crashes per Day in", input$year))+
        theme_minimal() +
        theme(axis.text.x = element_text(angle=-55, hjust=-.2),
              text = element_text(size=15))
      
      
      ## Plot of Crashes per Hour
      p2 <- ggplot(crash_average_hour) +
        geom_line(aes(x=crashtime, y=average, group=1),size=2)+
        labs(y="Average Fatalities", 
             x="Hours of the Day (24-Hour Time)",
             title=paste("Average Fatalities due to Crashes per Hour in", input$year),
             caption="Source: NHTSA FARS Data")+
        scale_x_continuous(breaks = seq(0,23,1), labels= seq(0,23,1),
                           limits = c(0,23),
                           minor_breaks = NULL)+
        theme_minimal() +
        theme(axis.text.x = element_text(),
              text = element_text(size=15))
      
      p1 + p2 + plot_layout(ncol=1)
    })
    
    ############# Gina's plot rendering ends here #############
    
    
    ############# Ha's Plots #################################
    
    output$vehiclePlots <- renderPlot({
      
      vehicle2ptop10 <- vehicle2
      
      # filter vehicle body type
      if (length(input$body) > 0) {
        vehicle2ptop10 <- vehicle2ptop10 %>%
          filter(body_typname %in% input$body)
      }
      
      # filter manufacturer 
      if (length(input$make) > 0) {
        vehicle2ptop10 <- vehicle2ptop10 %>%
          filter(makename %in% input$make)
      }
      
      # filter year
      if (length(input$vInfoyear) > 0) {
        vehicle2ptop10 <- vehicle2ptop10 %>%
          filter(caseyear %in% input$vInfoyear)
      }
      
      # filter states
      if (length(input$vInfostate) > 0) {
        vehicle2ptop10 <- vehicle2ptop10 %>%
          filter(statename %in% input$vInfostate)
      }
      
      # display the counts for 10 most harmful events
      top10 <- vehicle2ptop10 %>%
        group_by(harm_evname) %>% # harmful event
        summarize(total = n()) %>%
        arrange(desc(total)) %>% # descending order
        head(n=10) %>%
        ggplot(aes(x=reorder(harm_evname, total), y= total)) + # reorder within the plot
        geom_col(width=0.4, fill = "orangered2")  +
        coord_flip()   +
        scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) + # reformat scale on thousands
        geom_text(aes(label = total), hjust = -0.2, color = "gray16") +
        theme_bw()+
        theme( panel.border = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major.y = element_blank(),
               axis.ticks = element_blank(), text = element_text(size=15)) +
        labs(x="", y="", title = "10 Most Common Harmful Events") 
      
      vehicle2pTopAndBot5 <- vehicle2
      
      # filter year
      if (length(input$vInfoyear) > 0) {
        vehicle2pTopAndBot5 <- vehicle2pTopAndBot5 %>%
          filter(caseyear %in% input$vInfoyear)
      }
      # filter state
      if (length(input$vInfostate) > 0) {
        vehicle2pTopAndBot5 <- vehicle2pTopAndBot5 %>%
          filter(statename %in% input$vInfostate)
      }
      # filter vehicle body type
      if (length(input$body) > 0) {
        vehicle2pTopAndBot5 <- vehicle2pTopAndBot5 %>%
          filter(body_typname %in% input$body)
      }
      
      # top 5 make and model of vehicle in fatal crashes
      top5 <- vehicle2pTopAndBot5 %>%
        group_by(mak_modname) %>%
        summarize(total = n()) %>%
        arrange(desc(total)) %>%
        head(n=5) %>% # 5 vehicles with most fatal accidents
        ggplot(aes(x=reorder(mak_modname, total), y= total)) + # reorder wihtin the plot
        geom_col(width=0.4, fill = "darkred")  +
        coord_flip()   +
        geom_text(aes(label = total), hjust = -0.2, color = "gray16") +
        theme_bw()+
        theme( panel.border = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major.y = element_blank(),
               axis.ticks = element_blank(), text = element_text(size=15)) +
        labs(x="", y="", title = "5 Vehicle Type with most fatal accidents") 
      
      # 5 vehicles with least crashes
      bot5 <- vehicle2pTopAndBot5 %>%
        group_by(mak_modname) %>% 
        summarize(total = n()) %>% # counts by vehicle make and model
        arrange(desc(total)) %>% # descending order
        tail(n=5) %>% # 5 vehicles with least fatal accidents
        ggplot(aes(x=reorder(mak_modname, - total), y= total)) +
        geom_col(width=0.4, fill = "forestgreen")  +
        coord_flip()   +
        geom_text(aes(label = total), hjust = -0.2, color = "gray16") +
        theme_bw()+
        theme( panel.border = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major.y = element_blank(),
               axis.ticks = element_blank(), text = element_text(size=15)) +
        labs(x="", y="", title = "5 Vehicle Type with least fatal crashes")
      
      # seasonality trend plots for hit and run crashes
      vehicle2YearlyAndMonthly <- vehicle2
      
      # filter by vehicle body type
      if (length(input$body) > 0) {
        vehicle2YearlyAndMonthly <- vehicle2YearlyAndMonthly %>%
          filter(body_typname %in% input$body)
      }
      # filter by vehicle make type
      if (length(input$make) > 0) {
        vehicle2YearlyAndMonthly <- vehicle2YearlyAndMonthly %>%
          filter(makename %in% input$make)
      }
      # filter by state location
      if (length(input$vInfostate) > 0) {
        vehicle2YearlyAndMonthly <- vehicle2YearlyAndMonthly %>%
          filter(statename %in% input$vInfostate)
      }
      
      # plot of yearly counts of hit and run fatal crashes for a state and vehicle type
      yearly <- vehicle2YearlyAndMonthly %>%
        filter(hit_run == 1) %>%
        group_by(caseyear) %>%
        summarise(total = n()) %>%
        ggplot(aes(x=caseyear, y = total)) +
        geom_point(color = "tan4") +
        geom_line(size = 0.5, color = "tan2") +
        theme_bw()+
        theme(
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size=15)) +
        labs(x="",y="",
             title = "Yearly Hit and Run Fatal Crashes")
      
      # filter year for the monthly report
      if (length(input$vInfoyear) > 0) {
        vehicle2YearlyAndMonthly <- vehicle2YearlyAndMonthly %>%
          filter(caseyear %in% input$vInfoyear)
      }
      
      # Plot of monthly counts for hit and run cases. Month is omitted if no count exists for the month
      monthly <- vehicle2YearlyAndMonthly %>%
        filter(hit_run == 1) %>% # get hit and run cases only
        group_by(monthname, month) %>%
        summarise(total = n()) %>%
        ggplot(aes(x=factor(monthname, levels = month.name), y = total)) + # order by month
        geom_col(width = 0.5, fill = "tan3") +
        theme_bw()+
        geom_text(aes(label = total), vjust = 1.5, color = "white") +
        theme(
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size=15)) +
        labs( x="", y="",
              title = "Monthly Hit and Run Fatal Crashes",
              caption="Source: NHTSA FARS Data")
      
      # arrange the plot in the tab
      vTabR1 <- ggarrange(top10, nrow=1) 
      vTabR2 <- ggarrange(top5, bot5, nrow=1)
      vTabR3 <- ggarrange(yearly, monthly, nrow=1)
      ggarrange(vTabR1, vTabR2, vTabR3, ncol=1)
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

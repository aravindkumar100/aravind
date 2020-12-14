library(lubridate)
library(dplyr)
library(plotly)
library(readr)
library(shiny)
library(shinydashboard)

datadispcovid <-  read_csv('covid_19_clean_complete.csv')
datadispsars <- read_csv('sars_2003_complete_dataset_clean.csv')
covid19data <- read_csv('covid_19_clean_complete.csv')


covid19data <- covid19data %>%
  select('Date', 'Country/Region', 'Confirmed', 'Deaths', 'Recovered')

colnames(covid19data)[2] <- 'Country' 

colnames(covid19data)[3] <- 'Cases' 

covid19data$Date <- format(as.Date(covid19data$Date, "%m/%d/%Y"), "20%y-%m-%d")

covid19data$Date <- as.Date(covid19data$Date, format="20%y-%m-%d")

groupeddatabydatecountry <- covid19data %>%
  group_by(Date, Country) %>%
  summarise(Cases=sum(Cases),
            Deaths= sum(Deaths),
            Recovered = sum(Recovered))

groupeddatabydate <- covid19data %>%
  group_by(Date) %>%
  summarise(Cases=sum(Cases),
            Deaths= sum(Deaths),
            Recovered = sum(Recovered))

groupeddatabycountry <- covid19data %>%
  group_by(Country) %>%
  summarise(Cases=sum(Cases),
            Deaths= sum(Deaths),
            Recovered = sum(Recovered))

newcaseddf <- groupeddatabydatecountry %>% 
  group_by(Country) %>% 
  mutate(Newcases = Cases - lag(Cases))

newcaseddf_deaths <- groupeddatabydatecountry %>% 
  group_by(Country) %>% 
  mutate(NewDeaths = Deaths - lag(Deaths))

newdata  <- filter(groupeddatabydatecountry,Date=='2020-4-26')
newdata <- subset(newdata,select = c('Country', 'Cases', 'Deaths', 'Recovered'))
Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year"))-20)
groupy<-transform(groupeddatabydatecountry, NumDays = Diff(groupeddatabydatecountry$Date, groupeddatabydatecountry$Date), TotalDays = Diff(groupeddatabydatecountry$Date, groupeddatabydatecountry$Date[1]))
groupy$ActiveCases <- groupeddatabydatecountry$Cases - (groupeddatabydatecountry$Deaths + groupeddatabydatecountry$Recovered)
Country <- unique(groupy$Country)
newdata$CODE <- c('AFG','ALB','DZA','AND','AGO','ATG','ARG','ARM','AUS','AUT','AZE','BHM','BHR','BGD','BRB',
                  'BLR','BEL','BLZ','BEN','BTN','BOL','BIH','BWA','BRA','BRN','BGR','BFA','MMR','BDI','CPV',
                  'KHM','CMR','CAN','CAF','TCD','CHL','CHN','COL','COD','COG','CRI','CIV','HRV','CUB','CYP',
                  'CZE','DNK','DMP','DJI','DMA','DOM','ECU','EGY','SLV','GNQ','ERI','EST','ESW','ETH','FJI',
                  'FIN','FRA','GAB','GMB','GEO','DEU','GHA','GRC','GRD','GTM','GIN','GNB','GUY','HTI','HOL',
                  'HND','HUN','ISL','IND','IDN','IRN','IRQ','IRL','ISR','ITA','JAM','JPN','JOR','KAZ','KEN',
                  'KSV','KWT','KGZ','LAO','LVA','LBN','LBR','LBY','LIE','LTU','LUX','MDG','MWI','MYS','MDV',
                  'MLI','MLT','MRT','MUS','MEX','MDA','MCO','MNG','MNE','MAR','MOZ','FSM','NAM','NPL','NLD',
                  'NZL','NIC','NER','NGA','MNP','NOR','OMN','PAK','PAN','PNG','PRY','PER','PHL','POL','PRT',
                  'QAT','ROU','RUS','RWA','KNA','LCA','VCT','SMR','STP','SAU','SEN','SRB','SYC','SLE','SGP',
                  'SVK','SVN','SOM','ZAF','KOR','SSD','ESP','LKA','SDN','SUR','SWE','CHE','SYR','TWN','TZA',
                  'THA','TLS','TGO','TTO','TUN','TUR','UGA','UKR','ARE','GBR','URY','USA','UZB','VEN','VNM',
                  'WBG','WSA','YEM','ZMB','ZWE')


#final <- read_csv('currentdata.csv')


# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

fig <- plot_geo(newdata)
fig <- fig %>% add_trace(
  z = ~Cases, color = ~Cases, colors = 'Reds',
  text = ~Country, locations = ~CODE, marker = list(line = l)
)
fig <- fig %>% colorbar(title = 'Numberofcases', tickprefix = '')
fig <- fig %>% layout(
  title = 'COVID-19<br>'
  #geo = g
)
fig

sarsdata <- read_csv('sars_2003_complete_dataset_clean.csv')
colnames(sarsdata)[3] <- 'Cases' 
colnames(sarsdata)[4] <- 'Deaths'
colnames(sarsdata)[5] <- 'Recovered' 
groupeddatabydatecountry_SARS <- sarsdata %>%
  group_by(Date, Country) %>%
  summarise(Cases=sum(Cases),
            Deaths= sum(Deaths),
            Recovered = sum(Recovered))
groupy_sars<-transform(groupeddatabydatecountry_SARS, NumDays = Diff(groupeddatabydatecountry_SARS$Date, groupeddatabydatecountry_SARS$Date), TotalDays = Diff(groupeddatabydatecountry_SARS$Date, groupeddatabydatecountry_SARS$Date[1]))
groupy_sars$ActiveCases <- groupeddatabydatecountry_SARS$Cases - (groupeddatabydatecountry_SARS$Deaths + groupeddatabydatecountry_SARS$Recovered)
Country_sars <- unique(groupy_sars$Country)
newcaseddf_SARS <- groupeddatabydatecountry_SARS %>% 
  group_by(Country) %>% 
  mutate(Newcases = Cases - lag(Cases))
newcaseddf_SARS$Newcases[newcaseddf_SARS$Newcases < 0] <- 0

newcaseddf_SARS_deaths <- groupeddatabydatecountry_SARS %>% 
  group_by(Country) %>% 
  mutate(NewDeaths = Deaths - lag(Deaths))
newcaseddf_SARS_deaths$NewDeaths[newcaseddf_SARS_deaths$NewDeaths < 0] <- 0

newdata_SARS  <- filter(groupeddatabydatecountry_SARS,Date=='2003-7-11')
newdata_SARS <- subset(newdata_SARS,select = c('Country', 'Cases', 'Deaths', 'Recovered'))

newdata_SARS$CODE <- c('BRA','CAN','CHN','COL','FIN','FRA','DEU','HKG','IND','IDN','ITA','KWT','MAC','MYS','MNG',
                       'NZL','PHL','IRL','PRK','ROU','RUS','SGP','ZAF','ESP','SWE','CHE','TWN','THA','GBR','USA',
                       'VNM')
fig_sars1 <- plot_geo(newdata_SARS)
fig_sars1 <- fig_sars1 %>% add_trace(
  z = ~Cases, color = ~Cases, colors = 'Reds',
  text = ~Country, locations = ~CODE, marker = list(line = l)
)
fig_sars1 <- fig_sars1 %>% colorbar(title = 'NumberOfCases', tickprefix = '')
fig_sars1 <- fig_sars1 %>% layout(
  title = 'SARS 2003<br>'
  #geo = g
)
Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year"))-20)
COVID19<-transform(groupeddatabydatecountry, NumDays = Diff(groupeddatabydatecountry$Date, groupeddatabydatecountry$Date), TotalDays = Diff(groupeddatabydatecountry$Date, groupeddatabydatecountry$Date[1]))
groupeddatabydatecountry1 <-groupeddatabydatecountry
groupeddatabydatecountry1<-mutate(group_by(groupeddatabydatecountry1,Country), Cases=cumsum(Cases))
groupeddatabydatecountry1<-mutate(group_by(groupeddatabydatecountry1,Country), Deaths=cumsum(Deaths ))
groupeddatabydatecountry1<-mutate(group_by(groupeddatabydatecountry1,Country), Recovered=cumsum(Recovered ))
Diff1 <- function(x, y) as.numeric(x/y)*100
COVID191<-transform(groupeddatabydatecountry1, FatalityRate= Diff1(groupeddatabydatecountry1$Deaths , groupeddatabydatecountry1$Cases ))
# Define UI for application that draws a histogram
COVID192<-transform(groupeddatabydatecountry1, Recoveryrate= Diff1(groupeddatabydatecountry1$Recovered , groupeddatabydatecountry1$Cases ))
#SARS
Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year"))-75)
SARS<-transform(groupeddatabydatecountry_SARS, NumDays = Diff(groupeddatabydatecountry_SARS$Date, groupeddatabydatecountry_SARS$Date), TotalDays = Diff(groupeddatabydatecountry_SARS$Date, groupeddatabydatecountry_SARS$Date[1]))
groupeddatabydatecountry_SARS1 <-groupeddatabydatecountry_SARS
groupeddatabydatecountry_SARS1<-mutate(group_by(groupeddatabydatecountry_SARS1,Country), Cases=cumsum(Cases))
groupeddatabydatecountry_SARS1<-mutate(group_by(groupeddatabydatecountry_SARS1,Country), Deaths=cumsum(Deaths ))
groupeddatabydatecountry_SARS1<-mutate(group_by(groupeddatabydatecountry_SARS1,Country), Recovered=cumsum(Recovered ))
Diff2 <- function(x, y) as.numeric(x/y)*100
SARS1<-transform(groupeddatabydatecountry_SARS1, FatalityRate= Diff2(groupeddatabydatecountry_SARS1$Deaths , groupeddatabydatecountry_SARS1$Cases ))
SARS2<-transform(groupeddatabydatecountry_SARS1, Recoveryrate= Diff2(groupeddatabydatecountry_SARS1$Recovered , groupeddatabydatecountry_SARS1$Cases ))
COVIDCountry <- unique(COVID19$Country)

SARSCountry <- unique(SARS$Country)


###Visualisation3 Data Manipulation
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#COVID
covid<-read.csv('covid_19_clean_complete.csv')
covid<-subset(covid,select = -c(Province.State,Lat,Long))
covid_date<-subset(covid,select = -c(Country.Region))
covid_date$Date<-as.Date(covid_date$Date,"%m/%d/%y")
covid_date<-covid_date[order(covid_date$Date),]
covid_date<- covid_date %>% 
  group_by(Date) %>% 
  summarise_each(funs(sum))

covid_date$Date<-format(covid_date$Date,"%d%b%Y")
covid_date$day<-time(covid_date$Date)
covid_date$Disease<-c("COVID-19")
covid_date$normalized<-normalize(covid_date$Confirmed)

##SARS
sars<-read.csv('sars_2003_complete_dataset_clean.csv')
sars_date<-subset(sars,select = -c(Country))
names(sars_date)<-c("Date","Confirmed","Deaths","Recovered")
sars_date$Date<-as.Date(sars_date$Date)
sars_date<-sars_date[order(sars_date$Date),]
sars_date<- sars_date %>% 
  group_by(Date) %>% 
  summarise_each(funs(sum))
sars_date$Date<-format(sars_date$Date,"%d%b%Y")
sars_date<-sars_date[1:nrow(covid_date),]
sars_date$day<-time(sars_date$Date)
sars_date$Disease<-c("SARS")
sars_date$normalized<-normalize(sars_date$Confirmed)


full_dataset<-rbind(covid_date,sars_date)

#COVID LINE
fig_covid<-plot_ly() %>%
  add_trace(x=~time(covid_date$Date), y=~covid_date$Confirmed,
            mode='lines+markers',
            name='COVID-19',
            text = ~paste("<b>Date:", covid_date$Date, "<br>Confirmed: ", covid_date$Confirmed),
            mode = "lines+markers",
            marker=list(size=10)) %>%
  layout( title="COVID-19 Confirmed Cases",
          xaxis = list(
            title = "Days",
            zeroline = F
          ),
          yaxis = list(
            title = "Total Confirmed Cases",
            zeroline = F
          ))

fig_covid

#sars LINE
fig_sars<-plot_ly() %>%
  add_trace(x=~time(sars_date$Date), y=~sars_date$Confirmed,
            mode='lines+markers',
            name='SARS',
            text = ~paste("<b>Date:", sars_date$Date, "<br>Confirmed: ", sars_date$Confirmed),
            mode = "lines+markers",
            marker=list(size=10)) %>%
  layout( title="SARS Confirmed Cases",
          xaxis = list(
            title = "Days",
            zeroline = F
          ),
          yaxis = list(
            title = "Total Confirmed Cases",
            zeroline = F
          ))

fig_sars

#Trend of SARS and CORONA
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

animydf <- full_dataset %>%
  accumulate_by(~day)

fig_v<-animydf%>%
  plot_ly(
    x = ~day, 
    y = ~normalized,
    split  =~Disease,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines',
    line = list(width=6)
  )
fig_v <- fig_v %>% layout( title = "The Trend of COVID and SARS",
                       xaxis = list(
                         title = "Days",
                         zeroline = F
                       ),
                       yaxis = list(
                         title = "Normalised Total Cases",
                         zeroline = F
                       )
) 
fig_v <- fig_v %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig_v <- fig_v %>% animation_slider(
  hide = T
)
fig_v <- fig_v %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)



## deaths and recovered Covid
fig_covid_DR <- plot_ly(line = list(width=6))  %>% 
  add_trace(x=covid_date$day, y = ~ covid_date$Recovered, name = 'Recovered', type = 'scatter',mode = 'lines',color = I('green'),
            text = ~paste("<b>Date:", covid_date$Date, "<br>Recovered: ", covid_date$Recovered)) %>%
  add_trace(x=covid_date$day, y = ~covid_date$Deaths, name = 'Deaths', type = 'scatter', mode = 'lines',color = I('red'),
            text = ~paste("<b>Date:", covid_date$Date, "<br>Deaths: ", covid_date$Deaths))%>%
  layout( title="COVID Recovered and Death cases ",
          xaxis = list(
            title = "Days",
            zeroline = F
          ),
          yaxis = list(
            title = "Cases",
            zeroline = F
          ))

fig_covid_DR

## deaths and recovered SARS
fig_SARS_DR <- plot_ly(line = list(width=6))  %>% 
  add_trace(x=sars_date$day, y = ~ sars_date$Recovered, name = 'Recovered', type = 'scatter',mode = 'lines',color = I('green'),
            text = ~paste("<b>Date:", sars_date$Date, "<br>Recovered: ", sars_date$Recovered)) %>%
  add_trace(x=sars_date$day, y = ~sars_date$Deaths, name = 'Deaths', type = 'scatter', mode = 'lines',color = I('red'),
            text = ~paste("<b>Date:", sars_date$Date, "<br>Deaths: ", sars_date$Deaths))%>%
  layout( title="SARS Recovered and Death cases ",
          xaxis = list(
            title = "Days",
            zeroline = F
          ),
          yaxis = list(
            title = "Cases",
            zeroline = F
          ))

fig_SARS_DR


ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "COVID 19 VS SARS"),
  dashboardSidebar(
    tags$head(
      tags$script(
        HTML(
          "
        $(document).ready(function(){
          // Bind classes to menu items, easiet to fill in manually
          var ids = ['Visualization1','Visualization2','Visualization3','Data'];
          for(i=0; i<ids.length; i++){
            $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
          }

          // Register click handeler
          $('.my_subitem_class').on('click',function(){
            // Unactive menuSubItems
            $('.my_subitem_class').parent().removeClass('active');
          })
        })
        "
        )
      )
    ),
    width = 290,
    menuItem("Visualization1",tabName = "Visualization1"),
    menuItem("Visualization2",tabName = "Visualization2"),
    menuItem("Visualization3",tabName = "Visualization3"),
    menuItem("Data",tabName = "Data")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "Data",
              fluidPage(
                bootstrapPage(
                radioButtons("pTypedata", "Choose the Pandemic Effect:",
                             list("COVID19", "SARS"))),
              h2("Reported Cases and Deaths by Country, Territory, or Conveyance"),
              conditionalPanel('input.pTypedata=="COVID19"',DT::dataTableOutput("mytableCOVID")),
              conditionalPanel('input.pTypedata=="SARS"',DT::dataTableOutput("mytableSARS"))
              )
              
        
      ),
      tabItem(tabName = "Visualization1",
              fluidRow(
                bootstrapPage(
                  radioButtons("pType", "Choose the Pandemic Effect:",
                               list("COVID19", "SARS"))),
                column(12,align="center",
                       
                       
                       conditionalPanel('input.pType=="COVID19"',plotlyOutput("d1",width = "100%",height = "400px", inline = FALSE)),
                       conditionalPanel('input.pType=="SARS"',plotlyOutput("d2"))
                       
                       
                )
                
              ),br(),br(),
              fluidRow(
                sidebarLayout(
                  column(2,
                         
                         conditionalPanel('input.pType=="COVID19"',selectizeInput(
                           inputId = "Country", 
                           label = 'Country',
                           # placeholder is enabled when 1st choice is an empty string
                           choices = c("Please choose a Country" = "", Country), 
                           multiple = TRUE
                         )),
                         conditionalPanel('input.pType=="SARS"',selectizeInput(
                           inputId = "Country_sars", 
                           label = 'Country',
                           # placeholder is enabled when 1st choice is an empty string
                           choices = c("Please choose a Country" = "", Country_sars), 
                           multiple = TRUE
                         ))
                  ),
                  column(10,
                         
                         conditionalPanel('input.pType=="COVID19"',plotlyOutput(outputId = "p")),
                         conditionalPanel('input.pType=="SARS"',plotlyOutput(outputId = "p1"))
                         
                  )
                )
              ),br(),br(),
              fluidRow(
                
                column(2,
                       
                       conditionalPanel('input.pType=="COVID19"',selectInput(inputId = "quintile", label = ("Daily New Cases and New Deaths by Country"),
                                                                             choices = unique(newcaseddf$Country))),
                       conditionalPanel('input.pType=="SARS"',selectInput(inputId = "quintile1", label = ("Daily New Cases and New Deaths by Country"),
                                                                          choices = unique(newcaseddf_SARS$Country)))
                       
                       
                       
                ),
                
                
                
                
                column(5,
                       
                       
                       conditionalPanel('input.pType=="COVID19"',plotlyOutput(outputId = "plot")),
                       conditionalPanel('input.pType=="SARS"',plotlyOutput(outputId = "plot1"))
                       
                ),
                column(5,
                       conditionalPanel('input.pType=="COVID19"',plotlyOutput(outputId = "plotdeath")),
                       conditionalPanel('input.pType=="SARS"',plotlyOutput(outputId = "plotdeath1"))
                       
                )
                
                
              )
              ),
      
      tabItem(tabName = "Visualization2",
        #####CODE HERE##########
        fluidRow(
          radioButtons("pType1", "Choose the Pandemic Effect:",
                       list("COVID19", "SARS")),
          
          conditionalPanel('input.pType1=="COVID19"',selectizeInput(
            inputId = "Countrys", 
            label = 'Country',
            # placeholder is enabled when 1st choice is an empty string
            choices = c("Please choose a Country" = "", Country), 
            multiple = TRUE
          )),
          conditionalPanel('input.pType1=="SARS"',selectizeInput(
            inputId = "Countrys1", 
            label = 'Country',
            # placeholder is enabled when 1st choice is an empty string
            choices = c("Please choose a Country" = "", Country_sars), 
            multiple = TRUE
          )),
          conditionalPanel('input.pType1=="COVID19"',plotlyOutput("f1")),
          conditionalPanel('input.pType1=="SARS"',plotlyOutput("f2"))
        ),
        fluidRow(
          column(6,conditionalPanel('input.pType1=="COVID19"',plotlyOutput("f3")),conditionalPanel('input.pType1=="SARS"',plotlyOutput("f4"))),
          column(6,conditionalPanel('input.pType1=="COVID19"',plotlyOutput("f5")),conditionalPanel('input.pType1=="SARS"',plotlyOutput("f6")))
          
        )
      ),
      tabItem(tabName = "Visualization3",
              fluidPage( fluidRow(
                bootstrapPage(
                  radioButtons("pType2", "Choose the Pandemic Effect:",
                               list("COVID19", "SARS"))),
                conditionalPanel('input.pType2=="COVID19"',
                                 plotlyOutput("e1")),
                conditionalPanel('input.pType2=="SARS"',plotlyOutput("e2"))
                
              ) ,br(),br(),
              fluidRow(
                conditionalPanel('input.pType2=="COVID19"',
                                 plotlyOutput("e3")),
                conditionalPanel('input.pType2=="SARS"',plotlyOutput("e4"))
                
              ), br(),br(), 
              fluidRow(fig_v)
              )
      )
    )
    
  )
)

server <- function(input, output) {
  filtered_data<- reactive({
    dplyr::filter(newcaseddf, Country==input$quintile)
  })
  filtered_data1<- reactive({
    dplyr::filter(newcaseddf_SARS, Country==input$quintile1)
  })
  filtered_datadeath<- reactive({
    dplyr::filter(newcaseddf_deaths, Country==input$quintile)
  })
  filtered_datadeath1<- reactive({
    dplyr::filter(newcaseddf_SARS_deaths, Country==input$quintile1)
  })
  output$mytableCOVID = DT::renderDataTable({
    validate(need(input$pTypedata=='COVID19',message=FALSE))
    expr = datadispcovid
  })
  output$mytableSARS = DT::renderDataTable({
    validate(need(input$pTypedata=='SARS',message=FALSE))
    expr = datadispsars
  })
  output$d1 <- renderPlotly({
    validate(need(input$pType=='COVID19',message=FALSE))
    expr = fig
  })
  output$d2 <- renderPlotly({
    validate(need(input$pType=='SARS',message=FALSE))
    expr = fig_sars1
  })
  output$p <- renderPlotly({
    validate(need(input$pType=='COVID19',message=FALSE))
    req(input$Country)
    fig <- filter(groupy,Country %in% input$Country)  %>%
      plot_ly(
        x = ~ActiveCases,
        y = ~Country, 
        frame = ~TotalDays,
        type = 'bar',
        orientation = 'h',
        name = 'ActiveCases',text= ~paste("<b>Date:",Date ,"<br><b> CASES:",Cases,"<br><b> ActiveCases:",ActiveCases,"<br><b> Recovered:",Recovered)
      )
    fig <- fig %>% add_trace(x = ~Deaths, name = 'Deaths')
    fig <- fig %>% add_trace(x = ~Recovered, name = 'Recovered')
    fig <- fig %>% layout(yaxis = list(title = 'Country'), barmode = 'stack')
    fig <- fig %>%
      layout(title="Visualizing the Relative decompositon of Confirmed Cases ")%>%
      animation_opts(
        frame = 100, 
        transition = 0, 
        redraw = FALSE)
    fig
  })
  output$p1 <- renderPlotly({
    validate(need(input$pType=='SARS',message=FALSE))
    req(input$Country_sars)
    fig <- filter(groupy_sars,Country %in% input$Country_sars)  %>%
      plot_ly(
        x = ~ActiveCases,
        y = ~Country, 
        frame = ~TotalDays,
        type = 'bar',
        orientation = 'h',
        name = 'ActiveCases',text= ~paste("<b>Date:",Date ,"<br><b> CASES:",Cases,"<br><b> ActiveCases:",ActiveCases,"<br><b> Recovered:",Recovered)
      )
    fig <- fig %>% add_trace(x = ~Deaths, name = 'Deaths')
    fig <- fig %>% add_trace(x = ~Recovered, name = 'Recovered')
    fig <- fig %>% layout(yaxis = list(title = 'Country'), barmode = 'stack')
    fig <- fig %>%
      layout(title="Visualizing the Relative decompositon of Confirmed Cases ")
      
    fig
  })
  output$plot <- renderPlotly({
    validate(need(input$pType=='COVID19',message=FALSE))
    par(mar=c(1,1,1,1))
    filtered_data <- filtered_data() %>%
      plot_ly(x= filtered_data()$Date, y= filtered_data()$Newcases, type="bar",text= ~paste("<b>Date:",Date ,"<br><b> NewCases:",Newcases),color = '#8B0000',name = 'Daily New Cases')
    filtered_data <- filtered_data %>% layout(yaxis= list(title = 'Daily New Cases'),xaxis = list(title = 'Date Range'),title = 'Daily New Cases')
    filtered_data
  })
  output$plot1 <- renderPlotly({
    validate(need(input$pType=='SARS',message=FALSE))
    par(mar=c(1,1,1,1))
    filtered_data1 <- filtered_data1() %>%
      plot_ly(x= filtered_data1()$Date, y= filtered_data1()$Newcases, type="bar",text= ~paste("<b>Date:",Date ,"<br><b> NewCases:",Newcases),color = '#8B0000',name = 'Daily New Cases')
    filtered_data1 <- filtered_data1 %>% layout(yaxis= list(title = 'Daily New Cases'),xaxis = list(title = 'Date Range'),title = 'Daily New Cases')
    filtered_data1
  })
  output$plotdeath <- renderPlotly({
    validate(need(input$pType=='COVID19',message=FALSE))
    par(mar=c(1,1,1,1))
    filtered_datadeath <- filtered_datadeath() %>%
      plot_ly(x= filtered_datadeath()$Date, y= filtered_datadeath()$NewDeaths, type="bar",text= ~paste("<b>Date:",Date ,"<br><b> NewCases:",NewDeaths),color = '#8B0000',name = 'Daily New Cases')
    filtered_datadeath <- filtered_datadeath %>% layout(yaxis= list(title = 'Daily New Deaths'),xaxis = list(title = 'Date Range'),title = 'Daily New Deaths')
    filtered_datadeath
  })
  output$plotdeath1 <- renderPlotly({
    validate(need(input$pType=='SARS',message=FALSE))
    par(mar=c(1,1,1,1))
    filtered_datadeath1 <- filtered_datadeath1() %>%
      plot_ly(x= filtered_datadeath1()$Date, y= filtered_datadeath1()$NewDeaths, type="bar",text= ~paste("<b>Date:",Date ,"<br><b> NewCases:",NewDeaths),color = '#8B0000',name = 'Daily New Cases')
    filtered_datadeath1 <- filtered_datadeath1 %>% layout(yaxis= list(title = 'Daily New Deaths'),xaxis = list(title = 'Date Range'),title = 'Daily New Deaths')
    filtered_datadeath1
  })
  output$f1 <- renderPlotly({
    req(input$Countrys)
    
    fig <- filter(COVID19,Country %in% input$Countrys)  %>%
      plot_ly(
        x = ~Cases,
        y = ~Deaths,
        frame = ~TotalDays,
        size = ~Cases,
        marker = list(size =25),
        color=~Country,
        type = 'scatter',
        legendgroup = ~'Country',
        showlegend = T
      )
    fig %>%
      layout(title="Spread Of Covid ")%>%
      animation_opts(
        frame = 100, 
        transition = 0, 
        redraw = FALSE)
  })
  output$f2 <- renderPlotly({
    req(input$Countrys1)
    
    fig <- filter(SARS,Country %in% input$Countrys1)  %>%
      plot_ly(
        x = ~Cases,
        y = ~Deaths,
        frame = ~TotalDays,
        marker = list(size =25),
        size = ~Cases,
        color=~Country,
        type = 'scatter',
        legendgroup = ~'Country',
        showlegend = T
      )
    fig %>%
      layout(title="Spread Of Covid ")%>%
      animation_opts(
        frame = 100, 
        transition = 0, 
        redraw = FALSE)
  })
  output$f3 <- renderPlotly({
    req(input$Countrys)
    
    fig1 <- filter(COVID191,Country %in% input$Countrys)  %>% 
      plot_ly(
        x = ~Date,
        y = ~FatalityRate,
        color=~Country,
        type = 'scatter',
        mode='lines',
        showlegend = F
      )
    fig1 %>%
      layout(yaxis = list(hoverformat = '.4f',ticksuffix = "%" ),title="Covid Fatal Trend ")
  })
  output$f4 <- renderPlotly({
    req(input$Countrys1)
    
    fig1 <- filter(SARS1,Country %in% input$Countrys1)  %>% 
      plot_ly(
        x = ~Date,
        y = ~FatalityRate,
        color=~Country,
        type = 'scatter',
        mode='lines',
        showlegend = F
      )
    fig1 %>%
      layout(yaxis = list(hoverformat = '.4f',ticksuffix = "%"),title="Sars Fatal Trend ")
  })
  output$f5 <- renderPlotly({
    req(input$Countrys)
    
    fig1 <- filter(COVID192,Country %in% input$Countrys)  %>% 
      plot_ly(
        x = ~Date,
        y = ~Recoveryrate,
        color=~Country,
        type = 'scatter',
        mode='lines',
        showlegend = F
      )
    fig1 %>%
      layout(yaxis = list(hoverformat = '.4f',ticksuffix = "%"),title="Covid Recovery Trend ")
  })
  output$f6 <- renderPlotly({
    req(input$Countrys1)
    
    fig1 <- filter(SARS2,Country %in% input$Countrys1)  %>% 
      plot_ly(
        x = ~Date,
        y = ~Recoveryrate,
        color=~Country,
        type = 'scatter',
        mode='lines',
        showlegend = F
      )
    fig1 %>%
      layout(yaxis = list( hoverformat = '.4f',ticksuffix = "%"),title="Sars Recovery Trend ")
  })
  output$e1 <- renderPlotly({
    validate(need(input$pType2=='COVID19',message=FALSE))
    expr = fig_covid
  })
  output$e2 <- renderPlotly({
    validate(need(input$pType2=='SARS',message=FALSE))
    expr = fig_sars
  })
  
  output$e3 <- renderPlotly({
    validate(need(input$pType2=='COVID19',message=FALSE))
    expr = fig_covid_DR
  })
  output$e4 <- renderPlotly({
    validate(need(input$pType2=='SARS',message=FALSE))
    expr = fig_SARS_DR
  })
}

shinyApp(ui, server)
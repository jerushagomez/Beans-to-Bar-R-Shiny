#-------------------------------------------------------------------------------
#                                  LIBRARIES NEEDED
#-------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(plotrix)
library(flexdashboard)
library(leaflet)
library(leaflet.extras)
library(shinycssloaders)
library(plotly)
library(bslib)





#-------------------------------------------------------------------------------
#                                        GLOBAL
#-------------------------------------------------------------------------------

title <- tags$img(src='logo.png',height='30', width='30'," BEANS TO BAR",
                  style = "font-family:Times New Roman")
data <- as.data.frame(read.csv("finaldata.csv"))
data1 <- as.data.frame(read.csv("location.csv"))
df5 <- as.data.frame(read.csv("bean_origin.csv"))



#1. creating bins
data$CocoaPer_bins <- 
  ifelse(data$CocoaPer < 50,"<50",
         ifelse(data$CocoaPer >= 50 & data$CocoaPer < 60,"50-60",
                ifelse(data$CocoaPer >=60 & data$CocoaPer < 70,"60-70",
                       ifelse(data$CocoaPer >=70 & data$CocoaPer
                              <80,"70-80",ifelse(data$CocoaPer >80 &
                                                data$CocoaPer<90,"80-90","90-100")))))



#2.most reviewed ingredient
tab_1<-(as.data.frame(table(data$ingri)))
tab_1<-arrange(tab_1,desc(Freq))
tab_1
tab_1 <- setNames(tab_1,c("Ingredients","Count"))
tab_1
#reordering Ingredients
tab_1$Ingredients <-reorder(tab_1$Ingredients,-tab_1$Count)
tab_1 <- head(tab_1,5)


#3.company produces the best rated chocolate
rating <- seq(1,4,1)
rating
count1 <- c(7,519,1806,111)
a <- data.frame(Rating = rating,Count = count1)
a

#4. Top Bean Orgin
country_top <-(as.data.frame(table(data$Bean_Origin)))
country_top<-arrange(country_top,desc(Freq))
country_top
country_top <- setNames(country_top,c("Countries","Count"))
country_top
#reordering Countries
country_top$Countries <-reorder(country_top$Countries,-country_top$Count)


#5.Rating over the years
tab_2<- aggregate(data.frame(Rating = data$Rating), list(Review_Date = data$Review_Date), 
                  FUN = mean)
tab_2


#6. 
y <- table(data$Review_Date)
y
y <- data.frame(y)
y
d <- setNames(y,c("Year","Count"))
d




#-------------------------------------------------------------------------------
#                                  USER INTERFACE
#-------------------------------------------------------------------------------


header <- dashboardHeader(title = title)
sidebar <- dashboardSidebar(sidebarMenu(
    menuItem("Home", icon = icon("home"), tabName = "home"),
    menuItem("Facts",icon=icon("bookmark"),tabName = 'facts'),
    menuItem("Map", icon = icon("thumbtack"), tabName = "map",
             menuSubItem("Company Distribution", tabName = "M1",
                         icon = icon("tag")),
             menuSubItem("Average Ratings", tabName = "M2",
                         icon = icon("tags"))
             ),
    menuItem("Plots",icon = icon("chart-line"),tabName = "plot",
             menuSubItem("Pie-Chart", tabName = "S1",
                         icon = icon("dice-one")),
             menuSubItem("Spike Chart", tabName = "S2",
                         icon = icon("dice-two")),
             menuSubItem("Line Graph", tabName = "S3",
                         icon = icon("dice-three")),
             menuSubItem("Bar Plot", tabName = "S4",
                         icon = icon("dice-four")),
             menuSubItem("Bar Plot 2", tabName = "S5",
                         icon = icon("dice-five"))),
    menuItem("Dataset", tabName = "data", icon = icon("database"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(
      HTML("#compare_state_option,#compare_year_option ,.compare-county-wrapper { display:flex; margin-bottom:-10px;}
    				#compare_state_option > div, #compare_year_option > div, .compare-county-wrapper > div > div {padding-right: 15px;}
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;}
    				.box.box-solid.box-primary {border: 1px solid #dd4b39 !important;}
    				.box.box-solid.box-primary>.box-header { background: #dd4b39 !important; background-color: #dd4b39 !important; }
    				.sidebar-menu>li {font-size:17px;}")
    )
  ),
  tabItems(
    #-------------------------------HOME TAB------------------------------------
    
    tabItem(tabName = "home",
            tags$img(src="pic1.png", height=200, width='100%'),
            br(),
            br(),
            
  
            fluidRow(
              column(width=12,tags$h1(width=5,"CHOCOLATE MAKING PROCESS")),
              HTML('<html>
                            <head>
                            <style>
                            table {
                              font-family: arial, sans-serif;
                              border-collapse: collapse;
                              width: 100%;
                            }p{
                              font-size: 25px;
                            }
                            </style>
                            </head>')
            ),
            
            fluidRow(
              box(width=12,HTML('<iframe width="100%" height="430" src="v1.mp4" 
                       frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; 
                       gyroscope; picture-in-picture" allowfullscreen></iframe>')),
            ),
            
            
            fluidRow(
              column(width=12,
                     tags$p("The word chocolate brings immense joy to a majority 
                            of people. From being a general energy booster to 
                            reducing heart risks, one could say that chocolates 
                            are rather pertinent in life. Additionally, dark 
                            chocolate is believed to boost brain levels of 
                            endorphins (natural opiates) as well as serotonin 
                            (a mood-altering chemical on which many antidepressants 
                            act). In case that went over your head, basically, 
                            dark chocolate makes one happy.")),
              column(width=12,
                     tags$p(width=7,"To talk a little bit on the history of 
                            chocolate, we accredit Joseph Fry with the first 
                            modern chocolate bar. In 1847, he discovered that 
                            he could make a mouldable chocolate paste by adding 
                            melted cacao butter back into Dutch cocoa. Ever since, there have been incessant 
                            efforts to make the best chocolate. In terms of quality, 
                            it is all about sourcing the finest cacao butter and cocoa beans. 
                            In terms of flavour profiles, there is no end to finding the 
                            latest, most exciting flavours.")),
              column(width=12,
                     tags$p(width=7,"If you are also one among us, an ardent chocolate lover, 
                            this is the place you will begin to understand and analyse various 
                            chocolates and the various factors affecting their taste and quality.")),
              column(width=7,
                     tags$h3(),
                     tags$p(width=5))
            ),
            br(),
            br(),
            fluidRow(
              column(width=7,tags$h1(width=5,"ABOUT US"))
            ),
            fluidRow(
              column(width=5,br(),br(),br(),br(),tags$img(src="logo.png", height=270, width="80%", hspace="45")),
              column(width=7,tags$p(style="text : 70;",width=5,"Everyone has a nostalgic memory of eating 
              their favourite chocolate.A mouth-watering memory to be honest. 
              Once we have relished it, we tend to only think of when we can 
              get our hands on it again. But have you stopped midway all the 
              drooling and wondered about the process of manufacturing the chocolate? 
              It is the process that determines everything about the chocolate. 
              The origin of the cocoa beans, the location of the manufacturing unit, 
              the various ingredients and their various combinations - 
              the list could be endless. ")),
              column(width=7,tags$p(style="text : 70;",width=5,
              "The main objective of this dashboard Beans to Bar is to help 
              you gain some insights on the various factors 
              affecting chocolates. This will help you understand how each 
              factor affects the quality and taste of the chocolate on the whole, 
              the most popular flavour profiles and so on. We have based our data from ", 
              tags$a(href = "https://flavorsofcacao.com/", "Flavours of Cacao"), "which is an organization that tastes and 
                                    reviews chocolates. They store all the ratings alongside various 
                                    factors that affect the chocolate.")),
              
              
            )),
    
    #-------------------------------FACTS TAB-----------------------------------

    tabItem(tabName = "facts",
            fluidRow(infoBoxOutput("years",width = 4),infoBoxOutput("company",width = 4),
                     infoBoxOutput("Loc",width = 4)),
            fluidRow(infoBoxOutput("rate",width = 4),infoBoxOutput("ingr",width = 4),
                     infoBoxOutput("siz",width = 4)),
    
    fluidRow(
      column(width=12,tags$h1(width=7,"What are Memorable Characteristics?")),
      column(width = 12,tags$p(
      "Everything in this world has characteristics. Some things have characteristics that stand out. 
      When we begin to associate something with its stand-out characteristics, it tends to linger on 
      in our memories. In our data, we have various chocolates. These chocolates are made of 
      different ingredients having varied proportions. Post review, it was found that each chocolate 
      left a lingering taste of a certain kind. This is what we like to call the memorable 
      characteristics of our chocolates. 
      Given below is a word cloud of all the memorable characteristics. 
      Their size in the cloud is proportional to their frequency and 
      hence their prevalence in the reviewed chocolates.")),
      br(),
      br(),
      br(),
      column(width=12,tags$img(src="wordcloud.jpeg", height=350, width="70%", hspace="105"))),
    br(),
    br(),
    
    fluidRow(
      column(width=12,tags$h1(width=5,"Top Reviewed Ingredients"))),
    fluidRow(
      column(5,
             wellPanel(tags$img(src="img1.jpeg", height=175, width="100%", hspace="0.5"), 
                       br(),
                       br(),
                       tags$p("There are various basic ingredients in a chocolate. 
                              Furthermore, there are various combinations of the same.
                              We have analysed the top five ingredient combinations 
                              that fetched the most reviews. From the bar chart shown, 
                              you will be able to see each of these ingredient 
                              combinations and their respective count of reviews. 
                              For easier representation, the graph has the abbreviations 
                              of the ingredients. The table shown helps one understand 
                              what each ingredient letter stands for.")   
             ) #wellPanel
      ),
      column(7,
             box(title = "Bar Chart of Most Reviewed Ingredients", solidHeader = TRUE, 
                 status = "warning", width = 12,
                 plotOutput("plt1"))
      )
    )
      ),
    #-------------------------------MapA TAB------------------------------------
    tabItem(tabName = "M1",
            
            fluidRow(
              box(title = "Location Distribution Across Countires",solidHeader = TRUE, status = "warning",height=750,
                  width = 17,leafletOutput(height = 750,"map"))
            )),
    #-------------------------------MapB TAB------------------------------------
    tabItem(tabName = "M2",
            
            fluidRow(
              box(title = "Average Rating of Bean Orgin",solidHeader = TRUE, status = "warning",height=650,
                  width = 12,leafletOutput(height = 590,"map2"))
            )),
    #-------------------------------plot1 TAB-----------------------------------
    tabItem(tabName = "S1",
            fluidRow(
              column(7,
                     box(title = "Ratings Proportion",solidHeader = TRUE,status="warning",width = 12,
                         plotlyOutput("plt2"))),
              
              column(5,br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     box(tags$p("The pie plot shown graphically represents the 
                     various ratings (1-4) and the number of observations 
                     having that rating as a part-to-whole composition. 
                                From the graph, we are able to see that rating of 
                                value 3 has the highest count and hence covers the 
                                largest portion of the pie."),width = 12))),
      
                HTML('<html>
                            <head>
                            <style>
                            table {
                              font-family: arial, sans-serif;
                              border-collapse: collapse;
                              width: 100%;
                            }p{
                              font-size: 19px;
                            }
                            </style>
                            </head>')
            
              
                           
            ),
    #-------------------------------plot2 TAB--------------------------------
    tabItem(tabName = "S2",
            fluidRow(box(
              title = "Number of countries to view:",
              numericInput(inputId = "obs",
                           label = " ",
                           value = 10),tableOutput("view"),
            ),
                     column(12,
                            box(title="Contribution of Chocolate Bean by Various Countries", 
                                solidHeader = TRUE,
                                status = "warning", width = 12,
                                plotOutput("plt3",height = 550))))
            
    ),
    #-------------------------------plot3 TAB-----------------------------------
    tabItem(tabName = "S3",
            fluidPage(column(12,
                             box(title="Ratings' Trend Over the Years", 
                                 solidHeader = TRUE,
                                 status = "warning", width = 12,
                                 plotlyOutput("plt4",height = 550)
                                 )))
      ),
    
    
    #-------------------------------plot4 TAB------------------------------------
    tabItem(tabName = "S4",
            fluidPage(column(12,
                             box(title="Number of Reviews Over the Years", 
                                 solidHeader = TRUE,
                                 status = "warning", width = 12,
                                 plotlyOutput("plt5",height = 550)
                             )))
            
    ),
    
    #-------------------------------plot5 TAB-------------------------------------
    tabItem(tabName = "S5",
            fluidPage(column(12,
                             box(title="Companies using various percentages of 
cocoa", 
                                 solidHeader = TRUE,
                                 status = "warning", width = 12,
                                 plotlyOutput("plt6",height = 550)
                             )))
            
    ),
    
    #-------------------------------RAW DATA TAB--------------------------------
    tabItem(tabName = "data",
            tags$h3('Download Data'),
            downloadButton("downloadData"),
            br(),
            br(),
            tableOutput("tableData"))
    )
  )










ui <- fluidPage(dashboardPage(skin = 'yellow',header, sidebar, body))






#-------------------------------------------------------------------------------
#                                  SERVER
#-------------------------------------------------------------------------------
server <- function(input,output){
 
  #-------------------------------FACTS TAB------------------------------------
  
  output$years <- renderInfoBox({
    infoBox(
      "Years Considered", "16", icon = icon("calendar", lib = "font-awesome"),
      color = "blue", fill =TRUE, width = 1
    )
  }) 
  
  output$company <- renderInfoBox({
    infoBox(
      "Companies", "542", icon = icon("building", lib = "font-awesome"),
      color = "fuchsia", fill =TRUE, width = 1
    )
  }) 
  
  output$Loc <- renderInfoBox({
    infoBox(
      "Company Location", "67", icon = icon("compass", lib = "font-awesome"),
      color = "purple", fill =TRUE, width = 1
    )
  }) 
  
  output$rate <- renderInfoBox({
    infoBox(
      "Min and Max Rating", "1-4", icon = icon("star", lib = "font-awesome"),
      color = "green", fill =TRUE, width = 1
    )
  }) 
  
  output$ingr <- renderInfoBox({
    infoBox(
      "Ingredient Combination", "21", icon = icon("candy-cane", lib = "font-awesome"),
      color = "maroon", fill =TRUE, width = 1
    )
  }) 
  
  output$siz <- renderInfoBox({
    infoBox(
      "Dimension", "2443 & 11", icon = icon("hashtag", lib = "font-awesome"),
      color = "navy", fill =TRUE, width = 1
    )
  }) 
  
  output$plt1 <- renderPlot({ggplot(tab_1, aes(x=Ingredients, y=Count)) + 
      geom_bar(stat = "identity", width=0.6)+
      geom_col(fill="saddlebrown")+
      geom_text(aes(label = Count), vjust = 2, size = 5, color = "#ffffff")+
      theme(axis.text.x=element_text(angle=90,hjust=0.1,vjust=0.1))})
  
  #-------------------------------MapA TAB--------------------------------------
  output$map<-renderLeaflet({
    
    data2<-data1 %>% 
      mutate(popup_Info=paste("Location: ",Loc,"</br>","Count: ",count))
    # gradient based on the count
    colour<-c("green","red")
    
    # creating a pallet out of it
    pal<-colorFactor(colour,data2$count)
    
    
    leaflet(data = data2[,]) %>% addTiles() %>%
      addCircleMarkers(lng = ~Longitude,lat =  ~Latitude, radius=20, popup = ~popup_Info, label = ~Loc, color = ~pal(count))
    
  })
  #-------------------------------MapB TAB--------------------------------------
  output$map2<-renderLeaflet({
    
    df5a<-df5 %>% 
      mutate(popup_Info1=paste("Bean_Origin: ",Bean_origin,"</br>","Average_Rating: ",Average_rating))
    
    greenLeafIcon <- makeIcon(
      iconUrl = "map_pin.png",
      iconWidth = 16, iconHeight = 30,
      iconAnchorX = 12, iconAnchorY = 54
    )
    
    leaflet(data = df5a[,]) %>% addTiles() %>%
      addMarkers(lng = ~Longitude,lat =  ~Latitude, popup = ~popup_Info1, label = ~Bean_origin, icon=greenLeafIcon)
    
  })
  #-------------------------------plot1 TAB-------------------------------------
  
  o <- plot_ly(data, labels = ~a$Rating, values = ~a$Count, type = 'pie')
  o <- o%>% layout(
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   marker = list(line = list(color = '#FFFFFF', width = 1)))
  
  
  
  output$plt2 <- renderPlotly({o})
  
  
  
  
  #-------------------------------plot2 TAB-------------------------------------
  
  output$view <- renderTable({head(country_top, n = input$obs)})
  output$plt3 <- renderPlot({
    
    country_top %>% 
      head(n = input$obs) %>% 
      ggplot(aes(x=Countries, y=Count)) + 
      geom_point(size=9) + 
      geom_segment(aes(x=Countries,
                       xend=Countries, 
                       y=0, 
                       yend=Count),color="dark orange",size=3)+
      labs(subtitle="Bean Origin v/s Count") + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
  })
  
  
  
  #-------------------------------plot3 TAB-------------------------------------
  p <- ggplot(tab_2, aes(Review_Date, Rating))
    
  p <-  p +geom_line(size=2) +
    geom_smooth(method='lm',se = FALSE) + geom_point(color="dark orange", size=3)+
    xlab("Year") + ylab("Average Rating")+
    theme(
          axis.title.x = element_text(color="black", size=9),
          axis.title.y = element_text(color="black", size=9))
  
  
   output$plt4 <- renderPlotly({
     ggplotly(p)
    })
  
  
  
  #-------------------------------plot4 TAB-------------------------------------
   fig5 <- plot_ly(d, x = ~Year, y = ~Count, type = 'bar',
                  text = y, textposition = 'auto',
                  marker = list(color = 'navy',
                                line = list(color = 'orange', width = 1.5)))
   fig5 <- fig5 %>% layout(xaxis = list("Year"))
   fig5 <- fig5 %>% layout(yaxis = list(title = 'Number of Reviews'))
   output$plt5 <- renderPlotly({fig5})       
  
  
  #-------------------------------plot5 TAB-------------------------------------
   cp<- aggregate(data.frame(No_of_companies = data$Company), list(CocoaPer = data$CocoaPer), length)
   cp
   data$CocoaPer_bins <- 
     ifelse(data$CocoaPer < 50,"<50",
            ifelse(data$CocoaPer >= 50 & data$CocoaPer < 60,"50-60",
                   ifelse(data$CocoaPer >=60 & data$CocoaPer < 70,"60-70",
                          ifelse(data$CocoaPer >=70 & data$CocoaPer
                                 <80,"70-80",ifelse(data$CocoaPer >80 &
                                                      data$CocoaPer<90,"80-90","90-100")))))
   cp1<- aggregate(data.frame(No_of_companies = data$Company), list(CocoaPer = data$CocoaPer_bins), length)
   cp2<-head(cp1)
   fig6<- plot_ly(cp2, x = ~CocoaPer, y = ~No_of_companies, type = 'bar',
                  text = y, textposition = 'auto',
                  marker = list(color = 'navy',
                                line = list(color = 'orange', width = 1.5)))
   fig6<- fig6 %>% layout(xaxis = list("CocoaPer"))
   fig6 <- fig6 %>% layout(yaxis = list(title = 'Number of company'))
   output$plt6 <- renderPlotly({fig6})
  #-------------------------------RAW DATA TAB----------------------------------
  output$downloadData <- downloadHandler(
    filename=function(){
      paste("final","csv", sep = '.')
    },
    content=function(file){
      write.csv(data,file)
    }
  )
  
  output$tableData <- renderTable(
    head(data,200),width = "100%"
  )
  
}




















#-------------------------------------------------------------------------------
#                                  RUNNING THE APP
#-------------------------------------------------------------------------------
shinyApp(ui = ui,server = server)

#Reading the data base
tipo_accidentes<-c("District","Double collision","Multiple collision","Collision with fixed object","Accident with human","Overturns","Fall off a motorcycle", "Fall off a moped","Fall off a bike","Fall off in a bus","Other","total_victims","Year")
madrid_data<-read.csv("./final_database.csv",header=TRUE)
madrid_data$X<-NULL
madrid_data$Year<-factor(madrid_data$Year)
names(madrid_data)<-tipo_accidentes #Setting the names better
###


#Loading the UI
shinyUI(pageWithSidebar(
    headerPanel("Madrid driving accidents 2009-2014"), #Title
    #Calling the sidebar
    sidebarPanel(
        #Creating the tabs panel, with values included to choosen them in server.R
        tabsetPanel(id="sidebar_panel",
            tabPanel("Accident",value = 1,
                     selectInput(inputId='year1','Choose a year',choices=c(2009,2010,2011,2012,2013,2014,"Sum over years","Mean over years"),selected=1),
                     selectInput(inputId='select_acc',"Choose the kind of accident",choices = names(madrid_data[-c(1,ncol(madrid_data)-1,ncol(madrid_data))]), selected = 1)
                     
        ),  tabPanel("District",value=2,
                     selectInput(inputId='year2','Choose a year',choices=c(2009,2010,2011,2012,2013,2014,"Sum over years","Mean over years"),selected=1),
                     selectInput(inputId='select_dis',"Choose the district",choices = levels(madrid_data$District), selected = 1))
                    
        ),
        h4("Instructions:"),
        p("This application allows to visualize data driving accidents at Madrid (Spain) over the years 2009-2014."),
        p("The tab Accident allows to choose one kind of accident,and a year. It also allows to calculate the sum and the mean over those 6 years."),
        p("The tab District allows to choose between the different districts and over the years in the same way."),
        p("There are two visualization available that can be chosen with the tabs in the main panel. The slice visualization shows percentages, thus it does not change between the sum and mean."),
        selected=1,type="pills"),

    #main Panel to loads the plot
    mainPanel(  
        tabsetPanel(id="main_panel",
            tabPanel("Bar Plot", value=1,plotOutput("mybarplot")),
            tabPanel("Pie Plot", value=2,plotOutput("mypieplot"))
    ))
))

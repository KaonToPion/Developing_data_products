
library(plotrix) #Loading library for the slice plot


#Reading the data base
tipo_accidentes<-c("District","Double collision","Multiple collision","Collision with fixed object","Accident with human","Overturns","Fall off a motorcycle", "Fall off a moped","Fall off a bike","Fall off in a bus","Other","total_victims","Year")
madrid_data<-read.csv("./final_database.csv",header=TRUE)
madrid_data$X<-NULL
madrid_data$Year<-factor(madrid_data$Year)
names(madrid_data)<-tipo_accidentes #Setting the names better
###


#loading the server
shinyServer(
    function(input, output){
        
        ##############Bar plot calling###################
        output$mybarplot <- renderPlot({
                
            #Condition to change from the Accident to the District panel
            if(input$sidebar_panel==1){ #Accident panel
                   
                    if(input$year1=="Sum over years"){ # Calculation of the Sum over years
                    barplot(aggregate(madrid_data[,input$select_acc],by=list(madrid_data$District),FUN=sum)[,2],names.arg = tolower(levels(madrid_data$District)),col="blue",las=2,cex.names =0.95,xlab="District", ylab="Total Number of accidents")   
                    
                    }else if(input$year1=="Mean over years"){ #Calculation of the mean over years
                    
                    barplot(aggregate(madrid_data[,input$select_acc],by=list(madrid_data$District),FUN=mean)[,2],names.arg = tolower(levels(madrid_data$District)),col="blue",las=2,cex.names =0.95,xlab="District", ylab="Mean Number of accidents")
                
                    }else{ #All the years separated
                    
                    barplot(madrid_data[,input$select_acc][madrid_data$Year==input$year1],names.arg = tolower(levels(madrid_data$District)),col="blue",las=2,cex.names =0.95,xlab="District",ylab="Number of accidents")
            
            }}else if(input$sidebar_panel==2){ #District panel
                
                par(mar=c(8,4,1,3)) #Setting different margins to fit the plot into the canvas
                
                    if(input$year2=="Sum over years"){ # Calculation of the Sum over years
                    
                        aux<-madrid_data[madrid_data$District==input$select_dis,]
                        aux<-aux[-c(1,ncol(aux)-1,ncol(aux))]
                        sum<-vector(mode="numeric",length(aux))
                        for (i in 1:length(aux)){sum[i]<-sum(aux[i])}
                        barplot(sum,names.arg = names(madrid_data[-c(1,ncol(madrid_data)-1,ncol(madrid_data))]),col="blue",las=2,cex.names =0.95,xlab="Accidents",ylab="Total Number of accidents")   
                
                    }else if(input$year2=="Mean over years"){ #Calculation of the mean over years
                    
                        aux<-madrid_data[madrid_data$District==input$select_dis,]
                        aux<-aux[-c(1,ncol(aux)-1,ncol(aux))]
                        sum<-vector(mode="numeric",length(aux))
                        for (i in 1:length(aux)){sum[i]<-sum(aux[i])}
                        barplot(sum/6,names.arg = names(madrid_data[-c(1,ncol(madrid_data)-1,ncol(madrid_data))]),col="blue",las=2,cex.names =0.95,xlab="Accidents",ylab="Mean Number of accidents")
                
                    }else{ #All the years separated
                    
                        aux<-madrid_data[madrid_data$District==input$select_dis&madrid_data$Year==input$year2,]
                        barplot(as.vector(aux[-c(1,ncol(aux)-1,ncol(aux))],mode="numeric"),names.arg = names(madrid_data[-c(1,ncol(madrid_data)-1,ncol(madrid_data))]),col="blue",las=2,cex.names =0.95,xlab="Accidents",ylab="Number of accidents")
                                    
            }}},width =800,height = 550)
            
            ##############Pie plot calling###################
        output$mypieplot <- renderPlot({
            #Condition to change from the Accident to the District panel   
            if(input$sidebar_panel==1){#Accident panel
                    
                    if(input$year1=="Sum over years"|input$year1=="Mean over years"){ #Calculating the total sum percentage for each district (the mean is the same)
                        lbl<-tolower(levels(madrid_data$District))
                        pct<-aggregate(madrid_data[,input$select_acc],by=list(madrid_data$District),FUN=mean)[,2]
                        pct<-round(pct/sum(pct)*100)
                        lbl<-paste(lbl,pct)#Pasting the percentage for each label
                        lbl<-paste(lbl,"%",sep="")
                        pie3D(x = pct,labels =lbl,col=rainbow(length(lbl)),explode=0.2)
                    }else{ #Calculating the percentage of accidents for each district and all the years separated
                    
                        lbl<-tolower(levels(madrid_data$District))
                        pct<-madrid_data[,input$select_acc][madrid_data$Year==input$year1]
                        pct<-round(pct/sum(pct)*100)
                        lbl<-paste(lbl,pct)#Pasting the percentage for each label
                        lbl<-paste(lbl,"%",sep="")
                        pie3D(x = pct,labels =lbl,col=rainbow(length(lbl)),explode=0.2)
                
            }}else if(input$sidebar_panel==2){#District panel
                    
                    if(input$year2=="Sum over years"|input$year2=="Mean over years"){ #Calculating the total sum percentage for each accident (the mean is the same)

                        aux<-madrid_data[madrid_data$District==input$select_dis,]
                        aux<-aux[-c(1,ncol(aux)-1,ncol(aux))]
                        sum<-vector(mode="numeric",length(aux))
                        for (i in 1:length(aux)){sum[i]<-sum(aux[i])}
                        lbl<-names(madrid_data[-c(1,ncol(madrid_data)-1,ncol(madrid_data))])
                        pct<-round(sum/sum(sum)*100)
                        lbl<-paste(lbl,pct)#Pasting the percentage for each label
                        lbl<-paste(lbl,"%",sep="")
                        pie3D(x = pct,labels =lbl,col=rainbow(length(lbl)),explode=0.1)
                    }else{ #Calculating the percentage of accidents for each kind of accident and all the years separated
                    aux<-madrid_data[madrid_data$District==input$select_dis&madrid_data$Year==input$year2,]
                    lbl<-names(madrid_data[-c(1,ncol(madrid_data)-1,ncol(madrid_data))])
                    pct<-as.vector(aux[-c(1,ncol(aux)-1,ncol(aux))],mode="numeric")
                    pct<-round(pct/sum(pct)*100)
                    lbl<-paste(lbl,pct) #Pasting the percentage for each label
                    lbl<-paste(lbl,"%",sep="")
                    pie3D(x = pct,labels =lbl,col=rainbow(length(lbl)),explode=0.1)
                
        }}},width =800,height = 550)
        
    }
)
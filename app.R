#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)


ui <- fluidPage(
    h1("Ireland's Transport Emissions"),
    fluidRow(column(3,wellPanel(h3("Avoid"),
                    sliderInput(inputId="caravoid",label="% car kilometres avoided",min=0,max=12,value=0),
                    sliderInput(inputId="lgvavoid",label="% lgv kilometres avoided",min=0,max=10,value=0),
                    sliderInput(inputId="hgvavoid",label="% hgv kilometres avoided",min=0,max=8,value=0))),
             column(3,wellPanel(h3("Shift"),
                                sliderInput(inputId = "car2walk",label="% car kilometres replaced by walking",min=0,max=4,value=0),
                                sliderInput(inputId = "car2bike",label="% car kilometres replaced by bike",min=0,max=3,value=0),
                                sliderInput(inputId = "car2ebike",label="% car kilometres replaced by e-bike",min=0,max=15,value=0),
                                sliderInput(inputId = "car2bus",label="% car kilometres replaced by bus",min=0,max=20,value=0),
                                sliderInput(inputId = "car2train",label="% car kilometres replaced by train",min=0,max=8,value=0),
                                sliderInput(inputId = "lgv2ebike",label="% lgv kilometres replaced by e-bike",min=0,max=10,value=0),
                                sliderInput(inputId = "hgv2rail",label="% hgv kilometres replaced by train",min=0,max=4,value=0)
                                )),
             column(3,wellPanel(h3("Electrify"),
                    sliderInput(inputId="carelec",label="% car kilometres electrified",min=1.5,max=50,value=1.5),
                    sliderInput(inputId="lgvelec",label="% lgv kilometres electrified",min=0.2,max=50,value=0.2),
                    sliderInput(inputId="hgvelec",label="% hgv kilometres electrified",min=0.01,max=20,value=0.01),
                    sliderInput(inputId="buselec",label="% bus kilometres electrified",min=0,max=100,value=0),
                    sliderInput(inputId="trainelec",label="% train kilometres electrified",min=50,max=100,value=50))),
             column(3,wellPanel(h3("Improve"),
                    sliderInput(inputId="careff",label="% car efficiency improvement",min=0,max=15,value=0),
                    sliderInput(inputId="lgveff",label="% lgv efficiency improvement",min=0,max=15,value=0),
                    sliderInput(inputId="hgveff",label="% hgv efficiency improvement",min=0,max=15,value=0)))),
    
    fluidRow(column(3,plotOutput(outputId="modes")),column(3,plotOutput(outputId="Ems")),column(3,plotOutput(outputId="emissions")),column(3,plotOutput(outputId="fuels")))
    )

{rownames<-c("Conventional, gCO2/km","Electric, kWH/km","Average Occupancy","Billion Vehicle km")
    colnames<-c("Walk","Cycle","Car","LGV","HGV","Bus","Train")
    matrix1<-matrix(c(0,0,1,2.48,0,0.0093,1,0.85,0.131,0.15 ,1.4,56.54,0.168,0.276,1,11.04,0.482,0.924,1,2.28,0.79,1.96,17.3,1.42,2.35,3.12,60,0.07),ncol=7,dimnames = list(rownames,colnames))
    elec<- function(x){
        result<-0.324-(0.324/14)*x
    }
    pop<-function(x){
        result<-1+0.23*x/29
    }
    uptake<-function(x,y){
        result<-1/(1+197/3*exp(log(3/197*(1-y)/y)*x/10))
    }
    carelec<-function(x,y){
        result<-uptake(x,y)*elec(x)*matrix1[2,3]+(1-uptake(x,y))*matrix1[1,3]
    }
    uptakelgv<-function(x,y){
        result<-1/(1+569*exp(log(1/569*(1-y)/y)*x/10))
    }
    lgvelec<-function(x,y){
        uptakelgv(x,y)*elec(x)*matrix1[2,4]+(1-uptakelgv(x,y))*matrix1[1,4]
    }
    uptakehgv<-function(x,y){
        result<-1/(1+7999*exp(log(1/7999*(1-y)/y)*x/10))
    }
    hgvelec<-function(x,y){
        uptakehgv(x,y)*elec(x)*matrix1[2,5]+(1-uptakehgv(x,y))*matrix1[1,5]
    }
    buselec<-function(x,y){
        result<-y/10*x*elec(x)*matrix1[2,6]+(1-y/10*x)*matrix1[1,6]
    }
    trainelec<-function(x,y){
        result<-y/10*x*elec(x)*matrix1[2,7]+(1-y/10*x)*matrix1[1,7]
    }
    x<-seq(2021,2030,1)
    tem<-function(x,p,q,r,c2w,c2b,c2eb,c2bs,c2t,l2eb,h2t,a,b,c,d,e,f,g,h){
        result<-(pop(x)*((1-(0.5+x/20)*p-c2w*x/10-c2b*x/10-c2eb*x/10-c2bs*x/10-c2t*x/10)*matrix1[4,3]*(1-f*x/10)*carelec(x,a)+matrix1[4,4]*(1-q*x/10-l2eb*x/10-g*x/10)*lgvelec(x,b)+matrix1[4,5]*(1-r*x/10-h2t*x/10-h*x/10)*hgvelec(x,c)+(1+c2bs*x/10*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,6]*matrix1[4,6])))*matrix1[4,6]*buselec(x,d)+(1+h2t*x/10*(matrix1[4,3]/(100*matrix1[4,7]))+c2t*x/10*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,7]*matrix1[4,7])))*matrix1[4,7]*trainelec(x,e)+c2eb*x/10*1.4*elec(x)*matrix1[4,3]*matrix1[2,2]+l2eb*x/10*10*matrix1[4,4]*matrix1[2,2]+0.028*12))
    }
    walk<-function(x,c2w){
        result<-(1+c2w*x/10*1.4*(matrix1[4,3]/matrix1[4,1]))*matrix1[4,1]
    }
    cycle<-function(x,c2b,c2eb){
        result<-(1+1.4*(c2b*x/10+c2eb*x/10)*(matrix1[4,3]/matrix1[4,2]))*matrix1[4,2]
    }
    car<-function(x,p,c2w,c2b,c2eb,c2bs,c2t){
        result<-(1-(0.5+x/20)*p-c2w*x/10-c2b*x/10-c2eb*x/10-c2bs*x/10-c2t*x/10)*matrix1[4,3]
    }
    bus<-function(x,c2bs){
        result<-(1+c2bs*x/10*((matrix1[4,3]*matrix1[3,3])/(matrix1[4,6]*matrix1[3,6])))*matrix1[4,6]
    }
    train<-function(x,c2t){
        result<-(1+c2t*x/10*((matrix1[4,3]*matrix1[3,3])/(matrix1[4,7]*matrix1[3,7])))*matrix1[4,7]
    }
    lgv<-function(x,q,l2eb){
        result<-(1-q-l2eb*x/10)*matrix1[4,4]
    }
    hgv<-function(x,r,h2t){
        result<-(1-r-h2t*x/10)*matrix1[4,5]
    }
    petrol<-function(x,p,c2w,c2b,c2eb,c2bs,c2t,a,f){
        result<-pop(x)*(0.554*(1-uptake(x,a))*(1-(0.5+x/20)*p-c2w*x/10-c2b*x/10-c2eb*x/10-c2bs*x/10-c2t*x/10)*matrix1[4,3]*0.29*(1-f*x/10))
    }
    diesel<-function(x,p,q,r,c2w,c2b,c2eb,c2bs,c2t,l2eb,h2t,a,b,c,d,e,f,g,h){
        result<-pop(x)*(0.498*(1-uptake(x,a))*(1-(0.5+x/20)*p-c2w*x/10-c2b*x/10-c2eb*x/10-c2bs*x/10-c2t*x/10)*matrix1[4,3]*0.71*(1-f*x/10)+0.653*(1-uptakelgv(x,b))*(1-g*x/10)*(1-q-l2eb*x/10)*matrix1[4,4]+1.87*(1-uptakehgv(x,c))*(1-h*x/10)*(1-r-h2t*x/10)*matrix1[4,5]+3.06*(1+c2bs*x/10*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,6]*matrix1[4,6])))*matrix1[4,6]*(1-d/10*x)+3.12*(1+h2t*x/10*(matrix1[4,3]/(100*matrix1[4,7]))+c2t*x/10*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,7]*matrix1[4,7])))*matrix1[4,7]*(1-e*x/10))
    }
    electric<-function(x,p,q,r,c2w,c2b,c2eb,c2bs,c2t,l2eb,h2t,a,b,c,d,e,f,g,h){
        result<-pop(x)*(matrix1[2,3]*(uptake(x,a))*(1-(0.5+x/20)*p-c2w*x/10-c2b*x/10-c2eb*x/10-c2bs*x/10-c2t*x/10)*matrix1[4,3]*(1-f*x/10)+matrix1[2,4]*(uptakelgv(x,b))*(1-g*x/10)*(1-q-l2eb*x/10)*matrix1[4,4]+matrix1[2,5]*(uptakehgv(x,c))*(1-h*x/10)*(1-r-h2t*x/10)*matrix1[4,5]+matrix1[2,6]*(1+c2bs*x/10*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,6]*matrix1[4,6])))*matrix1[4,6]*(d/10*x)+matrix1[2,7]*(1+h2t*x/10*(matrix1[4,3]/(100*matrix1[4,7]))+c2t*x/10*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,7]*matrix1[4,7])))*matrix1[4,7]*(e*x/10)+matrix1[2,2]*(c2eb*x/10*1.4*matrix1[4,3]+l2eb*x/10*matrix1[4,4]))
    }
   }

server <- function(input, output) {
    
    output$modes<-renderPlot({
        title3<-"Mode share by passenger km in 2030"
        labels2<-c("Walking","Cycling","Car","Bus","Train")
        z2<-c(walk(10,input$car2walk/100),cycle(10,input$car2bike/100,input$car2ebike/100),1.4*car(10,input$caravoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100),17.3*bus(10,input$car2bus/100),60*train(10,input$car2train/100))
        pipercent2<-round(100*z2/sum(z2),1)
        pie(z2,pipercent2,main=title3,col=c( "#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6"))
        legend("bottomleft",labels2,fill=c( "#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6"))
    })
    
    output$Ems<-renderPlot({
        title4<-"Emissions"
        labels4<-c("Car","LGV","Bus","HGV","Train","Other")
        z4<-c(uptake(0,3/200)*elec(0)*matrix1[2,3]*car(0,0,0,0,0,0,0)+(1-uptake(0,3/200))*matrix1[1,3]*car(0,0,0,0,0,0,0),uptakelgv(0,input$lgvelec/100)*elec(0)*matrix1[2,4]*(1)*lgv(0,0,0)+(1-uptakelgv(0,input$lgvelec/100))*matrix1[1,4]*(1)*lgv(0,0,0),matrix1[1,6] *bus(0,0),uptakehgv(00,input$hgvelec/100)*elec(00)*matrix1[2,5]*hgv(00,0,0)+(1-uptakehgv(0,input$hgvelec/100))*matrix1[1,5]*hgv(0,0,0),0.5*elec(0)*matrix1[2,7]*train(0,0)+0.5*matrix1[1,7]*train(0,0),0.028*12,pop(10)*uptake(10,input$carelec/100)*elec(9)*matrix1[2,3]*(1-input$careff/100)*car(10,input$caravoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100)+pop(10)*(1-uptake(10,input$carelec/100))*matrix1[1,3]*(1-input$careff/100)*car(10,input$caravoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100),pop(10)*uptakelgv(10,input$lgvelec/100)*elec(9)*matrix1[2,4]*(1-input$lgveff/100)*lgv(10,input$lgvavoid/100,input$lgv2ebike/100)+pop(10)*(1-uptakelgv(10,input$lgvelec/100))*matrix1[1,4]*(1-input$lgveff/100)*lgv(10,input$lgvavoid/100,input$lgv2ebike/100),pop(10)*input$buselec/100*elec(9)*matrix1[2,6] *bus(10,input$car2bus/100)+pop(10)*(1-input$buselec/100)*matrix1[1,6] *bus(10,input$car2bus/100),pop(10)*uptakehgv(10,input$hgvelec/100)*elec(9)*matrix1[2,5]*(1-input$hgveff/100)*hgv(10,input$hgvavoid/100,input$hgv2rail/100)+pop(10)*(1-uptakehgv(10,input$hgvelec/100))*matrix1[1,5]*(1-input$hgveff/100)*hgv(10,input$hgvavoid/100,input$hgv2rail/100),pop(10)*input$trainelec/100*elec(9)*matrix1[2,7]*train(10,input$car2train/100)+pop(10)*(1-input$trainelec/100)*matrix1[1,7]*train(10,input$car2train/100),0.028*12+elec(9)*matrix1[2,2]*(input$lgv2ebike/100 *matrix1[4,4]+1.4*input$car2ebike/100*matrix1[4,3]))
        z5<-matrix(z4,nrow=6,ncol=2)
        years<-c("2019","2030")
        barplot(z5,ylim=c(0,12),col=c("#D73027", "#FC8D59", "#FEE090", "#E0F3F8", "#91BFDB", "#4575B4"),xlab="year",ylab=expression("Mt CO"[2]),names.arg=years,legend.text=labels4)
    })
    
    output$emissions<-renderPlot({
        title<-"Transport emissions"
        labels5<-c("Avoided","Shifted","Electrified","Improved","Emitted")
        
        plot(x,tem(x-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100),ylab=expression("Annual emissions, Mt CO"[2]),xlab="Year",type="l",main=title,ylim=c(0,14),lwd=1)
        grid()
        lines(x,tem(x-2021,0,0,0,0,0,0,0,0,0,0,0.015,0.002,0.0001,0,0.5,0,0,0),lty=2,lwd=1)
        polygon(c(seq(2021,2030,1),rev(seq(2021,2030,1))),c(tem(seq(2021,2030,1)-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100),rep(0,10)),col = "#4575B4", lty = 0)
        polygon(c(x,rev(x)),c(tem(x-2021,0,0,0,0,0,0,0,0,0,0,0.015,0.002,0.0001,0,0.5,0,0,0),rev(tem(x-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,0,0,0,0,0,0,0,0.015,0.002,0.0001,0,0.5,0,0,0))),col = "#D73027", lty = 0)
        polygon(c(x,rev(x)),c(tem(x-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,0,0,0,0,0,0,0,0.015,0.002,0.0001,0,0.5,0,0,0),rev(tem(x-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,0.015,0.002,0.0001,0,0.5,0,0,0))),col = "#FC8D59", lty = 0)
        polygon(c(x,rev(x)),c(tem(x-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,0.015,0.002,0.0001,0,0.5,0,0,0),rev(tem(x-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,0,0,0))),col = "#FEE090", lty = 0)
        polygon(c(x,rev(x)),c(tem(x-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,0,0,0),rev(tem(x-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100))),col = "#E0F3F8", lty = 0)
        legend(2021,6,labels5,c("#D73027", "#FC8D59", "#FEE090", "#E0F3F8", "#91BFDB", "#4575B4"))
        lines(x,tem(x-2021,0,0,0,0,0,0,0,0,0,0,0.015,0.002,0.0001,0,0.5,0,0,0),lty=1,lwd=2)
        lines(x,tem(x-2021,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100),lty=1,lwd=2)
        
        
})
    output$fuels<-renderPlot({
        title2<-"Energy used by fuel"
        labels<-c("Petrol","Diesel","Electricity")
        z<-c(petrol(0,input$caravoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$carelec/100,input$careff/100),diesel(0,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100),electric(0,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100),petrol(10,input$caravoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$carelec/100,input$careff/100),diesel(10,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100),electric(10,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100))
        z7<-matrix(z,nrow=3,ncol=2)
        years<-c("2019","2030")
        barplot(z7,main=title2,col=c("#FC8D59", "#FFFFBF", "#91BFDB"),xlab="year",ylab="TWh",names.arg=years)
        legend("topleft",labels,fill=c("#FC8D59", "#FFFFBF", "#91BFDB"))
        
})
    }


# Run the application 
shinyApp(ui = ui, server = server)

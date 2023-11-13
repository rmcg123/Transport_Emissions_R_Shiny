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

    # Function that returns the electricity carbon intensity in kgCO2 per KWh
    # at some point in the future x on the assumption that a zero carbon
    # electricity sector is reached by 2035.
    elec<- function(x){
        result<-0.324-(0.324/14)*x
    }

    # Function that returns the multiplier for population at some point in the
    # future versus now. From CSO data.
    pop<-function(x){
        result<-1+0.23*x/29
    }

    # Function that gives the uptake of electric cars at some point in future x
    # presuming that the level of penetration y is reached in 2030.
    uptake<-function(x,y){
        result<-1/(1+197/3*exp(log(3/197*(1-y)/y)*x/10))
    }

    # Function that gives the average emissions intensity in kgCO2/km of the
    # entire car fleet, electric and fossil fuel.
    carelec<-function(x,y){
        result<-uptake(x,y)*elec(x)*matrix1[2,3]+(1-uptake(x,y))*matrix1[1,3]
    }

    # Function that gives the uptake of electric light goods vehicles at
    # some point in future x presuming that the level of penetration y is
    # reached in 2030.
    uptakelgv<-function(x,y){
        result<-1/(1+569*exp(log(1/569*(1-y)/y)*x/10))
    }

    # Function that gives the average emissions intensity in kgCO2/km of the
    # entire light goods vehicle fleet,electric and fossil fuel.
    lgvelec<-function(x,y){
        uptakelgv(x,y)*elec(x)*matrix1[2,4]+(1-uptakelgv(x,y))*matrix1[1,4]
    }
    
    # Function that gives the uptake of electric heavy goods vehicles at
    # some point in future x presuming that the level of penetration y is
    # reached in 2030.
    uptakehgv<-function(x,y){
        result<-1/(1+7999*exp(log(1/7999*(1-y)/y)*x/10))
    }

    # Function that gives the average emissions intensity in kgCO2/km of the
    # entire heavy goods vehicle fleet,electric and fossil fuel.
    hgvelec<-function(x,y){
        uptakehgv(x,y)*elec(x)*matrix1[2,5]+(1-uptakehgv(x,y))*matrix1[1,5]
    }

    # Function that gives the average emissions intensity in kgCO2/km of the
    # entire bus fleet,electric and fossil fuel.
    buselec<-function(x,y){
        result<-y/10*x*elec(x)*matrix1[2,6]+(1-y/10*x)*matrix1[1,6]
    }

    # Function that gives the average emissions intensity in kgCO2/km of the
    # entire train fleet,electric and fossil fuel.
    trainelec<-function(x,y){
        result<-y/10*x*elec(x)*matrix1[2,7]+(1-y/10*x)*matrix1[1,7]
    }
    
    # Function that gives the amount of kilometres walked in year x based on a
    # a selected shift from car to walking percentage.
    walk<-function(x,c2w){
      result<-(1+c2w*x/10*1.4*(matrix1[4,3]/matrix1[4,1]))*matrix1[4,1]
    }
    
    # Function that gives the amount of kilometres cycled in year x based on a
    # a selected shift from car to bike percentage.
    cycle<-function(x,c2b){
      result<-(1+1.4*(c2b*x/10)*(matrix1[4,3]/matrix1[4,2]))*matrix1[4,2]
    }
    
    
    # Function that gives the amount of kilometres driven by cars in year x
    # based on selected amounts of avoid and shifts to various modes.
    car<-function(x,p,c2w,c2b,c2eb,c2bs,c2t){
      result<-(1-(0.5+x/20)*p-c2w*x/10-c2b*x/10-c2eb*x/10-c2bs*x/10-c2t*x/10)*matrix1[4,3]
    }
    
    # Function that gives the amount of kilometres driven by buses in year x
    # based on selected amounts of shifts from car to bus.
    bus<-function(x,c2bs){
      result<-(1+c2bs*x/10*((matrix1[4,3]*matrix1[3,3])/(matrix1[4,6]*matrix1[3,6])))*matrix1[4,6]
    }
    
    # Function that gives the amount of kilometres cycled by ebike in year x
    # based on shifts from car and light goods vehicles.
    ebike<-function(x,c2eb,l2eb){
      result<-(c2eb*1.4*matrix1[4,3]+l2eb*10*matrix1[4,4])*x/10
    }
    
    # Function that gives the amount of kilometres driven by trains in year x
    # based on shifts from car and heavy goods vehicles.
    train<-function(x,c2t,h2t){
      result<-(1+h2t*x/10*(matrix1[4,3]/(100*matrix1[4,7]))+c2t*x/10*((matrix1[4,3]*matrix1[3,3])/(matrix1[4,7]*matrix1[3,7])))*matrix1[4,7]
    }
    
    # Function that gives the amount of kilometres driven by light goods
    # vehicles in year x based on avoided demand and shifts to ebike
    lgv<-function(x,q,l2eb){
      result<-(1-q*x/9-l2eb*x/9)*matrix1[4,4]
    }
    
    # Function that gives the amount of kilometres driven by heavy goods
    # vehicles in year x based on avoided demand and shifts to train
    hgv<-function(x,r,h2t){
      result<-(1-r*x/9-h2t*x/9)*matrix1[4,5]
    }
    
    # list of years from 2021 to 2030.
    x<-seq(2021,2030,1)
    
    # The emissions from the car fleet is the kilometres driven times the average
    # fleet efficiency times the population multiplier factor.
    car_emissions<-function(x,p,c2w,c2b,c2eb,c2bs,c2t,f,a){
      result<-(pop(x)*(car(x,p,c2w,c2b,c2eb,c2bs,c2t)*(1-f*x/9)*carelec(x,a)))
    }
    
    # The emissions from the lgv fleet is the kilometres driven times the average
    # fleet efficiency times the population multiplier factor.
    lgv_emissions<-function(x,q,l2eb,g,b){
      result<-(pop(x)*(lgv(x,q,l2eb)*(1-g*x/9)*lgvelec(x,b)))
    }
    
    # The emissions from the hgv fleet is the kilometres driven times the average
    # fleet efficiency times the population multiplier factor.
    hgv_emissions<-function(x,r,h2t,h,c){
      result<-(pop(x)*(hgv(x,r,h2t)*(1-h*x/9)*hgvelec(x,c)))
    }
    
    # The emissions from the bus fleet is the kilometres driven times the average
    # fleet efficiency times the population multiplier factor.
    bus_emissions<-function(x,c2bs,d){
      result<-(pop(x)*(bus(x,c2bs)*buselec(x,d)))
    }
    
    # The emissions from the train fleet is the kilometres driven times the average
    # fleet efficiency times the population multiplier factor.
    train_emissions<-function(x,c2t,h2t,e){
      result<-(pop(x)*(train(x,c2t,h2t)*trainelec(x,e)))
    }
    
    # The emissions from the ebike fleet is the kilometres cycled times the average
    # fleet efficiency times the population multiplier factor.
    ebike_emissions<-function(x,c2eb,l2eb){
      result<-(pop(x)*(ebike(x,c2eb,l2eb)*elec(x)*matrix1[2,2]))
    }
    
    # Total emissions function is the sum of all emitting sectors of transport,
    # including "other" transport emissions.
    tem<-function(x,p,q,r,c2w,c2b,c2eb,c2bs,c2t,l2eb,h2t,a,b,c,d,e,f,g,h){
        result<-(
          car_emissions(x,p,c2w,c2b,c2eb,c2bs,c2t,f,a)
          + lgv_emissions(x,q,l2eb,g,b)
          + hgv_emissions(x,r,h2t,h,c)
          + bus_emissions(x,c2bs,d)
          + train_emissions(x,c2t,h2t,e)
          + ebike_emissions(x,c2eb,l2eb)
          + pop(x)*0.028*12)
    }
    
    # Petrol usage in year x based on selected avoided demand, shifts,
    # electrification and other vehicle efficiency.
    petrol<-function(x,p,c2w,c2b,c2eb,c2bs,c2t,a,f){
        result<-pop(x)*(0.554*(1-uptake(x,a))*(1-(0.5+x/20)*p-c2w*x/9-c2b*x/9-c2eb*x/9-c2bs*x/9-c2t*x/9)*matrix1[4,3]*0.29*(1-f*x/9))
    }
    
    # Diesel usage in year x based on selected avoided demand, shifts,
    # electrification and other vehicle efficiency.
    diesel<-function(x,p,q,r,c2w,c2b,c2eb,c2bs,c2t,l2eb,h2t,a,b,c,d,e,f,g,h){
        result<-pop(x)*(0.498*(1-uptake(x,a))*(1-(0.5+x/20)*p-c2w*x/9-c2b*x/9-c2eb*x/9-c2bs*x/9-c2t*x/9)*matrix1[4,3]*0.71*(1-f*x/9)+0.653*(1-uptakelgv(x,b))*(1-g*x/9)*(1-q-l2eb*x/9)*matrix1[4,4]+1.87*(1-uptakehgv(x,c))*(1-h*x/9)*(1-r-h2t*x/9)*matrix1[4,5]+3.06*(1+c2bs*x/9*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,6]*matrix1[4,6])))*matrix1[4,6]*(1-d/9*x)+3.12*(1+h2t*x/9*(matrix1[4,3]/(100*matrix1[4,7]))+c2t*x/9*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,7]*matrix1[4,7])))*matrix1[4,7]*(1-e*x/9))
    }
    
    # Electricity usage in year x based on selected avoided demand, shifts,
    # electrification and other vehicle efficiency.
    electric<-function(x,p,q,r,c2w,c2b,c2eb,c2bs,c2t,l2eb,h2t,a,b,c,d,e,f,g,h){
        result<-pop(x)*(matrix1[2,3]*(uptake(x,a))*(1-(0.5+x/20)*p-c2w*x/9-c2b*x/9-c2eb*x/9-c2bs*x/9-c2t*x/9)*matrix1[4,3]*(1-f*x/9)+matrix1[2,4]*(uptakelgv(x,b))*(1-g*x/9)*(1-q-l2eb*x/9)*matrix1[4,4]+matrix1[2,5]*(uptakehgv(x,c))*(1-h*x/9)*(1-r-h2t*x/9)*matrix1[4,5]+matrix1[2,6]*(1+c2bs*x/9*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,6]*matrix1[4,6])))*matrix1[4,6]*(d/9*x)+matrix1[2,7]*(1+h2t*x/9*(matrix1[4,3]/(100*matrix1[4,7]))+c2t*x/9*((matrix1[3,3]*matrix1[4,3])/(matrix1[3,7]*matrix1[4,7])))*matrix1[4,7]*(e*x/9)+matrix1[2,2]*(c2eb*x/9*1.4*matrix1[4,3]+l2eb*x/9*matrix1[4,4]))
    }
   }

server <- function(input, output) {
    
    output$modes<-renderPlot({
        title3<-"Mode share by passenger km in 2030"
        labels2<-c("Walking","Cycling","Car","Bus","Train")
        z2<-c(walk(9,input$car2walk/100),cycle(9,input$car2bike/100) + ebike(9,input$car2ebike/100, 0),1.4*car(9,input$caravoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100),17.3*bus(9,input$car2bus/100),60*train(9,input$car2train/100,input$hgv2rail/100))
        pipercent2<-round(100*z2/sum(z2),1)
        pie(z2,pipercent2,main=title3,col=c( "#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6"))
        legend("bottomleft",labels2,fill=c( "#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6"))
    })
    
    output$Ems<-renderPlot({
        title4<-"Emissions"
        labels4<-c("Car","LGV","Bus","HGV","Train","Other")
        z4<-c(car_emissions(0,0,0,0,0,0,0,0,3/200),
              lgv_emissions(0,0,0,0,0.002),
              bus_emissions(0,0,0),
              hgv_emissions(0,0,0,0,0.0001),
              train_emissions(0,0,0,0.1),
              0.028*12,
              car_emissions(9,input$caravoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$careff/100,input$carelec/100),
              lgv_emissions(9,input$lgvavoid/100,input$lgv2ebike/100,input$lgveff/100,input$lgvelec/100),
              bus_emissions(9,input$car2bus/100,input$buselec/100),
              hgv_emissions(9,input$hgvavoid/100,input$hgv2rail/100,input$hgveff/100,input$hgvelec/100),
              train_emissions(9,input$car2train/100,input$hgv2rail/100,input$trainelec/100),
              0.028*12+ebike_emissions(9,input$car2ebike/100,input$lgv2ebike/100))
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
        z<-c(petrol(0,input$caravoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$carelec/100,input$careff/100),
             diesel(0,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100),
             electric(0,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100),
             petrol(9,input$caravoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$carelec/100,input$careff/100),
             diesel(9,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100),
             electric(9,input$caravoid/100,input$lgvavoid/100,input$hgvavoid/100,input$car2walk/100,input$car2bike/100,input$car2ebike/100,input$car2bus/100,input$car2train/100,input$lgv2ebike/100,input$hgv2rail/100,input$carelec/100,input$lgvelec/100,input$hgvelec/100,input$buselec/100,input$trainelec/100,input$careff/100,input$lgveff/100,input$hgveff/100))
        z7<-matrix(z,nrow=3,ncol=2)
        years<-c("2019","2030")
        barplot(z7,main=title2,col=c("#FC8D59", "#FFFFBF", "#91BFDB"),xlab="year",ylab="TWh",names.arg=years)
        legend("topleft",labels,fill=c("#FC8D59", "#FFFFBF", "#91BFDB"))
        
})
    }


# Run the application 
shinyApp(ui = ui, server = server)

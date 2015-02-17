library(shiny)
shinyServer(function(input, output) { 
  output$regressionPlot <- renderPlot({
    library(datasets)
    data<-mtcars
    data$cyl<-factor(data$cyl)
    data$am<-factor(data$am)
    levels(data$am)<-c("automatic","manual")
    data$gear<-factor(data$gear)
    data$carb<-factor(data$carb)
    #7 modles for analysis
    model1<-lm(mpg~am,data=data)
    model2<-lm(mpg~cyl,data=data)
    model3<-lm(mpg~cyl+wt,data=data)
    model4<-lm(mpg~cyl+wt+hp,data=data)
    model5<-lm(mpg~am+cyl+wt+hp,data=data)
    model6<-lm(mpg~.,data=data)
    model7<-lm(mpg~ wt + qsec + am,data=data)
    
    par(cex=.8)
    attach(data)
    if( input$model.name=="model1") {
      plot(am, mpg, main="Scatterplot mpg vs am and regression line lm<-mpg~am", 
           xlab="Automatic or Manula ", ylab="Miles Per Gallon ", pch=19)
      abline(model1)
      p <- ggplot(mtcars, aes(wt, mpg)) + 
        geom_point(aes(colour = factor(cyl)), size = 4)+
        geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
    } else if ( input$model.name=="model2") {
      p <- ggplot(data=mtcars, aes( x=cyl, y=mpg)) + 
        geom_point(aes(colour = factor(cyl)), size = 4)+
        geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
      #plot(wt, mpg, main="Scatterplot mpg vs wt and and regression line lm<-mpg~cyl", 
      #xlab="Car weight ", ylab="Miles Per Gallon ", pch=19)
      #abline(model2)
    } else if ( input$model.name=="model3") {
      p <- ggplot(data=mtcars, aes( x=cyl+wt, y=mpg)) + 
        geom_point(aes(colour = factor(cyl)), size = 4)+
        geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
      #plot(wt, mpg,main="Scatterplot mpg vs wt and and regression line lm<-mpg~cyl+wt", 
      #xlab="Car weight ", ylab="Miles Per Gallon ", pch=19)
      #abline(model3)
    }else if ( input$model.name=="model4") {
      p <- ggplot(data=mtcars, aes( x=cyl+wt+hp, y=mpg)) +
        geom_point(aes(colour = factor(cyl)), size = 4)+
        geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
      #plot(wt, mpg,main="Scatterplot mpg vs wt  and regression line lm<-mpg~cyl", 
      #xlab="Car weight ", ylab="Miles Per Gallon ", pch=19)
      #abline(model4)
    } else if ( input$model.name=="model5") {
      
      p <- ggplot(data=mtcars, aes( x=am+cyl+wt+hp, y=mpg)) + 
        geom_point(aes(colour = factor(am)), size = 4)+
        geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
      #plot(wt, mpg,main="Scatterplot mpg vs wt and regression line lm<-am + cyl + wt + hp", 
      #xlab="Car weight ", ylab="Miles Per Gallon ", pch=19)
      #abline(model5)
    } else  if ( input$model.name=="model6") {
      
      p <- ggplot(data=mtcars, aes( x=cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, y=mpg)) + 
        geom_point(aes(colour = factor(am)), size = 4)+
        geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
      #plot(wt, mpg,main="Scatterplot mpg vs wt  and regression line lm<-mpg~.", 
      #xlab="Car weight ", ylab="Miles Per Gallon ", pch=19)
      #abline(model6)
    }else  if ( input$model.name=="model7") {      
      p <- ggplot(data=mtcars, aes( x=wt + qsec + am, y=mpg)) + 
        geom_point(aes(colour = factor(am)), size = 4)+
        geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
      #plot(wt, mpg,main="Scatterplot mpg vs wt and regression line lm<-mpg~wt + qsec + am", 
      #xlab="Car weight ", ylab="Miles Per Gallon ", pch=19)
      #abline(model7)
    }
    print(p)
  })
}
)

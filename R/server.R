library(htmltools)
library(shiny)
library(DT)
library(nlme)
library(ggplot2)
library(gridExtra)
library(data.table)
library(plyr)
library(dplyr)
library(formattable)
library(tidyr)
library(MASS)
library(shinyjs)
library(compiler)
library(future.apply)

server <- shinyServer(function(input, output){


  compTime = eventReactive(input$input_action_time, {
    
     withProgress(message = 'Estimating expected computational time',
                   style = getShinyOption("progress.style",
     default = "old"),{

    time.sim(input$Model, input$N, input$N.0, input$N.1, input$T, 
    input$b00, input$b01.Z, input$b01.W, input$b10, input$b11.Z, input$b11.W, 
    input$sigma, input$rho, input$sigma.v0, input$sigma.v1,input$rho.v, 
    input$mu.W, input$sigma.W, input$mu.X, input$mu.X0, input$mu.X1, input$sigma.X, 
    input$sigma.X0, input$sigma.X1,
    input$is.rho.zero,input$isW.center,input$isX.center,input$Ylag.center, 
    input$alpha,input$R,input$Opt.Method)
    })                 
  })

  power.sim = eventReactive(input$input_action, {

     withProgress(message = 'Running... (check estimated computational time)',
                   detail = 'Go get a cup of tea...',style = getShinyOption("progress.style",
     default = "old"),{
                     
     withCallingHandlers({
     shinyjs::html("text", "")

    Sim.model.IL(input$Model, input$N, input$N.0, input$N.1, input$T, 
    input$b00, input$b01.Z, input$b01.W, input$b10, input$b11.Z, input$b11.W, 
    input$sigma, input$rho, input$sigma.v0, input$sigma.v1,input$rho.v, 
    input$mu.W, input$sigma.W, input$mu.X, input$mu.X0, input$mu.X1, input$sigma.X, 
    input$sigma.X0, input$sigma.X1,
    input$is.rho.zero,input$isW.center,input$isX.center,input$Ylag.center, 
    input$alpha,input$R,input$Opt.Method) 
      },
        message = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
      })
    })
  })
  
  observeEvent(input$reset_button, {
      shinyjs::reset("side-panel")
    })


  output$TimeHat <- renderText({ compTime()$time.sim })


   output$powerplot <-renderPlot({

     if (input$Model==1){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2,3)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N.0,power.IL.b00$N.1,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")

     power.IL.b01 = data.frame(power.sim()$power[,c(1,2,4)])
     data.IL.b01 = data.frame(Participants=c(paste(power.IL.b01$N.0,power.IL.b01$N.1,sep=";")),
     Power=power.IL.b01$power.b01)
     Power.b01 = ggplot(data = data.IL.b01, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b01$Participants) +
     labs(title = "Power curve for the effect of the level-2 dummy variable on the intercept")

     coef.plot = grid.arrange(Power.b00,Power.b01,ncol=1)
     }

     if (input$Model==2){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")

     power.IL.b01 = data.frame(power.sim()$power[,c(1,3)])
     data.IL.b01 = data.frame(Participants=c(paste(power.IL.b01$N,sep=";")),
     Power=power.IL.b01$power.b01)
     Power.b01 = ggplot(data = data.IL.b01, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b01$Participants) +
     labs(title = "Power curve for the effect of the level-2 continuos variable on the intercept")

     coef.plot = grid.arrange(Power.b00,Power.b01,ncol=1)
     }

     if (input$Model==3){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")
   
     power.IL.b10 = data.frame(power.sim()$power[,c(1,3)])
     data.IL.b10 = data.frame(Participants=c(paste(power.IL.b10$N,sep=";")),
     Power=power.IL.b10$power.b10)
     Power.b10 = ggplot(data = data.IL.b10, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b10$Participants) +
     labs(title = "Power curve for the fixed slope")

     coef.plot = grid.arrange(Power.b00,Power.b10,ncol=1)
     }

     if (input$Model==4){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")
   
     power.IL.b10 = data.frame(power.sim()$power[,c(1,3)])
     data.IL.b10 = data.frame(Participants=c(paste(power.IL.b10$N,sep=";")),
     Power=power.IL.b10$power.b10)
     Power.b10 = ggplot(data = data.IL.b10, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b10$Participants) +
     labs(title = "Power curve for the fixed slope")

     coef.plot = grid.arrange(Power.b00,Power.b10,ncol=1)
     }

     if (input$Model==5){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2,3)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N.0,power.IL.b00$N.1,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")

     power.IL.b01 = data.frame(power.sim()$power[,c(1,2,4)])
     data.IL.b01 = data.frame(Participants=c(paste(power.IL.b01$N.0,power.IL.b01$N.1,sep=";")),
     Power=power.IL.b01$power.b01)
     Power.b01 = ggplot(data = data.IL.b01, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b01$Participants) +
     labs(title = "Power curve for the effect of the level-2 dummy variable on the intercept")
     
     power.IL.b10 = data.frame(power.sim()$power[,c(1,2,5)])
     data.IL.b10 = data.frame(Participants=c(paste(power.IL.b10$N.0,power.IL.b10$N.1,sep=";")),
     Power=power.IL.b10$power.b10)
     Power.b10 = ggplot(data = data.IL.b10, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b10$Participants) +
     labs(title = "Power curve for the fixed slope")

     power.IL.b11 = data.frame(power.sim()$power[,c(1,2,6)])
     data.IL.b11 = data.frame(Participants=c(paste(power.IL.b11$N.0,power.IL.b11$N.1,sep=";")),
     Power=power.IL.b11$power.b11)
     Power.b11 = ggplot(data = data.IL.b11, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b11$Participants) +
     labs(title = "Power curve for the effect of the level-2 dummy variable on the slope")

     coef.plot = grid.arrange(Power.b00,Power.b01,Power.b10,Power.b11,ncol=1)
     }

     if (input$Model==6){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2,3)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N.0,power.IL.b00$N.1,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")

     power.IL.b01 = data.frame(power.sim()$power[,c(1,2,4)])
     data.IL.b01 = data.frame(Participants=c(paste(power.IL.b01$N.0,power.IL.b01$N.1,sep=";")),
     Power=power.IL.b01$power.b01)
     Power.b01 = ggplot(data = data.IL.b01, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b01$Participants) +
     labs(title = "Power curve for the effect of the level-2 dummy variable on the intercept")
     
     power.IL.b10 = data.frame(power.sim()$power[,c(1,2,5)])
     data.IL.b10 = data.frame(Participants=c(paste(power.IL.b10$N.0,power.IL.b10$N.1,sep=";")),
     Power=power.IL.b10$power.b10)
     Power.b10 = ggplot(data = data.IL.b10, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b10$Participants) +
     labs(title = "Power curve for the fixed slope")

     power.IL.b11 = data.frame(power.sim()$power[,c(1,2,6)])
     data.IL.b11 = data.frame(Participants=c(paste(power.IL.b11$N.0,power.IL.b11$N.1,sep=";")),
     Power=power.IL.b11$power.b11)
     Power.b11 = ggplot(data = data.IL.b11, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b11$Participants) +
     labs(title = "Power curve for the effect of the level-2 dummy variable on the slope")

     coef.plot = grid.arrange(Power.b00,Power.b01,Power.b10,Power.b11,ncol=1)
     }

     if (input$Model==7){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")

     power.IL.b01 = data.frame(power.sim()$power[,c(1,3)])
     data.IL.b01 = data.frame(Participants=c(paste(power.IL.b01$N,sep=";")),
     Power=power.IL.b01$power.b01)
     Power.b01 = ggplot(data = data.IL.b01, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b01$Participants) +
     labs(title = "Power curve for the effect of the level-2 continuos variable on the intercept")
     
     power.IL.b10 = data.frame(power.sim()$power[,c(1,4)])
     data.IL.b10 = data.frame(Participants=c(paste(power.IL.b10$N,sep=";")),
     Power=power.IL.b10$power.b10)
     Power.b10 = ggplot(data = data.IL.b10, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b10$Participants) +
     labs(title = "Power curve for the fixed slope")

     power.IL.b11 = data.frame(power.sim()$power[,c(1,5)])
     data.IL.b11 = data.frame(Participants=c(paste(power.IL.b11$N,sep=";")),
     Power=power.IL.b11$power.b11)
     Power.b11 = ggplot(data = data.IL.b11, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b11$Participants) +
     labs(title = "Power curve for the effect of the level-2 continuos variable on the slope")

     coef.plot = grid.arrange(Power.b00,Power.b01,Power.b10,Power.b11,ncol=1)
     }

     if (input$Model==8){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")

     power.IL.b01 = data.frame(power.sim()$power[,c(1,3)])
     data.IL.b01 = data.frame(Participants=c(paste(power.IL.b01$N,sep=";")),
     Power=power.IL.b01$power.b01)
     Power.b01 = ggplot(data = data.IL.b01, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b01$Participants) +
     labs(title = "Power curve for the effect of the level-2 continuos variable on the intercept")
     
     power.IL.b10 = data.frame(power.sim()$power[,c(1,4)])
     data.IL.b10 = data.frame(Participants=c(paste(power.IL.b10$N,sep=";")),
     Power=power.IL.b10$power.b10)
     Power.b10 = ggplot(data = data.IL.b10, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b10$Participants) +
     labs(title = "Power curve for the fixed slope")

     power.IL.b11 = data.frame(power.sim()$power[,c(1,5)])
     data.IL.b11 = data.frame(Participants=c(paste(power.IL.b11$N,sep=";")),
     Power=power.IL.b11$power.b11)
     Power.b11 = ggplot(data = data.IL.b11, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b11$Participants) +
     labs(title = "Power curve for the effect of the level-2 continuos variable on the slope")

     coef.plot = grid.arrange(Power.b00,Power.b01,Power.b10,Power.b11,ncol=1)
     }

     if (input$Model==9){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")
   
     power.IL.b10 = data.frame(power.sim()$power[,c(1,3)])
     data.IL.b10 = data.frame(Participants=c(paste(power.IL.b10$N,sep=";")),
     Power=power.IL.b10$power.b10)
     Power.b10 = ggplot(data = data.IL.b10, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b10$Participants) +
     labs(title = "Power curve for the fixed slope")

     coef.plot = grid.arrange(Power.b00,Power.b10,ncol=1)
     }

     if (input$Model==10){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2,3)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N.0,power.IL.b00$N.1,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")

     power.IL.b01 = data.frame(power.sim()$power[,c(1,2,4)])
     data.IL.b01 = data.frame(Participants=c(paste(power.IL.b01$N.0,power.IL.b01$N.1,sep=";")),
     Power=power.IL.b01$power.b01)
     Power.b01 = ggplot(data = data.IL.b01, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b01$Participants) +
     labs(title = "Power curve for the effect of the level-2 dummy variable on the intercept")
     
     power.IL.b10 = data.frame(power.sim()$power[,c(1,2,5)])
     data.IL.b10 = data.frame(Participants=c(paste(power.IL.b10$N.0,power.IL.b10$N.1,sep=";")),
     Power=power.IL.b10$power.b10)
     Power.b10 = ggplot(data = data.IL.b10, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b10$Participants) +
     labs(title = "Power curve for the fixed slope")

     power.IL.b11 = data.frame(power.sim()$power[,c(1,2,6)])
     data.IL.b11 = data.frame(Participants=c(paste(power.IL.b11$N.0,power.IL.b11$N.1,sep=";")),
     Power=power.IL.b11$power.b11)
     Power.b11 = ggplot(data = data.IL.b11, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b11$Participants) +
     labs(title = "Power curve for the effect of the level-2 dummy variable on the slope")

     coef.plot = grid.arrange(Power.b00,Power.b01,Power.b10,Power.b11,ncol=1)
     }

     if (input$Model==11){
     power.IL.b00 = data.frame(power.sim()$power[,c(1,2)])
     data.IL.b00 = data.frame(Participants=c(paste(power.IL.b00$N,sep=";")),
     Power=power.IL.b00$power.b00)
     Power.b00 = ggplot(data = data.IL.b00, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b00$Participants) +
     labs(title = "Power curve for the fixed intercept")

     power.IL.b01 = data.frame(power.sim()$power[,c(1,3)])
     data.IL.b01 = data.frame(Participants=c(paste(power.IL.b01$N,sep=";")),
     Power=power.IL.b01$power.b01)
     Power.b01 = ggplot(data = data.IL.b01, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b01$Participants) +
     labs(title = "Power curve for the effect of the level-2 continuos variable on the intercept")
     
     power.IL.b10 = data.frame(power.sim()$power[,c(1,4)])
     data.IL.b10 = data.frame(Participants=c(paste(power.IL.b10$N,sep=";")),
     Power=power.IL.b10$power.b10)
     Power.b10 = ggplot(data = data.IL.b10, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b10$Participants) +
     labs(title = "Power curve for the fixed slope")

     power.IL.b11 = data.frame(power.sim()$power[,c(1,5)])
     data.IL.b11 = data.frame(Participants=c(paste(power.IL.b11$N,sep=";")),
     Power=power.IL.b11$power.b11)
     Power.b11 = ggplot(data = data.IL.b11, aes(x=Participants, y=Power, group=1)) + 
     geom_line(size=1) + geom_point() + scale_x_discrete(limits=data.IL.b11$Participants) +
     labs(title = "Power curve for the effect of the level-2 continuos variable on the slope")

     coef.plot = grid.arrange(Power.b00,Power.b01,Power.b10,Power.b11,ncol=1)
     }

   })


  output$power = renderFormattable({formattable(power.sim()$`coef.sim`)})

  output$covariance = renderFormattable({formattable(power.sim()$`cov.sim`)})

   output$gmplot <-renderPlot({

     if (input$Model==1){
     if (input$is.rho.zero==TRUE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.Z),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error")  + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = rho.hat)) + geom_density() + xlab("") + labs(title = "Autocorrelation of level-1 error")  + geom_vline(data=b.sim, aes(xintercept=input$rho),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = rho.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),ncol=2)
     }
     if (input$is.rho.zero==FALSE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.Z),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error")  + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.5,boxp.5,nrow=2),ncol=2)
     }}

     if (input$Model==2){
     if (input$is.rho.zero==TRUE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.W),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error")  + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = rho.hat)) + geom_density() + xlab("") + labs(title = "Autocorrelation of level-1 error")  + geom_vline(data=b.sim, aes(xintercept=input$rho),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = rho.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),ncol=2)
     }
     if (input$is.rho.zero==FALSE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.W),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error")  + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.5,boxp.5,nrow=2),ncol=2)
     }}

     if (input$Model==3){
     if (input$is.rho.zero==TRUE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = rho.hat)) + geom_density() + xlab("") + labs(title = "Autocorrelation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$rho),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = rho.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.6 = ggplot(data=b.sim, aes(x = sigma.v1.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random slope") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v1),linetype="dashed")
     boxp.6 = ggplot(data=b.sim, aes(x = "", y = sigma.v1.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = rho.v.hat)) + geom_density() + xlab("") + labs(title = "Correlation between the random intercept and random slope") + geom_vline(data=b.sim, aes(xintercept=input$rho.v),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = rho.v.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.6,boxp.6,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),ncol=2)
     }
     if (input$is.rho.zero==FALSE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.6 = ggplot(data=b.sim, aes(x = sigma.v1.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random slope") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v1),linetype="dashed")
     boxp.6 = ggplot(data=b.sim, aes(x = "", y = sigma.v1.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = rho.v.hat)) + geom_density() + xlab("") + labs(title = "Correlation between the random intercept and random slope") + geom_vline(data=b.sim, aes(xintercept=input$rho.v),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = rho.v.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.6,boxp.6,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),ncol=2)
     }}

     if (input$Model==4){
     if (input$is.rho.zero==TRUE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = rho.hat)) + geom_density() + xlab("") + labs(title = "Autocorrelation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$rho),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = rho.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),ncol=2)
     }
     if (input$is.rho.zero==FALSE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.5,boxp.5,nrow=2),ncol=2)
     }}

     if (input$Model==5){
     if (input$is.rho.zero==TRUE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.Z),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.Z),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.6 = ggplot(data=b.sim, aes(x = rho.hat)) + geom_density() + xlab("") + labs(title = "Autocorrelation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$rho),linetype="dashed")
     boxp.6 = ggplot(data=b.sim, aes(x = "", y = rho.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.8 = ggplot(data=b.sim, aes(x = sigma.v1.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random slope") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v1),linetype="dashed")
     boxp.8 = ggplot(data=b.sim, aes(x = "", y = sigma.v1.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.9 = ggplot(data=b.sim, aes(x = rho.v.hat)) + geom_density() + xlab("") + labs(title = "Correlation between the random intercept and random slope") + geom_vline(data=b.sim, aes(xintercept=input$rho.v),linetype="dashed")
     boxp.9 = ggplot(data=b.sim, aes(x = "", y = rho.v.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.6,boxp.6,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),
     arrangeGrob(hist.8,boxp.8,nrow=2),arrangeGrob(hist.9,boxp.9,nrow=2),ncol=2)
     }
     if (input$is.rho.zero==FALSE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.Z),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.Z),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.8 = ggplot(data=b.sim, aes(x = sigma.v1.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random slope") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v1),linetype="dashed")
     boxp.8 = ggplot(data=b.sim, aes(x = "", y = sigma.v1.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.9 = ggplot(data=b.sim, aes(x = rho.v.hat)) + geom_density() + xlab("") + labs(title = "Correlation between the random intercept and random slope") + geom_vline(data=b.sim, aes(xintercept=input$rho.v),linetype="dashed")
     boxp.9 = ggplot(data=b.sim, aes(x = "", y = rho.v.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),
     arrangeGrob(hist.8,boxp.8,nrow=2),arrangeGrob(hist.9,boxp.9,nrow=2),ncol=2)
     }}

     if (input$Model==6){
     if (input$is.rho.zero==TRUE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.Z),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.Z),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.6 = ggplot(data=b.sim, aes(x = rho.hat)) + geom_density() + xlab("") + labs(title = "Autocorrelation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$rho),linetype="dashed")
     boxp.6 = ggplot(data=b.sim, aes(x = "", y = rho.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.6,boxp.6,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),ncol=2)
     }
     if (input$is.rho.zero==FALSE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.Z),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.Z),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),ncol=2)
     }}

     if (input$Model==7){
     if (input$is.rho.zero==TRUE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.W),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.W),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.6 = ggplot(data=b.sim, aes(x = rho.hat)) + geom_density() + xlab("") + labs(title = "Autocorrelation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$rho),linetype="dashed")
     boxp.6 = ggplot(data=b.sim, aes(x = "", y = rho.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.8 = ggplot(data=b.sim, aes(x = sigma.v1.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random slope") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v1),linetype="dashed")
     boxp.8 = ggplot(data=b.sim, aes(x = "", y = sigma.v1.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.9 = ggplot(data=b.sim, aes(x = rho.v.hat)) + geom_density() + xlab("") + labs(title = "Correlation between the random intercept and random slope") + geom_vline(data=b.sim, aes(xintercept=input$rho.v),linetype="dashed")
     boxp.9 = ggplot(data=b.sim, aes(x = "", y = rho.v.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.6,boxp.6,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),
     arrangeGrob(hist.8,boxp.8,nrow=2),arrangeGrob(hist.9,boxp.9,nrow=2),ncol=2)
     }
     if (input$is.rho.zero==FALSE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.W),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.W),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.8 = ggplot(data=b.sim, aes(x = sigma.v1.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random slope") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v1),linetype="dashed")
     boxp.8 = ggplot(data=b.sim, aes(x = "", y = sigma.v1.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.9 = ggplot(data=b.sim, aes(x = rho.v.hat)) + geom_density() + xlab("") + labs(title = "Correlation between the random intercept and random slope") + geom_vline(data=b.sim, aes(xintercept=input$rho.v),linetype="dashed")
     boxp.9 = ggplot(data=b.sim, aes(x = "", y = rho.v.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),
     arrangeGrob(hist.8,boxp.8,nrow=2),arrangeGrob(hist.9,boxp.9,nrow=2),ncol=2)
     }}

     if (input$Model==8){
     if (input$is.rho.zero==TRUE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.W),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.W),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.6 = ggplot(data=b.sim, aes(x = rho.hat)) + geom_density() + xlab("") + labs(title = "Autocorrelation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$rho),linetype="dashed")
     boxp.6 = ggplot(data=b.sim, aes(x = "", y = rho.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.6,boxp.6,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),ncol=2)
     }
     if (input$is.rho.zero==FALSE){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.W),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.W),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),ncol=2)
     }}

     if (input$Model==9){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.v1.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random slope") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v1),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.v1.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.6 = ggplot(data=b.sim, aes(x = rho.v.hat)) + geom_density() + xlab("") + labs(title = "Correlation between the random intercept and random slope") + geom_vline(data=b.sim, aes(xintercept=input$rho.v),linetype="dashed")
     boxp.6 = ggplot(data=b.sim, aes(x = "", y = rho.v.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.6,boxp.6,nrow=2),ncol=2)
     }

     if (input$Model==10){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.Z),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 dummy variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.Z),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.6 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.6 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v1.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random slope") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v1),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v1.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.8 = ggplot(data=b.sim, aes(x = rho.v.hat)) + geom_density() + xlab("") + labs(title = "Correlation between the random intercept and random slope") + geom_vline(data=b.sim, aes(xintercept=input$rho.v),linetype="dashed")
     boxp.8 = ggplot(data=b.sim, aes(x = "", y = rho.v.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.6,boxp.6,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),
     arrangeGrob(hist.8,boxp.8,nrow=2),ncol=2)
     }

     if (input$Model==11){
     b.sim = power.sim()$b.sim
     hist.1 = ggplot(data=b.sim, aes(x = b00.hat)) + geom_density() + xlab("") + labs(title = "Fixed Intercept") + geom_vline(data=b.sim, aes(xintercept=input$b00),linetype="dashed")
     boxp.1 = ggplot(data=b.sim, aes(x = "", y = b00.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("") + coord_flip()
     hist.2 = ggplot(data=b.sim, aes(x = b01.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the intercept") + geom_vline(data=b.sim, aes(xintercept=input$b01.W),linetype="dashed")
     boxp.2 = ggplot(data=b.sim, aes(x = "", y = b01.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.3 = ggplot(data=b.sim, aes(x = b10.hat)) + geom_density() + xlab("") + labs(title = "Fixed Slope") + geom_vline(data=b.sim, aes(xintercept=input$b10),linetype="dashed")
     boxp.3 = ggplot(data=b.sim, aes(x = "", y = b10.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.4 = ggplot(data=b.sim, aes(x = b11.hat)) + geom_density() + xlab("") + labs(title = "Effect of the level-2 continuous variable on the slope") + geom_vline(data=b.sim, aes(xintercept=input$b11.W),linetype="dashed")
     boxp.4 = ggplot(data=b.sim, aes(x = "", y = b11.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.5 = ggplot(data=b.sim, aes(x = sigma.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of level-1 error") + geom_vline(data=b.sim, aes(xintercept=input$sigma),linetype="dashed")
     boxp.5 = ggplot(data=b.sim, aes(x = "", y = sigma.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.6 = ggplot(data=b.sim, aes(x = sigma.v0.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random intercept") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v0),linetype="dashed")
     boxp.6 = ggplot(data=b.sim, aes(x = "", y = sigma.v0.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.7 = ggplot(data=b.sim, aes(x = sigma.v1.hat)) + geom_density() + xlab("") + labs(title = "Standard deviation of random slope") + geom_vline(data=b.sim, aes(xintercept=input$sigma.v1),linetype="dashed")
     boxp.7 = ggplot(data=b.sim, aes(x = "", y = sigma.v1.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     hist.8 = ggplot(data=b.sim, aes(x = rho.v.hat)) + geom_density() + xlab("") + labs(title = "Correlation between the random intercept and random slope") + geom_vline(data=b.sim, aes(xintercept=input$rho.v),linetype="dashed")
     boxp.8 = ggplot(data=b.sim, aes(x = "", y = rho.v.hat, group = 1)) + geom_boxplot() + xlab("") + ylab("")  + coord_flip()
     coef.plot = grid.arrange(arrangeGrob(hist.1,boxp.1,nrow=2),arrangeGrob(hist.2,boxp.2,nrow=2),arrangeGrob(hist.3,boxp.3,nrow=2),
     arrangeGrob(hist.4,boxp.4,nrow=2),arrangeGrob(hist.5,boxp.5,nrow=2),arrangeGrob(hist.6,boxp.6,nrow=2),arrangeGrob(hist.7,boxp.7,nrow=2),
     arrangeGrob(hist.8,boxp.8,nrow=2),ncol=2)
     }

   })


   output$yplot05 <-renderPlot({

     if (input$Model==1){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==2){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) +
     ylim(Y.min, Y.max) + 
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==3){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) +
     ylim(Y.min, Y.max) + 
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==4){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==5){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==6){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==7){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) +
     ylim(Y.min, Y.max) + 
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==8){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) +
     ylim(Y.min, Y.max) + 
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==9){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==10){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==11){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 5% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

   })

   output$yplot5 <-renderPlot({

     if (input$Model==1){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==2){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==3){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==4){
     data.sim = power.sim()$data.IL.Q05
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==5){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==6){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==7){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==8){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==9){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==10){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==11){
     data.sim = power.sim()$data.IL.Q5
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 50% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

   })


   output$yplot95 <-renderPlot({

     if (input$Model==1){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==2){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==3){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==4){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==5){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==6){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==7){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==8){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==9){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==10){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, "Z", summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y, group=Z, colour=Z)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y, color=Z), 
     linetype="dashed") + scale_colour_grey(start = 0.1, end = .6) + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

     if (input$Model==11){
     data.sim = power.sim()$data.IL.Q95
     Y.min = power.sim()$Y.IL.min
     Y.max = power.sim()$Y.IL.max
     Y_means = ddply(data.sim, 'subjno' ,summarise, mean_Y = mean(Y))
     Yplot = ggplot(data = data.sim, aes(x=Time, y=Y)) + 
     ylim(Y.min, Y.max) +
     geom_line(size=1) + geom_hline(data = Y_means, aes(yintercept= mean_Y), 
     linetype="dashed") + labs(title = "Participant in the 95% percentile")
     coef.plot = grid.arrange(Yplot,ncol=1)
     }

   })
  
 output$img <-
    renderText({
      c(
        '<img src="',
        "https://lh3.googleusercontent.com/pw/ACtC-3f75Hgk1v-0iytFCN3RYRqvmDrnNp0mQpozel40gUSzjdhbVsxb2aiTCNoQzr9A5x_NvCUBn4yu9kmeAZG0lAjj33K3NJa46uW6gdGtMknE19TZJbEW4vA6YJhjA8UX_YK6UZFG79YxHbzI7ta3nNYuag=w1804-h969-no?authuser=0",
        '">'
      )
    }) 
  

})

library(shiny)
library(grDevices)
shinyServer(
        function(input, output){

                output$normaldist <- renderPlot({
                        sampleorpopstd <- input$sampleorpopstd   #ttest or normal
                        testtype <- input$testtype               #test type
                        n <- as.numeric(input$n)                 #sample size
                        xbar <- as.numeric(input$xbar)           #sample mean
                        mu <- as.numeric(input$mu)               #proposed mean
                        sigma <- as.numeric(input$sigma)         #standard deviation
                        alpha <- as.numeric(input$alpha)         #alpha
                        SE <- sigma/sqrt(n)                      #standard error
                        zstat <- (xbar-mu)/SE                    #standardized test statistic


                        #plot bounds
                        if(abs((xbar-mu)/SE)<3){
                                boundl <- mu-4*SE
                                boundu <- mu+4*SE
                        }else{
                                boundl <- mu - abs(zstat)*1.1*SE
                                boundu <- mu + abs(zstat)*1.1*SE
                        }

                        #########################################
                        ########Z Testing########################
                        #########################################

                        if(sampleorpopstd=="popstdev"){

                                ###################
                                #Creating the plot#
                                ###################
                                curve(dnorm(x,mu,SE),
                                      xlim=c(boundl,boundu),
                                      main='Normal Density',
                                      ylab="",yaxt="n",
                                      xlab="Sample Mean")

                                ###################
                                #Right Tailed Test#
                                ###################
                                if(testtype=="righttailed"){
                                zcrit <- qnorm(1-alpha)       #standardized z critical value
                                crit  <- zcrit*SE + mu        #critical value

                                cord1.x <- c(crit,seq(crit,boundu,length.out=1000),boundu)
                                cord1.y <- c(0,dnorm(seq(crit,boundu,length.out=1000),mean=mu,sd=SE),0)
                                cord2.x <- c(boundl,seq(boundl,crit,length.out=1000),crit)
                                cord2.y <- c(0,dnorm(seq(boundl,crit,length.out=1000),mean=mu,sd=SE),0)
                                polygon(cord1.x,cord1.y,col='tomato')
                                polygon(cord2.x,cord2.y,col='skyblue')
                                if(zstat>zcrit){
                                        lines(x=c(xbar,xbar),y=c(0,200),col="red",lwd=3)  #Test Stat
                                }else{
                                        lines(x=c(xbar,xbar),y=c(0,200),col="blue",lwd=3)  #Test Stat
                                }

                                ##################
                                #Left Tailed Test#
                                ##################
                                }else if(testtype=="lefttailed"){
                                zcrit <- qnorm(alpha)       #standardized z critical value
                                crit  <- zcrit*SE + mu        #critical value

                                cord1.x <- c(boundl,seq(boundl,crit,length.out=1000),crit)
                                cord1.y <- c(0,dnorm(seq(boundl,crit,length.out=1000),mean=mu,sd=SE),0)
                                cord2.x <- c(crit,seq(crit,boundu,length.out=1000),boundu)
                                cord2.y <- c(0,dnorm(seq(crit,boundu,length.out=1000),mean=mu,sd=SE),0)
                                polygon(cord1.x,cord1.y,col='tomato')
                                polygon(cord2.x,cord2.y,col='skyblue')
                                if(zstat<zcrit){
                                        lines(x=c(xbar,xbar),y=c(0,200),col="red",lwd=3)  #Test Stat
                                }else{
                                        lines(x=c(xbar,xbar),y=c(0,200),col="blue",lwd=3)  #Test Stat
                                }

                                #################
                                #Two Tailed Test#
                                #################
                                }else if(testtype=="twotailed"){
                                zcrit1 <- qnorm(alpha/2)       #standardized z critical value (lower)
                                zcrit2 <- qnorm(1-(alpha/2))     #standardized z critical value (upper)
                                crit1  <- zcrit1*SE + mu       #critical value (lower)
                                crit2  <- zcrit2*SE + mu       #critical value (upper)

                                cord1.x <- c(boundl,seq(boundl,crit1,length.out=1000),crit1)
                                cord1.y <- c(0,dnorm(seq(boundl,crit1,length.out=1000),mean=mu,sd=SE),0)
                                cord2.x <- c(crit2,seq(crit2,boundu,length.out=1000),boundu)
                                cord2.y <- c(0,dnorm(seq(crit2,boundu,length.out=1000),mean=mu,sd=SE),0)
                                cord3.x <- c(crit1,seq(crit1,crit2,length.out=1000),crit2)
                                cord3.y <- c(0,dnorm(seq(crit1,crit2,length.out=1000),mean=mu,sd=SE),0)
                                polygon(cord1.x,cord1.y,col='tomato')
                                polygon(cord2.x,cord2.y,col='tomato')
                                polygon(cord3.x,cord3.y,col='skyblue')
                                if(abs(zstat)>abs(zcrit1)){
                                        lines(x=c(xbar,xbar),y=c(0,200),col="red",lwd=3)  #Test Stat
                                }else{
                                        lines(x=c(xbar,xbar),y=c(0,200),col="blue",lwd=3)  #Test Stat
                                }
                        }

                        ####################################
                        ######T Testing#####################
                        ####################################

                        }else if(sampleorpopstd=="samplestdev" & n!=1){

                                ###################
                                #Creating the plot#
                                ###################
                                x <- seq((boundl-mu)/SE,(boundu-mu)/SE,.001)
                                y <- dt(x,n-1)
                                x <- SE*x+mu
                                plot(x,y,type="l",
                                     xlim=c(boundl,boundu),
                                     main="T Density",
                                     ylab="",yaxt="n",
                                     xlab="Sample Mean")

                                ###################
                                #Right Tailed Test#
                                ###################
                                if(testtype=="righttailed"){
                                tcrit <- qt(1-alpha,n-1)      #standardized t critical value
                                crit  <- tcrit*SE + mu        #critical value

                                cord1.x <- c(crit,seq(crit,boundu,length.out=1000),boundu)
                                cord1.y <- c(0,dt(seq(tcrit,(boundu-mu)/SE,length.out=1000),n-1),0)
                                cord2.x <- c(boundl,seq(boundl,crit,length.out=1000),crit)
                                cord2.y <- c(0,dt(seq((boundl-mu)/SE,tcrit,length.out=1000),n-1),0)
                                polygon(cord1.x,cord1.y,col='tomato')
                                polygon(cord2.x,cord2.y,col='skyblue')
                                if(zstat>tcrit){
                                        lines(x=c(xbar,xbar),y=c(0,200),col="red",lwd=3)  #Test Stat
                                }else{
                                        lines(x=c(xbar,xbar),y=c(0,200),col="blue",lwd=3)  #Test Stat
                                }


                                ##################
                                #Left Tailed Test#
                                ##################
                                }else if(testtype=="lefttailed"){
                                tcrit <- qt(alpha,n-1)      #standardized t critical value
                                crit  <- tcrit*SE + mu        #critical value

                                cord1.x <- c(boundl,seq(boundl,crit,length.out=1000),crit)
                                cord1.y <- c(0,dt(seq((boundl-mu)/SE,tcrit,length.out=1000),n-1),0)
                                cord2.x <- c(crit,seq(crit,boundu,length.out=1000),boundu)
                                cord2.y <- c(0,dt(seq(tcrit,(boundu-mu)/SE,length.out=1000),n-1),0)
                                polygon(cord1.x,cord1.y,col='tomato')
                                polygon(cord2.x,cord2.y,col='skyblue')
                                if(zstat<tcrit){
                                        lines(x=c(xbar,xbar),y=c(0,200),col="red",lwd=3)  #Test Stat
                                }else{
                                        lines(x=c(xbar,xbar),y=c(0,200),col="blue",lwd=3)  #Test Stat
                                }


                                #################
                                #Two Tailed Test#
                                #################
                                }else if(testtype=="twotailed"){
                                tcrit1 <- qt(alpha/2,n-1)      #standardized t critical value (lower)
                                tcrit2 <- qt(1-(alpha/2),n-1)    #standardized t critical value (upper)
                                crit1  <- tcrit1*SE + mu     #critical value (lower)
                                crit2  <- tcrit2*SE + mu     #critical value (upper)

                                cord1.x <- c(boundl,seq(boundl,crit1,length.out=1000),crit1)
                                cord1.y <- c(0,dt(seq((boundl-mu)/SE,tcrit1,length.out=1000),n-1),0)
                                cord2.x <- c(crit2,seq(crit2,boundu,length.out=1000),boundu)
                                cord2.y <- c(0,dt(seq(tcrit2,(boundu-mu)/SE,length.out=1000),n-1),0)
                                cord3.x <- c(crit1,seq(crit1,crit2,length.out=1000),crit2)
                                cord3.y <- c(0,dt(seq(tcrit1,tcrit2,length.out=1000),n-1),0)
                                polygon(cord1.x,cord1.y,col='tomato')
                                polygon(cord2.x,cord2.y,col='tomato')
                                polygon(cord3.x,cord3.y,col='skyblue')
                                if(abs(zstat)>abs(tcrit1)){
                                        lines(x=c(xbar,xbar),y=c(0,200),col="red",lwd=3)  #Test Stat
                                }else{
                                        lines(x=c(xbar,xbar),y=c(0,200),col="blue",lwd=3)  #Test Stat
                                }
                        }

                        ###########################
                        #######Errors##############
                        ###########################

                        }else if(sampleorpopstd=="samplestdev" & n==1 & sigma>0 ){
                                plot(5,5,type="n",axes=FALSE,ann=FALSE,xlim=c(0,10),ylim=c(0,10))
                                text(5,5,"Error: If your standard deviation came from a sample size of 1, it must be equal to 0")

                        }else if(sigma<0){
                                plot(5,5,type="n",axes=FALSE,ann=FALSE,xlim=c(0,10),ylim=c(0,10))
                                text(5,5,"Error: Standard deviation cannot be negative.")

                        }else if(sigma==0){
                                plot(5,5,type="n",axes=FALSE,ann=FALSE,xlim=c(0,10),ylim=c(0,10))
                                text(5,5,"Error: Standard deviation is zero.  No inferences to be made.")
                        }

                })

                ##################################
                ######Hypothesis Testing##########
                ##################################
                output$nullhypothesis <- renderText({
                        mu <- as.numeric(input$mu)
                        if(input$testtype=="righttailed"){
                                paste("&#8804 &nbsp;")
                        }else if(input$testtype=="lefttailed"){
                                paste("&#8805 &nbsp;")
                        }else{
                                paste("= &nbsp;")
                        }

                })

                output$althypothesis <- renderText({
                        if(input$testtype=="righttailed"){
                                paste(" > &nbsp;")
                        }else if(input$testtype=="lefttailed"){
                                paste(" < &nbsp;")
                        }else{
                                paste("&#8800 &nbsp;")
                        }
                })

                output$mu1 <- renderText({as.numeric(input$mu)})
                output$mu2 <- renderText({as.numeric(input$mu)})

                #####################################
                #####Inferential Statistics##########
                output$id1 <- renderText({
                        alpha=input$alpha
                        n <- as.numeric(input$n)
                        mu <- as.numeric(input$mu)
                        sigma <- as.numeric(input$sigma)
                        SE <- sigma/sqrt(n)
                        if(input$sampleorpopstd=="popstdev"){
                                if(input$testtype=="righttailed"){
                                        zcrit <- qnorm(1-alpha)
                                        round(zcrit*SE + mu,4)
                                }else if(input$testtype=="lefttailed"){
                                        zcrit <- qnorm(alpha)
                                        round(zcrit*SE + mu,4)
                                }else{
                                        zcrit1 <- qnorm(alpha/2)
                                        zcrit2 <- qnorm(1-(alpha/2))
                                        crit1  <- round(zcrit1*SE + mu,4)
                                        crit2  <- round(zcrit2*SE + mu,4)
                                        paste(c(crit1, ",", crit2))
                                }
                        }else{
                                if(input$testtype=="righttailed"){
                                        tcrit <- qt(1-alpha,n-1)
                                        round(tcrit*SE + mu,4)
                                }else if(input$testtype=="lefttailed"){
                                        tcrit <- qt(alpha,n-1)
                                        round(tcrit*SE + mu,4)
                                }else{
                                        tcrit1 <- qnorm(alpha/2)
                                        tcrit2 <- qnorm(1-(alpha/2))
                                        crit1  <- round(tcrit1*SE + mu,4)
                                        crit2  <- round(tcrit2*SE + mu,4)
                                        paste(c(crit1, ",", crit2))
                                }
                        }
                })
                output$id2 <- renderText({
                        alpha=input$alpha
                        if(input$sampleorpopstd=="popstdev"){
                                if(input$testtype=="righttailed"){
                                        paste(round(qnorm(1-alpha),4),"(critical value)")
                                }else if(input$testtype=="lefttailed"){
                                        paste(round(qnorm(alpha),4),"(critical value)")
                                }else{
                                        zcrit1 <- round(qnorm(alpha/2),4)
                                        zcrit2 <- round(qnorm(1-(alpha/2)),4)
                                        paste(zcrit1,",",zcrit2, "(critical values)")
                                }
                        }else{
                                if(input$testtype=="righttailed"){
                                        paste(round(qt(1-alpha,n-1),4), "(critical value)")
                                }else if(input$testtype=="lefttailed"){
                                        paste(round(qt(alpha,n-1),4),"(critical value)")
                                }else{
                                        zcrit1 <- round(qnorm(alpha/2),4)
                                        zcrit2 <- round(qnorm(1-(alpha/2)),4)
                                        paste(zcrit1,",", zcrit2,"(critical values)")
                                }
                        }
                })
                output$id3 <- renderText({
                        paste(input$alpha,"(alpha)")
                })
                output$id4 <- renderText({
                        as.numeric(input$xbar)
                })
                output$id5 <- renderText({
                        xbar <- as.numeric(input$xbar)
                        mu <- as.numeric(input$mu)
                        n <- as.numeric(input$n)
                        sigma <- as.numeric(input$sigma)
                        SE <- sigma/sqrt(n)
                        paste(zstat <- (xbar-mu)/SE,"(test statistic)")

                })
                output$id6 <- renderText({
                        xbar <- as.numeric(input$xbar)
                        mu <- as.numeric(input$mu)
                        n <- as.numeric(input$n)
                        sigma <- as.numeric(input$sigma)
                        SE <- sigma/sqrt(n)
                        zstat <- (xbar-mu)/SE
                        if(input$sampleorpopstd=="popstdev"){
                                if(input$testtype=="righttailed"){
                                        paste(round(1-pnorm(zstat),4),"(p-value)")
                                }else if(input$testtype=="lefttailed"){
                                        paste(round(pnorm(zstat),4),"(p-value)")
                                }else{
                                        paste(round(2*(1-pnorm(abs(zstat))),4),"(p-value)")
                                }

                        }else{
                                if(input$testtype=="righttailed"){
                                        paste(round(1-pt(zstat,n-1),4),"(p-value)")
                                }else if(input$testtype=="lefttailed"){
                                        paste(round(pt(zstat,n-1),4),"(p-value)")
                                }else{
                                        paste(round(2*(1-pt(abs(zstat),n-1)),4),"(p-value)")
                                }
                        }

                })
                output$conclusion <- renderText({
                                alpha=input$alpha
                                n <- as.numeric(input$n)
                                mu <- as.numeric(input$mu)
                                sigma <- as.numeric(input$sigma)
                                xbar <- as.numeric(input$xbar)
                                SE <- sigma/sqrt(n)
                                zstat <- (xbar-mu)/SE
                                if(input$sampleorpopstd=="popstdev"){
                                        if(input$testtype=="righttailed"){
                                                zcrit <- qnorm(1-alpha)
                                                crit <- round(zcrit*SE + mu,4)
                                                if(zstat>zcrit){
                                                        paste("Reject the Null")
                                                }else{
                                                        paste("Fail to Reject the Null")
                                                }
                                        }else if(input$testtype=="lefttailed"){
                                                zcrit <- qnorm(alpha)
                                                crit <- round(zcrit*SE + mu,4)
                                                if(zstat<zcrit){
                                                        paste("Reject the Null")
                                                }else{
                                                        paste("Fail to Reject the Null")
                                                }
                                        }else{
                                                zcrit1 <- qnorm(alpha/2)
                                                zcrit2 <- qnorm(1-(alpha/2))
                                                crit1  <- round(zcrit1*SE + mu,4)
                                                crit2  <- round(zcrit2*SE + mu,4)
                                                if(zstat<zcrit1 | zstat>zcrit2){
                                                        paste("Reject the Null")
                                                }else{
                                                        paste("Fail to Reject the Null")
                                                }
                                        }
                                }else{
                                        if(input$testtype=="righttailed"){
                                                tcrit <- qt(1-alpha,n-1)
                                                crit <- round(tcrit*SE + mu,4)
                                                if(zstat>tcrit){
                                                        paste("Reject the Null")
                                                }else{
                                                        paste("Fail to Reject the Null")
                                                }
                                        }else if(input$testtype=="lefttailed"){
                                                tcrit <- qt(alpha,n-1)
                                                crit <- round(tcrit*SE + mu,4)
                                                if(zstat<tcrit){
                                                        paste("Reject the Null")
                                                }else{
                                                        paste("Fail to Reject the Null")
                                                }
                                        }else{
                                                tcrit1 <- qnorm(alpha/2)
                                                tcrit2 <- qnorm(1-(alpha/2))
                                                crit1  <- round(tcrit1*SE + mu,4)
                                                crit2  <- round(tcrit2*SE + mu,4)
                                                if(zstat<tcrit1 | zstat>tcrit2){
                                                        paste("Reject the Null")
                                                }else{
                                                        paste("Fail to Reject the Null")
                                                }
                                        }
                                }
                        })

        }
)

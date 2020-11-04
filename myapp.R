## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(tidyr)
library(extrafont)
library(repr)
library(Hmisc)
library(flexsurv)

header <-  dashboardHeader(title = "TTAnalysis",titleWidth = 350)

  ## Sidebar content
  sidebar <- dashboardSidebar(width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("network-wired")),
      menuItem("Clinical/Observational Trial Parameters", tabName = "TRparameters", icon = icon("procedures")),
      menuItem("Survival Parameters", tabName = "SURparameters", icon = icon("dashboard")),
      menuItem("Analytical Results", tabName = "analytical", icon = icon("bar-chart-o")),
      hr(),
      menuItem("Simulation", tabName = "simulation", icon = icon("th")),
      sliderInput("sliderMaxTime", "Maximum Time for Plotting (Days):", 1, 1000, 365),
      numericInput("sliderNsim", "Number of Simulation Repeats:", min = 1, max = 10000, value = 10),
      numericInput("statsign", "Statistical Significance Level:", min = 0, max = 1, value = 0.05),
      hr(),
      menuItem("Statistical Summary", tabName = "report", icon = icon("flag-checkered")),
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  )
  
  
  ## Body content
body <- dashboardBody(
    tabItems(
      
      tabItem(tabName = "intro",
              fluidRow(
                box(height=750,width=12, status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Introduction",
                    includeMarkdown("Introduction.Rmd"),
                    br(),
                    plotOutput("plotdiagram", width = "10%")
                )
              )
      ),
      
      # First tab content
      tabItem(tabName = "TRparameters",
              fluidRow(
                box(height=350,status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Treatment Groups",
                    sliderInput("sliderTRTG1", "Group 1 (Days):", 1, 50, c(3,7)),
                    sliderInput("sliderTRTG2", "Group 2 (Days):", 1, 50, c(7,11)),
                    sliderInput("sliderTRTG3", "Group 3 (Days):", 1, 50, c(11,15))
                ),
                box(height=350,status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                  title = "Mortality Endpoints",
                  sliderInput("sliderMort1", "Endpoint 1 (Days):", 1, 100, 7),
                  sliderInput("sliderMort2", "Endpoint 2 (Days):", 1, 100, 28)
                ),
                box(height=350,status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Number of Patients in Groups",
                    numericInput("numG1", "Group1:", min = 1, max = 10000, value = 1000),
                    numericInput("numG2", "Group2:", min = 1, max = 10000, value = 1000),
                    numericInput("numG3", "Group3:", min = 1, max = 10000, value = 1000)
                )                                

              )
      ),      

      # Second tab content
      tabItem(tabName = "SURparameters",
              fluidRow(              
              box(height=350,status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Survival Distribution Selection",
                  selectInput("pickDistr", "Distribution:",
                              c("Exponential" = "dist_expo",
                                "Gamma" = "dist_gamma",
                                "Generalized Gamma" = "dist_gengamma",
                                "Gompertz" = "dist_gompertz",
                                "Log-logistic" = "dist_loglog",
                                "Lognormal" = "dist_lognormal",
                                "Uniform" = "dist_unif",
                                "Weibull" = "dist_weibull"))    
              ),

              uiOutput("distpar"),
              
              box(height=460, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Survival Distribution Plot",
                  plotOutput("plotdist",  width = "100%")
              ),
              
              box(height=460, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Percent Survival Plot",
                  plotOutput("plotsurperc",  width = "100%")
              )              
              )
      ),
            
      # Third tab content
      tabItem(tabName = "analytical",
              fluidRow(
                box(height=460, status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Treatment Group Percent Survival",
                    plotOutput("plotmortprob",  width = "100%")
                ),
                
                box(height=460, status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,align = "center",
                    title = "Percent Mortality Table",
                    tableOutput("tblmortprob")
                )
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "simulation",
            fluidRow(
              box(height=460, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Histogram of the Simulation Results",
                  plotOutput("plotsimhist",  width = "100%")
              ),
              
              box(height=460, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,align = "center",
                  title = "Percent Mortality Table (simulated)",
                  tableOutput("tblsimmortprob"),
                  radioButtons("confintsel", "Confidence Interval",
                              choices = c("95%" = "option1",
                                          "90%" = "option2",
                                          "80%" = "option3"),
                              selected = "option1"
                  )
              )
            )
      ),
      
      # Fifth tab content
      tabItem(tabName = "report",
              fluidRow(
                box(width=12, status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Statistical Analysis of the Simulation Results",
                    uiOutput("mort1"),
                    tags$h3("Between treatment groups 1 and 2:"),
                    textOutput("analyzeD1_g1vsg2"),
                    tags$h3("Between treatment groups 1 and 3:"),
                    textOutput("analyzeD1_g1vsg3"),
                    tags$h3("Between treatment groups 2 and 3:"),
                    textOutput("analyzeD1_g2vsg3"),
                    tags$br(),
                    uiOutput("mort2"),
                    tags$h3("Between treatment groups 1 and 2:"),
                    textOutput("analyzeD2_g1vsg2"),
                    tags$h3("Between treatment groups 1 and 3:"),
                    textOutput("analyzeD2_g1vsg3"),
                    tags$h3("Between treatment groups 2 and 3:"),
                    textOutput("analyzeD2_g2vsg3"),
                )
              )
      ),   
      
      tabItem(tabName = "about",
              fluidRow(
                box(width=12, status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "About",
                includeMarkdown("About.Rmd")
              )
              )
      )

    )
  )

ui <- dashboardPage(header, sidebar, body,skin = "yellow")

##########################################################################################
server <- function(input, output) {
  
  maxlimnumeric <- 10000
  set.seed(95)
  
  output$plotdiagram <- renderImage({
    filename <- normalizePath(file.path('./images',"modeldiagram.png"))
    
    list(src = filename, height=450, width=800)
    
  }, deleteFile = FALSE)
  
  output$mort1 <- renderUI({
    txt<-paste("Mortality endpoint followup-Day:",toString(input$sliderMort1))
    h2(txt)
  })
  
  output$mort2 <- renderUI({
    txt<-paste("Mortality endpoint followup-Day:",toString(input$sliderMort2))
    h2(txt)
  })
  
  testnormal <- function(simdist1,pvallim,distname){
    outtxt<-""
    isnormal <- TRUE
    Ntest1 <- shapiro.test(simdist1)
    pval <- Ntest1$p.value
    if(pval <= pvallim){
      s1<-print(paste('The distribution of ',distname))#deparse(substitute(simdist1))
      s2<-print(paste('is non-normal according to Shapiro-Wilk normality test, pval:',format(pval, digits = 5),".",sep=""))
      s3<-print('Hence Two-Samples T-test may not be appropriate.')
      isnormal <- FALSE
      outtxt<-paste(s1,s2,s3)
    }
    outtxt
  }
  
  testsignificantdifference <- function(simdist1,simdist2,pvals,distname1,distname2){
    
    s1<-testnormal(simdist1,pvallim=pvals,distname1)
    s2<-testnormal(simdist2,pvallim=pvals,distname2)
    
    vardiffflag = 0
    res<-var.test(simdist1, simdist2, alternative = "two.sided")
    pval <- res$p.value
    
    s3<-""
    s4<-""
    if(pval <= pvals){
      s3<-print(paste('WARN: Simulation distribution variances are not similar, pval:',format(pval, digits = 5),".",sep=""))
      s4<-print('We will use Welch T-test.')
      vardiffflag <- 1
    }
    
    #Is there any significant difference between the two distributions?
    res <- t.test(simdist1, simdist2, var.equal = TRUE)
    pval <- res$p.value
    
    s5a<-""
    if(pval <= pvals){
      s5<-print(paste('The two distributions are significantly different, pval:',format(pval, digits = 5),".",sep=""))
      if(vardiffflag==1){
        res <- t.test(simdist1, simdist2, var.equal = FALSE, alternative = "less")
        pval <- res$p.value
      } else {
        res <- t.test(simdist1, simdist2, var.equal = TRUE, alternative = "less")
        pval <- res$p.value
      }
      if(pval <= pvals){
        s5a<-print(paste(distname2,' has significantly higher mortality, pval:',format(pval, digits = 5),".",sep=""))
      } else {
        s5a<-print(paste(distname1,' has significantly higher mortality, pval:',format(pval, digits = 5),".",sep=""))
      }
    } else{
      s5<-print('The two distributions are not significantly different.')
      pval <- NULL
    }
    #return(pval)
    outtxt<-paste(s1,s2,s3,s4,s5,s5a)
    outtxt
  }

  output$analyzeD1_g1vsg2 <- renderText({
    pval<-input$statsign
    dfFinalGroupdelta <- simres()
    g1m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort1 & group=="Group1")$pmort
    g2m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort1 & group=="Group2")$pmort
    outtxt<-testsignificantdifference(g1m1,g2m1,pvals=pval,"Group 1","Group 2")
    outtxt
  })
  
  output$analyzeD1_g1vsg3 <- renderText({
    pval<-input$statsign
    dfFinalGroupdelta <- simres()
    g1m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort1 & group=="Group1")$pmort
    g3m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort1 & group=="Group3")$pmort
    outtxt<-testsignificantdifference(g1m1,g3m1,pvals=pval,"Group 1","Group 3")
    outtxt
  }) 
  
  output$analyzeD1_g2vsg3 <- renderText({
    pval<-input$statsign
    dfFinalGroupdelta <- simres()
    g2m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort1 & group=="Group2")$pmort
    g3m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort1 & group=="Group3")$pmort
    outtxt<-testsignificantdifference(g2m1,g3m1,pvals=pval,"Group 2","Group 3")
    outtxt
  }) 
  #############################################################################################
  
  output$analyzeD2_g1vsg2 <- renderText({
    pval<-input$statsign
    dfFinalGroupdelta <- simres()
    g1m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort2 & group=="Group1")$pmort
    g2m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort2 & group=="Group2")$pmort
    outtxt<-testsignificantdifference(g1m1,g2m1,pvals=pval,"Group 1","Group 2")
    outtxt
  })
  
  output$analyzeD2_g1vsg3 <- renderText({
    pval<-input$statsign
    dfFinalGroupdelta <- simres()
    g1m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort2 & group=="Group1")$pmort
    g3m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort2 & group=="Group3")$pmort
    outtxt<-testsignificantdifference(g1m1,g3m1,pvals=pval,"Group 1","Group 3")
    outtxt
  }) 
  
  output$analyzeD2_g2vsg3 <- renderText({
    pval<-input$statsign
    dfFinalGroupdelta <- simres()
    g2m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort2 & group=="Group2")$pmort
    g3m1 <- subset(dfFinalGroupdelta, delta == input$sliderMort2 & group=="Group3")$pmort
    outtxt<-testsignificantdifference(g2m1,g3m1,pvals=pval,"Group 2","Group 3")
    outtxt
  }) 

  output$distpar <- renderUI({
    if (is.null(input$pickDistr))
      return()

    switch(input$pickDistr,
           "dist_expo" = box(height = 350, status = "primary", solidHeader = TRUE,
                   collapsible = TRUE,
                   title = "Exponential Distribution Parameters",
                   numericInput("numeric1expo", "Rate:", min = 0.000001, max = maxlimnumeric, value = 0.03)
           ),
           "dist_gamma" = box(height = 350, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Gamma Distribution Parameters",
                  numericInput("numeric1gamma", "Shape:", min = 0.000001, max = maxlimnumeric, value = 3),
                  numericInput("numeric2gamma", "Scale:", min = 0.000001, max = maxlimnumeric, value = 30)
           ),
           "dist_gengamma"= box(height = 350, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Generalized Gamma Distribution Parameters",
                  numericInput("numeric1gengamma", "Shape:", min = 0.000001, max = maxlimnumeric, value = 0.2),
                  numericInput("numeric2gengamma", "Scale:", min = 0.000001, max = maxlimnumeric, value = 1),
                  numericInput("numeric3gengamma", "Location:", min = 0.000001, max = maxlimnumeric, value = 5)
           ),
           "dist_gompertz" = box(height = 350, status = "primary", solidHeader = TRUE,
                 collapsible = TRUE,
                 title = "Gompertz Distribution Parameters",
                 numericInput("numeric1gompertz", "Shape:", min = 0.000001, max = maxlimnumeric, value = 0.1),
                 numericInput("numeric2gompertz", "Rate:", min = 0.000001, max = maxlimnumeric, value = 0.001)
           ),
           "dist_loglog" = box(height = 350, status = "primary", solidHeader = TRUE,
                 collapsible = TRUE,
                 title = "Log-logistic Distribution Parameters",
                 numericInput("numeric1loglog", "Shape:", min = 0.000001, max = maxlimnumeric, value = 3),
                 numericInput("numeric2loglog", "Scale:", min = 0.000001, max = maxlimnumeric, value = 70)
           ),
           "dist_lognormal" = box(height = 350, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Lognormal Distribution Parameters",
                  numericInput("numeric1lognormal", "Meanlog:", min = 0.000001, max = maxlimnumeric, value = 5),
                  numericInput("numeric2lognormal", "STDEVlog:", min = 0.000001, max = maxlimnumeric, value = 1)
           ),
           "dist_unif" = box(height = 350, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Uniform Distribution Parameters",
                  numericInput("numeric1unif", "Min:", min = 0.000001, max = maxlimnumeric, value = 1),
                  numericInput("numeric2unif", "Max:", min = 0.000001, max = maxlimnumeric, value = 100)
           ),
           "dist_weibull" = box(height = 350, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Weibull Distribution Parameters",
                  numericInput("numeric1weibull", "Shape:", min = 0.000001, max = maxlimnumeric, value = 1.5),
                  numericInput("numeric2weibull", "Scale:", min = 0.000001, max = maxlimnumeric, value = 36)  
           )    
    )
  })

  output$plotdist <- renderPlot({
    x <- seq(0.01, input$sliderMaxTime, length.out=input$sliderMaxTime*10)
    if (input$pickDistr == "dist_expo" & is.null(input$numeric1expo))
      return()
    if (input$pickDistr == "dist_gamma" & is.null(input$numeric1gamma))
      return()
    if (input$pickDistr == "dist_gengamma" & is.null(input$numeric3gengamma))
      return()
    if (input$pickDistr == "dist_gompertz" & is.null(input$numeric1gompertz))
      return()
    if (input$pickDistr == "dist_loglog" & is.null(input$numeric1loglog))
      return()
    if (input$pickDistr == "dist_lognormal" & is.null(input$numeric1lognormal))
      return()
    if (input$pickDistr == "dist_unif" & is.null(input$numeric1unif))
      return()       
    if (input$pickDistr == "dist_weibull" & is.null(input$numeric1weibull))
      return()    
    
    dat <- switch(input$pickDistr,
                  "dist_expo" = data.frame(time=x, pmort=dexp(x, rate=input$numeric1expo)),
                  "dist_gamma" = data.frame(time=x, pmort=dgamma(x, shape=input$numeric1gamma, scale=input$numeric2gamma)),
                  "dist_gengamma"= data.frame(time=x, pmort=dgengamma(x, mu = input$numeric3gengamma, sigma = input$numeric2gengamma, Q = input$numeric1gengamma)),
                  "dist_gompertz" = data.frame(time=x, pmort=dgompertz(x, shape = input$numeric1gompertz, rate = input$numeric2gompertz)),
                  "dist_loglog" = data.frame(time=x, pmort=dllogis(x, shape = input$numeric1loglog, scale = input$numeric2loglog)),
                  "dist_lognormal" = data.frame(time=x, pmort=dlnorm(x, meanlog = input$numeric1lognormal, sdlog = input$numeric2lognormal)),
                  "dist_unif" = data.frame(time=x, pmort=dunif(x, min = input$numeric1unif, max = input$numeric2unif)),
                  "dist_weibull" = data.frame(time=x, pmort=dweibull(x, shape = input$numeric1weibull, scale = input$numeric2weibull))    
    )
    
    dat %>%
      ggplot( aes(x=time, y=pmort)) +
      geom_line(size=1.3) +
      theme_ipsum(axis_title_size = 14,ticks = TRUE) +
      theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
      ylab("Survival Density Function") +
      xlab('Survival time from diagnosis (Days)') + 
      #legend.position = c(0.7, 0.6),
      theme(text = element_text(size=14),
            axis.text.x=element_text(size=14,face = "bold"),
            axis.text.y=element_text(size=14,face = "bold"),
            axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
            legend.background = element_rect(size=0.5, 
                                             linetype="solid", 
                                             colour ="darkblue")) 

  })        

  output$plotsurperc <- renderPlot({
    x <- seq(0.01, input$sliderMaxTime, length.out=input$sliderMaxTime*10)
    if (input$pickDistr == "dist_expo" & is.null(input$numeric1expo))
      return()
    if (input$pickDistr == "dist_gamma" & is.null(input$numeric1gamma))
      return()
    if (input$pickDistr == "dist_gengamma" & is.null(input$numeric3gengamma))
      return()
    if (input$pickDistr == "dist_gompertz" & is.null(input$numeric1gompertz))
      return()
    if (input$pickDistr == "dist_loglog" & is.null(input$numeric1loglog))
      return()
    if (input$pickDistr == "dist_lognormal" & is.null(input$numeric1lognormal))
      return()
    if (input$pickDistr == "dist_unif" & is.null(input$numeric1unif))
      return()       
    if (input$pickDistr == "dist_weibull" & is.null(input$numeric1weibull))
      return()   
    
    dat <- switch(input$pickDistr,
                  "dist_expo" = data.frame(time=x, pmort=pexp(x, rate=input$numeric1expo)),
                  "dist_gamma" = data.frame(time=x, pmort=pgamma(x, shape=input$numeric1gamma, scale=input$numeric2gamma)),
                  "dist_gengamma"= data.frame(time=x, pmort=pgengamma(x, mu = input$numeric3gengamma, sigma = input$numeric2gengamma, Q = input$numeric1gengamma)),
                  "dist_gompertz" = data.frame(time=x, pmort=pgompertz(x, shape = input$numeric1gompertz, rate = input$numeric2gompertz)),
                  "dist_loglog" = data.frame(time=x, pmort=pllogis(x, shape = input$numeric1loglog, scale = input$numeric2loglog)),
                  "dist_lognormal" = data.frame(time=x, pmort=plnorm(x, meanlog = input$numeric1lognormal, sdlog = input$numeric2lognormal)),
                  "dist_unif" = data.frame(time=x, pmort=punif(x, min = input$numeric1unif, max = input$numeric2unif)),
                  "dist_weibull" = data.frame(time=x, pmort=pweibull(x, shape = input$numeric1weibull, scale = input$numeric2weibull))    
    )
    
    dat %>%
      ggplot( aes(x=time, y=100*(1-pmort))) +
      geom_line(size=1.3) +
      theme_ipsum(axis_title_size = 14,ticks = TRUE) +
      theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
      ylab("Percent Survival") +
      xlab('Survival time from diagnosis (Days)') + 
      #legend.position = c(0.7, 0.6),
      theme(text = element_text(size=14),
            axis.text.x=element_text(size=14,face = "bold"),
            axis.text.y=element_text(size=14,face = "bold"),
            axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
            legend.background = element_rect(size=0.5, 
                                             linetype="solid", 
                                             colour ="darkblue")) +
      ylim(0,100)
  })
  
  
  output$plotmortprob <- renderPlot({
    if (is.null(input$pickDistr))
      return()
    
    deltalistmin<-1
    deltalistmax<-input$sliderMort2+10
    G1lo<-input$sliderTRTG1[1]
    G2lo<-input$sliderTRTG2[1] 
    G3lo<-input$sliderTRTG3[1] 
    G1hi<-input$sliderTRTG1[2] 
    G2hi<-input$sliderTRTG2[2] 
    G3hi<-input$sliderTRTG3[2]
    
    dfpivot <- switch(input$pickDistr,
          "dist_expo" = get_expo_anal(ratepar=input$numeric1expo,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
          "dist_gamma" = get_gamma_anal(shapepar=input$numeric1gamma,scalepar=input$numeric2gamma,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
          "dist_gengamma"= get_gengamma_anal(mupar=input$numeric3gengamma, sigmapar=input$numeric2gengamma, Qpar=input$numeric1gengamma, deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
          "dist_gompertz" = get_gompertz_anal(shapepar=input$numeric1gompertz, ratepar=input$numeric2gompertz,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
          "dist_loglog" = get_loglog_anal(shapepar=input$numeric1loglog,scalepar=input$numeric2loglog,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
          "dist_lognormal" = get_lognormal_anal(meanlogpar=input$numeric1lognormal,sdlogpar=input$numeric2lognormal,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
          "dist_unif" = get_unif_anal(minpar=input$numeric1unif,maxpar=input$numeric2unif,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
          "dist_weibull" = get_weibull_anal(shapepar=input$numeric1weibull,scalepar=input$numeric2weibull,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi)    
    )
    
    dfpivot %>%
      ggplot( aes(x=MortalityEndpoint, y=pmort, color=group)) +
      geom_line(size=1.3) +
      theme_ipsum(axis_title_size = 14,ticks = TRUE) +
      ylab("Probability of Mortality") +
      xlab('Followup time after treatment (mortality endpoint)') + 
      theme(text = element_text(size=14),
            axis.text.x=element_text(size=14,face = "bold"),
            axis.text.y=element_text(size=14,face = "bold"),
            axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
      scale_color_viridis(discrete = TRUE,name = "Treatment Groups", labels = c("Group 1", 
                                                                                "Group 2", 
                                                                                "Group 3"))
  })
  
  
  output$tblmortprob <- renderTable({
    if (is.null(input$pickDistr))
      return()
    
    deltalistmin<-1
    deltalistmax<-input$sliderMort2+10
    G1lo<-input$sliderTRTG1[1]
    G2lo<-input$sliderTRTG2[1] 
    G3lo<-input$sliderTRTG3[1] 
    G1hi<-input$sliderTRTG1[2] 
    G2hi<-input$sliderTRTG2[2] 
    G3hi<-input$sliderTRTG3[2]
    
    dfpivot <- switch(input$pickDistr,
      "dist_expo" = get_expo_anal(ratepar=input$numeric1expo,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
      "dist_gamma" = get_gamma_anal(shapepar=input$numeric1gamma,scalepar=input$numeric2gamma,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
      "dist_gengamma"= get_gengamma_anal(mupar=input$numeric3gengamma, sigmapar=input$numeric2gengamma, Qpar=input$numeric1gengamma, deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
      "dist_gompertz" = get_gompertz_anal(shapepar=input$numeric1gompertz, ratepar=input$numeric2gompertz,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
      "dist_loglog" = get_loglog_anal(shapepar=input$numeric1loglog,scalepar=input$numeric2loglog,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
      "dist_lognormal" = get_lognormal_anal(meanlogpar=input$numeric1lognormal,sdlogpar=input$numeric2lognormal,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
      "dist_unif" = get_unif_anal(minpar=input$numeric1unif,maxpar=input$numeric2unif,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
      "dist_weibull" = get_weibull_anal(shapepar=input$numeric1weibull,scalepar=input$numeric2weibull,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi)    
    )
    
    outdf1 <- dfpivot %>% filter(MortalityEndpoint==input$sliderMort1)
    outdf2 <- dfpivot %>% filter(MortalityEndpoint==input$sliderMort2)
    outdf <- rbind(outdf1,outdf2)
    outdf$pmort <- outdf$pmort*100
    outdf$MortalityEndpoint <- paste("Day", outdf$MortalityEndpoint, sep=" ")
    colnames(outdf)[1] <- "Mortality Endpoint"
    colnames(outdf)[2] <- "Treatment Group"
    colnames(outdf)[3] <- "%Mortality"
    outdf
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE,  
  align = 'c',  
  digits = 2)
  
  output$plotsimhist <- renderPlot({
    dfFinalGroupdelta <- simres()
    
    dfFinalGroupdelta %>%
      ggplot( aes(x=pmort, fill=group)) +
      geom_histogram(binwidth=0.005, color="#e9ecef", alpha=0.6, position = 'identity') +
      theme_ipsum(axis_title_size = 14,ticks = TRUE) +
      ggtitle('Simulated mortality rate distributions for the two followup periods')+
      ylab("Histogram of simulated mortality probabilities") +
      xlab('p(mort)') + 
      theme(text = element_text(size=14),
            axis.text.x=element_text(size=14,face = "bold"),
            axis.text.y=element_text(size=14,face = "bold"),
            axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
      labs(fill="")
  })
  
  
  simres <- reactive({
    if (is.null(input$pickDistr))
      return()
    
    delta1<-input$sliderMort1
    delta2<-input$sliderMort2
    Nsimpar<-input$sliderNsim
    Nsubj1<-input$numG1
    Nsubj2<-input$numG2
    Nsubj3<-input$numG3
    G1lo<-input$sliderTRTG1[1]
    G2lo<-input$sliderTRTG2[1] 
    G3lo<-input$sliderTRTG3[1] 
    G1hi<-input$sliderTRTG1[2] 
    G2hi<-input$sliderTRTG2[2] 
    G3hi<-input$sliderTRTG3[2]
    
    dSim <- switch(input$pickDistr,
        "dist_expo" = dSim_expo(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,rate=input$numeric1expo,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
        "dist_gamma" = dSim_gamma(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,shape=input$numeric1gamma,scale=input$numeric2gamma,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
        "dist_gengamma"= dSim_gengamma(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,mu=input$numeric3gengamma,sigma=input$numeric2gengamma,Q=input$numeric1gengamma,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
        "dist_gompertz" = dSim_gompertz(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,shape=input$numeric1gompertz,rate=input$numeric2gompertz,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
        "dist_loglog" = dSim_loglog(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,shape=input$numeric1loglog,scale=input$numeric2loglog,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
        "dist_lognormal" = dSim_lognormal(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,meanlog=input$numeric1lognormal,sdlog=input$numeric2lognormal,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
        "dist_unif" = dSim_unif(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,min=input$numeric1unif,max=input$numeric2unif,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi),
        "dist_weibull" = dSim_weibull(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,shape=input$numeric1weibull,scale=input$numeric2weibull,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi)
    )

    dSim
  })
  
  getperc<-function(dist,alpha=95){
    res<-unname(quantile(dist,  probs = c((100-alpha)/2,50,100-(100-alpha)/2)/100))
    return(res*100)
  }
  
  output$tblsimmortprob <- renderTable({
    if (is.null(input$pickDistr))
      return()

    dfFinalGroupdelta <- simres()
    
    if(input$confintsel == "option1"){
      confintv = 95
    }else if(input$confintsel == "option2"){
      confintv = 90
    }else{
      confintv = 80
    }
    
    dfSimCI <- data.frame(Group=character(), 
                          CILO=double(),
                          Median=double(),
                          CIHI=double(),
                          stringsAsFactors=FALSE) 
    
    dfSimCI[nrow(dfSimCI) + 1,1] = paste("Group1 Day-", input$sliderMort1, sep="")#"G1-7day"
    dfSimCI[nrow(dfSimCI),2:4] = getperc(subset(dfFinalGroupdelta, delta == input$sliderMort1 & group=="Group1")$pmort,confintv)
    
    dfSimCI[nrow(dfSimCI) + 1,1] = paste("Group2 Day-", input$sliderMort1, sep="")#"G2-7day"
    dfSimCI[nrow(dfSimCI),2:4] = getperc(subset(dfFinalGroupdelta, delta == input$sliderMort1 & group=="Group2")$pmort,confintv)
    
    dfSimCI[nrow(dfSimCI) + 1,1] = paste("Group3 Day-", input$sliderMort1, sep="")#"G3-7day"
    dfSimCI[nrow(dfSimCI),2:4] = getperc(subset(dfFinalGroupdelta, delta == input$sliderMort1 & group=="Group3")$pmort,confintv)
    
    dfSimCI[nrow(dfSimCI) + 1,1] = paste("Group1 Day-", input$sliderMort2, sep="")#"G1-28day"
    dfSimCI[nrow(dfSimCI),2:4] = getperc(subset(dfFinalGroupdelta, delta == input$sliderMort2 & group=="Group1")$pmort,confintv)
    
    dfSimCI[nrow(dfSimCI) + 1,1] = paste("Group2 Day-", input$sliderMort2, sep="")#"G2-28day"
    dfSimCI[nrow(dfSimCI),2:4] = getperc(subset(dfFinalGroupdelta, delta == input$sliderMort2 & group=="Group2")$pmort,confintv)
    
    dfSimCI[nrow(dfSimCI) + 1,1] = paste("Group3 Day-", input$sliderMort2, sep="")#"G3-28day"
    dfSimCI[nrow(dfSimCI),2:4] = getperc(subset(dfFinalGroupdelta, delta == input$sliderMort2 & group=="Group3")$pmort,confintv)
    
    dfSimCI
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE,  
  align = 'c',  
  digits = 2)  

#####################################CALCULATIONS
integrandweibull <- function(t, delta, shape, scale) {
  a1 <- pweibull(t+delta, shape=shape, scale=scale)
  a0 <- pweibull(t, shape=shape, scale=scale)
  res <- ifelse(a0<1,(a1-a0)/(1-a0),1)
  return(res)
}

propgroupdeadweibull <- function(shape, scale, delta, grouplimlo, grouplimhi ) {
  ints <- integrate(integrandweibull, lower = grouplimlo, upper = grouplimhi, delta=delta, shape=shape, scale=scale)
  res <- ints$value/(grouplimhi-grouplimlo)
  return(res)
}

get_weibull_anal <- function(shapepar,scalepar,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  deltalistlist<-seq(from = deltalistmin, to = deltalistmax, by = 0.1)
  
  resG1 <- sapply(deltalistlist, propgroupdeadweibull, shape=shapepar, scale=scalepar, grouplimlo=G1lo, grouplimhi=G1hi)
  resG2 <- sapply(deltalistlist, propgroupdeadweibull, shape=shapepar, scale=scalepar, grouplimlo=G2lo, grouplimhi=G2hi)
  resG3 <- sapply(deltalistlist, propgroupdeadweibull, shape=shapepar, scale=scalepar, grouplimlo=G3lo, grouplimhi=G3hi)
  
  df <- data.frame("MortalityEndpoint" = deltalistlist, 
                   "Group1" = resG1,
                   "Group2" = resG2,
                   "Group3" = resG3)
  dfpivot <- df %>%
    gather(group, pmort, -MortalityEndpoint)
  
  return(dfpivot)
}

simconfint95_weibull <- function(Nsim,Nsubj,deltapar,shapepar,scalepar,grouplimlo,grouplimhi){  
  replicate(Nsim,{
    Ttimes <- runif(Nsubj, min = grouplimlo, max = grouplimhi) #Get time of treatment
    pdeadvec <- sapply(Ttimes,integrandweibull, shape=shapepar, delta=deltapar, scale=scalepar)
    ptest <- runif(Nsubj, min = 0, max = 1) #Get time of treatment
    sum(pdeadvec>ptest)/Nsubj
  })
}

dSim_weibull <- function(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,shape,scale,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  
  g7days_1<-simconfint95_weibull(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta1,shapepar=shape,scalepar=scale,grouplimlo=G1lo,grouplimhi=G1hi)
  g7days_2<-simconfint95_weibull(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta1,shapepar=shape,scalepar=scale,grouplimlo=G2lo,grouplimhi=G2hi)
  g7days_3<-simconfint95_weibull(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta1,shapepar=shape,scalepar=scale,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g7days_1,
                   "Group2" = g7days_2,
                   "Group3" = g7days_3)
  dfpivot7 <- df %>%
    gather(group, pmort)
  dfpivot7["delta"] <- list(rep(delta1,nrow(dfpivot7))) 
  
  g28days_1<-simconfint95_weibull(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta2,shapepar=shape,scalepar=scale,grouplimlo=G1lo,grouplimhi=G1hi)
  g28days_2<-simconfint95_weibull(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta2,shapepar=shape,scalepar=scale,grouplimlo=G2lo,grouplimhi=G2hi)
  g28days_3<-simconfint95_weibull(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta2,shapepar=shape,scalepar=scale,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g28days_1,
                   "Group2" = g28days_2,
                   "Group3" = g28days_3)
  dfpivot28 <- df %>%
    gather(group, pmort)
  dfpivot28["delta"] <- list(rep(delta2,nrow(dfpivot28))) 
  
  dfFinalGroupdelta <- rbind(dfpivot7, dfpivot28)
  return(dfFinalGroupdelta)
}
#########################  
integrandexpo <- function(t, delta, rate) {
  a1 <- pexp(t+delta, rate=rate)
  a0 <- pexp(t, rate=rate)
  res <- ifelse(a0<1,(a1-a0)/(1-a0),1)
  return(res)
}

propgroupdeadexpo <- function(rate, delta, grouplimlo, grouplimhi ) {
  ints <- integrate(integrandexpo, lower = grouplimlo, upper = grouplimhi, delta=delta, rate=rate)
  res <- ints$value/(grouplimhi-grouplimlo)
  return(res)
}

get_expo_anal <- function(ratepar,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  deltalistlist<-seq(from = deltalistmin, to = deltalistmax, by = 0.1)
  
  resG1 <- sapply(deltalistlist, propgroupdeadexpo, rate=ratepar, grouplimlo=G1lo, grouplimhi=G1hi)
  resG2 <- sapply(deltalistlist, propgroupdeadexpo, rate=ratepar, grouplimlo=G2lo, grouplimhi=G2hi)
  resG3 <- sapply(deltalistlist, propgroupdeadexpo, rate=ratepar, grouplimlo=G3lo, grouplimhi=G3hi)
  
  df <- data.frame("MortalityEndpoint" = deltalistlist, 
                   "Group1" = resG1,
                   "Group2" = resG2,
                   "Group3" = resG3)
  dfpivot <- df %>%
    gather(group, pmort, -MortalityEndpoint)
  
  return(dfpivot)
}  

simconfint95_expo <- function(Nsim,Nsubj,deltapar,ratepar,grouplimlo,grouplimhi){  
  replicate(Nsim,{
    Ttimes <- runif(Nsubj, min = grouplimlo, max = grouplimhi) #Get time of treatment
    pdeadvec <- sapply(Ttimes,integrandexpo, delta=deltapar, rate=ratepar)
    ptest <- runif(Nsubj, min = 0, max = 1) #Get time of treatment
    sum(pdeadvec>ptest)/Nsubj
  })
}

dSim_expo <- function(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,rate,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  
  g7days_1<-simconfint95_expo(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta1,ratepar=rate,grouplimlo=G1lo,grouplimhi=G1hi)
  g7days_2<-simconfint95_expo(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta1,ratepar=rate,grouplimlo=G2lo,grouplimhi=G2hi)
  g7days_3<-simconfint95_expo(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta1,ratepar=rate,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g7days_1,
                   "Group2" = g7days_2,
                   "Group3" = g7days_3)
  dfpivot7 <- df %>%
    gather(group, pmort)
  dfpivot7["delta"] <- list(rep(delta1,nrow(dfpivot7))) 
  
  g28days_1<-simconfint95_expo(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta2,ratepar=rate,grouplimlo=G1lo,grouplimhi=G1hi)
  g28days_2<-simconfint95_expo(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta2,ratepar=rate,grouplimlo=G2lo,grouplimhi=G2hi)
  g28days_3<-simconfint95_expo(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta2,ratepar=rate,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g28days_1,
                   "Group2" = g28days_2,
                   "Group3" = g28days_3)
  dfpivot28 <- df %>%
    gather(group, pmort)
  dfpivot28["delta"] <- list(rep(delta2,nrow(dfpivot28))) 
  
  dfFinalGroupdelta <- rbind(dfpivot7, dfpivot28)
  return(dfFinalGroupdelta)
}
#########################     
integrandgamma <- function(t, delta, shape, scale) {
  a1 <- pgamma(t+delta, shape=shape, scale=scale)
  a0 <- pgamma(t, shape=shape, scale=scale)
  res <- ifelse(a0<1,(a1-a0)/(1-a0),1)
  return(res)
}

propgroupdeadgamma <- function(shape, scale, delta, grouplimlo, grouplimhi ) {
  ints <- integrate(integrandgamma, lower = grouplimlo, upper = grouplimhi, delta=delta, shape=shape, scale=scale)
  res <- ints$value/(grouplimhi-grouplimlo)
  return(res)
}

get_gamma_anal <- function(shapepar,scalepar,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  deltalistlist<-seq(from = deltalistmin, to = deltalistmax, by = 0.1)
  
  resG1 <- sapply(deltalistlist, propgroupdeadgamma, shape=shapepar, scale=scalepar, grouplimlo=G1lo, grouplimhi=G1hi)
  resG2 <- sapply(deltalistlist, propgroupdeadgamma, shape=shapepar, scale=scalepar, grouplimlo=G2lo, grouplimhi=G2hi)
  resG3 <- sapply(deltalistlist, propgroupdeadgamma, shape=shapepar, scale=scalepar, grouplimlo=G3lo, grouplimhi=G3hi)
  
  df <- data.frame("MortalityEndpoint" = deltalistlist, 
                   "Group1" = resG1,
                   "Group2" = resG2,
                   "Group3" = resG3)
  dfpivot <- df %>%
    gather(group, pmort, -MortalityEndpoint)
  
  return(dfpivot)
}

simconfint95_gamma <- function(Nsim,Nsubj,deltapar,shapepar,scalepar,grouplimlo,grouplimhi){  
  replicate(Nsim,{
    Ttimes <- runif(Nsubj, min = grouplimlo, max = grouplimhi) #Get time of treatment
    pdeadvec <- sapply(Ttimes,integrandgamma, shape=shapepar, delta=deltapar, scale=scalepar)
    ptest <- runif(Nsubj, min = 0, max = 1) #Get time of treatment
    sum(pdeadvec>ptest)/Nsubj
  })
}

dSim_gamma <- function(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,shape,scale,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  
  g7days_1<-simconfint95_gamma(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta1,shapepar=shape,scalepar=scale,grouplimlo=G1lo,grouplimhi=G1hi)
  g7days_2<-simconfint95_gamma(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta1,shapepar=shape,scalepar=scale,grouplimlo=G2lo,grouplimhi=G2hi)
  g7days_3<-simconfint95_gamma(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta1,shapepar=shape,scalepar=scale,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g7days_1,
                   "Group2" = g7days_2,
                   "Group3" = g7days_3)
  dfpivot7 <- df %>%
    gather(group, pmort)
  dfpivot7["delta"] <- list(rep(delta1,nrow(dfpivot7))) 
  
  g28days_1<-simconfint95_gamma(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta2,shapepar=shape,scalepar=scale,grouplimlo=G1lo,grouplimhi=G1hi)
  g28days_2<-simconfint95_gamma(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta2,shapepar=shape,scalepar=scale,grouplimlo=G2lo,grouplimhi=G2hi)
  g28days_3<-simconfint95_gamma(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta2,shapepar=shape,scalepar=scale,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g28days_1,
                   "Group2" = g28days_2,
                   "Group3" = g28days_3)
  dfpivot28 <- df %>%
    gather(group, pmort)
  dfpivot28["delta"] <- list(rep(delta2,nrow(dfpivot28))) 
  
  dfFinalGroupdelta <- rbind(dfpivot7, dfpivot28)
  return(dfFinalGroupdelta)
}
#########################   
integrandloglog <- function(t, delta, shape, scale) {
  a1 <- pllogis(t+delta, shape=shape, scale=scale)
  a0 <- pllogis(t, shape=shape, scale=scale)
  res <- ifelse(a0<1,(a1-a0)/(1-a0),1)
  return(res)
}

propgroupdeadloglog <- function(shape, scale, delta, grouplimlo, grouplimhi) {
  ints <- integrate(integrandloglog, lower = grouplimlo, upper = grouplimhi, delta=delta, shape=shape, scale=scale)
  res <- ints$value/(grouplimhi-grouplimlo)
  return(res)
}

get_loglog_anal <- function(shapepar,scalepar,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  deltalistlist<-seq(from = deltalistmin, to = deltalistmax, by = 0.1)
  
  resG1 <- sapply(deltalistlist, propgroupdeadloglog, shape=shapepar, scale=scalepar, grouplimlo=G1lo, grouplimhi=G1hi)
  resG2 <- sapply(deltalistlist, propgroupdeadloglog, shape=shapepar, scale=scalepar, grouplimlo=G2lo, grouplimhi=G2hi)
  resG3 <- sapply(deltalistlist, propgroupdeadloglog, shape=shapepar, scale=scalepar, grouplimlo=G3lo, grouplimhi=G3hi)
  
  df <- data.frame("MortalityEndpoint" = deltalistlist, 
                   "Group1" = resG1,
                   "Group2" = resG2,
                   "Group3" = resG3)
  dfpivot <- df %>%
    gather(group, pmort, -MortalityEndpoint)
  
  return(dfpivot)
}

simconfint95_loglog <- function(Nsim,Nsubj,deltapar,shapepar,scalepar,grouplimlo,grouplimhi){  
  replicate(Nsim,{
    Ttimes <- runif(Nsubj, min = grouplimlo, max = grouplimhi) #Get time of treatment
    pdeadvec <- sapply(Ttimes,integrandloglog, shape=shapepar, delta=deltapar, scale=scalepar)
    ptest <- runif(Nsubj, min = 0, max = 1) #Get time of treatment
    sum(pdeadvec>ptest)/Nsubj
  })
}

dSim_loglog <- function(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,shape,scale,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  
  g7days_1<-simconfint95_loglog(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta1,shapepar=shape,scalepar=scale,grouplimlo=G1lo,grouplimhi=G1hi)
  g7days_2<-simconfint95_loglog(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta1,shapepar=shape,scalepar=scale,grouplimlo=G2lo,grouplimhi=G2hi)
  g7days_3<-simconfint95_loglog(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta1,shapepar=shape,scalepar=scale,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g7days_1,
                   "Group2" = g7days_2,
                   "Group3" = g7days_3)
  dfpivot7 <- df %>%
    gather(group, pmort)
  dfpivot7["delta"] <- list(rep(delta1,nrow(dfpivot7))) 
  
  g28days_1<-simconfint95_loglog(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta2,shapepar=shape,scalepar=scale,grouplimlo=G1lo,grouplimhi=G1hi)
  g28days_2<-simconfint95_loglog(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta2,shapepar=shape,scalepar=scale,grouplimlo=G2lo,grouplimhi=G2hi)
  g28days_3<-simconfint95_loglog(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta2,shapepar=shape,scalepar=scale,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g28days_1,
                   "Group2" = g28days_2,
                   "Group3" = g28days_3)
  dfpivot28 <- df %>%
    gather(group, pmort)
  dfpivot28["delta"] <- list(rep(delta2,nrow(dfpivot28))) 
  
  dfFinalGroupdelta <- rbind(dfpivot7, dfpivot28)
  return(dfFinalGroupdelta)
}
#########################   
integrandlognormal <- function(t, delta, meanlog, sdlog) {
  a1 <- plnorm(t+delta, meanlog=meanlog, sdlog=sdlog)
  a0 <- plnorm(t, meanlog=meanlog, sdlog=sdlog)
  res <- ifelse(a0<1,(a1-a0)/(1-a0),1)
  return(res)
}

propgroupdeadlognormal <- function(meanlog, sdlog, delta, grouplimlo, grouplimhi) {
  ints <- integrate(integrandlognormal, lower = grouplimlo, upper = grouplimhi, delta=delta, meanlog=meanlog, sdlog=sdlog)
  res <- ints$value/(grouplimhi-grouplimlo)
  return(res)
}

get_lognormal_anal <- function(meanlogpar,sdlogpar,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  deltalistlist<-seq(from = deltalistmin, to = deltalistmax, by = 0.1)
  
  resG1 <- sapply(deltalistlist, propgroupdeadlognormal, meanlog=meanlogpar, sdlog=sdlogpar, grouplimlo=G1lo, grouplimhi=G1hi)
  resG2 <- sapply(deltalistlist, propgroupdeadlognormal, meanlog=meanlogpar, sdlog=sdlogpar, grouplimlo=G2lo, grouplimhi=G2hi)
  resG3 <- sapply(deltalistlist, propgroupdeadlognormal, meanlog=meanlogpar, sdlog=sdlogpar, grouplimlo=G3lo, grouplimhi=G3hi)
  
  df <- data.frame("MortalityEndpoint" = deltalistlist, 
                   "Group1" = resG1,
                   "Group2" = resG2,
                   "Group3" = resG3)
  dfpivot <- df %>%
    gather(group, pmort, -MortalityEndpoint)
  
  return(dfpivot)
}

simconfint95_lognormal <- function(Nsim,Nsubj,deltapar,meanlogpar,sdlogpar,grouplimlo,grouplimhi){  
  replicate(Nsim,{
    Ttimes <- runif(Nsubj, min = grouplimlo, max = grouplimhi) #Get time of treatment
    pdeadvec <- sapply(Ttimes,integrandlognormal, meanlog=meanlogpar, delta=deltapar, sdlog=sdlogpar)
    ptest <- runif(Nsubj, min = 0, max = 1) #Get time of treatment
    sum(pdeadvec>ptest)/Nsubj
  })
}

dSim_lognormal <- function(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,meanlog,sdlog,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  
  g7days_1<-simconfint95_lognormal(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta1,meanlogpar=meanlog,sdlogpar=sdlog,grouplimlo=G1lo,grouplimhi=G1hi)
  g7days_2<-simconfint95_lognormal(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta1,meanlogpar=meanlog,sdlogpar=sdlog,grouplimlo=G2lo,grouplimhi=G2hi)
  g7days_3<-simconfint95_lognormal(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta1,meanlogpar=meanlog,sdlogpar=sdlog,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g7days_1,
                   "Group2" = g7days_2,
                   "Group3" = g7days_3)
  dfpivot7 <- df %>%
    gather(group, pmort)
  dfpivot7["delta"] <- list(rep(delta1,nrow(dfpivot7))) 
  
  g28days_1<-simconfint95_lognormal(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta2,meanlogpar=meanlog,sdlogpar=sdlog,grouplimlo=G1lo,grouplimhi=G1hi)
  g28days_2<-simconfint95_lognormal(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta2,meanlogpar=meanlog,sdlogpar=sdlog,grouplimlo=G2lo,grouplimhi=G2hi)
  g28days_3<-simconfint95_lognormal(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta2,meanlogpar=meanlog,sdlogpar=sdlog,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g28days_1,
                   "Group2" = g28days_2,
                   "Group3" = g28days_3)
  dfpivot28 <- df %>%
    gather(group, pmort)
  dfpivot28["delta"] <- list(rep(delta2,nrow(dfpivot28))) 
  
  dfFinalGroupdelta <- rbind(dfpivot7, dfpivot28)
  return(dfFinalGroupdelta)
}
#########################   
integrandgompertz <- function(t, delta, shape, rate) {
  a1 <- pgompertz(t+delta, shape=shape, rate=rate)
  a0 <- pgompertz(t, shape=shape, rate=rate)
  res <- ifelse(a0<1,(a1-a0)/(1-a0),1)
  return(res)
}

propgroupdeadgompertz <- function(shape, rate, delta, grouplimlo, grouplimhi) {
  ints <- integrate(integrandgompertz, lower = grouplimlo, upper = grouplimhi, delta=delta, shape=shape, rate=rate)
  res <- ints$value/(grouplimhi-grouplimlo)
  return(res)
}

get_gompertz_anal <- function(shapepar, ratepar,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  deltalistlist<-seq(from = deltalistmin, to = deltalistmax, by = 0.1)
  
  resG1 <- sapply(deltalistlist, propgroupdeadgompertz, shape=shapepar, rate=ratepar, grouplimlo=G1lo, grouplimhi=G1hi)
  resG2 <- sapply(deltalistlist, propgroupdeadgompertz, shape=shapepar, rate=ratepar, grouplimlo=G2lo, grouplimhi=G2hi)
  resG3 <- sapply(deltalistlist, propgroupdeadgompertz, shape=shapepar, rate=ratepar, grouplimlo=G3lo, grouplimhi=G3hi)
  
  df <- data.frame("MortalityEndpoint" = deltalistlist, 
                   "Group1" = resG1,
                   "Group2" = resG2,
                   "Group3" = resG3)
  dfpivot <- df %>%
    gather(group, pmort, -MortalityEndpoint)
  
  return(dfpivot)
}

simconfint95_gompertz <- function(Nsim,Nsubj,deltapar,shapepar,ratepar,grouplimlo,grouplimhi){  
  replicate(Nsim,{
    Ttimes <- runif(Nsubj, min = grouplimlo, max = grouplimhi) #Get time of treatment
    pdeadvec <- sapply(Ttimes,integrandgompertz, shape=shapepar, delta=deltapar, rate=ratepar)
    ptest <- runif(Nsubj, min = 0, max = 1) #Get time of treatment
    sum(pdeadvec>ptest)/Nsubj
  })
}

dSim_gompertz <- function(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,shape,rate,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  
  g7days_1<-simconfint95_gompertz(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta1,shapepar=shape,ratepar=rate,grouplimlo=G1lo,grouplimhi=G1hi)
  g7days_2<-simconfint95_gompertz(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta1,shapepar=shape,ratepar=rate,grouplimlo=G2lo,grouplimhi=G2hi)
  g7days_3<-simconfint95_gompertz(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta1,shapepar=shape,ratepar=rate,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g7days_1,
                   "Group2" = g7days_2,
                   "Group3" = g7days_3)
  dfpivot7 <- df %>%
    gather(group, pmort)
  dfpivot7["delta"] <- list(rep(delta1,nrow(dfpivot7))) 
  
  g28days_1<-simconfint95_gompertz(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta2,shapepar=shape,ratepar=rate,grouplimlo=G1lo,grouplimhi=G1hi)
  g28days_2<-simconfint95_gompertz(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta2,shapepar=shape,ratepar=rate,grouplimlo=G2lo,grouplimhi=G2hi)
  g28days_3<-simconfint95_gompertz(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta2,shapepar=shape,ratepar=rate,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g28days_1,
                   "Group2" = g28days_2,
                   "Group3" = g28days_3)
  dfpivot28 <- df %>%
    gather(group, pmort)
  dfpivot28["delta"] <- list(rep(delta2,nrow(dfpivot28))) 
  
  dfFinalGroupdelta <- rbind(dfpivot7, dfpivot28)
  return(dfFinalGroupdelta)
}
#########################   
integrandgengamma <- function(t, delta, mu, sigma, Q) {
  a1 <- pgengamma(t+delta, mu=mu, sigma=sigma, Q=Q)
  a0 <- pgengamma(t, mu=mu, sigma=sigma, Q=Q)
  res <- ifelse(a0<1,(a1-a0)/(1-a0),1)
  return(res)
}

propgroupdeadgengamma <- function(mu, sigma, Q, delta, grouplimlo, grouplimhi) {
  ints <- integrate(integrandgengamma, lower = grouplimlo, upper = grouplimhi, delta=dealta, mu=mu, sigma=sigma, Q=Q)
  res <- ints$value/(grouplimhi-grouplimlo)
  return(res)
}

get_gengamma_anal <- function(mupar, sigmapar, Qpar, deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  deltalistlist<-seq(from = deltalistmin, to = deltalistmax, by = 0.1)
  
  resG1 <- sapply(deltalistlist, propgroupdeadgengamma, mu=mupar, sigma=sigmapar, Q=Qpar, grouplimlo=G1lo, grouplimhi=G1hi)
  resG2 <- sapply(deltalistlist, propgroupdeadgengamma, mu=mupar, sigma=sigmapar, Q=Qpar, grouplimlo=G2lo, grouplimhi=G2hi)
  resG3 <- sapply(deltalistlist, propgroupdeadgengamma, mu=mupar, sigma=sigmapar, Q=Qpar, grouplimlo=G3lo, grouplimhi=G3hi)
  
  df <- data.frame("MortalityEndpoint" = deltalistlist, 
                   "Group1" = resG1,
                   "Group2" = resG2,
                   "Group3" = resG3)
  dfpivot <- df %>%
    gather(group, pmort, -MortalityEndpoint)
  
  return(dfpivot)
}

simconfint95_gengamma <- function(Nsim,Nsubj,deltapar,mupar,sigmapar,Qpar,grouplimlo,grouplimhi){  
  replicate(Nsim,{
    Ttimes <- runif(Nsubj, min = grouplimlo, max = grouplimhi) #Get time of treatment
    pdeadvec <- sapply(Ttimes,integrandgengamma, mu=mupar, delta=deltapar, sigma=sigmapar, Q=Qpar)
    ptest <- runif(Nsubj, min = 0, max = 1) #Get time of treatment
    sum(pdeadvec>ptest)/Nsubj
  })
}

dSim_gengamma <- function(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,mu,sigma,Q,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  
  g7days_1<-simconfint95_gengamma(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta1,mupar=mu,sigmapar=sigma,Qpar=Q,grouplimlo=G1lo,grouplimhi=G1hi)
  g7days_2<-simconfint95_gengamma(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta1,mupar=mu,sigmapar=sigma,Qpar=Q,grouplimlo=G2lo,grouplimhi=G2hi)
  g7days_3<-simconfint95_gengamma(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta1,mupar=mu,sigmapar=sigma,Qpar=Q,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g7days_1,
                   "Group2" = g7days_2,
                   "Group3" = g7days_3)
  dfpivot7 <- df %>%
    gather(group, pmort)
  dfpivot7["delta"] <- list(rep(delta1,nrow(dfpivot7))) 
  
  g28days_1<-simconfint95_gengamma(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta2,mupar=mu,sigmapar=sigma,Qpar=Q,grouplimlo=G1lo,grouplimhi=G1hi)
  g28days_2<-simconfint95_gengamma(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta2,mupar=mu,sigmapar=sigma,Qpar=Q,grouplimlo=G2lo,grouplimhi=G2hi)
  g28days_3<-simconfint95_gengamma(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta2,mupar=mu,sigmapar=sigma,Qpar=Q,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g28days_1,
                   "Group2" = g28days_2,
                   "Group3" = g28days_3)
  dfpivot28 <- df %>%
    gather(group, pmort)
  dfpivot28["delta"] <- list(rep(delta2,nrow(dfpivot28))) 
  
  dfFinalGroupdelta <- rbind(dfpivot7, dfpivot28)
  return(dfFinalGroupdelta)
}
#########################   
integrandunif <- function(t, delta, min, max) {
  a1 <- punif(t+delta, min=min, max=max)
  a0 <- punif(t, min=min, max=max)
  res <- ifelse(a0<1,(a1-a0)/(1-a0),1)
  return(res)
}

propgroupdeadunif <- function(min, max, delta, grouplimlo, grouplimhi) {
  ints <- integrate(integrandunif, lower = grouplimlo, upper = grouplimhi, delta=delta, min=min, max=max)
  res <- ints$value/(grouplimhi-grouplimlo)
  return(res)
}

get_unif_anal <- function(minpar, maxpar,deltalistmin,deltalistmax, G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  deltalistlist<-seq(from = deltalistmin, to = deltalistmax, by = 0.1)
  
  resG1 <- sapply(deltalistlist, propgroupdeadunif, min=minpar, max=maxpar, grouplimlo=G1lo, grouplimhi=G1hi)
  resG2 <- sapply(deltalistlist, propgroupdeadunif, min=minpar, max=maxpar, grouplimlo=G2lo, grouplimhi=G2hi)
  resG3 <- sapply(deltalistlist, propgroupdeadunif, min=minpar, max=maxpar, grouplimlo=G3lo, grouplimhi=G3hi)
  
  df <- data.frame("MortalityEndpoint" = deltalistlist, 
                   "Group1" = resG1,
                   "Group2" = resG2,
                   "Group3" = resG3)
  dfpivot <- df %>%
    gather(group, pmort, -MortalityEndpoint)
  
  return(dfpivot)
}

simconfint95_unif <- function(Nsim,Nsubj,deltapar,minpar,maxpar,grouplimlo,grouplimhi){  
  replicate(Nsim,{
    Ttimes <- runif(Nsubj, min = grouplimlo, max = grouplimhi) #Get time of treatment
    pdeadvec <- sapply(Ttimes,integrandunif, min=minpar, delta=deltapar, max=maxpar)
    ptest <- runif(Nsubj, min = 0, max = 1) #Get time of treatment
    sum(pdeadvec>ptest)/Nsubj
  })
}

dSim_unif <- function(Nsimpar,Nsubj1,Nsubj2,Nsubj3,delta1,delta2,min,max,G1lo, G2lo, G3lo, G1hi, G2hi, G3hi){
  
  g7days_1<-simconfint95_unif(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta1,minpar=min,maxpar=max,grouplimlo=G1lo,grouplimhi=G1hi)
  g7days_2<-simconfint95_unif(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta1,minpar=min,maxpar=max,grouplimlo=G2lo,grouplimhi=G2hi)
  g7days_3<-simconfint95_unif(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta1,minpar=min,maxpar=max,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g7days_1,
                   "Group2" = g7days_2,
                   "Group3" = g7days_3)
  dfpivot7 <- df %>%
    gather(group, pmort)
  dfpivot7["delta"] <- list(rep(delta1,nrow(dfpivot7))) 
  
  g28days_1<-simconfint95_unif(Nsim=Nsimpar,Nsubj=Nsubj1,deltapar=delta2,minpar=min,maxpar=max,grouplimlo=G1lo,grouplimhi=G1hi)
  g28days_2<-simconfint95_unif(Nsim=Nsimpar,Nsubj=Nsubj2,deltapar=delta2,minpar=min,maxpar=max,grouplimlo=G2lo,grouplimhi=G2hi)
  g28days_3<-simconfint95_unif(Nsim=Nsimpar,Nsubj=Nsubj3,deltapar=delta2,minpar=min,maxpar=max,grouplimlo=G3lo,grouplimhi=G3hi)
  
  df <- data.frame("Group1" = g28days_1,
                   "Group2" = g28days_2,
                   "Group3" = g28days_3)
  dfpivot28 <- df %>%
    gather(group, pmort)
  dfpivot28["delta"] <- list(rep(delta2,nrow(dfpivot28))) 
  
  dfFinalGroupdelta <- rbind(dfpivot7, dfpivot28)
  return(dfFinalGroupdelta)
}
#####################################END CALCULATIONS      
}

shinyApp(ui, server)
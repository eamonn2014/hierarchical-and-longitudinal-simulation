#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
set.seed(2345)
library(mvtnorm) 
library(rms)
library(ggplot2)
library(shiny) 
library(nlme)
library(MASS)
library(tidyverse)
library(shinyWidgets)
library(lme4)
library(DT)
library("shinyalert")
library(gghighlight)



options(max.print=1000000)
fig.width <- 1300
fig.height <- 550
fig.height2 <- 450
library(shinythemes)        # more funky looking apps
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=100)
colz = c("lightblue", "blue",  "lightgreen", "darkgreen")

# function to create longitudinal data
is.even <- function(x){ x %% 2 == 0 }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                # paper
                useShinyalert(),  # Set up shinyalert
                setBackgroundColor(
                  color = c( "#2171B5", "#F7FBFF"), 
                  gradient = "linear",
                  direction = "bottom"
                ),
                h3("xxxxxxxxxxxxxxx"),
                
                h4(p("xxxxxxxxxxxxxxx.
             
             
            xxxxxxxxxxxxxxx
            
            
                  ")),
                h4(p("xxxxxxxxxxxxxxx")),
                
                shinyUI(pageWithSidebar(
                  headerPanel(" "),
                  
                  sidebarPanel( width=3 ,
                                tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                
                                div(
                                  actionButton(inputId='ab1', label="Shiny",   icon = icon("th"), 
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Longitudinal-RCT-treatment-effect-estimation-simulation/master/app/app.R', '_blank')"),   
                                  actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Longitudinal-RCT-treatment-effect-estimation-simulation/master/R%20code%20to%20simulate%20and%20analyse%20longitudinal%20RCT.R', '_blank')"),   
                                  actionButton("resample", "Simulate a new sample"),
                                  br(), br(),
                                  tags$style(".well {background-color:#b6aebd ;}"), 
                                  
                                  div(h5(tags$span(style="color:blue", "Select the parameters using the sliders below...be patient as the GLS models can take ~30 secs to run..."))),
                                  tags$head(
                                    tags$style(HTML('#ab1{background-color:orange}'))
                                  ),
                                  
                                  tags$head(
                                    tags$style(HTML('#resample{background-color:orange}'))
                                  ),
                                  
                                  # selectInput("Plot1",
                                  #             div(h5(tags$span(style="color:blue", "Select plot"))),
                                  #             choices=c("Overall","Individual" )),
                                  # 
                                  # textInput('vec1', 
                                  #           div(h5(tags$span(style="color:blue", "Select patient(s) to view. If 'Select plot' 'Individual' is chosen, enter sample ID(s) (comma delimited); 
                                  #     enter 999 to show all profiles"))),
                                  #           "1,2,3,4"),
                                  # 
                                  
                                  selectInput("Plot1",
                                              div(h5(tags$span(style="color:blue", "Select plot"))),
                                              choices=c("Overall","Individual" )),
                                  
                                  textInput('vec1', 
                                            div(h5(tags$span(style="color:blue", "Select patient(s) to view. If 'Select plot' 'Individual' is chosen, enter sample ID(s) (comma delimited); 
                                      enter 999 to show all profiles"))),
                                            "1,2,3,4"),
                                  
                                  
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  
                                  # intercept =40 
                                  # top      = 15         ##number of groups at top of hierarchy
                                  # range1     = 10        ##app samples betwen these, levels e
                                  # range2     = 10        ##app sample these
                                  # replicates = c(3, 10) ##app sample these
                                  # 
                                  # x1 <- x2 <- range1 
                                  # x4 <- x3 <- range2
                                  # x5 <- replicates[1]
                                  # x6 <- replicates[2]
                                  # 
                                  # # random effect SDs
                                  # a     =1        # top 
                                  # b     =9        # middle
                                  # sigma =50       # residual
                                  # trtB =  8       # trt effect
                                  # interB= 2       # time x trt interaction
                                  # beta1  <- -1    # slope
                                  # tau0   <-  18   # person intercept SD
                                  # tau1   <-  1    # person slope  SD
                                  # tau01  <- -0.62 # slope intercept corr 
                                  # 
                                  
                                  
                                  
                                  # user input setup
                                  # sliderInput("N",
                                  #             div(h5(tags$span(style="color:blue", "total number of subjects"))),
                                  #             min=2, max=500, step=1, value=200, ticks=FALSE),
                                  
                                  
                                  ################
                                  sliderInput("intercept",
                                              "True intercept",
                                              min=0, max=1000, step=.5, value=100, ticks=FALSE),
                                  
                                  sliderInput("top",
                                              "Number of levels of top component",
                                              min=2, max=100, step=1, value=4, ticks=FALSE),
                                  
                                  sliderInput("range1", "Middle level: Randomly select using range or precisely select no of 'mid' groups within each top level group:", 
                                              min = 2, max = 10, value = c(2, 10), ticks=FALSE) ,
                                  
                                  sliderInput("range2", "Lower level: Randomly select using range or precisely select no of 'low' groups within each mid level group:",
                                              min = 2, max = 10, value = c(5, 10),ticks=FALSE),
                                  
                                  sliderInput("replicates", "Visits",
                                              min = 2, max = 50, value = c(3, 10), ticks=FALSE),
                                  sliderInput("a",
                                              "True top level SD",
                                              min=1, max=100, step=.5, value=20, ticks=FALSE),
                                  sliderInput("b",
                                              "True middle level SD",
                                              min=1, max=100, step=.5, value=2, ticks=FALSE),
                                  # sliderInput("c",
                                  #             "True lower level SD",
                                  #             min=1, max=100, step=.5, value=2, ticks=FALSE),
                                  # sliderInput("d",
                                  #             "True error",
                                  #             min=1, max=100, step=.5, value=2, ticks=FALSE),
                                  ###################
                                  
                                  
                                  # sliderInput("J",
                                  #             div(h5(tags$span(style="color:blue", "Maximum visit in data simulation including baseline"))),
                                  #             min=3, max=10, step=1, value=9, ticks=FALSE),
                                  
                                  sliderInput("trt.effect",
                                              div(h5(tags$span(style="color:blue", "Treatment effect"))),
                                              min = -20, max = 20, value = c(16), step=1, ticks=FALSE),
                                  
                                  sliderInput("interaction",    
                                              div(h5(tags$span(style="color:blue", "Treatment time interaction"))),
                                              min = -10, max = 10, value = c(-.2), step=.1, ticks=FALSE),
                                  
                                  # sliderInput("beta0", 
                                  #             div(h5(tags$span(style="color:blue", "Average intercept"))),
                                  #             min = -100, max = 500, step=1, value = c(150), ticks=FALSE) ,
                                  # 
                                  sliderInput("beta1", 
                                              div(h5(tags$span(style="color:blue", "Average slope"))),
                                              min = -5, max =5, step=.5, value = c(0),ticks=FALSE),
                                  
                                  sliderInput("q",   
                                              div(h5(tags$span(style="color:blue", "True intercept SD"))),
                                              min = .1, max = 20, value = c(17), step=.5, ticks=FALSE),
                                  
                                  sliderInput("s",      
                                              div(h5(tags$span(style="color:blue", "True slope SD"))),
                                              min = .01, max = 10, value = c(.8),step=.01,  ticks=FALSE),
                                  
                                  sliderInput("r", 
                                              div(h5(tags$span(style="color:blue", "True intercept slope correlation"))),
                                              min = -1, max = 1, value = c(.95), step=0.05, ticks=FALSE),
                                  
                                  sliderInput("sigma",  
                                              div(h5(tags$span(style="color:blue",  "True error SD"  ))),
                                              min =.01, max = 30, value = c(26), step=.1, ticks=FALSE),
                                  
                                  # sliderInput("time.ref",
                                  #             div(h5(tags$span(style="color:blue", "Estimate treatment effect at this visit"))),
                                  #             min=1, max=10, step=1, value=4, ticks=FALSE),
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  
                                  div(p( strong("References:"))),  
                                  
                                  tags$a(href = "https://github.com/eamonn2014/Longitudinal-RCT-treatment-effect-estimation-simulation/blob/master/recommendations-for-the-primary-analysis-of-continuous-endpoints4590.pdf", "[1] MMRM"),
                                  div(p(" ")),
                                  tags$a(href = "https://twitter.com/f2harrell/status/1220700181496320001", "[4] Purpose of RCT"),
                                  div(p(" ")),
                                  
                                )
                                
                  ),
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                  mainPanel(width=9 ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            navbarPage(       
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                              tags$style(HTML(" 
                            .navbar-default .navbar-brand {color: cyan;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: #b6aebd;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
     
                   ")), 
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                              tabPanel("A1. Plot & LMM", 
                                       
                                       div(plotOutput("reg.plot1", width=fig.width, height=fig.height)),  
                                       #  #h4(paste("Figure 1. xxxxxxxxxxxxxxx")), 
                                       #  h3(" "),
                                       #  div(plotOutput("reg.plot3", width=fig.width, height=fig.height)), 
                                       # # h4(paste("Figure 2. xxxxxxxxxxxxxxx")), 
                                       #  div(class="span7", verbatimTextOutput("reg.summary")),
                                       # h4(paste("Table 1. xxxxxxxxxxxxxxx")), 
                                       # div(class="span7", verbatimTextOutput("reg.summary0407")),
                                       
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("A2. GLS", value=3, 
                                       
                                       div(class="span7", verbatimTextOutput("reg.summary0407")),
                                       #h4(paste("Table 2. xxxxxxxxxxxxxxx")), 
                                       
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("A3. GLS treatment effect", value=6, 
                                       
                                       #   div(plotOutput("reg.plot2", width=fig.width, height=fig.height)), 
                                       #   h4(paste("Figure 3. xxxxxxxxxxxxxxxs")), 
                                       
                                       #div(class="span7", verbatimTextOutput("reg.summary2c")),
                                       #  h4(paste("Table 3. xxxxxxxxxxxxxxx")), 
                                       
                                       splitLayout(
                                         
                                         
                                         selectInput("group",
                                                     div(h5(tags$span(style="color:blue", "Select group"))),
                                                     choices=c("top","mid", "ID" )),
                                         
                                         textInput("level", div(h5(tags$span(style="color:blue", "what group level to display?"))), value= "1")
                                         
                                         
                                       ),
                                       
                                       div(plotOutput("reg.plot2", width=fig.width, height=fig.height)),
                                       
                                       
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("B1. Plots", 
                                       
                                       #   div(plotOutput("reg.plot99", width=fig.width, height=fig.height)), 
                                       #   h4(paste("Figure 4. xxxxxxxxxxxxxxx")), 
                                       
                                       #     div(plotOutput("reg.plot33", width=fig.width, height=fig.height)),  
                                       # h4(paste("Figure 5. xxxxxxxxxxxxxxx")), 
                                       
                              ) ,
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("B2. GLS & LMM", 
                                       
                                       #     div(class="span7", verbatimTextOutput("reg.summaryb1")),
                                       #     h4(paste("Table 4. xxxxxxxxxxxxxxx")), 
                                       #  div(class="span7", verbatimTextOutput("reg.summaryb2")),
                                       #     h4(paste("Table 5. xxxxxxxxxxxxxxx")), 
                                       
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("B3. GLS treatment effect", 
                                       
                                       #   div(plotOutput("reg.plot2b", width=fig.width, height=fig.height)),  
                                       #  h4(paste("Figure 6.  xxxxxxxxxxxxxxx")), 
                                       
                                       #  div(class="span7", verbatimTextOutput("reg.summaryb3copy")),
                                       #    h4(paste("Table 6. xxxxxxxxxxxxxxx.")), 
                                       
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("B4. Diagnostics",
                                       h4("Figure 7. xxxxxxxxxxxxxxx"),
                                       # div(plotOutput("res.diag", width=fig.width, height=fig.height)),       
                                       p(strong("xxxxxxxxxxxxxxx
                                              ")),
                              ),
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("C1. Data listing", value=3, 
                                       
                                       h6("xxxxxxxxxxxxxxx"),
                                       #    DT::dataTableOutput("table1"),
                              ),
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("D. Notes", value=3, 
                                       
                                       
                                       h4("
                                     
                                     xxxxxxxxxxxxxxx
 "),
                                       
                                       h4("xxxxxxxxxxxxxxx"),
                                       
                                       h4("xxxxxxxxxxxxxxx"),
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       h4("xxxxxxxxxxxxxxx"),
                                       
                                       
                                       
                              ) 
                              
                              
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            )
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  )
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                  
                )
                )
)

server <- shinyServer(function(input, output   ) {
  
  
  #__________________________________________________________________________
  
  
  shinyalert("Welcome! \nxxxxxxxxxxxxxxx",
             "xxxxxxxxxxxxxxx", 
             type = "info")
  
  
  
  # --------------------------------------------------------------------------
  # This is where a new sample is instigated 
  random.sample <- reactive({
    
    # Dummy line to trigger off button-press
    foo <-      input$resample
    
    
    
    
    # N <-      input$N
    beta0 <-  input$intercept  
    beta1 <-  input$beta1
    sigma <-  input$sigma
    q <-      input$q
    s <-      input$s
    r <-      input$r
    J <-      input$J
    trt <-    input$trt.effect 
    interaction = input$interaction
    time.ref <-   input$time.ref
    
    
    top <-         input$top
    range1 <-      input$range1
    range2  <-     input$range2
    replicates  <- input$replicates
    a <-           input$a  # random effect sds
    b <-           input$b
    #  c <-           input$c
    # d  <-          input$d
    
    x1 <- range1[1]
    x2 <- range1[2]
    x3 <- range2[1]
    x4 <- range2[2]
    x5 <- replicates[1]
    x6 <- replicates[2]
    
    
    # this is from my app so the sections below are not necessary for plain R code
    # seems that I need to use both c(x1,x2) c(x1:x2) so sample function works correctly
    
    if (x1==x2) {
      
      middle <-  sample(c(x1,x2),   top, replace=TRUE)    # ditto groups in each top level 6
      
    } else {
      
      middle <-  sample(c(x1:x2),   top, replace=TRUE)    # ditto groups in each top level 6
    }
    
    
    if (x3==x4) {
      
      lower <-   sample(c(x3,x4),   sum(middle), replace=TRUE )
      
    } else {
      
      lower <-   sample(c(x3:x4),   sum(middle), replace=TRUE )
      
    }
    
    if (x5==x6) {
      
      replicates <-  sample(c(x5,x6),   sum(lower), replace=TRUE )
      
    } else {
      
      replicates <-  sample(c(x5:x6),   sum(lower), replace=TRUE ) 
      
    }
    
    
    #residual <- d
    d <- intercept
    
    
    # random effects
    top.r <-    rnorm(top,          beta0,                a)    
    middle.r <- rnorm(sum(middle),  0,                b)     
    #lower.r <-  rnorm(sum(lower),   0,                c)     # not needed as we create this later
    
    # ids
    lower.id <- rep(seq_len(sum(lower)), replicates )        
    middle.id <- cut(lower.id, c(0,cumsum(lower)),  labels=FALSE)
    top.id   <- cut(middle.id, c(0,cumsum(middle)), labels=FALSE)
    
    
    return(list(   beta0=beta0, beta1=beta1, sigma=sigma, q=q, s=s, r=r, J=J, 
                   interaction=interaction, trt=trt, top=top, range1=range1, range2=range2, replicates=replicates,
                   a=a, b=b, c=c, 
                   #d=d, 
                   middle=middle, lower=lower, top.r=top.r, middle.r=middle.r,
                   lower.id=lower.id, middle.id=middle.id, top.id=top.id
    )) 
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  #  start creating data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  make.data <- reactive({
    
    sample <- random.sample()
    
    # n <-N        <-      sample$N 
    intercept <-     sample$beta0 
    beta1    <-      sample$beta1
    sigma    <-      sample$sigma
    tau0        <-   sample$q # standard deviations for the intercept 
    tau1        <-   sample$s # standard deviations for slope
    tau01        <-  sample$r # random effects correlation of slope internet
    J        <-      sample$J
    time.ref <-      sample$time.ref
    interaction<-    sample$interaction
    trt   <-         sample$trt
    
    
    top=          sample$top
    # range1=       sample$range1
    # range2=       sample$range2
    replicates=   sample$replicates
    # a=sample$a
    #    b=sample$b
    c=sample$c
    #    d=sample$d
    middle=  sample$middle
    lower=   sample$lower
    
    top.r=     sample$top.r
    middle.r=  sample$middle.r
    lower.id=  sample$lower.id
    middle.id= sample$middle.id
    top.id=   sample$top.id
    
    ar.val=.66
    
    #######################################################################
    # n <-N        <-     100
    #  intercept <-     100
    #  beta1    <-      1
    #   sigma    <-      1
    #    tau0        <-   1 # standard deviations for the intercept
    #  tau1        <-   1 # standard deviations for slope
    #    tau01        <-  .5 # random effects correlation of slope internet
    #    J        <-      10
    #   time.ref <-      4
    #   interaction<-    1
    #   trt   <-         1
    
    
    #   top=          4
    #   range1=      4
    #   range2=       8
    #   replicates=   10
    #
    #  c=3
    #    d=sample$d
    #    middle=  10
    #   lower=   10
    
    # top.r=     sample$top.r
    # middle.r=  sample$middle.r
    # lower.id=  sample$lower.id
    # middle.id= sample$middle.id
    #top.id=   sample$top.id
    
    #    ar.val=.66
    
    # random effects
    #    top.r <-    rnorm(top,          d,                lower)
    #    middle.r <- rnorm(sum(middle),  0,                middle)
    #######################################################################
    
    
    
    ### simulate (correlated) random effects for intercepts and slopes
    mu  <- c(0,0)
    S   <- matrix(c(1, tau01, tau01, 1), nrow=2)
    tau <- c(tau0, tau1)
    S   <- diag(tau) %*% S %*% diag(tau)
    U   <- mvrnorm(n, mu=mu, Sigma=S)
    
    p <- rle(lower.id)$lengths
    eij <- unlist(sapply(p, function(x) arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^.2) * sigma))
    eij <- as.vector(eij)   #new
    
    
    #--------------------------------------------------------------------------
    time = ave(lower.id, lower.id, FUN = seq_along)  # time variable
    
    Data <- data.frame( top=top.id, mid=middle.id, low=lower.id, 
                        
                        time=time,
                        
                        # random noise
                        y2= rnorm( sum(replicates), 
                                   top.r[top.id] + 
                                     middle.r[middle.id] +
                                     U[,1][lower.id] +                               # random intercepts
                                     ((beta1 +  U[,2][lower.id]) *  time), sigma),   # random slopes
                        
                        # ar1 nosie
                        y=  top.r[top.id] + 
                          middle.r[middle.id] +
                          U[,1][lower.id] +                       # random intercepts
                          ((beta1 +  U[,2][lower.id]) *  time) +  # random slopes
                          eij
    )
    
    df <- as.data.frame(Data)
    df$time <- df$time-1  # set baseline to time=0
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # incorporating trt effect
    
    trtB =  trt
    interB= interaction
    ### so we can expect 5+(2.5)*(1:9),, trt effect over time  7.5 10.0 12.5 15.0 17.5 20.0 22.5 25.0 27.5
    
    trt <- sample( c(1,0),  length(unique(lower.id)), replace=TRUE)  # trt indicator
    df$trt <- rep(trt, times=p)
    df$interaction <- ifelse(df$trt==1,1,0)
    
    # trt effect only 1 group.
    # interaction only in treated
    df$y2b = with(df, y2+ (trt*trtB)+  ((time)*interaction*interB))
    df$y2b = with(df,ifelse((trt %in% 1 & time %in% 0), y2, y2b  ))
    
    df$yb = with(df, y+ (trt*trtB)+  ((time)*interaction*interB))
    df$yb = with(df,ifelse((trt %in% 1 & time %in% 0), y, yb  ))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    return(list(  df=df) )
    
  }) 
  
  
  output$reg.summary0407 <- renderPrint({
    
    summary <- make.data()$df
    
    return(list(summary))
    
  })  
  

 
  output$reg.plot1 <- renderPlot({ 
    
    df <- make.data()$df
    
    
    df$VISIT <- df$time
    df$value <- df$y2       # ar1 is th ey response
    # df$value <- df$simulated
    df$variable <- "BIOCHEM.B"
    df$ID <- factor(df$low)
    #df$VISIT=as.numeric(levels(df$VISIT))[df$VISIT]
    
    df_summary <- df %>% # the names of the new data frame and the data frame to be summarised
      group_by(VISIT, variable) %>%                # the grouping variable
      summarise(mean_PL = mean(value, na.rm=TRUE),  # calculates the mean of each group
                sd_PL = sd(value, na.rm=TRUE),      # calculates the sd of each group
                n_PL = length(na.omit(value)),      # calculates the sample size per group
                SE_PL = sd(value, na.rm=TRUE)/sqrt(length(na.omit(value)))) # SE of each group
    
    df_summary1 <- merge(df, df_summary)  # merge stats to dataset
    
    df_summary1$L2SE <- df_summary1$mean_PL - 2*df_summary1$SE_PL
    df_summary1$H2SE <- df_summary1$mean_PL + 2*df_summary1$SE_PL
    
    
    pr1 <- ggplot((df_summary1), aes(x = VISIT, y =value, color = ID)) +
      geom_line( size=.5, alpha=0.2) +
      #scale_color_gradient(low = "blue", high = "red")+
      #scale_color_brewer(palette = "Dark2") +
      stat_summary(geom="line",  fun=mean, colour="black", lwd=0.5) +  # , linetype="dashed"
      stat_summary(geom="point", fun=mean, colour="black") +
      geom_errorbar(data=(df_summary1), 
                    aes( ymin=L2SE, ymax=H2SE ), color = "black",
                    width=0.05, lwd = 0.05) +
      scale_y_continuous(expand = c(.1,0) ) +
      
      
      
      scale_x_continuous(breaks = c(unique(df$VISIT)),
                         labels = 
                           c(unique(df$VISIT))
      ) +
      
      
      theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change plot and panel background
        plot.background=element_rect(fill = "white"),
        panel.background = element_rect(fill = 'black'),
        # Change legend
        legend.position = c(0.6, 0.07),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white")
      ) +
      
      
      EnvStats::stat_n_text(size = 4, y.pos = max(df_summary1$value, na.rm=T)*1.1 , y.expand.factor=0, 
                            angle = 0, hjust = .5, family = "mono", fontface = "plain") + 
      
      theme(panel.background=element_blank(),
            # axis.text.y=element_blank(),
            # axis.ticks.y=element_blank(),
            # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
            # stop axis being clipped
            plot.title=element_text(), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            legend.position="none",
            axis.text.x  = element_text(size=10),
            axis.text.y  = element_text(size=10),
            axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black"),
            plot.caption=element_text(hjust = 0, size = 7))
    
    
    print(pr1 + labs(y="Response", x = "Visit") + 
            ggtitle(paste0("Individual responses ",
                           length(unique(df$ID))," patients & arithmetic mean with 95% CI shown in black\nNumber of patient values at each time point") )
    )
    
    
    ######################################################
    
    
    
    
    
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # end of spaghetti plots of data at which trt effect starts after baseline allowing highlighting of selected patients
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  }) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # fit lmer regression on the data in which trt effect starts at baseline
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # model output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # fit gls regression on the data in which trt effect starts at baseline and get contrasts over time
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # start of spaghetti plots of data at which trt effect starts after baseline allowing highlighting of selected patients
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # --------------------------------------------------------------------------
  # -----------------------------------------------OVERALL PLOT
  # ---------------------------------------------------------------------------
  
  output$reg.plot2 <- renderPlot({ 
    
    df<- make.data()$df
    
    
    df$VISIT <- df$time
    df$value <- df$y2       # ar1 is th ey response
    # df$value <- df$simulated
    df$variable <- "BIOCHEM.B"
    df$ID <- factor(df$low)
    #df$VISIT=as.numeric(levels(df$VISIT))[df$VISIT]
    
    df_summary <- df %>% # the names of the new data frame and the data frame to be summarised
      group_by(VISIT, variable) %>%                # the grouping variable
      summarise(mean_PL = mean(value, na.rm=TRUE),  # calculates the mean of each group
                sd_PL = sd(value, na.rm=TRUE),      # calculates the sd of each group
                n_PL = length(na.omit(value)),      # calculates the sample size per group
                SE_PL = sd(value, na.rm=TRUE)/sqrt(length(na.omit(value)))) # SE of each group
    
    df_summary1 <- merge(df, df_summary)  # merge stats to dataset
    
    df_summary1$L2SE <- df_summary1$mean_PL - 2*df_summary1$SE_PL
    df_summary1$H2SE <- df_summary1$mean_PL + 2*df_summary1$SE_PL
    
    
    pr1 <- ggplot((df_summary1), aes(x = VISIT, y =value, color = ID)) +
      geom_line( size=.5, alpha=0.2) +
      #scale_color_gradient(low = "blue", high = "red")+
      #scale_color_brewer(palette = "Dark2") +
      stat_summary(geom="line",  fun=mean, colour="black", lwd=0.5) +  # , linetype="dashed"
      stat_summary(geom="point", fun=mean, colour="black") +
      geom_errorbar(data=(df_summary1), 
                    aes( ymin=L2SE, ymax=H2SE ), color = "black",
                    width=0.05, lwd = 0.05) +
      scale_y_continuous(expand = c(.1,0) ) +
      
      
      
      scale_x_continuous(breaks = c(unique(df$VISIT)),
                         labels = 
                           c(unique(df$VISIT))
      ) +
      
      
      theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change plot and panel background
        plot.background=element_rect(fill = "white"),
        panel.background = element_rect(fill = 'black'),
        # Change legend
        legend.position = c(0.6, 0.07),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white")
      ) +
      
      
      EnvStats::stat_n_text(size = 4, y.pos = max(df_summary1$value, na.rm=T)*1.1 , y.expand.factor=0, 
                            angle = 0, hjust = .5, family = "mono", fontface = "plain") + 
      
      theme(panel.background=element_blank(),
            # axis.text.y=element_blank(),
            # axis.ticks.y=element_blank(),
            # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
            # stop axis being clipped
            plot.title=element_text(), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            legend.position="none",
            axis.text.x  = element_text(size=10),
            axis.text.y  = element_text(size=10),
            axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black"),
            plot.caption=element_text(hjust = 0, size = 7))
    
    
    
    
    if (input$Plot1 == "Overall") {

      
      print(pr1 + labs(y="Response", x = "Visit") + 
              ggtitle(paste0("Individual responses ",
                             length(unique(df$ID))," patients & arithmetic mean with 95% CI shown in black\nNumber of patient values at each time point") )
      )
  
   
    }  else  if (input$Plot1 == "Individual") {
      
      i <- as.numeric(unlist(strsplit(input$vec1,",")))
   
      
      if("999" %in% i) {
        
        dd<-df #d
        
      } else {
        
        
        #dd <- df[df$ID %in% i,]
        dd <- df[df$mid %in% i,]
        
      }
      
           
      print(pr1 + labs(y="Response", x = "Visit") + 
              ggtitle(paste0("Individual responses ",
                             length(unique(df$ID))," patients & arithmetic mean with 95% CI shown in black\nNumber of patient values at each time point") )
      )
      
      pxx <- pr1 +  geom_line(data = dd,
                             aes(group=ID, x = VISIT, y = value),    linetype="solid", col='red', size=1) 
      
   
      print(pxx)
    
      
    }   
  })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # end of spaghetti plots of data at which trt effect starts after baseline allowing highlighting of selected patients
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # end of boxplots of data at which trt effect starts after baseline allowing highlighting of selected patients
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # start of spaghetti plots of data at which trt effect starts after baseline allowing highlighting of selected patients
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # --------------------------------------------------------------------------
  # -----------------------------------------------OVERALL PLOT
  # ---------------------------------------------------------------------------
  
 
  # Diagnostics using data at which trt effect starts after baseline
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
   
 
  
})

# Run the application 
shinyApp(ui = ui, server = server)
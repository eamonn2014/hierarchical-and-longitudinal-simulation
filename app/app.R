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
                                      sliderInput("N",
                                                  div(h5(tags$span(style="color:blue", "total number of subjects"))),
                                                  min=2, max=500, step=1, value=200, ticks=FALSE),
                                      
                                      
                                      ################
                                      sliderInput("intercept",
                                                  "True intercept",
                                                  min=0, max=1000, step=.5, value=100, ticks=FALSE),
                                      
                                      sliderInput("top",
                                                  "Number of levels of top component (demarked by blue or thick lines)",
                                                  min=2, max=100, step=1, value=4, ticks=FALSE),
                                      
                                      sliderInput("range1", "Middle level: Randomly select using range or precisely select no of 'mid' groups within each top level group:", 
                                                  min = 2, max = 10, value = c(2, 10), ticks=FALSE) ,
                                      
                                      sliderInput("range2", "Lower level: Randomly select using range or precisely select no of 'low' groups within each mid level group:",
                                                  min = 2, max = 10, value = c(5, 10),ticks=FALSE),
                                      
                                      sliderInput("replicates", "Randomly select using range or precisely select no of replicates",
                                                  min = 2, max = 50, value = c(3, 10), ticks=FALSE),
                                      sliderInput("a",
                                                  "True top level SD",
                                                  min=1, max=100, step=.5, value=20, ticks=FALSE),
                                      sliderInput("b",
                                                  "True middle level SD",
                                                  min=1, max=100, step=.5, value=2, ticks=FALSE),
                                      sliderInput("c",
                                                  "True lower level SD",
                                                  min=1, max=100, step=.5, value=2, ticks=FALSE),
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
        
        
        
        
        N <-      input$N
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
        c <-           input$c
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
        top.r <-    rnorm(top,          d,                a)    
        middle.r <- rnorm(sum(middle),  0,                b)     
        #lower.r <-  rnorm(sum(lower),   0,                c)     # not needed as we create this later
        
        # ids
        lower.id <- rep(seq_len(sum(lower)), replicates )        
        middle.id <- cut(lower.id, c(0,cumsum(lower)),  labels=FALSE)
        top.id   <- cut(middle.id, c(0,cumsum(middle)), labels=FALSE)
        
        
    return(list( N=N,  beta0=beta0, beta1=beta1, sigma=sigma, q=q, s=s, r=r, J=J, 
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
        
        n <-N        <-      sample$N 
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
        
        trtB =  5
        interB= 2.5
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
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # creating baseline variable, I want treatment effect to start after baseline,
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # make.data2 <- reactive({
    #     
    #     sample <- random.sample()
    #     
    #     N         <-  sample$N 
    #     intercept <-  sample$beta0 
    #     slope     <-  sample$beta1
    #     error     <-  sample$sigma
    #     time.ref <-  sample$time.ref
    #     flat.df        <- make.data()$flat.df
    #     random.effects <- make.data()$random.effects
    #     p <- make.data()$p
    #     
    #     nbaseline <- flat.df[(flat.df$time !=0),]  
    #     
    #     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #     # select all observations that are baseline
    #     baseline <- flat.df[(flat.df$time ==0),]  
    #     # overwrite so that no treatment effect manifests at baseline
    #     baseline$alpha <- intercept + random.effects[, 1]
    #     baseline$beta <-  slope     + random.effects[, 2]
    #     
    #     # use tmp later for a plot
    #     tmp <- baseline <- within(baseline, y <-  alpha + 0 * beta + error * rnorm(n = N) )
    #     
    #     baseline <- baseline[,c("unit","y"   )] 
    #     names(baseline) <- c("unit","baseline"  )  # rename y to baseline
    #     
    #     # merge baseline and no baseline
    #     both <- merge (baseline , nbaseline  , all=TRUE)
    #     
    #     d <-  both[, c("unit", "baseline", "treat", "time", "y")]
    #     d$time<-factor(d$time)
    #     d$treat<-factor(d$treat)
    #     
    #     d$time <- relevel(d$time, ref=time.ref)
    #     
    #     # just put in random countries so no association
    #     #d$country <-  factor(sort(rep(sample(1:8 ),   N, times=J-1)))   # balanced
    #     d$country <- factor(rep(sample(1:8 , N, replace=T), times=p-1))  # unbalanced
    #     
    #     return(list( d=d , tmp=tmp, nbaseline=nbaseline) )
    #     
    # }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # run lmer and gls (and get contrasts) on the data were treatment effect starts after time 0
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    fit.regression.base<- reactive({
        
        d<- make.data2()$d
        
        d$treat <- relevel(d$treat, ref= "Placebo")  # NEW
        
        my.lmer <-  lmer(y ~  country + baseline * time + time * treat + (1 + as.numeric(time) | unit), data = d)
        
        ddz <<- datadist(d)  # need the double in this environ <<
        options(datadist='ddz')
        
        
        (fit.res <-  
                tryCatch(Gls(y  ~ country + baseline * time + time * treat ,
                             correlation=corSymm(form = ~as.numeric(time) | unit) ,
                             weights=varIdent(form=~1|time),
                             d, x=TRUE,
                             na.action=na.exclude ),  
                         error=function(e) e)
        ) 
        
        J <-  input$J
        time. <- rep(1:(J-1))
        
        
        # k1 <- contrast(fit.res, list(time=time.,  treat ="Placebo", baseline=0, country=1),
        #                         list(time=time.,  treat = "Active"  , baseline=0, country=1))
        # 
        # k1a <- contrast(fit.res, list(time=time.,  treat ="Placebo",   baseline=median(d$baseline), country=1),
        #                          list(time=time.,  treat = "Active"  , baseline=median(d$baseline), country=1))
        
        k1a <- rms::contrast(fit.res, list(time=time.,  treat ="Active" ),
                             list(time=time.,  treat = "Placebo" ))
        
        
        x <- as.data.frame(k1a[c('time', 'Contrast', 'Lower', 'Upper')]) 
        
        namez <- c("Follow-up Visit", "Active - Placebo", "Lower 95%CI","Upper 95%CI")
        
        names(x) <- namez
        
        return(list( fit.lmer= summary(my.lmer) , fit.res=fit.res , x=x))
        
    })   
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.summaryb1 <- renderPrint({
        
        summary <- fit.regression.base()$fit.res
        
        return(list(summary))
        
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.summaryb2 <- renderPrint({
        
        summary <- fit.regression.base()$fit.lmer
        
        return(list(summary))
        
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.summaryb3copy <- output$reg.summaryb3 <- renderPrint({
        
        summary <- fit.regression.base()$x
        
        return(list(summary))
        
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # spaghetti plot of the data in which trt effect starts at baseline 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # output$reg.plot1 <- renderPlot({         
    #     
    #     flat.df <- make.data()$flat.df
    #     
    #     ggplot(flat.df,   aes (x = time, y = y, group = unit, color = treat)) +
    #         geom_line() + geom_point() + ylab("response") + xlab("visit") +
    #         stat_summary(fun=mean,geom="line", colour="black",lwd=1,aes(group=treat ) ) +
    #         # geom_smooth(method=lm, se=FALSE, fullrange=TRUE )+
    #         # scale_shape_manual(values=c(3, 16))+ 
    #         scale_color_manual(values=c('#999999','#E69F00'))+
    #         theme(legend.position="top") +
    #         xlim(0, J) +
    #         scale_x_continuous(breaks=c(0:J)) 
    #     
    # }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # start of spaghetti plots of data at which trt effect starts after baseline allowing highlighting of selected patients
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # --------------------------------------------------------------------------
    # -----------------------------------------------OVERALL PLOT
    # ---------------------------------------------------------------------------
    
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
    
    fit.regression0 <- reactive({
        
        flat.df <- make.data()$flat.df
        
        flat.df$treat <- relevel(flat.df$treat, ref= "Placebo")  #NEW
        
        my.lmer <-  lmer(y ~    time * treat + (1 + time | unit), data = flat.df)
        
        return(list(fit.res= summary(my.lmer) ))
        
    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # model output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$reg.summary <- renderPrint({
        
        summary <- fit.regression0()$fit.res
        
        return(list(summary))
        
    })  
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # fit gls regression on the data in which trt effect starts at baseline and get contrasts over time
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    fit.regression.gls0 <- reactive({
        
        flat.df <- make.data()$flat.df
        ## new is this what we expect
        
        time.ref <-  input$time.ref
        
        tmp <- flat.df
        
        tmp$j <- factor(tmp$j)
        tmp$j <- relevel(tmp$j, ref=time.ref)
        
        #table(tmp$j, tmp$time)
        tmp$j <- as.factor(tmp$j )
        
        tmp$treat <- relevel(tmp$treat, ref= "Placebo")
        
        
        
        ddz <<- datadist(tmp)  # need the double in rshiny environ <<
        options(datadist='ddz')
        
        #j works but not time... consecutive integer error in gls?
        #table(tmp$j, tmp$time)
        #tmp$j <- as.factor(tmp$j )
        
        fit.res <- NULL
        (fit.res <-
                tryCatch(Gls(y  ~ treat + j * treat ,
                             correlation=corSymm(form = ~as.numeric(j) | unit) ,
                             weights=varIdent(form=~1|j),
                             tmp, x=TRUE,
                             na.action=na.exclude ),
                         error=function(e) e)
        )
        
        fit <-  fit.res
        
        J <-  input$J
        time. <- rep(1:(J))
        
        
        k1a <- rms::contrast(fit, list(j=time.,  treat = "Active"  ),
                             list(j=time.,  treat = "Placebo"  ))
        
        # k1a$j <- k1a$j-1 # new
        
        return(list(fit.res= fit.res , k1a=k1a  ))
        
    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # plot of treatment effect contrasts for data at which trt effect starts at baseline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.plot2 <- renderPlot({
        
        
        
        k1a <-  fit.regression.gls0()$k1a
        
        J <-  input$J
        time. <- rep(1:(J))
        k1a <- as.data.frame(k1a[c(1,2,4,5)])
        
        mi <- floor(min(k1a$Lower))
        ma <- ceiling(max(k1a$Upper))
        
        names(k1a) <- (c( "Time",'Contrast', 'Lower', 'Upper'))
        
        xl <- xlab( 'Follow up visit (Visit 1 is baseline)')
        
        ggplot (k1a, aes(x=time. , y=Contrast, group=1)) + geom_point () + geom_line () +
            ylim(mi,ma) +
            #   xlim(1, input$V-1) +
            xlim(1, J) +
            scale_x_continuous(breaks=c(time.)) +
            
            ylab( 'Active - Placebo')+ xl +
            geom_errorbar(aes(ymin=Lower, ymax=Upper ), width =0) +
            ggtitle(paste0("Outcome measure "," \ntreatment effect estimate at each visit with 95% CI")) +
            geom_hline(aes(yintercept = 0, colour = 'red'), linetype="dashed") +
            
            #scale_x_discrete(labels= newxticks) +
            
            theme_bw() +
            theme(legend.position="none") +
            theme(#panel.background=element_blank(),
                # axis.text.y=element_blank(),
                # axis.ticks.y=element_blank(),
                # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                # stop axis being clipped
                plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                legend.text=element_text(size=14),
                legend.title=element_text(size=14),
                legend.position="none",
                axis.text.x  = element_text(size=15),
                axis.text.y  = element_text(size=15),
                axis.line.x = element_line(color="black"),
                axis.line.y = element_line(color="black"),
                plot.caption=element_text(hjust = 0, size = 7),
                strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                axis.title.y = element_text(size = rel(1.5), angle = 90),
                axis.title.x = element_text(size = rel(1.5), angle = 0),
                panel.grid.major.x = element_line(color = "grey80", linetype="dotted", size = 1),
                panel.grid.major.y = element_line(color = "grey80", linetype="dotted", size = 1),
                strip.background = element_rect(colour = "black", fill = "#ececf0"),
                panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                plot.background = element_rect(fill = '#ececf0', colour = '#ececf0')
            )
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # boxplots of data at which trt effect starts at baseline allowing highlighting of selected patients
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # --------------------------------------------------------------------------
    # -----------------------------------------------OVERALL PLOT
    # ---------------------------------------------------------------------------
    
    output$reg.plot3 <- renderPlot({ 
        
        flat.df <- make.data()$flat.df
        d <- flat.df
        
        d$trt <- d$treat
        d$rep <- d$unit
        d$yij <- d$y
        d$time <- factor(d$time)
        
        if (input$Plot1 == "Overall") {
            
            
            # lets get counts to put in ribbons
            d$trt <- factor(d$trt)
            dx <- unique(d[,c("rep","trt")])
            table(dx$trt)
            n <- as.vector(table(dx$trt))
            levels(d$trt) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
            
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            pd <- position_dodge(.4)
            pr1=NULL
            pr1 <- ggplot(d,aes(x=time ,y=yij,color=trt, fill=trt ))  + 
                stat_boxplot(geom = "errorbar", width = 0.3) +
                geom_boxplot( outlier.colour = NA  ) +  # removed fill=NA
                geom_line(aes(group=rep), position = pd,  alpha=0.6, linetype="dotted")   + 
                scale_size_manual( values = c( 1) ) +
                geom_point(aes(fill=trt, group=rep), pch=1, size=1, alpha=0.3, position = pd ) +
                stat_summary(fun=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                             position=pd, show.legend=FALSE) +
                scale_color_manual(name = "Treatment", values = c("blue", "darkgreen")) +
                scale_fill_manual(name = "Treatment", values = c("lightblue", "green")) +
                facet_wrap(~trt , ncol=2)    +
                labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 - 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.") +
                
                geom_text(data = d %>% group_by( time, trt) %>%
                              dplyr::summarise(Count = n()) %>%
                              ungroup %>%
                              mutate(yij=min((d$yij)) - 0.05 * diff(range((d$yij)))),
                          aes(label = paste0("n = ", Count)),
                          position = pd, size=3, show.legend = FALSE) 
            
            
            print(pr1 + labs(y="Response", x = 'Visit') +    
                      ggtitle(paste0("There are N=",
                                     length(unique(d$rep)),  
                                     " patients with data at baseline, presenting all patient profiles, with boxplots and the number of patient values at each visit") ) +
                      theme_bw() +
                      theme(legend.position="none") +
                      theme(#panel.background=element_blank(),
                          # axis.text.y=element_blank(),
                          # axis.ticks.y=element_blank(),
                          # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                          # stop axis being clipped
                          plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                          legend.text=element_text(size=14),
                          legend.title=element_text(size=14),
                          legend.position="none",
                          axis.text.x  = element_text(size=15),
                          axis.text.y  = element_text(size=15),
                          axis.line.x = element_line(color="black"),
                          axis.line.y = element_line(color="black"),
                          plot.caption=element_text(hjust = 0, size = 11),
                          strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                          axis.title.y = element_text(size = rel(1.5), angle = 90),
                          axis.title.x = element_text(size = rel(1.5), angle = 0),
                          strip.background = element_rect(colour = "black", fill = "#ececf0"),
                          panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                          plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'),#
                      ) 
            )
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Individual profiles
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
        }  else  if (input$Plot1 == "Individual") {
            
            
            i <- as.numeric(unlist(strsplit(input$vec1,",")))
            
            d$trt <- factor(d$trt)
            dx <- unique(d[,c("rep","trt")])
            table(dx$trt)
            n <- as.vector(table(dx$trt))
            levels(d$trt) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
            
            
            #  if 999 is entered all subjects are shown
            
            if("999" %in% i) {
                
                dd<-d
                
            } else {
                
                dd <- d[d$rep %in% i,]
            }
            
            sel <- unique(dd$trt) #if only one arm , dont show the empty arm
            d <- d[d$trt %in% sel,]
            
            dx <- unique(dd[,c("rep","trt")])
            
            nn <- as.vector(table(dx$trt))
            
            levels(d$trt)  <- levels(dd$trt) <-
                c(paste0("Active N=",n[1], " with " ,nn[1]," patient profile(s) shown"),
                  paste0("Placebo N=",n[2], " with " ,nn[2]," patient profile(s) shown"))
            
            
            
            pd <- position_dodge(.4)
            pr1=NULL
            pr1<-ggplot(d,aes(x=time ,y=yij,color=trt, fill=trt )) +
                stat_boxplot(geom = "errorbar", width = 0.3) +
                geom_boxplot( outlier.colour = NA) +#,alpha=0.1, color="lightblue",)  +
                
                geom_line(data = dd,
                          aes(group=rep,x = time, y = yij),  size = .6, linetype="solid", colour='red' , position = pd) +  # pd added here
                scale_size_manual( values = c( 1) ) +
                
                geom_point(data=dd, aes(fill=trt, group=rep),
                           pch=19, size=3, colour='black',alpha=0.7, position = pd ) +
                
                geom_point(aes(fill=trt, group=rep),
                           pch=1, size=2, alpha=0.2, position = pd ) +
                
                stat_summary(fun.y=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                             position=pd, show.legend=FALSE) +
                scale_color_manual(name = "Treatment", values = c("blue", "darkgreen") ) +
                scale_fill_manual(name = "Treatment", values = c("lightblue", "green") ) +
                facet_wrap(~trt , ncol=2)    +
                labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 – 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.")+
                
                geom_text(data = dd %>% group_by( time, trt) %>%
                              summarise(Count = n()) %>%
                              ungroup %>%
                              mutate(yij=min((d$yij)) - 0.05 * diff(range((d$yij)))),
                          aes(label = paste0("n = ", Count)),
                          position = pd, size=5, show.legend = FALSE)
            
            print(pr1 + labs(y="Response", x = 'Visit (0 denotes the baseline visit)') +
                      
                      ggtitle(paste0("There are N=",
                                     length(unique(d$rep)),
                                     " patients with data at baseline, presenting selected patient profiles") ) +
                      theme_bw() +
                      theme(legend.position="none") +
                      theme(#panel.background=element_blank(),
                          # axis.text.y=element_blank(),
                          # axis.ticks.y=element_blank(),
                          # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                          # stop axis being clipped
                          plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                          legend.text=element_text(size=14),
                          legend.title=element_text(size=14),
                          legend.position="none",
                          axis.text.x  = element_text(size=15),
                          axis.text.y  = element_text(size=15),
                          axis.line.x = element_line(color="black"),
                          axis.line.y = element_line(color="black"),
                          plot.caption=element_text(hjust = 0, size = 11),
                          strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                          axis.title.y = element_text(size = rel(1.5), angle = 90),
                          axis.title.x = element_text(size = rel(1.5), angle = 0),
                          strip.background = element_rect(colour = "black", fill = "#ececf0"),
                          panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                          plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'),#
                      )
            )
            
        }   
        
        
    }) 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # end boxplots of data at which trt effect starts at baseline highlighting individuals
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # plot of treatment effect for data at which trt effect starts after baseline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.plot2b <- renderPlot({         
        
        d<- make.data2()$d
        
        k1a <- fit.regression.base()$x
        fit <- fit.regression.base()$fit.res
        J <-  input$J
        time. <- rep(1:(J-1))
        
        mi <- floor(min(k1a$Lower))
        ma <- ceiling(max(k1a$Upper))
        
        names(k1a) <- (c( "Time",'Contrast', 'Lower', 'Upper'))
        
        k1 <- as.data.frame(k1a)
        
        xl <- xlab( 'Visit')
        
        ggplot (k1, aes(x=time. , y=Contrast, group=1)) + geom_point () + geom_line () +
            ylim(mi,ma) +
            #   xlim(1, input$V-1) +
            xlim(1, J) +
            scale_x_continuous(breaks=c(time.)) +
            
            ylab( 'Active - Placebo')+ xl +
            geom_errorbar(aes(ymin=Lower, ymax=Upper ), width =0) +
            ggtitle(paste0("Outcome measure "," \ntreatment effect estimate at each visit with 95% CI")) +
            geom_hline(aes(yintercept = 0, colour = 'red'), linetype="dashed") +
            theme_bw() +
            theme(legend.position="none") +
            theme(#panel.background=element_blank(),
                # axis.text.y=element_blank(),
                # axis.ticks.y=element_blank(),
                # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                # stop axis being clipped
                plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                legend.text=element_text(size=14),
                legend.title=element_text(size=14),
                legend.position="none",
                axis.text.x  = element_text(size=15),
                axis.text.y  = element_text(size=15),
                axis.line.x = element_line(color="black"),
                axis.line.y = element_line(color="black"),
                plot.caption=element_text(hjust = 0, size = 7),
                strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                axis.title.y = element_text(size = rel(1.5), angle = 90),
                axis.title.x = element_text(size = rel(1.5), angle = 0),
                panel.grid.major.x = element_line(color = "grey80", linetype="dotted", size = 1),
                panel.grid.major.y = element_line(color = "grey80", linetype="dotted", size = 1),
                strip.background = element_rect(colour = "black", fill = "#ececf0"),
                panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                plot.background = element_rect(fill = '#ececf0', colour = '#ececf0')
            )
        
    }) 
    
    
    output$reg.summary1 <- renderPrint({
        
        summary <- fit.regression.gls0()$fit.res
        
        return(list(summary))
        
    })  
    
    output$reg.summary2c <- output$reg.summary2 <- renderPrint({
        
        summary <- fit.regression.gls0()$k1a
        
        return(list(summary))
        
    })  
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # boxplots of data at which trt effect starts after baseline allowing highlighting of selected patients
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # --------------------------------------------------------------------------
    # -----------------------------------------------OVERALL PLOT
    # ---------------------------------------------------------------------------
    
    output$reg.plot33 <- renderPlot({ 
        
        tmp <- make.data2()$tmp
        nbaseline <- make.data2()$nbaseline
        all <- rbind(tmp, nbaseline)
        d <- all  # add this to shown baslein
        d$trt <- d$treat
        d$rep <- d$unit
        d$yij <- d$y
        d$time <- factor(d$time)
        
        if (input$Plot1 == "Overall") {
            
            
            # lets get counts to put in ribbons
            d$trt <- factor(d$trt)
            dx <- unique(d[,c("rep","trt")])
            table(dx$trt)
            n <- as.vector(table(dx$trt))
            levels(d$trt) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
            
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            pd <- position_dodge(.4)
            pr1=NULL
            pr1 <- ggplot(d,aes(x=time ,y=yij,color=trt, fill=trt ))  + 
                stat_boxplot(geom = "errorbar", width = 0.3) +
                geom_boxplot( outlier.colour = NA  ) +  # removed fill=NA
                geom_line(aes(group=rep), position = pd,  alpha=0.6, linetype="dotted")   + 
                scale_size_manual( values = c( 1) ) +
                geom_point(aes(fill=trt, group=rep), pch=1, size=1, alpha=0.3, position = pd ) +
                stat_summary(fun=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                             position=pd, show.legend=FALSE) +
                scale_color_manual(name = "Treatment", values = c("blue", "darkgreen")) +
                scale_fill_manual(name = "Treatment", values = c("lightblue", "green")) +
                facet_wrap(~trt , ncol=2)    +
                labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 - 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.") +
                
                geom_text(data = d %>% group_by( time, trt) %>%
                              dplyr::summarise(Count = n()) %>%
                              ungroup %>%
                              mutate(yij=min((d$yij)) - 0.05 * diff(range((d$yij)))),
                          aes(label = paste0("n = ", Count)),
                          position = pd, size=3, show.legend = FALSE) 
            
            
            print(pr1 + labs(y="Response", x = 'Visit') +    
                      ggtitle(paste0("There are N=",
                                     length(unique(d$rep)),  
                                     " patients with data at baseline, presenting all patient profiles, with boxplots and the number of patient values at each visit") ) +
                      theme_bw() +
                      theme(legend.position="none") +
                      theme(#panel.background=element_blank(),
                          # axis.text.y=element_blank(),
                          # axis.ticks.y=element_blank(),
                          # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                          # stop axis being clipped
                          plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                          legend.text=element_text(size=14),
                          legend.title=element_text(size=14),
                          legend.position="none",
                          axis.text.x  = element_text(size=15),
                          axis.text.y  = element_text(size=15),
                          axis.line.x = element_line(color="black"),
                          axis.line.y = element_line(color="black"),
                          plot.caption=element_text(hjust = 0, size = 11),
                          strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                          axis.title.y = element_text(size = rel(1.5), angle = 90),
                          axis.title.x = element_text(size = rel(1.5), angle = 0),
                          strip.background = element_rect(colour = "black", fill = "#ececf0"),
                          panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                          plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'),#
                      ) 
            )
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Individual profiles
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
        }  else  if (input$Plot1 == "Individual") {
            
            
            i <- as.numeric(unlist(strsplit(input$vec1,",")))
            
            d$trt <- factor(d$trt)
            dx <- unique(d[,c("rep","trt")])
            table(dx$trt)
            n <- as.vector(table(dx$trt))
            levels(d$trt) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
            
            
            #  if 999 is entered all subjects are shown
            
            if("999" %in% i) {
                
                dd<-d
                
            } else {
                
                dd <- d[d$rep %in% i,]
            }
            
            sel <- unique(dd$trt) #if only one arm , dont show the empty arm
            d <- d[d$trt %in% sel,]
            
            dx <- unique(dd[,c("rep","trt")])
            
            nn <- as.vector(table(dx$trt))
            
            levels(d$trt)  <- levels(dd$trt) <-
                c(paste0("Active N=",n[1], " with " ,nn[1]," patient profile(s) shown"),
                  paste0("Placebo N=",n[2], " with " ,nn[2]," patient profile(s) shown"))
            
            
            
            pd <- position_dodge(.4)
            pr1=NULL
            pr1<-ggplot(d,aes(x=time ,y=yij,color=trt, fill=trt )) +
                stat_boxplot(geom = "errorbar", width = 0.3) +
                geom_boxplot( outlier.colour = NA) +#,alpha=0.1, color="lightblue",)  +
                
                geom_line(data = dd,
                          aes(group=rep,x = time, y = yij),  size = .6, linetype="solid", position = pd, colour='red',) +  # new pd added
                scale_size_manual( values = c( 1) ) +
                geom_point(data=dd, aes(fill=trt, group=rep),
                           pch=19, size=3, colour='black',alpha=0.7, position = pd ) +
                
                geom_point(aes(fill=trt, group=rep),
                           pch=1, size=2, alpha=0.2, position = pd ) +
                stat_summary(fun.y=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                             position=pd, show.legend=FALSE) +
                scale_color_manual(name = "Treatment", values = c("blue", "darkgreen") ) +
                scale_fill_manual(name = "Treatment", values = c("lightblue", "green") ) +
                facet_wrap(~trt , ncol=2)    +
                labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 – 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.")+
                
                geom_text(data = dd %>% group_by( time, trt) %>%
                              summarise(Count = n()) %>%
                              ungroup %>%
                              mutate(yij=min((d$yij)) - 0.05 * diff(range((d$yij)))),
                          aes(label = paste0("n = ", Count)),
                          position = pd, size=5, show.legend = FALSE)
            
            print(pr1 + labs(y="Response", x = 'Visit (0 denotes the baseline visit)') +
                      
                      ggtitle(paste0("There are N=",
                                     length(unique(d$rep)),
                                     " patients with data at baseline, presenting selected patient profiles") ) +
                      theme_bw() +
                      theme(legend.position="none") +
                      theme(#panel.background=element_blank(),
                          # axis.text.y=element_blank(),
                          # axis.ticks.y=element_blank(),
                          # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                          # stop axis being clipped
                          plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                          legend.text=element_text(size=14),
                          legend.title=element_text(size=14),
                          legend.position="none",
                          axis.text.x  = element_text(size=15),
                          axis.text.y  = element_text(size=15),
                          axis.line.x = element_line(color="black"),
                          axis.line.y = element_line(color="black"),
                          plot.caption=element_text(hjust = 0, size = 11),
                          strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                          axis.title.y = element_text(size = rel(1.5), angle = 90),
                          axis.title.x = element_text(size = rel(1.5), angle = 0),
                          strip.background = element_rect(colour = "black", fill = "#ececf0"),
                          panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                          plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'),#
                      )
            )
            
        }   
        
        
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # end of boxplots of data at which trt effect starts after baseline allowing highlighting of selected patients
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # start of spaghetti plots of data at which trt effect starts after baseline allowing highlighting of selected patients
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # --------------------------------------------------------------------------
    # -----------------------------------------------OVERALL PLOT
    # ---------------------------------------------------------------------------
    
    output$reg.plot99 <- renderPlot({ 
        
        J <-  input$J
        tmp <- make.data2()$tmp
        nbaseline <- make.data2()$nbaseline
        all <- rbind(tmp, nbaseline)
        all$time <- as.numeric(as.character(all$time ))
        
        if (input$Plot1 == "Overall") {
            
            
            # lets get counts to put in ribbons
            
            
            ggplot(all,   aes (x = time, y = y, group = unit, color = treat)) +
                geom_line() + geom_point() + ylab("Response") + xlab("Visit") +
                stat_summary(fun=mean,geom="line", lwd=1,aes(group=treat  ,    color=paste(treat, "Mean")     ) )+
                scale_color_manual(values=colz)+
                theme(legend.position="top") +
                xlim(0, J) +
                scale_x_continuous(breaks=c(0:J)) +
                
                theme_bw() +
                theme(#panel.background=element_blank(),
                    # axis.text.y=element_blank(),
                    # axis.ticks.y=element_blank(),
                    # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                    # stop axis being clipped
                    plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=14),
                    legend.position="top",
                    axis.text.x  = element_text(size=15),
                    axis.text.y  = element_text(size=15),
                    axis.line.x = element_line(color="black"),
                    axis.line.y = element_line(color="black"),
                    plot.caption=element_text(hjust = 0, size = 7),
                    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                    axis.title.y = element_text(size = rel(1.5), angle = 90),
                    axis.title.x = element_text(size = rel(1.5), angle = 0),
                    #panel.grid.major.x = element_line(color = "grey80", linetype="dotted", size = 1),
                    panel.grid.major.y = element_line(color = "grey80", linetype="dotted", size = 1),
                    strip.background = element_rect(colour = "black", fill = "#ececf0"),
                    panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                    plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'))
            
            
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Individual profiles
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
        }  else  if (input$Plot1 == "Individual") {
            
            i <- as.numeric(unlist(strsplit(input$vec1,",")))
            
            #  if 999 is entered all subjects are shown
            
            if("999" %in% i) {
                
                dd<-all #d
                
            } else {
                
                
                dd <- all[all$unit %in% i,]
            }
            
            
            px <-  ggplot(all,   aes (x = time, y = y, group = unit, color = treat)) +
                geom_line() + geom_point() + ylab("Response") + xlab("Visit") +
                stat_summary(fun=mean,geom="line", lwd=1,aes(group=treat  ,    color=paste(treat, "Mean")     ) )+
                scale_color_manual(values=colz)+          
                theme(legend.position="top") +
                xlim(0, J) +
                scale_x_continuous(breaks=c(0:J)) +
                
                theme_bw() +
                theme(#panel.background=element_blank(),
                    # axis.text.y=element_blank(),
                    # axis.ticks.y=element_blank(),
                    # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                    # stop axis being clipped
                    plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=14),
                    legend.position="top",
                    axis.text.x  = element_text(size=15),
                    axis.text.y  = element_text(size=15),
                    axis.line.x = element_line(color="black"),
                    axis.line.y = element_line(color="black"),
                    plot.caption=element_text(hjust = 0, size = 7),
                    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                    axis.title.y = element_text(size = rel(1.5), angle = 90),
                    axis.title.x = element_text(size = rel(1.5), angle = 0),
                    #panel.grid.major.x = element_line(color = "grey80", linetype="dotted", size = 1),
                    panel.grid.major.y = element_line(color = "grey80", linetype="dotted", size = 1),
                    strip.background = element_rect(colour = "black", fill = "#ececf0"),
                    panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                    plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'))
            
            pxx <- px +  geom_line(data = dd,
                                   aes(group=unit,x = time, y = y),    linetype="solid", col='red', size=1) 
            
            print(pxx)
            
            
            
            
        }   
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # end of spaghetti plots of data at which trt effect starts after baseline allowing highlighting of selected patients
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
    }) 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Diagnostics using data at which trt effect starts after baseline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$res.diag  <- renderPlot({
        
        fit <- fit.regression.base()$fit.res
        
        d<- make.data2()$d
        
        # target <- input$Plot
        #  d <- d[d$test %in% target,]
        d2 <- d
        
        d2$resid <- r <- resid(fit)
        
        d2$fitted <- fitted(fit)
        
        yl <- ylab('Residuals')
        
        xl <- xlab("time")
        
        p1 <- ggplot(d2 , aes(x=fitted , y=resid)) + geom_point (   colour="#69b3a2") + yl
        
        p3 <- ggplot(d2 , aes(x=time , y=resid )) +  geom_point ( colour="#69b3a2") + yl  + xl +
            stat_summary(fun.data ="mean_sdl", geom='smooth')
        
        p4 <- ggplot(d2 , aes(sample=resid )) + stat_qq(colour="#69b3a2") +
            geom_abline(intercept=mean(r), slope=sd(r)  ,  colour="black") +
            xlab('Residuals')   +
            ggtitle( " ")
        
        # p5 <- d2 %>%
        #   ggplot( aes(x=r)) +
        #   geom_histogram( fill="#69b3a2", color="#e9ecef", alpha=0.9) + #binwidth=1,
        #  theme(
        #     plot.title = element_text(size=15)
        #   )
        library(gridExtra)
        library(grid)
        df <- data.frame(Residuals = r)
        p5 <- ggplot(df, aes(x = Residuals)) +
            geom_histogram(aes(y =..density..),
                           #breaks = seq(-50, 50, by = 2),
                           colour = "black",
                           fill = "#69b3a2") +
            stat_function(fun = dnorm, args = list(mean = 0, sd = sigma(fit)  ))
        
        grid.arrange(p1,  p3, p4,p5, ncol=2,
                     
                     top = textGrob(paste0(input$Plot, " GLS model fit diagnostics"),gp=gpar(fontsize=20,font=3)))
        #+
        #main=paste0(input$Plot, "GLS model fit diagnostics")  #
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # listing of simulated data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$table1 <- DT::renderDataTable({
        
        foo<- make.data2()$d
        
        foo$time <- relevel(foo$time , ref=1)
        
        foo <- plyr::arrange(foo, unit, time)
        
        names(foo) <- c("Patient","Baseline", "Treatment", "Visit","Response", "Country")
        
        rownames(foo) <- NULL
        
        
        datatable(foo,
                  
                  rownames = TRUE,
                  
                  options = list(
                      searching = TRUE,
                      pageLength = 16,
                      paging=TRUE,
                      lengthMenu = FALSE ,
                      lengthChange = FALSE,
                      autoWidth = FALSE
                      #colReorder = TRUE,
                      #  deferRender = TRUE
                      #scrollY = 600,
                      # scroller = T
                  )) %>%
            
            formatRound(
                columns= c("Baseline", "Response"), 
                digits=4 )
        
        
        
    })
    
})

# Run the application 
shinyApp(ui = ui, server = server)
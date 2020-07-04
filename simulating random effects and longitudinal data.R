# top=.8338
# mid=8.5985
# low=18.4160
# residual=50.4589
# trt=4
# slope=-.166
# interaction =-.4

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## simulating hierarchical random effects and longitudinal response 
## this is good...hybrid approach from my nested app and longitudinal modelling app
## need to include the baseline as covariate
## need to predict, simulate and check residuals...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rm(list=ls())
    set.seed(786)
    
    library(lme4)
    library(VCA)
    library(ggplot2)
    library(MASS)
    library(nlme)
    library(tidyverse)
    
    p1 <- function(x) {formatC(x, format="f", digits=1)}
    
    # 3 level nested app chunk~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # design
    intercept =100 
    top = 5 # number of groups at top of hierarchy
    
    # in the app this is selectable...
    range1     = c(2, 10) ##app samples between these, levels 
    range2     = c(5, 10) ##app sample these
    replicates = c(3, 10) ##app sample these
    
    x1 <- range1[1]
    x2 <- range1[2]
    x3 <- range2[1]
    x4 <- range2[2]
    x5 <- replicates[1]
    x6 <- replicates[2]
    
    # random effect SDs
    a =20
    b =10 
    #c =2   not need as we add a random intercept and random slope later
    d =2

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
    
    
    N <- sum(replicates)
    
    residual <- d
    d <- intercept


# random effects
  top.r <-    rnorm(top,          d,                a)    
  middle.r <- rnorm(sum(middle),  0,                b)     
  #lower.r <-  rnorm(sum(lower),   0,                c)     # not needed as we create this later
  
  # ids
  lower.id <- rep(seq_len(sum(lower)), replicates )        
  middle.id <- cut(lower.id, c(0,cumsum(lower)),  labels=FALSE)
  top.id   <- cut(middle.id, c(0,cumsum(middle)), labels=FALSE)

#---------------------------------------------------------------------------
 ### set number of individuals, this chunk from longitudianl app
    n      <- N
    beta1  <- -1 # slope
    ar.val <- 0.9
    sigma  <- residual 
    tau0   <-  1.2748
    tau1   <-  0.2276
    tau01  <- -0.62
  
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
                       U[,1][lower.id] +                              # random intercepts
                      ((beta1 +  U[,2][lower.id]) *  time), sigma),   # random slopes
  
                      # ar1 nosie
                      y=   top.r[top.id] + 
                                middle.r[middle.id] +
                                U[,1][lower.id] +             # random intercepts
                      ((beta1 +  U[,2][lower.id]) *  time) +  # random slopes
                                eij
  )
                              
  df <- as.data.frame(Data)
  df$time <- df$time-1  # set baseline to time=0

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
# ignoring treatment

  df[1:40,]
  
  df$time <- factor(df$time)
  
  (fit2 <- lmer( y2 ~ time + (1|top) + (1|mid) + (as.numeric(time)|low), data=df))
  
  (fit3 <- lmer( y ~ time + (1|top) + (1|mid) + (as.numeric(time)|low), data=df))
  
  (fit.res <-  
      tryCatch(gls(y2 ~ time +0 ,
                   correlation=corAR1(form=~ as.numeric(time)|low), 
                   weights=varIdent(form=~1|time),
                   df, na.action=na.omit) , 
               error=function(e) e))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analysisng  treatment

  
  (fit2 <- lmer( y2b ~ time* trt + (1|top) + (1|mid) + (as.numeric(time)|low), data=df))
  
  (fit3 <- lmer( yb ~ time* trt  + (1|top) + (1|mid) + (as.numeric(time)|low), data=df))
  
  (fit.res <-  
      tryCatch(gls(y2b ~ time* trt +0 ,
                   correlation=corAR1(form=~ as.numeric(time)|low), 
                   weights=varIdent(form=~1|time),
                   df, na.action=na.omit) , 
               error=function(e) e))
  
  
  summary(fit2)
  summary(fit3)
  summary(fit.res)
  
  #########################################
  
  df$predicted <- predict (fit3, newdata=df, allow.new.levels=T)
  df$simulated <- simulate(fit3, seed=1, newdata=df , re.form=NA,
                           allow.new.levels=F)$sim_1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot not model based~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# no treatment effect plotted here, just a mean effect overall

  df$VISIT <- df$time
  df$value <- df$y2       # ar1 is th ey response
 # df$value <- df$simulated
  df$variable <- "BIOCHEM.B"
  df$ID <- factor(df$low)
  df$VISIT=as.numeric(levels(df$VISIT))[df$VISIT]
  
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
############## plot of treatment effect not from model
######################################################

  df$time <- as.numeric(as.character(df$VISIT))
  df$y <- df$yb
  df$unit <- df$low
  df$treat <- factor(df$trt)
  
  pd <- position_dodge(.4)
  
  plot1 <-  ggplot(df,   aes (x = time, y = y, group = unit, color = treat)) +
    geom_line() + geom_point() + ylab("response") + xlab("visit") +
    stat_summary(fun=mean,geom="line", colour="red",lwd=2,aes(group=treat ) ) +
    # geom_smooth(method=lm, se=FALSE, fullrange=TRUE )+
    # scale_shape_manual(values=c(3, 16))+ 
    scale_color_manual(values=c('black','orange'))+
    theme(legend.position="top") +
    #xlim(0, J) +
    scale_x_continuous(breaks=c(0:max(df$VISIT)))
  
  print(plot1 + labs(y="Response", x = "Visit")) + 
          ggtitle(paste0("Individual responses from ",
                         length(unique(df$unit)),
                         " patients, true trt effect ",trtB," true interaction ", interB  ,"
  Red lines arithmetic means at each time point in treatment groups.") )


  summary(fit2)
  
  df$time <- factor(df$time)
  
  j = sort(levels(df$time))
  
  res<- rep(NA, length(j))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # print trt effects from a model
  
  ests <- function(x){
  
  for (i in j) {
   
    # relevel to estimate  
    df$time <- relevel(df$time, ref=i)
    f <- (lmer( yb ~ time* trt  + (1|top) + (1|mid) + (as.numeric(time)|low), data=df))
    #print(summary(f))
   # print(paste("Time",i))
    res[i] <- fixed.effects(f)['trt'][1][[1]]
  }
    return(res)
  
  }
  
  x <- ests()
  print(as.vector(na.omit(as.vector(x))), digits=1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# true trt effect - that which we should expect to  estimate
  c(0,trtB + interB*(1:9))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
 

  # below here chckong model diagnostics



  plot(fit3)

  ggplot(data.frame(eta=predict(fit3,type="link"),pearson=residuals(fit3,type="pearson")),
                aes(x=eta,y=pearson)) +
         geom_point() +
         theme_bw()

  qqnorm(residuals(fit3))


  ggplot(data.frame(lev=hatvalues(fit3),pearson=residuals(fit3,type="pearson")),
         aes(x=lev,y=pearson)) +
    geom_point() +
    theme_bw()

  #https://stackoverflow.com/questions/13847936/plot-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot-how-to-mak
  coef(fit3)
 
 # https://r.789695.n4.nabble.com/R-lme4-package-Fitted-values-and-residuals-td812638.html
  fitted(fit3)
  resid(fit3)
  
  fixef(fit3)

  #and a list of the conditional modes of the random effects (also called
                  #                                           the BLUPs) as
  
  ranef(fit3)
  


  lattice::qqmath(ranef(fit3, condVar = TRUE), strip = FALSE)$mid
  
  
  lattice::qqmath(ranef(fit3, condVar = TRUE), strip = FALSE)$top
  #https://stackoverflow.com/questions/13847936/plot-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot-how-to-mak


  qqnorm(ranef(fit3)$low[,1] )
  qqline(ranef(fit3)$low[,1], col = "red")
  qqnorm(ranef(fit3)$low[,2] )
  qqline(ranef(fit3)$low[,2], col = "red")
  
  
  
  p <- ggplot(df, aes(x = time, y = y, colour = ID)) +
    geom_point(size=3) +
    geom_line(aes(y = predict(fit3)),size=1) +
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
          axis.line.y = element_line(color="black"))
  print(p)

  
  
  df$predicted <- predict (fit3, newdata=df, allow.new.levels=T)
  df$simulated <- simulate(fit3, seed=1, newdata=df , re.form=NA,
                          allow.new.levels=F)$sim_1
  
  
  library(sjstats)
  performance::icc(fit3)
  performance::model_performance(fit3, metrics = "all", verbose = TRUE)
  
  
  
  
  
  
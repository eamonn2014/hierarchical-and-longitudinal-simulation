#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## simulating hierarchical random effects and longitudinal response 
## this is good...hybrid approach from my nested app and longitudinal modelling app
## need to include the baseline as covariate
## need to predict, simulate and check residuals...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
#set.seed(786)

library(lme4)
library(VCA)
library(ggplot2)
library(MASS)
library(nlme)
library(tidyverse)

p1 <- function(x) {formatC(x, format="f", digits=1)}

# 3 level nested app chunk~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# design
last.visit =8
intercept =40 
top      = 5         ##number of groups at top of hierarchy
range1     = 10        ##app samples betwen these, levels e
range2     = 10        ##app sample these
replicates = c(3, last.visit) ##app sample these

x1 <- x2 <- range1 
x4 <- x3 <- range2
x5 <- replicates[1]
x6 <- replicates[2]

# random effect SDs
a     =1        # top 
b     =9        # middle
sigma =50       # residual
trtB =  8       # trt effect
interB= 2       # time x trt interaction
beta1  <- -10    # slope
tau0   <-  18   # person intercept SD
tau1   <-  1    # person slope  SD
tau01  <- -0.62 # slope intercept corr 



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

# random effects
top.r <-    rnorm(top,          intercept,        a)    
middle.r <- rnorm(sum(middle),  0,                b)     

# ids
lower.id <- rep(seq_len(sum(lower)), replicates )        
middle.id <- cut(lower.id, c(0,cumsum(lower)),  labels=FALSE)
top.id   <- cut(middle.id, c(0,cumsum(middle)), labels=FALSE)

#---------------------------------------------------------------------------
### set number of individuals, this chunk from longitudianl app

### simulate (correlated) random effects for intercepts and slopes
mu  <- c(0,0)
S   <- matrix(c(1, tau01, tau01, 1), nrow=2)
tau <- c(tau0, tau1)
S   <- diag(tau) %*% S %*% diag(tau)
U   <- mvrnorm(N, mu=mu, Sigma=S)

p <- rle(lower.id)$lengths

#--------------------------------------------------------------------------
time = ave(lower.id, lower.id, FUN = seq_along)  # time variable

Data <- data.frame( top=top.id, mid=middle.id, low=lower.id, 
                    
                    time=time,
                    
                    # random noise
                    y= rnorm( sum(replicates), 
                              top.r[top.id] + 
                                middle.r[middle.id] +
                                U[,1][lower.id] +                              # random intercepts
                                ((beta1 +  U[,2][lower.id]) *  time), sigma)   # random slopes
                    
                    
)

df <- as.data.frame(Data)
df$time <- df$time-1  # set baseline to time=0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# incorporating trt effect

trt <- sample( c(1,0),  length(unique(lower.id)), replace=TRUE)  # trt indicator
df$trt <- rep(trt, times=p)
df$interaction <- ifelse(df$trt==1,1,0)

df$yb = with(df, y+ (trt*trtB)+  ((time)*interaction*interB))
df$yb = with(df,ifelse((trt %in% 1 & time %in% 0), y, yb  ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ignoring treatment



df$time <- factor(df$time)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot not model based~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# no treatment effect plotted here, just a mean effect overall

df$VISIT <- df$time
# df$value <- df$y        
# df$variable <- "BIOCHEM.B"
df$ID <- factor(df$low)
df$VISIT=as.numeric(levels(df$VISIT))[df$VISIT]


####################DATA CREATED#############################################
####################now investigate no pooling/pooling/partial pooling#############

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot
# plot 20 subjects at random...no pooling linear fits....

library(ggplot2)


xlab <- "Visit"
ylab <- "response"

sel <- sample(1:max(df$low), 20, replace =F)  # select small  number of samples

df1 <- df[ df$low %in% sel,]

ggplot(df1) + 
  aes(x = VISIT, y = y) + 
  stat_smooth(method = "lm", se = FALSE) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("ID") +
  labs(x = xlab, y = ylab) + 
  # We also need to help the x-axis, so it doesn't 
  # create gridlines/ticks  
  scale_x_continuous(breaks = 0:last.visit)

 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ols fits by id

df_no_pooling <- lmList( y ~ (VISIT) | ID, data=df) %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("ID") %>% 
  rename(Intercept = `(Intercept)`, time =  `VISIT` ) %>% 
  add_column(Model = "No pooling")  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pooled, lm ignoring ids
# Fit a model on all the data pooled together, so ignoring ID
m_pooled <- lm(y ~ VISIT  , data=df) 

# Repeat the intercept and slope terms for each participant
df_pooled <- data_frame(
  Model = "Complete pooling",
  ID = unique(df$ID),
  Intercept = coef(m_pooled)[1], 
  time = coef(m_pooled)[2])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~join pool no pool
df1 <- df[,c("ID", "VISIT","y")]

# Join the raw data so we can use plot the points and the lines.
df_models <- bind_rows(df_pooled, df_no_pooling) %>% 
  left_join(df1, by = "ID")
head(df_models)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot both
df1 <- df_models[ df_models$ID %in% sel,]
df1$VISIT <- as.numeric(as.character(df1$VISIT))

p_model_comparison <- ggplot(df1) + 
  aes(x = VISIT, y = y) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = time, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap("ID") +
  labs(x = xlab, y = ylab) + 
  scale_x_continuous(breaks = 0:last.visit) + 
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top")

p_model_comparison


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
m <- (lmer( y ~  1  + VISIT + (1+  (VISIT)|ID), data=df))
m <- (lmer( y ~  VISIT + (  (VISIT)|ID), data=df))

arm::display(m)


df_partial_pooling <- coef(m)[["ID"]] %>% 
  rownames_to_column("ID") %>% 
  as_tibble() %>% 
  rename(Intercept = `(Intercept)`, time =  `VISIT`) %>% 
  add_column(Model = "Partial pooling")

df_partial_pooling$VISIT <- NULL
# Join the raw data so we can use plot the points and the lines.
df1 <- df[,c("ID", "VISIT","y")]
df_models2 <- (df_partial_pooling) %>% 
  left_join(df1, by = "ID")
head(df_models2)

d <- rbind(df_models2, df_models)
d <- d[ d$ID %in% sel,]

d$VISIT <- as.numeric(as.character(d$VISIT))

p_model_comparison <- ggplot(d) + 
  aes(x = VISIT, y = y) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = time, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap("ID") +
  labs(x = xlab, y = ylab) + 
  scale_x_continuous(breaks = 0:last.visit) + 
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top")

p_model_comparison

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~








library(readxl)
library(ggplot2)
library(MASS)
library(robustbase)

data <-  read_excel("Dataset CBH_FINAL.xlsx"  )

##question 1 - in different countries are models significantly 
##different??

## https://stats.oarc.ucla.edu/r/dae/robust-regression/

data$Species<-as.factor(data$Species)
data$Country<-as.factor(data$Country)
outliers <- which(data$cbh > data$h)
data <- data[-outliers,]
##intercept must 
lm.pars <- list()

lm.pars.params <- data.frame(
  Species=character(), 
  N = numeric(),
  
  Slope.lm = numeric(),
  Intercept.lm = numeric() , 
  Rmse.lm = numeric(),
  
  Slope.rlm = numeric(),
  Intercept.rlm = numeric() , 
  Rmse.rlm = numeric(),
  
  Slope.rlm2 = numeric(),
  Intercept.rlm2 = numeric() , 
  Rmse.rlm2 = numeric()
)

lm.pars.params.lm <- data.frame(
  Species=character(),
  Slope = numeric(),
  Intercept = numeric() ,
  Rmse = numeric(), 
  N = numeric()
)


lm.parsT <- data.frame(
  Species=character(),
  Slope.lm = numeric(),
  Intercept.lm = numeric() ,
  Slope.rlm = numeric(),
  Intercept.rlm = numeric() ,
  Slope.rlm2 = numeric(),
  Intercept.rlm2 = numeric() ,
  cbh = numeric(),
  h = numeric()  
)

# pdf("plotsCBH.pdf", height=4, width=6)
for(i in levels(data$Species) ){
  message(i)
  dt <- data[data$Species==i,]  
  # rlm.res <- MASS::rlm(cbh ~ h, data=data )  
  lm.res <-  lm(cbh ~ h, data=dt )
  roblm.res <- robustbase::ltsReg(cbh ~ h, data=dt )
  roblm.res2 <- robustbase::ltsReg(cbh ~ h - 1, data=dt )
  
   
  lm.pars.params[nrow(lm.pars.params) + 1,] <- list(species= i, 
                                                    n = length(roblm.res$raw.resid),
                                                    
                                                    slope.lm = lm.res$coefficients[[2]],
                                                    intercept.lm = lm.res$coefficients[[1]] , 
                                                    rmse.lm = sqrt(sum(lm.res$residuals^2)/length(lm.res$residuals) ),
                                                    
                                                    slope.rlm = roblm.res$coefficients[[2]],
                                                    intercept.rlm = roblm.res$coefficients[[1]] , 
                                                    rmse.rlm = sqrt(sum(roblm.res$raw.resid^2)/length(roblm.res$raw.resid) ),
                                                    
                                                    slope.rlm2 = roblm.res2$coefficients[[1]],
                                                    intercept.rlm2 = 0, 
                                                    rmse.rlm2 = sqrt(sum(roblm.res2$raw.resid^2)/length(roblm.res2$raw.resid) )
                                                    
                                                      )
 

  
  lm.parsT  <- rbind(lm.parsT, data.frame(Species= i,
                                        Slope.lm = lm.res$coefficients[[2]],
                                        Intercept.lm = lm.res$coefficients[[1]] ,
                                        Slope.rlm = roblm.res$coefficients[[2]],
                                        Intercept.rlm = roblm.res$coefficients[[1]] ,
                                        Slope.rlm2 = roblm.res2$coefficients[[1]],
                                        Intercept.rlm2 = 0 ,
                                        cbh=dt$cbh,
                                        h=dt$h )  )
 
  
  lm.pars[[i]] <- list( 
                       rlm.coeffs.intercept = roblm.res$coefficients[[1]],
                       rlm.coeffs.slope = roblm.res$coefficients[[2]],
                       lm.coeffs.intercept = lm.res$coefficients[[1]],
                       lm.coeffs.slope = lm.res$coefficients[[2]],
                       cbh=lm.res$model$cbh,
                       h=lm.res$model$h
                       )
  
  
}

lm.parsT$Species<-as.factor(lm.parsT$Species)

pdf("plotsCBH.pdf", height=8, width=7.7)

  p <-ggplot(lm.parsT, aes(x = h, y = cbh)) +
    geom_hex( alpha=0.8, show.legend = FALSE) +  
    facet_wrap( vars(Species))+
    # scale_fill_gradient2(limits = c(0, 300), oob = scales::squish) +
    
    scale_fill_viridis_c(limits = c(0, 300), oob = scales::squish) + 
    ggtitle("Least Trimmed Squares Robust Regression") +
    ylab("Canopy Base Height (m)") +
    # annotate("text", label =  , 
    #          size = 4, 
    #          x = 1, y = 22) +
    geom_abline( 
                aes(slope=1 , 
                    intercept = 0) ) + 
    geom_abline(
                aes(slope=Slope.lm , color="Standard LM", 
                    intercept = Intercept.lm) ) + 
    geom_abline( 
                aes(slope=Slope.rlm , color="Robust LM",
                    intercept = Intercept.rlm) ) + 
    geom_abline( 
                aes(color="Robust LM i0", slope=Slope.rlm2 , 
                    intercept = Intercept.rlm2) ) + 
    xlab("Height (m)") + theme_bw()
  print(p)

# coeffs <- data.frame()
dev.off() 

writexl::write_xlsx(list(data=lm.pars.params), "output.xlsx")

library(readxl)
library(ggplot2)

data <-  read_excel("foliageData.xlsx", range = "Fraction!A2:I18" )

  names(data) <- c("Species", 
                   "Stem.B0" , "Stem.B1" ,
                   "Branch.B0", "Branch.B1", 
                   "Foliage.B0", "Foliage.B1", 
                   "Tot.B0", "Tot.B1")
 
  # dataf <- reshape2::melt(data)  
  # dataf$Component <-  stringr::str_split_fixed(as.character(dataf$variable), stringr::coll("."), 2)[,1] 
  # dataf$par <-  stringr::str_split_fixed(as.character(dataf$variable), stringr::coll("."), 2)[,2] 
  # dataf$variable <- NULL
  logfun <- function(b, dbh){
     log10(exp(b[[1]]+b[[2]]*log(dbh)))
  }
  vfun <- function(r){
    sp <- r[[1]]
    pars <- as.list(as.numeric(r[2:9]))
    vals <- names(r[2:9])
    names(pars)<- vals
    out <- list()
    for(i in c("Stem", "Foliage", "Branch")){
      ww <- grep(i, names(pars))
      out[[i]] <- logfun( pars[ww], 10:150 )
    }  
    return(out)
  }
  
  fout <- apply(data, MARGIN = 1, vfun)
  names(fout)<- data$Species
  fout.df <- reshape2::melt(fout) 
  names(fout.df)<-c("Biomass", "Component", "Species")
  fout.df$DBH <- 10:150
  fout.df$Component<-factor(fout.df$Component,
                            levels = c("Foliage", "Branch", "Stem"))
  pdf("plotsFoliageFraction.pdf", height=8, width=8)
     
      p <-ggplot(fout.df, aes(x = DBH, y = Biomass, 
                                    fill = Component)) +
          facet_wrap( vars(Species)) +
        geom_area( alpha=0.5) +  
         
        ylab("Biomass (log10 Mg)") +
        xlab("Diameter DBH (cm)") + theme_bw()
      print(p)
 
      
      p <-ggplot(fout.df, aes(x = DBH, y = 10^(Biomass)/1000, 
                             fill = Component)) +
        geom_area( alpha=0.5) + 
        facet_wrap( vars(Species)) +
        ggtitle("Biomass fractions") +
        scale_y_continuous(breaks = seq(0, 25, by = 1)) +
        ylab("Biomass (Mg)") +
        xlab("Diameter DBH (cm)") +
        theme(panel.grid.minor.y = ) + theme_bw()
      print(p)
      
 
  dev.off() 
 
library(tidyverse)
library(ggplot2)
library(vcd)
library(gridExtra)
library(kknn)

graficas_una_variable <- function(df, todas, vars){
  params <- list(data = df)
  if(todas == TRUE) {
    vars <- do.call("names", list(x=params$data))
  }

  for(ind in 1:length(vars)) {
    cva <- is.numeric(params$data[vars[ind]][1,])
    va <- vars[ind]
    if(cva==TRUE){
      bins <- floor((min(params$data[va] %>% unlist()) - min(params$data[va] %>% unlist()))/10)
      bins1 <- ifelse(bins==0,1,bins)
      do.call("hist", list(x=params$data[va] %>% unlist(), breaks=bins1,col = 'darkgray', border = 'white', 
           main="Histograma de una variable continua",xlab=va))
    }
    else{
      do.call("boxplot", list(x=c(params$data[va] %>% unlist()), names=va, pch=16, col="royalblue2", xlab=va))
    }
  }
}


graficas_par_variables <- function(df, todas, vars) {
  params <- list(data = df)
  if(todas == TRUE) {
    vars <- do.call("names", list(x=params$data))
  }
  pars <- combn(vars,2)

  for(ind in 1:length(pars[1,])) {
    cva1 <- is.numeric(params$data[pars[1,ind]][1,])
    cva2 <- is.numeric(params$data[pars[2,ind]][1,])
    va1 <- pars[1,ind]
    va2 <- pars[2,ind]
    if(cva1==TRUE && cva2==TRUE){
      y_lab <- do.call("ylab", list(label=va2)) 
      x_lab <- do.call("xlab", list(label=va1)) 
      aess <- do.call("aes_string", list(x=va1,y=va2))
      point <- do.call("geom_point", list())
      densi <- do.call("geom_density2d", list())
      smoo <- do.call("geom_smooth", list(method="lm", colour="red"))
      titl <- do.call("ggtitle", list(label='scatter con ajuste de curvas'))
      
      li <- list(x_lab, y_lab, aess,point, densi,smoo, titl)
      print(do.call("ggplot", params) + li)
    }
    else if(cva1==FALSE && cva2==TRUE) {
      y_lab <- do.call("ylab", list(label=va2)) 
      x_lab <- do.call("xlab", list(label=va1)) 
      aess <- do.call("aes_string", list(x=va1,y=va2))
      boxplo <- do.call("geom_boxplot", list(varwidth=TRUE))
      titl <- do.call("ggtitle", list(label='boxplot categórica vs continua'))
      them <- do.call("theme", list(axis.text.x=element_text(angle=-80,hjust=1)))

      li <- list(x_lab, y_lab, aess, boxplo, titl, them)
      print(do.call("ggplot", params) + li)
    }
    else if(cva2==FALSE && cva1==TRUE) {
      y_lab <- do.call("ylab", list(label=va1)) 
      x_lab <- do.call("xlab", list(label=va2)) 
      aess <- do.call("aes_string", list(x=va2,y=va1))
      boxplo <- do.call("geom_boxplot", list(varwidth=TRUE))
      titl <- do.call("ggtitle", list(label='boxplot categórica vs. continua'))
      them <- do.call("theme", list(axis.text.x=element_text(angle=-80,hjust=1)))
      
      li <- list(x_lab, y_lab, aess, boxplo, titl, them)
      print(do.call("ggplot", params) + li)
    }
    else {
      do.call("doubledecker", c(as.formula(paste(va2,va1, sep="~")), list(data=params$data, gp=gpar(fill=c("grey90", "red")))))
    }
  }
}


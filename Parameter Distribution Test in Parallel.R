
parameter_distribution_test = function(muestra3,precision) {
  
  library("goftest")
  
  df_todos = data.frame()
  n = length(muestra3)
  
  ### Normal Distribution -------------------------
  
  job::job({
  
  try({
    
    df_norm = data.frame()
    
    for (i in 1:100) {
      
      try({
        
        muestra99 = sample(muestra3, n, replace = TRUE)
        
        param<-c()
        
        LL <- function(param) {
          -sum(dnorm(muestra99, param[1], param[2], log=TRUE)) 
        }
        
        ML <- optim(c(mean(muestra99),sd(muestra99)),LL,method="L-BFGS-B")$par
        
        df_norm = rbind(df_norm, c(ML[1], ML[2]))
        
      }, silent = TRUE)
    }
    
    colnames(df_norm) = c("Mean", "Sd")
    
    
    mean_norm_seq = seq(min(df_norm$Mean) + min(df_norm$Mean) - max(df_norm$Mean), max(df_norm$Mean) + max(df_norm$Mean) - min(df_norm$Mean), precision)
    sd_norm_seq = seq(min(df_norm$Sd) + min(df_norm$Sd) - max(df_norm$Sd), max(df_norm$Sd) + max(df_norm$Sd) - min(df_norm$Sd),precision)
    
    
    df_norm = data.frame()
    
    for (i in mean_norm_seq) {
      
      for (z in sd_norm_seq) {
        
        df_norm = rbind(df_norm, c(i,z, ks.test(muestra3,"pnorm", i ,z)$statistic, "Normal"))
        
      }
    }
    
    
    colnames(df_norm) = c("Param_1", "Param_2","KS", "Distribution")
    
    
  }, silent = TRUE)
    
    df_todos1 = rbind(df_todos, c(df_norm[which(df_norm$KS == min(df_norm$KS)),]))
    
    return(df_todos1)

  })
  
  ### GAMMA-----------------------------------------------
  
  job::job({
  
  
  try({
    
    df_gamma = data.frame()
    
    for (i in 1:100) {
      
      try({
        
        muestra99 = sample(muestra3, n, replace = TRUE)
        
        param<-c()
        
        LL <- function(param) {
          -sum(dgamma(muestra99, param[1], param[2], log=TRUE)) 
        }
        
        ML <- optim(c(mean(muestra99),sd(muestra99)),LL,method="L-BFGS-B")$par
        
        df_gamma = rbind(df_gamma, c(ML[1], ML[2]))
        
      }, silent = TRUE)
      
    }
    
    colnames(df_gamma) = c("Shape", "Scale")
    
    shape_g_seq = seq(min(df_gamma$Shape) + min(df_gamma$Shape) - max(df_gamma$Shape),max(df_gamma$Shape) + max(df_gamma$Shape) - min(df_gamma$Shape),precision)
    scale_g_seq = seq(min(df_gamma$Scale) + min(df_gamma$Scale) - max(df_gamma$Scale),max(df_gamma$Scale) + max(df_gamma$Scale) - min(df_gamma$Scale),precision)
    
    
    df_gamma = data.frame()
    
    for (i in shape_g_seq) {
      
      for (z in scale_g_seq) {
        
        df_gamma = rbind(df_gamma, c(i,z, ks.test(muestra3,"pgamma", i ,z)$statistic, "Gamma"))
        
      }
    }
    
    colnames(df_gamma) = c("Param_1", "Param_2","KS","Distribution")
    
  }, silent = TRUE)
    
    df_todos2 = rbind(df_todos, c(df_gamma[which(df_gamma$KS == min(df_gamma$KS)),]))
    
    return(df_todos2)
  })
    
  ### WEIBULL --------------------------------------------
  
  job::job({
  
  try({
    
    df_weibull = data.frame()
    
    for (i in 1:100) {
      
      try({
        
        muestra99 = sample(muestra3, n, replace = TRUE)
        
        param<-c()
        
        LL <- function(param) {
          -sum(dweibull(muestra99, param[1], param[2], log=TRUE)) 
        }
        
        ML <- optim(c(mean(muestra99),sd(muestra99)),LL,method="L-BFGS-B")$par
        
        
        df_weibull = rbind(df_weibull, c(ML[1], ML[2]))
        
      }, silent = TRUE)
      
    }
    
    colnames(df_weibull) = c("Shape", "Scale")
    
    shape_w_seq = seq(min(df_weibull$Shape) + min(df_weibull$Shape) - max(df_weibull$Shape) ,max(df_weibull$Shape) + max(df_weibull$Shape) - min(df_weibull$Shape),precision)
    scale_w_seq = seq(min(df_weibull$Scale) + min(df_weibull$Scale) - max(df_weibull$Scale),max(df_weibull$Scale) + max(df_weibull$Scale) - min(df_weibull$Scale),precision)
    
    df_weibull = data.frame()
    
    for (i in shape_w_seq) {
      
      for (z in scale_w_seq) {
        
        df_weibull = rbind(df_weibull, c(i,z, ks.test(muestra3,"pweibull", i ,z)$statistic,"Weibull"))
        
      }
    }
    
    
    colnames(df_weibull) = c("Param_1", "Param_2","KS","Distribution")
    
  }, silent = TRUE)
    
    df_todos3 = rbind(df_todos, c(df_weibull[which(df_weibull$KS == min(df_weibull$KS)),]))
    
    return(df_todos3)
    
  })
  ##### T - student--------------------------------------
  
  job::job({
  
  try({
    df_student = data.frame()
    
    for (i in 1:100) {
      
      try({
        
        muestra99 = sample(muestra3, n, replace = TRUE)
        
        param<-c()
        
        LL <- function(param) {
          -sum(dt(muestra99,n - 1, param, log=TRUE)) 
        }
        
        ML <- optim(5,LL,method="L-BFGS-B", lower = 0)$par 
        
        
        df_student = rbind(df_student, ML[1])
        
      }, silent = TRUE)
      
    }
    
    colnames(df_student) = c("NCP")
    
    ncp_seq = seq(min(df_student$NCP) + min(df_student$NCP) - max(df_student$NCP),max(df_student$NCP) + max(df_student$NCP) - min(df_student$NCP),precision)
    
    
    df_student = data.frame()
    
    for (i in ncp_seq) {
      
      df_student = rbind(df_student, c(i, ks.test(muestra3,"pt",i)$statistic))
      
    }
    
    colnames(df_student) = c("NCP","KS")
    
    Param_1 = df_student[which(df_student$KS == min(df_student$KS)),][,1]
    Param_2 = 0
    KS = df_student[which(df_student$KS == min(df_student$KS)),][,2]
    Distribution = "T-student"
    
    df_student = data.frame(Param_1,Param_2,KS, Distribution)

    
  }, silent = TRUE)
    
    df_todos4 = data.frame(Param_1,Param_2,KS,Distribution)
    
    return(df_todos4)
    
  })
  
  
  #Log normal-------------------------------------
  
  job::job({
  
  try({
    
    df_lognorm = data.frame()
    
    for (i in 1:100) {
      
      try({
        
        muestra99 = sample(muestra3, n, replace = TRUE)
        
        param<-c()
        
        LL <- function(param) {
          -sum(dlnorm(muestra99, param[1], param[2], log=TRUE)) 
        }
        
        ML <- optim(c(mean(muestra99), sd(log(muestra99))),LL,method="L-BFGS-B")$par
        
        
        df_lognorm = rbind(df_lognorm, c(ML[1], ML[2]))
        
      }, silent = TRUE)
    }
    
    
    
    colnames(df_lognorm) = c("Meanlog", "Sdlog")
    
    
    mean_lnorm_seq = seq(min(df_lognorm$Meanlog) + min(df_lognorm$Meanlog) - max(df_lognorm$Meanlog),max(df_lognorm$Meanlog) + max(df_lognorm$Meanlog) - min(df_lognorm$Meanlog),precision)
    sd_lnorm_seq = seq(min(df_lognorm$Sdlog) + min(df_lognorm$Sdlog) - max(df_lognorm$Sdlog),max(df_lognorm$Sdlog) + max(df_lognorm$Sdlog) - min(df_lognorm$Sdlog),precision)
    
    
    df_lognorm = data.frame()
    
    for (i in mean_lnorm_seq) {
      
      for (z in sd_lnorm_seq) {
        
        df_lognorm = rbind(df_lognorm, c(i,z, ks.test(muestra3,"plnorm", i ,z)$statistic, "Log-Normal"))
        
      }
    }
    
    colnames(df_lognorm) = c("Param_1", "Param_2","KS","Distribution")
    
  }, silent = TRUE)
    
    df_todos5 = rbind(df_todos, c(df_lognorm[which(df_lognorm$KS == min(df_lognorm$KS)),]))
    
    return(df_todos5)
    
    
  })
    
  ##### CAUCHY -------------------
  
  job::job({
  
  try({
    
    df_cauchy = data.frame()
    
    
    for (i in 1:100) {
      try({
        
        muestra99 = sample(muestra3, n, replace = TRUE)
        
        param<-c()
        
        LL <- function(param) {
          -sum(dcauchy(muestra99, param[1], param[2], log=TRUE)) 
        }
        
        ML <- optim(c(mean(muestra99),sd(muestra99)),LL,method="L-BFGS-B")$par
        
        
        df_cauchy = rbind(df_cauchy, c(ML[1], ML[2]))
        
      }, silent = TRUE)
      
    }
    
    colnames(df_cauchy) = c("Location", "Scale")
    
    
    location_cauchy_seq = seq(min(df_cauchy$Location) + min(df_cauchy$Location) - max(df_cauchy$Location),max(df_cauchy$Location) + max(df_cauchy$Location) - min(df_cauchy$Location),precision)
    scale_cauchy_seq = seq(min(df_cauchy$Scale) + min(df_cauchy$Scale) - max(df_cauchy$Scale),max(df_cauchy$Scale) + max(df_cauchy$Scale) - min(df_cauchy$Scale),precision)
    
    
    df_cauchy = data.frame()
    
    for (i in location_cauchy_seq) {
      
      for (z in scale_cauchy_seq) {
        
        df_cauchy = rbind(df_cauchy, c(i,z, ks.test(muestra3,"pcauchy", i ,z)$statistic,"Cauchy"))
        
      }
    }
    
    colnames(df_cauchy) = c("Param_1", "Param_2","KS","Distribution")
    
  }, silent = TRUE)
    
    df_todos6 = rbind(df_todos, c(df_cauchy[which(df_cauchy$KS == min(df_cauchy$KS)),]))
    
    return(df_todos6)
    
  })
    
  ### LOGISTA --------------
  
  job::job({
  
  try({
    df_logis = data.frame()
    
    
    for (i in 1:100) {
      
      try({
        
        muestra99 = sample(muestra3, n, replace = TRUE)
        
        param<-c()
        
        LL <- function(param) {
          -sum(dlogis(muestra99, param, 1, log=TRUE)) 
        }
        
        ML <- optim(mean(muestra99),LL,method="L-BFGS-B")$par
        
        df_logis = rbind(df_logis, ML[1])
        
      }, silent = TRUE)
    }
    
    
    colnames(df_logis) = c("Location")
    
    location_logis_seq = c(min(df_logis$Location) + min(df_logis$Location) -  max(df_logis$Location),max(df_logis$Location) + max(df_logis$Location) -  min(df_logis$Location), precision)
    scale_logis_seq = seq(0.01,8,0.01)
    
    df_logis = data.frame()
    
    for (i in location_logis_seq) {
      
      for (z in scale_logis_seq) {
        
        df_logis = rbind(df_logis, c(i,z,ks.test(muestra3,"plogis", i ,1)$statistic,"Logistic"))
        
      }
      
    }
    
    colnames(df_logis) = c("Param_1", "Param_2","KS","Distribution")
  
    
  }, silent = TRUE)
  
  
    df_todos7 = rbind(df_todos, c(df_logis[which(df_logis$KS == min(df_logis$KS)),][1,]))
  
    return(df_todos7)
      
  
  })
  
  
}


parameter_distribution_test(muestra3, 0.01)


try({
  
  df_total999 = data.frame()
  
  try({
    df_total999 = rbind(df_total999,df_todos1) #Normal
  }, silent = TRUE)
  
  try({
    df_total999 = rbind(df_total999,df_todos2) #Gamma
  }, silent = TRUE)
  
  try({
    df_total999 = rbind(df_total999,df_todos3) #Weibull
  }, silent = TRUE)
  
  try({
    df_total999 = rbind(df_total999,df_todos4) #T-student
  }, silent = TRUE)
  
  try({
    df_total999 = rbind(df_total999,df_todos5) #Log-Norm
  }, silent = TRUE)
  
  try({
    df_total999 = rbind(df_total999,df_todos6) #Cauchy
  }, silent = TRUE)
  
  try({
    df_total999 = rbind(df_total999,df_todos7) #Logistic
  }, silent = TRUE)
  
   parameter_distribution_test_result = df_total999
   
   parameter_distribution_test_result$Param_1 = round(as.numeric(parameter_distribution_test_result$Param_1),4)
   parameter_distribution_test_result$Param_2 = round(as.numeric(parameter_distribution_test_result$Param_2),4)
   parameter_distribution_test_result$KS = round(as.numeric(parameter_distribution_test_result$KS),4)
   
   parameter_distribution_test_result = parameter_distribution_test_result[order(parameter_distribution_test_result$KS),]
   
}, silent = TRUE) #Create the desiare table


parameter_distribution_test_result


input_path <- "../Dataset/RQ2 and RQ3/"

projects = c(
  'JSWCLOUD',
  'CONFSERVER',
  'JSWSERVER',
  'BAM',
  'CLOV',
  'MESOS',
  'USERGRID',
  'TIDOC',
  'APSTUD',
  'TISTUD',
  'TIMOB',
  'DAEMON',
  'DNN',
  'BE',
  'FAB',
  'INDY',
  'STL',
  'IS',
  'DM',
  'DURACLOUD',
  'COMPASS',
  'CXX',
  'SERVER',
  'EVG',
  'MULE',
  'APIKIT',
  'NEXUS',
  'XD',
  'TDQ',
  'TDP',
  'TMDM',
  'TESB'
)

do_correlation <- function(data){

  Story_Point <- as.numeric(as.character(data$Story_Point))    
  if(length(Story_Point) >= 100){    
    
    In_Progress_Minutes <- as.numeric(as.character(data$In_Progress_Minutes))
    
    # Perform correlation analysis
    s <- cor.test(Story_Point, In_Progress_Minutes, method =  "spearman", alternative = "two.sided", exact = F)
    k <- cor.test(Story_Point, In_Progress_Minutes, method =  "kendall", alternative = "two.sided", exact = F)
    p <- cor.test(Story_Point, In_Progress_Minutes, method =  "pearson", alternative = "two.sided")
    row <- c(round(s$estimate,2), 
             round(s$p.value,3), 
             round(k$estimate,2), 
             round(k$p.value,3), 
             round(p$estimate,2),
             round(p$p.value,3)
    )
    
    Effort_Time_Minutes <- as.numeric(as.character(data$Total_Effort_Minutes))
    s <- cor.test(Story_Point, Effort_Time_Minutes, method =  "spearman", alternative = "two.sided", exact = F)
    k <- cor.test(Story_Point, Effort_Time_Minutes, method =  "kendall", alternative = "two.sided", exact = F)
    p <- cor.test(Story_Point, Effort_Time_Minutes, method =  "pearson", alternative = "two.sided")
    row <- c(row, 
             round(s$estimate,2), 
             round(s$p.value,3), 
             round(k$estimate,2), 
             round(k$p.value,3), 
             round(p$estimate,2),
             round(p$p.value,3)
    )
    
    Resolution_Time_Minutes <- as.numeric(as.character(data$Resolution_Time_Minutes))
    s <- cor.test(Story_Point, Resolution_Time_Minutes, method =  "spearman", alternative = "two.sided", exact = F)
    k <- cor.test(Story_Point, Resolution_Time_Minutes, method =  "kendall", alternative = "two.sided", exact = F)
    p <- cor.test(Story_Point, Resolution_Time_Minutes, method =  "pearson", alternative = "two.sided")
    row <- c(row, 
             round(s$estimate,2), 
             round(s$p.value,3), 
             round(k$estimate,2), 
             round(k$p.value,3), 
             round(p$estimate,2),
             round(p$p.value,3)
    )
    
    return(row)
  }else{
    return(NULL)
  }
}

rq2.1.result.table = data.frame(stringsAsFactors = FALSE)

for(project in projects){
  
  data <- read.csv(paste0(input_path, project, ".csv"), stringsAsFactors = FALSE)[, c('Issue_Key', 'Story_Point', 'In_Progress_Minutes', 'Total_Effort_Minutes', 'Resolution_Time_Minutes')]
  
  # OUTLIER_REMOVAL ##########BEGIN##############
  # Remove In_Progress_Minutes less than 2
  data = data[data$In_Progress_Minutes >= 2,]
  
  # Remove In_Progress_Minutes Interquartile Range
  Q.te <- quantile(data[,"In_Progress_Minutes"], probs=c(.25, .75), na.rm = FALSE)
  iqr.te <- IQR(data[,"In_Progress_Minutes"])
  up.te <-  Q.te[2]+1.5*iqr.te # Upper Fence
  low.te <- Q.te[1]-1.5*iqr.te # Lower Fence
  
  data <- data[(data[,"In_Progress_Minutes"] > low.te & data[,"In_Progress_Minutes"] < up.te),]
  # OUTLIER_REMOVAL ##########END###############
  
  row <- do_correlation(data)
  if(!is.null(row))
    rq2.1.result.table <- rbind(rq2.1.result.table, c(project, row))
  else
    print(paste0(project, ' ignorred for having less than 100 issues after filtering'))
}

colnames(rq2.1.result.table) <- c("Project"
                            
                            ,"InProgress Spearman rho"
                            ,"InProgress Spearman p-value"
                            ,"InProgress Kendall tau"
                            ,"InProgress Kendall p-value"
                            ,"InProgress Pearson r"
                            ,"InProgress Pearson p-value"
                            
                            ,"Effort Spearman rho"
                            ,"Effort Spearman p-value"
                            ,"Effort Kendall tau"
                            ,"Effort Kendall p-value"
                            ,"Effort Pearson r"
                            ,"Effort Pearson p-value"
                            
                            ,"Resolution Spearman rho"
                            ,"Resolution Spearman p-value"
                            ,"Resolution Kendall tau"
                            ,"Resolution Kendall p-value"
                            ,"Resolution Pearson r"
                            ,"Resolution Pearson p-value"
)

write.csv(rq2.1.result.table, paste0("results/RQ2_SP_Correlation_with_TimeProxies.csv"))




input_path <- "../Dataset/RQ2 and RQ3/"

projects = c(
  'DM',
  'MDL',
  'TDQ',
  'TMDM'
)

do_correlation <- function(data){

  Story_Point <- as.numeric(as.character(data$Story_Point))   
  if(length(Story_Point) >= 100){    
    
    Timespent_Minutes <- as.numeric(as.character(data$Timespent))
    
    # Perform correlation analysis
    s <- cor.test(Story_Point, Timespent_Minutes, method =  "spearman", alternative = "two.sided", exact = F)
    k <- cor.test(Story_Point, Timespent_Minutes, method =  "kendall", alternative = "two.sided", exact = F)
    p <- cor.test(Story_Point, Timespent_Minutes, method =  "pearson", alternative = "two.sided")
    row <- c(round(s$estimate,2), 
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

rq2.2.result.table = data.frame(stringsAsFactors = FALSE)

for(project in projects){
  print(project)
  data <- read.csv(paste0(input_path, project, ".csv"), stringsAsFactors = FALSE)[, c('Issue_Key', 'Story_Point', 'Timespent')]
  
  # Remove Issues with no Timespent and Timespent less than 2
  data = data[data$Timespent!='NULL',]
  data = data[data$Timespent >= 2,]
  
  row <- do_correlation(data)
  if(!is.null(row))
    rq2.2.result.table <- rbind(rq2.2.result.table, c(project, row))
  else
    print(paste0(project, ' ignorred for having less than 100 issues after filtering'))
}

colnames(rq2.2.result.table) <- c("Project"
                            ,"Timespent Spearman rho"
                            ,"Timespent Spearman p-value"
                            ,"Timespent Kendall tau"
                            ,"Timespent Kendall p-value"
                            ,"Timespent Pearson r"
                            ,"Timespent Pearson p-value"
                            )

write.csv(rq2.2.result.table, paste0("results/RQ2_SP_Correlation_with_Timespent.csv"))




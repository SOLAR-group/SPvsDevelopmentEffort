library(effsize)

path = "../Dataset/RQ1/"

projects = c(
  'CWD',
  'JSWCLOUD',
  'JSWSERVER',
  'JRASERVER',
  'BAM',
  'CLOV',
  'FE',
  'TIDOC',
  'DM',
  'NEXUS',
  'TDQ',
  'TDP',
  'TMDM',
  'TBD',
  'TESB'
)

wxT <- function(arrayA, arrayB){
  return(wilcox.test(arrayA, arrayB, PAIRED=F, alternative = "two.sided"))
}

residuals = list()

for(project in projects){
  
  print(paste0("working with ", project, " project... "))
  
  data <- read.csv(paste(path, "TS_", project, ".csv", sep = ""), stringsAsFactors = FALSE)

  # Remove issues with Timespent < 2
  data <- data[data$Timespent >= 2,]
   
  # Skip the project if it has less than 100 issues (this is already applied and all 15 projects listed above have more than 100 issues)
  if(dim(data)[1]<100){
    print(paste0(project, ' HAS BEEN SKIPPED SINCE AFTER FILTERATION IT HAD LESS THAN 100 ISSUES!'))
    next
  }
  
  # compute the error of Timespent w.r.t the proxies and put all in a list
  residuals[[project]] = list(
    "timespent" = data[,"Timespent"],
    "InProgress" = as.numeric(data[,"In_Progress_Minutes"]),
    "EffortTime" = as.numeric(data[,"Total_Effort_Minutes"]),
    "ResolutionTime" = as.numeric(data[,"Resolution_Time_Minutes"]),

    "InProgressError" =  abs(as.numeric(data[,"Timespent"]) - as.numeric(data[,"In_Progress_Minutes"])),
    "EffortTimeError" =  abs(as.numeric(data[,"Timespent"]) - as.numeric(data[,"Total_Effort_Minutes"])),
    "ResolutionTimeError" =  abs(as.numeric(data[,"Timespent"]) - as.numeric(data[,"Resolution_Time_Minutes"]))
  )
  
}

saveRDS(residuals, file = paste0(path, "results/Residuals.list.data"))


datasets = c()

rq1.results = data.frame(stringsAsFactors = FALSE)

for(i in 1:length(residuals)){
  
  dataset= names(residuals)[i]

  row = c(residuals[[i]]$InProgressError, residuals[[i]]$EffortTimeError, residuals[[i]]$ResolutionTimeError)
  
  row = c(row, wxT(residuals[[i]]$timespent, residuals[[i]]$InProgress)$p.value, 
          VD.A(residuals[[i]]$InProgress, residuals[[i]]$timespent)$estimate)
  
  row = c(row, wxT(residuals[[i]]$timespent, residuals[[i]]$EffortTime)$p.value,
          VD.A(residuals[[i]]$EffortTime, residuals[[i]]$timespent)$estimate)
  
  row = c(row, wxT(residuals[[i]]$timespent, residuals[[i]]$ResolutionTime)$p.value,
          VD.A(residuals[[i]]$ResolutionTime, residuals[[i]]$timespent)$estimate)

  datasets = c(datasets, dataset)
  
  rq1.results = rbind(rq1.results, row)
}

rq1.results = data.frame(datasets, rq1.results, check.names = F)

colnames(rq1.results) = c(
  "Project"

  ,"IP SAE"
  ,"ET SAE"
  ,"RT SAE"
  
  ,"TS vs IP"
  ,"eff"
  ,"TS vs TE"
  ,"eff"
  ,"TS vs RT"
  ,"eff"
)

write.csv(dds, file = paste0(path, "results/RQ1_results.csv"))

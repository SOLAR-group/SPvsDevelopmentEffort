library(dplyr)
library(ggplot2)
require(graphics)
library(REdaS)

input_path <- "../Dataset/RQ2 and RQ3/"

break.point <- 5
scaleset <- 2
bigset = c("DAEMON")
smalset = c("JSWSERVER", "TESB", "TMDM")

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

clean.data <- function (dataset){
  data = read.csv(paste(input_path, dataset, ".csv", sep = ""))
  
  # Remove In_Progress_Minutes less than 2
  data = data[data$In_Progress_Minutes >= 2,]
  
  # Remove outliers based on In_Progress_Minutes Interquartile Range
  Q.te <- quantile(data[,"In_Progress_Minutes"], probs=c(.25, .75), na.rm = FALSE)
  iqr.te <- IQR(data[,"In_Progress_Minutes"])
  up.te <-  Q.te[2]+1.5*iqr.te # Upper Range
  low.te <- Q.te[1]-1.5*iqr.te # Lower Range
  
  data <- data[(data[,"In_Progress_Minutes"] > low.te & data[,"In_Progress_Minutes"] < up.te),]
  
  data$Story_Point = as.numeric(as.character(data$Story_Point))
  
  return(data)
}

boxplotts <- function(projct){

  if(projct %in% bigset)
    scaleset = 4
  else
    if(projct %in% smalset)
      scaleset = 1

  data = clean.data(project)
  filename = paste0('results/plots/', project, "_SP_Boxplot.pdf")
    
  # find the distinct SPs and their class-median time effort in hours
  sps = data %>% distinct(distinct_values = Story_Point)
  sps = sort(sps$distinct_values, decreasing=FALSE)
  med = matrix(nrow = 0, ncol = 2)
  for(sp in sps){
    sps.data = data[data$Story_Point == sp,]
    med = rbind(med, c(sp, median(sps.data$In_Progress_Minutes)/60))
  }
  data_chunk <- data[data$Story_Point == min(sps),]
  if(dim(data_chunk)[1]>1)
    med_value <- median(data_chunk$In_Progress_Minutes)/60
  else
    stop("This project has not enough SP=1 issues in data")
  
  colnames(med) <- c("Story_Point", "Median Effort (hours)")
  med <- cbind(med, 'STD_SP' = med_value*sps)
  medx = med[med[,"Story_Point"]<= 100,]
  medx <- data.frame(medx)

  pdf(filename)

  ylim1 = boxplot.stats(data$In_Progress_Minutes/60)$stats[c(1, 5)]
  p = ggplot(data, aes(x=factor(Story_Point), y= In_Progress_Minutes/60), fill=factor(Story_Point)) +
    xlab("Story Point")+
    ylab("In Progress Time (hours)")+
    geom_boxplot(outlier.shape =  NA, outlier.colour = "transparent") +
    coord_cartesian(ylim = ylim1*scaleset)+
    ggtitle(project)+
    geom_boxplot()+
    geom_path(data = medx, aes(x=factor(Story_Point), y= STD_SP), color = "red", size = 1, linejoin='round', group=1)+
  geom_point(data = medx, aes(x=factor(Story_Point), y= STD_SP), color = "blue", size = 4, shape=18)
  print(p)
  dev.off()
}
  
lineplots <- function(project){
  
  data = clean.data(project)
  filename = paste0('results/plots/', project, "_SP_LR_plot.pdf")
  pdf(filename)
  
  # find the distinct SPs and their class-median time effort in hours
  sps = data %>% distinct(distinct_values = Story_Point)
  sps = sort(sps$distinct_values, decreasing=FALSE)
  med = matrix(nrow = 0, ncol = 2)
  for(sp in sps){
    sps.data = data[data$Story_Point == sp,]
    med = rbind(med, c(sp, median(sps.data$In_Progress_Minutes)/1440))
  }
  
  data_chunk <- data[data$Story_Point == min(sps),]
  if(dim(data_chunk)[1]>1)
    med_value <- median(data_chunk$In_Progress_Minutes)/1440
  else
    stop("This project has not enough SP=1 issues in data")
  
  colnames(med) <- c("Story_Point", "Median Effort (days)")
  med <- cbind(med, 'STD_SP' = med_value*sps)
  medx = med[med[,"Story_Point"]<= 100,]
  medx <- data.frame(medx)
  
  plot(medx[,2], pch = 16, xlab="Story Point", ylab="Median Effort (days)", main=project, las = 2, xaxt = "n")
  x100 = lm(medx[,2] ~ medx[,1])
  abline(x100, col = "red", lty = 1)

  medx = med[med[,"Story_Point"]<= break.point,]
  x5 = lm(medx[,2] ~ medx[,1])
  abline(x5, col = "blue", lty = 2)
  
  legend("topleft", legend=c("SP <= 100", paste("SP <=",break.point)),
         col=c("red", "blue"), lty=1:2, cex=0.8)
  axis(1, at= c(1:length(sps)) , labels = factor(sps), las = 2)
  dev.off()
  
  return(c(
    round(rad2deg(atan(  x5$coefficients[2])), 2),
    round(rad2deg(atan(x100$coefficients[2])), 2),
    abs(round(rad2deg(atan(  x5$coefficients[2])), 2)-round(rad2deg(atan(x100$coefficients[2])), 2))
  ))
}

angle_diff <- matrix(NA, nrow = 0, ncol = 3)
colnames(angle_diff) <-c("Slope(<=5)","Slope(<=100)", "Diff")
for(project in projects){
  boxplotts(project)
  angle_diff <- rbind(angle_diff, lineplots(project))
}
angle_diff <- cbind(projects, angle_diff)
write.csv(x = angle_diff, file = paste0("results/LineAngleDiff.csv"))
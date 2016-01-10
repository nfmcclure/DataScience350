require(logging)
require(data.table)
logFile="JitteredHeadCountExplorer-marmo.log"
workingDirectory="c:/datasci350"
bInteractive=interactive()
nonNumericColRange=1:2
numericColRange=3:9
workWeekRange=1:4
leisureWeekRange=5:7


initializeProgram=function(){
  tryCatch(
    setwd(workingDirectory),
    error=function(e){
      logerror("initializeProgram: Working directory does not exist.",logger="myLogger")
      print("initializeProgram: Working directory does not exist.")
      stop(e)
    }
  )
  
  addHandler(writeToFile,logger = "myLogger",file=logFile)
  if (bInteractive){
    loginfo("initializeProgram (interactive)",logger="myLogger")
    print("Interactive session!")
    print("This is interactive too...")}
  else{
    loginfo("initializeProgram (batch)",logger='myLogger')    
    print("non-interactive")}
  print("This always runs.")  
}

loadCSVAsDataTable=function(fileName){
  loginfo("loadCSVAsDataTable",logger="myLogger")
  df=tryCatch(
    read.csv(fileName,stringsAsFactors=FALSE),
    error=function(e){
      logerror("loadCSVAsDataTable: Could not read CSV file.",logger="myLogger")
      print("loadCSVAsDataTable: Could not read CSV file.")
      stop(e)
    }
  )
  loginfo(cat("Loaded ",nrow(df),"observations."))
  result=as.data.table(df)
  result
}
missingValueExploration=function(dt){
  loginfo("missingValueExploration",logger="myLogger")
  print("Missing values---------------")
  apply(sapply(dt[,,with=FALSE],FUN=is.na),2,FUN=sum)
}
uniqueValueExploration=function(dt,nonNumericCols){
  loginfo("uniqueValueExploration",logger="myLogger")
  print("Unique values---------------")
  lapply(sapply(dt[,nonNumericCols,with=FALSE],FUN=unique),FUN=length)
}
numericalExploration=function(dt,numericCols){
  loginfo("numericalExploration",logger="myLogger")
  print("Summary---------------")
  print(summary(dt))
  Sys.sleep(0.5)
  print("Standard deviation---------------")
  print(apply(dt[,numericCols,with=FALSE],2,sd))
  Sys.sleep(0.5)
  print("Head---------------")
  print(head(dt))
  Sys.sleep(0.5)
  print("Tail---------------")
  print(tail(dt))
  Sys.sleep(0.5)
  
}
plotHistograms=function(dt,numericCols){
  loginfo("plotHistograms",logger="myLogger")
  for(j in numericCols){
    hist(dt[[j]],main=paste("Histogram of",names(dt)[j]),xlab=names(dt)[j])
  }
}
saveHistograms=function(dt,numericCols)
{
  loginfo("saveHistograms",logger="myLogger")
  tryCatch(
    pdf("JitteredHeadCount Histograms.pdf"),
    error=function(e){
      logerror("saveHistograms: Fatal error writting JitteredHeadCount Histograms.pdf",logger="myLogger")
      print("pdf() failed while writting file. Verify it is not blocked.")
      stop(e)
    }
  )
  plotHistograms(dt,numericCols)
  dev.off()
}
histogramExploration=function(dt,numericCols){
  loginfo("exploreHistograms",logger="myLogger")
  if(bInteractive){
    plotHistograms(dt,numericCols)
  }
  saveHistograms(dt,numericColRange)
}

boxplotExploration=function(dt){
  loginfo("boxplotExploration",logger="myLogger")
  tryCatch(
    pdf("JitteredHeadCount Boxplots.pdf"),
    error=function(e){
      logerror("boxplotExploration: Fatal error writting JitteredHeadCount Boxplots.pdf",logger="myLogger")
      print("pdf() failed while writting file. Verify it is not blocked.")
      stop(e)
    }
  )
  
  #Boxplot for Hour columns
  boxplot(dt$Hour,main="Hour Boxplot",xlab="Hour",ylab="Frequency")
  #Boxplot for Table* columns
  boxplot(dt[,4:6,with=FALSE],names(dt)[4:6],main="Tables* Comparative Boxplot", ylab="Frequency")
  #Boxplot for HeadCount columns
  boxplot(dt$HeadCount,main="HeadCount Boxplot",xlab="HeadCount",ylab="Frequency")
  #Boxplot for Hour columns
  boxplot(dt$Hour,main="Hour Boxplot",xlab="Hour",ylab="Frequency")    
  dev.off()
}
removeOutliers=function(dt,columnsWithOutliers){
  for(col in columnsWithOutliers){
    colMean=mean(dt[[col]])
    colSD=sd(dt[[col]])
    lowerBound=colMean-2*colSD
    upperBound=colMean+2*colSD
    
  }
}
enrichJitteredHeadCountDataset=function(dt){
  loginfo("enrichJitteredHeadCountDataset",logger="myLogger")
  #Convert the GameCode column to factor
  dt$GameCode=as.factor(dt$GameCode)
  #Convert the DateFormat column to datetime
  dt$DateFormat = as.Date(dt$DateFormat, format="%m/%d/%Y")
  #Add a column with the month number for DateFormat 
  dt[,MonthNumber:=month(DateFormat)]
  #Add a column with a numeric code for GameCode
  tmp=1:length(unique(dt$GameCode))
  names(tmp)=unique(dt$GameCode)
  dt[,GameCodeNumber:=tmp[GameCode]]
  #Add a column with the total number of tables=open+closed
  dt[,TotalTables:=TablesOpen+TablesClosed]
}
summarizeEnrichcedDataset=function(dt){
  loginfo("summarizeEnrichcedDataset",logger="myLogger")
  print("Enriched GameCode summary---------------")
  print(summary(dt$GameCode))
  
  print("Enriched GameCode table---------------")
  print(table(dt$GameCode))
  print("Enrhiched DateFormat summary---------------")
  print(summary(dt$DateFormat))
  print("Enrhiched GameCodeNumber summary---------------")
  print(summary(dt$GameCodeNumber))
  print("Enrhiched TotalTable summary---------------")
  print(summary(dt$TotalTables))
}
plotEnrichedDataset=function(dt){
  loginfo("plotEnrichedDataset",logger="myLogger")
  tryCatch(
    pdf("JitteredHeadCount Enriched Dataset.pdf"),
    error=function(e){
      logerror("plotEnrichedDataset: Fatal error writting Enriched Dataset.pdf",logger="myLogger")
      print("pdf() failed while writting file. Verify it is not blocked.")
      stop(e)
    }
  )
  
  #Barplot of GameCode
  barplot(table(dt$GameCode),main="Barplot of GameCode",xlab="GameCode",ylab="Frequency")
  #histogram of MonthNumber
  hist(dt$MonthNumber,main="Histogram of MonthNumber",xlab="MonthNumber",ylab="Frequency")
  #histogram of MonthNumber
  hist(dt$GameCodeNumber,main="Histogram of GameCodeNumber",xlab="GameCodeNumber",ylab="Frequency")
  #Histogram of TotalTables
  hist(dt$TotalTables,main="Histogram of TotalTables",xlab="TotalTables",ylab="Frequency")
  #Boxplot of GameCodeNumber
  boxplot(dt$GameCodeNumber, main="Boxplot of GameCodeNumber",xlab="GameCodeNumber",ylab="Frequency")
  dev.off()

}
isWorkingDay=function(dayNumber){
  dayNumber >=1&&dayNumber<=4
}
summarizeFindings=function(dt){
  loginfo("summarizeFindings",logger="myLogger")
  tryCatch(
    pdf("JitteredHeadCount Finding Summary.pdf"),
    error=function(e){
      logerror("summarizeFindings: Fatal error writting JitteredHeadCount Finding Summary.pdf",logger="myLogger")
      print("pdf() failed while writting file. Verify it is not blocked.")
      stop(e)
    }
  )
  #KP#1: Barplot of GameCode, outliers
  barplot(table(dt$GameCode),main="Barplot of GameCode",xlab="GameCode",ylab="Frequency")
  nrow(dt[GameCode=="BA"])
  nrow(dt[GameCode=="MS"])
  #KP#2: Boxplot for Table* columns
  boxplot(dt[,4:6,with=FALSE],names(dt)[4:6],main="Tables* Comparative Boxplot", ylab="Frequency")
  #KP3: Headcount vs TablesOcc
  plot(dt$HeadCount, dt$TablesOcc,xlab="HeadCount",ylab="TablesOcc", main = "Scatterplot of HeadCount by TablesOcc")
  var(dt$HeadCount, dt$TablesOcc)
  cov(dt$HeadCount, dt$TablesOcc)
  cor(dt$HeadCount, dt$TablesOcc,method="pearson")
  #KP4: Headcount by Weekday
  plot(as.matrix(dt[,sum(HeadCount)/sum(TablesOcc),by=DayOfWeek]),main="Average Heads per Table by Week",ylab="Average Heads")
  dev.off()
}

main=function(){
  cat("\014")
  
  initializeProgram()
  dt=loadCSVAsDataTable("JitteredHeadCount.csv")
  
  #Structure of the dataset
  str(dt)
  
  #Missing values
  print(missingValueExploration(dt))
  
  #Unique values
  print(uniqueValueExploration(dt,nonNumericColRange))
  
  #Numerical exploration of each column
  numericalExploration(dt,numericColRange)
  
  #Graphical exploration
  #Histograms
  histogramExploration(dt,numericColRange)
  
  #Boxplots
  boxplotExploration(dt)
  
  #Outlier removal
  #Enrich the JitteredHeadCount dataset (conver DateFormat and add MonthNumber and GameCodeNumber)
  dt=enrichJitteredHeadCountDataset(dt)
  
  #Summary of new columns
  summarizeEnrichcedDataset(dt)
  
  #Plot of new columns
  plotEnrichedDataset(dt)
  
  #findings
  summarizeFindings(dt)
}


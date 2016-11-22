#install.packages("data.table")
library(data.table)

#study about data frame
#http://www.ats.ucla.edu/stat/r/faq/subset_R.htm

#==========================================================================================================================================
# load Blood Pressure Data
#==========================================================================================================================================
BLOODPRESSURE_DATA <- fread(paste("D:/BIG DATA/Prof Yehh/DATA/new_BloodPressureData.csv", sep=""), header = T, sep = ',', verbose=F)
setnames(BLOODPRESSURE_DATA, c("a", "b", "c", "d")) #change Name column
COLUMNNAMES1 <- colnames(BLOODPRESSURE_DATA) #view column names
#Identify date, The higher one is SBP (systolic  BP), the lower one is diastolic  BP (DBP)
setkey(BLOODPRESSURE_DATA, "a") #set key for searching ("a" means Pateint ID column, "c" means Systolic BP column and "d" means diastolic column)

#==========================================================================================================================================
# load data Patient with column of Mark Patient Died 
#==========================================================================================================================================
PATIENT_IABLE_RESULT <- fread(paste('D:/BIG DATA/Prof Yehh/DOC/Result/3.PateintWith_AgeOfInitialInclusionDataandConsecutivelyHospital.csv', sep=""), header = T, sep = ',', verbose=F, colClasses = "character")
COLUMNNAMES2 <- colnames(PATIENT_IABLE_RESULT) #view column names
TOTAL_PATIENT <- nrow(PATIENT_IABLE_RESULT) #check total patient

#==========================================================================================================================================
# Declaration Variable
#==========================================================================================================================================
meanOfSBP	<- array(c(0))
STDOfSBP	<- array(c(0))
highestSBP	<- array(c(0))
lowestSBP	<- array(c(0))

meanOfDBP	<- array(c(0))
STDOfDBP	<- array(c(0))
highestDBP	<- array(c(0))
lowestDBP <- array(c(0))


InclusionDateBP <- array(c(""))
ageOfInitialInclusion <- array(c(0))
DateOfLastOPD <- array(c(""))
NumberOfOPDVisit <- array(c(0))
ConsecutivelyHospitalBP <- array(c(0))

#Looping for each patient ID
#==========================================================================================================================================
for (i in 1:TOTAL_PATIENT) {
  print(paste(i, PATIENT_IABLE_RESULT[i, PatientID], sep = ". "))
  #get patient ID "i" is number of looping process and "b" is column name of patient_ID
  searchResult <- na.omit(BLOODPRESSURE_DATA[.(PATIENT_IABLE_RESULT[i, PatientID])], cols=c("a", "b", "c", "d"))#search and Remove Na if no result in all column
  if(nrow(searchResult)==0){
    InclusionDateBP[i] <- "0";
    ageOfInitialInclusion[i] <- 0;
    DateOfLastOPD[i] <- "0";
    NumberOfOPDVisit[i] <- 0;
    ConsecutivelyHospitalBP[i] <- 0;
    
    highestSBP[i] = 0;
    lowestSBP[i] = 0;
    meanOfSBP[i] = 0;
    STDOfSBP[i] = 0;
    
    highestDBP[i] = 0;
    lowestDBP[i] = 0;
    meanOfDBP[i] = 0;
    STDOfDBP[i] = 0;
    
  }else{
    InclusionDateBP[i] <- min(as.Date(as.character(searchResult$b), "%Y%m%d"))
    DateOfLastOPD[i] <- max(as.Date(as.character(searchResult$b), "%Y%m%d"))
    NumberOfOPDVisit[i] <- nrow(searchResult)
    #print(year(min(as.Date(as.character(searchResult$b), "%Y%m%d"))))
    #print(year(as.Date(PATIENT_IABLE_RESULT[i, BirthDate], "%Y%m%d")))
    ageOfInitialInclusion[i] <- year(min(as.Date(as.character(searchResult$b), "%Y%m%d"))) - year(as.Date(PATIENT_IABLE_RESULT[i, BirthDate], "%Y%m%d"))
    ConsecutivelyHospitalBP[i] <- year(max(as.Date(as.character(searchResult$b), "%Y%m%d"))) - year(min(as.Date(as.character(searchResult$b), "%Y%m%d")))
  
    if(nrow(searchResult) >= 8 & ConsecutivelyHospitalBP[i] >= 2)
    {
      resultSBPMean <- mean((searchResult[c>60 & c<250]$c), na.rm = T); #SBP<60 should be considered as a typo error, SBP>250 should be discarded.
      resultDBPMean <- mean((searchResult[d>30 & d<130]$d), na.rm = T); #DBP<30 or DBP>130 should not be taken into the calculation.
      resultSBPSD <- sd((searchResult[c>60 & c<250]$c), na.rm = T); #SBP<60 should be considered as a typo error, SBP>250 should be discarded.
      resultDBPSD <- sd((searchResult[d>30 & d<130]$d), na.rm = T); #DBP<30 or DBP>130 should not be taken into the calculation.
      #save SBP
      if(is.nan(resultSBPMean) & is.nan(resultSBPSD)){
        highestSBP[i] = 0;
        lowestSBP[i] = 0;
        meanOfSBP[i] = 0;
        STDOfSBP[i] = 0;
      }else{
        highestSBP[i] = max(searchResult[c>60 & c<250]$c);
        lowestSBP[i] = min(searchResult[c>60 & c<250]$c);
        meanOfSBP[i] = round(resultSBPMean);
        STDOfSBP[i] = round(resultSBPSD);
      }
      
      #save DBP
      if(is.nan(resultDBPMean) & is.nan(resultDBPSD)){
        highestDBP[i] = 0;
        lowestDBP[i] = 0;
        meanOfDBP[i] = 0;
        STDOfDBP[i] = 0;
      }else{
        highestDBP[i] = max(searchResult[d>30 & d<130]$d);
        lowestDBP[i] = min(searchResult[d>30 & d<130]$d);
        meanOfDBP[i] = round(resultDBPMean);
        STDOfDBP[i] = round(resultDBPSD);
      }
    }else{
      highestSBP[i] = 0;
      lowestSBP[i] = 0;
      meanOfSBP[i] = 0;
      STDOfSBP[i] = 0;
      
      highestDBP[i] = 0;
      lowestDBP[i] = 0;
      meanOfDBP[i] = 0;
      STDOfDBP[i] = 0;
    }
  }
}

#Save in a variable data table and combain/add into patient data table with mark in patient died column #
#==========================================================================================================================================
PATIENT_IABLE_RESULT$InclusionDateBP<-as.data.table(InclusionDateBP)
PATIENT_IABLE_RESULT$DateOfLastOPDBP<-as.data.table(DateOfLastOPD)
PATIENT_IABLE_RESULT$ageOfInitialInclusionBP<-as.data.table(ageOfInitialInclusion)
PATIENT_IABLE_RESULT$NumberOfOPDVisitBP<-as.data.table(NumberOfOPDVisit)
PATIENT_IABLE_RESULT$ConsecutivelyHospitalBP<-as.data.table(ConsecutivelyHospitalBP)
PATIENT_IABLE_RESULT$meanOfSBP<-as.data.table(meanOfSBP)
PATIENT_IABLE_RESULT$STDOfSBP<-as.data.table(STDOfSBP)
PATIENT_IABLE_RESULT$highestSBP<-as.data.table(highestSBP)
PATIENT_IABLE_RESULT$lowestSBP<-as.data.table(lowestSBP)
PATIENT_IABLE_RESULT$meanOfDBP<-as.data.table(meanOfDBP)
PATIENT_IABLE_RESULT$STDOfDBP<-as.data.table(STDOfDBP)
PATIENT_IABLE_RESULT$highestDBP<-as.data.table(highestDBP)
PATIENT_IABLE_RESULT$lowestDBP<-as.data.table(lowestDBP)

#Finally, save result into a file #
#==========================================================================================================================================
write.table(PATIENT_IABLE_RESULT, file = paste('D:/BIG DATA/Prof Yehh/DOC/Result/4.PatientResultwith_BP.csv', sep=""), sep = ",", quote = FALSE, col.names = TRUE, row.names=FALSE, append=FALSE)






#change date in table column "InclusionDateBP" and "DateOfLastOPD"
readData <- fread(paste('D:/BIG DATA/Prof Yehh/DOC/Result/4.PatientResultwith_BP old.csv', sep=""), header = T, sep = ',', verbose=F, colClasses = "character")

readData$InclusionDateBP[readData$InclusionDateBP<=0] <- "sem"
readData$InclusionDateBP <- as.character(as.Date(as.numeric(readData$InclusionDateBP)))
readData$InclusionDateBP[is.na(readData$InclusionDateBP)] <- "0"

readData$DateOfLastOPDBP[readData$DateOfLastOPDBP<=0] <- "sem"
readData$DateOfLastOPDBP <- as.character(as.Date(as.numeric(readData$DateOfLastOPDBP)))
readData$DateOfLastOPDBP[is.na(readData$DateOfLastOPDBP)] <- "0"

#Finally, save result into a file #
#==========================================================================================================================================
write.table(readData, file = paste('D:/BIG DATA/Prof Yehh/DOC/Result/5.PatientResultwith_BP.csv', sep=""), sep = ",", quote = FALSE, col.names = TRUE, row.names=FALSE, append=FALSE)









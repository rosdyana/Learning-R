library(data.table)

  sdate1 <- "19550811"
  sdate2 <- "4/20/2004"
  
  ndates1 <- as.Date(sdate1, "%Y%m%d");
  ndates2 <- as.Date(sdate2, "%m/%d/%Y");

  allDate1 <- as.Date(c("19280201", "20500101"), format="%Y%m%d");
  allDate2 <- as.Date(c("2/26/2004", "2/26/2004"), format="%m/%d/%Y");
  Result <- year(allDate1) - year(allDate2)

  #Total days
  day = as.Date(c("20000101"), format="%Y%m%d") - as.Date(c("20010101"), format="%Y%m%d")
  
#Read new and unique patient ID in file #
#==========================================================================================================================================
PATIENT_IABLE <- fread(paste('D:/BIG DATA/Prof Yehh/DOC/Result/PateintWith_InclusionData.csv', sep=""), header = T, sep = ',', verbose=F, colClasses = "character")
TATOL_PATIENT <- nrow(PATIENT_IABLE)
colnames(PATIENT_IABLE)
setnames(PATIENT_IABLE, c("PatientID","Gender","BirthDate","NumberComeToHospital","FirstYearEnterHospital(InclusionDate)","LastYearComeToHospital")) #change Name column

#calculate age of initial inclusion date
InclusionDate <- as.Date(PATIENT_IABLE$`FirstYearEnterHospital(InclusionDate)`, "%m/%d/%Y");
PATIENT_IABLE$AgeOfInitialInclusionDate <- year(InclusionDate) - year(as.Date(PATIENT_IABLE$BirthDate, "%Y%m%d"))

#Finally, save result into a file (calculate age of initial inclusion date)#
#==========================================================================================================================================
write.table(PATIENT_IABLE, file = paste('D:/BIG DATA/Prof Yehh/DOC/Result/','2.PateintWith_AgeOfInitialInclusionData.csv', sep=""), sep = ",", quote = FALSE, col.names = TRUE, row.names=FALSE, append=FALSE)


#calculate Consecutively Hospital (rumah sakit berturut2)
Consecutively <- as.Date(PATIENT_IABLE$`FirstYearEnterHospital(InclusionDate)`, "%m/%d/%Y");
PATIENT_IABLE$ConsecutivelyHospital <- year(as.Date(PATIENT_IABLE$LastYearComeToHospital, "%m/%d/%Y")) - year(Consecutively) 

#PATIENT_IABLE[, ConsecutivelyHospital := ifelse(as.numeric(NumberComeToHospital) >= 8 & as.numeric(NumberComeToHospital) > 0, 1, 0)]

#Finally, save result into a file (calculate Consecutively Hospital (rumah sakit berturut2))#
#==========================================================================================================================================
write.table(PATIENT_IABLE, file = paste('D:/BIG DATA/Prof Yehh/DOC/Result/','3.PateintWith_AgeOfInitialInclusionDataandConsecutivelyHospital.csv', sep=""), sep = ",", quote = FALSE, col.names = TRUE, row.names=FALSE, append=FALSE)
















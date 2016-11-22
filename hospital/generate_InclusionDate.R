sdate1 <- c("19550811", "19550812", "19550810")
sdate2 <- "20160904"

ndates1 <- as.Date(sdate1, "%Y%m%d");
ndates2 <- as.Date(sdate2, "%Y%m%d");

min(ndates1)

if(ndates1 > ndates2){
  print("betul")
}
#Convert int to date
library("zoo")
install.packages("zoo")
as.Date(13474)

#Install and call Package #
#==========================================================================================================================================
#install.packages("sqldf")
#install.packages("data.table")
library(sqldf)
library(data.table)

#Read patient ID in file #
#==========================================================================================================================================
PATIENT_IABLE <- fread(paste('D:/BIG DATA/Prof Yehh/DATA/newUniquePatientID.csv', sep=""), header = T, sep = ',', verbose=F, colClasses = "character")
setnames(PATIENT_IABLE, c("a","b","c")) #change Name column
TATOL_PATIENT <- nrow(PATIENT_IABLE)

#Read fast 77 csv files into single datatable #
#==========================================================================================================================================
DATA_DIRECTORY_77Files <- "D:/BIG DATA/Prof Yehh/DATA/CSV/77File/";
READ_ALL_files <- list.files(path = DATA_DIRECTORY_77Files, pattern = ".csv")
#READ_ALL_DATA_IN_FILE <- lapply(paste(DATA_DIRECTORY_77Files, READ_ALL_files, sep = "/"), fread, header = T, sep = ',', verbose=F, colClasses = "character")
#DATA_IN_SINGLE_DATATABLE <- do.call(rbind, READ_ALL_DATA_IN_FILE)
DATA_IN_SINGLE_DATATABLE <- do.call(rbind, lapply(paste(DATA_DIRECTORY_77Files, READ_ALL_files, sep = "/"), fread, header = T, sep = ',', verbose=F, colClasses = "character"))
setnames(DATA_IN_SINGLE_DATATABLE, c("a","b","c", "d","e", "f","g", "h","i", "j")) #change Name column
setkey(DATA_IN_SINGLE_DATATABLE, "c") #set key for searching
nrow(DATA_IN_SINGLE_DATATABLE)


enterHospital <- array(c(""))
lastYearComeToHospital <- array(c(""))
numberOfComeToHospital <- array(c(""))

for (i in 1:TATOL_PATIENT) {
  print(paste(i, PATIENT_IABLE[i, a], sep = ". "))
  PATIENT_ID <- c(PATIENT_IABLE[i, a]) #get patient ID "i" is number of looping process and "b" is column name of patient_ID
  searchDate <- na.omit(DATA_IN_SINGLE_DATATABLE[.(PATIENT_ID)], cols=c("a","b","c", "d","e", "f","g","i", "j"))#Remove Na if no result in all column
  resultDATE = nrow(searchDate)
  
  
  numberOfComeToHospital[i] = resultDATE
  enterHospital[i] = min(as.Date(searchDate$d, "%Y%m%d"))
  lastYearComeToHospital[i] = max(as.Date(searchDate$d, "%Y%m%d"))
  
  print(numberOfComeToHospital[i])
  print(enterHospital[i])
  print(lastYearComeToHospital[i])
  
}

setnames(PATIENT_IABLE, c("Patient ID", "Gender", "Birth Date")) #set column (change Name column)

PATIENT_IABLE$NumberOfComeToHospital<-as.data.table(numberOfComeToHospital)
PATIENT_IABLE$FirstYearEnterHospital<-as.data.table(as.Date(as.integer(enterHospital)))
PATIENT_IABLE$lastYearComeToHospital<-as.data.table(as.Date(as.integer(lastYearComeToHospital)))

#Finally, save result into a file #
#==========================================================================================================================================
write.table(PATIENT_IABLE, file = paste('D:/BIG DATA/Prof Yehh/DOC/Result/','PateintWith_InclusionData.csv', sep=""), sep = ",", quote = FALSE, col.names = TRUE, row.names=FALSE, append=FALSE)









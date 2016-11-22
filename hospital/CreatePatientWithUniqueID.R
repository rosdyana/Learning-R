library(data.table)

#==========================================================================================================================================
# load data Patient 
#==========================================================================================================================================
PATIENT_IABLE <- fread(paste('D:/BIG DATA/Prof Yehh/DATA/CSV/10804_Patient_ID_1.csv', sep=""), header = T, sep = ',', verbose=F, colClasses = "character")
colnames(PATIENT_IABLE) #view column names
TOTAL_PATIENT <- nrow(PATIENT_IABLE) #check total patient ###all pateint ID are 650184 patient###
setnames(PATIENT_IABLE, c("a", "b", "c", "d")) #change Name column
setkey(PATIENT_IABLE, "b") #set key for searching

for (i in 1:TOTAL_PATIENT) {
  PATIENT_ID <- c(PATIENT_IABLE[i, b]) #get patient ID "i" is number of looping process and "b" is column name of patient_ID
  searchResultID <- na.omit(PATIENT_IABLE[.(PATIENT_ID)], cols=c("a","b","c", "d"))#Remove Na if no result in all column
  countSameID = nrow(searchResultID)
  
  if(countSameID >= 2){
    print("", sep="")
    print(paste("Patient ID : ", i, ". ", PATIENT_IABLE[i, b], sep = ""))
    print(paste("Total Same ID : ", countSameID, sep=""))
    print(paste("Print Data : ", sep="\n"))  
    print(searchResultID)
    print("", sep="")
  }
}

#Search unique based on patient ID (342823 patients)
setkey(PATIENT_IABLE, "b")
search1 <- unique(PATIENT_IABLE)
nrow(unique(PATIENT_IABLE))

#Search unique based on patient ID and gender (342832 patients)
setkey(PATIENT_IABLE, "b", "c")
nrow(unique(PATIENT_IABLE))

#Search unique based on patient ID and birth date (344061 patients)
setkey(PATIENT_IABLE, "b", "d")
nrow(unique(PATIENT_IABLE))

#Search unique based on patient ID, gender and birth date 344064 patients)
setkey(PATIENT_IABLE, "b", "c", "d")
nrow(unique(PATIENT_IABLE))


#create new patient data
#Search unique based on patient ID (342823 patients)
setkey(PATIENT_IABLE, "b")
nrow(unique(PATIENT_IABLE))

new_dataPatient <- unique(PATIENT_IABLE)
new_dataPatient$a <- NULL # remove first column
setnames(new_dataPatient, c("Patient ID", "Gender", "Birth Date")) #set column (change Name column)

#Finally, save result into a file #
#==========================================================================================================================================
write.table(new_dataPatient, file = paste('D:/BIG DATA/Prof Yehh/DATA/','newUniquePatientID.csv', sep=""), sep = ",", quote = FALSE, col.names = TRUE, row.names=FALSE, append=FALSE)





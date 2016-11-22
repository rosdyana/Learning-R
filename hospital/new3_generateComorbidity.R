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

#Variable declaration #
#==========================================================================================================================================
DSM <- array(c(0)) #declaration integer array for Diabetes, Stroke, and MI
SM <- array(c(0)) #declaration integer array for Stroke and MI
DS <- array(c(0)) #declaration integer array for Diabetes and Stroke
DM <- array(c(0)) #declaration integer array for Diabetes and MI
MI <- array(c(0)) #declaration integer array for MI
S <- array(c(0)) #declaration integer array for Stroke
D <- array(c(0)) #declaration integer array for Daibetes

#Variable declaration of all DISEASE ID #
#==========================================================================================================================================
DIABETES_ID = c("24900", "24901", "24910", "24911", "24920", "24921", "24930", "24931", "24940", "24941", "24950", "24951", "24960", "24961", "24970", "24971", "24980", "24981", "24990", "24991", "25000", "25001", "25002", "25003", "25010", "25011", "25012", "25013", "25020", "25021", "25022", "25023", "25030", "25031", "25032", "25033", "25040", "25041", "25042", "25043", "25050", "25051", "25052", "25053", "25060", "25061", "25062", "25063", "25070", "25071", "25072", "25073", "25080", "25081", "25082", "25083", "25090", "25091", "25092", "25093", "3572", "36201", "36202", "36203", "36204", "36205", "36206", "36641")
STROKE_ID = c("430", "431", "43301", "43311", "43321", "43331", "43381", "43391", "43400", "43401", "43410", "43411", "43490", "43491", "4350", "4351", "4353", "4358", "4359", "436", "99702")
MI_ID = c("41000", "41010", "41020", "41030", "41040", "41050", "41060", "41070", "41080", "41090", "41001", "41011", "41021", "41031", "41041", "41051", "41061", "41071", "41081", "41091")

#Read patient ID in file #
#==========================================================================================================================================
PATIENT_IABLE <- fread(paste('D:/BIG DATA/Prof Yehh/DOC/Result/2.PateintWith_AgeOfInitialInclusionData.csv', sep=""), header = T, sep = ',', verbose=F, colClasses = "character")
setnames(PATIENT_IABLE, c("a","b","c", "d", "e", "f", "g")) #change Name column
TATOL_PATIENT <- nrow(PATIENT_IABLE)
#testSearchDate = PATIENT_IABLE[c>="20040101" & c<"20060101"]

#Read fast 77 csv files into single datatable #
#==========================================================================================================================================
DATA_DIRECTORY_77Files <- "D:/BIG DATA/Prof Yehh/DATA/CSV/77File/";
READ_ALL_files <- list.files(path = DATA_DIRECTORY_77Files, pattern = ".csv")
#READ_ALL_DATA_IN_FILE <- lapply(paste(DATA_DIRECTORY_77Files, READ_ALL_files, sep = "/"), fread, header = T, sep = ',', verbose=F, colClasses = "character")
#DATA_IN_SINGLE_DATATABLE <- do.call(rbind, READ_ALL_DATA_IN_FILE)
DATA_IN_SINGLE_DATATABLE <- do.call(rbind, lapply(paste(DATA_DIRECTORY_77Files, READ_ALL_files, sep = "/"), fread, header = T, sep = ',', verbose=F, colClasses = "character"))
setnames(DATA_IN_SINGLE_DATATABLE, c("a","b","c", "d","e", "f","g", "h","i", "j")) #change Name column
setkey(DATA_IN_SINGLE_DATATABLE, "c", "f") #set key for searching patient ID and diseases code
nrow(DATA_IN_SINGLE_DATATABLE)


for (i in 1:TATOL_PATIENT) {
  print(paste(i, PATIENT_IABLE[i, a], sep = ". "))
  PATIENT_ID <- c(PATIENT_IABLE[i, a]) #get patient ID "i" is number of looping process and "a" is column name of patient_ID
  getPATIENTFirstDateComeHospital <- c(PATIENT_IABLE[i, e]) #get patient First day come to hospital
  getPATIENTLastDateComeHospital <- c(PATIENT_IABLE[i, f]) #get patient last day come to hospital
  
  #First, needs to search Pateint based on their ID and Search 3 Diseases during 2 years period
  searchResultDIABETES_ID <- na.omit(DATA_IN_SINGLE_DATATABLE[.(PATIENT_ID, DIABETES_ID)], cols=c("a","b","c", "d","e", "f","g","i", "j"))#Remove Na if no result in all column
  nb_Diabetes = nrow(searchResultDIABETES_ID[d>=getPATIENTFirstDateComeHospital & d<getPATIENTLastDateComeHospital])#SET and search period
  
  searchResultSTROKE_ID <- na.omit(DATA_IN_SINGLE_DATATABLE[.(PATIENT_ID, STROKE_ID)], cols=c("a","b","c", "d","e", "f","g","i", "j"))#Remove Na if no result in all column
  nb_Stroke = nrow(searchResultSTROKE_ID[d>=getPATIENTFirstDateComeHospital & d<getPATIENTLastDateComeHospital])#SET and search period
  
  searchResultMI_ID <- na.omit(DATA_IN_SINGLE_DATATABLE[.(PATIENT_ID, MI_ID)], cols=c("a","b","c", "d","e", "f","g","i", "j"))#Remove Na if no result in all column
  nb_MI = nrow(searchResultMI_ID[d>=getPATIENTFirstDateComeHospital & d<getPATIENTLastDateComeHospital])#SET and search period
  
  if(nb_Diabetes>=1 & nb_Stroke>=1 & nb_MI>=1){
    DSM[i] = 1;
    SM[i] = 0;
    DS[i] = 0;
    DM[i] = 0;
    MI[i] = 0;
    S[i] = 0;
    D[i] = 0;
  }else if(nb_Stroke>=1 & nb_MI>=1){
    DSM[i] = 0;
    SM[i] = 1;
    DS[i] = 0;
    DM[i] = 0;
    MI[i] = 0;
    S[i] = 0;
    D[i] = 0;
  }else if(nb_Diabetes>=1 & nb_Stroke>=1){
    DSM[i] = 0;
    SM[i] = 0;
    DS[i] = 1;
    DM[i] = 0;
    MI[i] = 0;
    S[i] = 0;
    D[i] = 0;
  }else if(nb_Diabetes>=1 & nb_MI>=1){
    DSM[i] = 0;
    SM[i] = 0;
    DS[i] = 0;
    DM[i] = 1;
    MI[i] = 0;
    S[i] = 0;
    D[i] = 0;
  }else if(nb_MI>=1){
    DSM[i] = 0;
    SM[i] = 0;
    DS[i] = 0;
    DM[i] = 0;
    MI[i] = 1;
    S[i] = 0;
    D[i] = 0;
  }else if(nb_Stroke>=1){
    DSM[i] = 0;
    SM[i] = 0;
    DS[i] = 0;
    DM[i] = 0;
    MI[i] = 0;
    S[i] = 1;
    D[i] = 0;
  }else if(nb_Diabetes>=1){
    DSM[i] = 0;
    SM[i] = 0;
    DS[i] = 0;
    DM[i] = 0;
    MI[i] = 0;
    S[i] = 0;
    D[i] = 1;
  }else{
    DSM[i] = 0;
    SM[i] = 0;
    DS[i] = 0;
    DM[i] = 0;
    MI[i] = 0;
    S[i] = 0;
    D[i] = 0;
  }
  
  #try to print the result
  print(DSM[i])
  print(SM[i])
  print(DS[i])
  print(DM[i])
  print(MI[i])
  print(S[i])
  print(D[i])
}

#Duplicate data table from original data #
#==========================================================================================================================================
setnames(PATIENT_IABLE, c("PatientID", "Gender", "BirthDate", "NumberVisitHospital", "FirstDateComeHospital", "LastDateComeHospital", "AgeOfInitialInclusionDate")) #set column (change Name column)


Duplicate_Data_Patient <-  PATIENT_IABLE;

#Save in a variable data table and combain/add into patient data table #
#==========================================================================================================================================
Duplicate_Data_Patient$Diabetes<-as.data.table(D)
Duplicate_Data_Patient$Stroke<-as.data.table(S)
Duplicate_Data_Patient$MI<-as.data.table(MI)
Duplicate_Data_Patient$DiabetesMI<-as.data.table(DM)
Duplicate_Data_Patient$DiabetesStroke<-as.data.table(DS)
Duplicate_Data_Patient$StrokeMI<-as.data.table(SM)
Duplicate_Data_Patient$DiabetesStrokeMI<-as.data.table(DSM) #convert from array to data table

sum(D[D==1])
sum(S[S==1])
sum(MI[MI==1])
sum(DM[DM==1])
sum(DS[DS==1])
sum(SM[SM==1])
sum(DSM[DSM==1])

#Finally, save result into a file #
#==========================================================================================================================================
write.table(Duplicate_Data_Patient, file = paste('D:/BIG DATA/Prof Yehh/DOC/Result/','3.patientResultComorbidity.csv', sep=""), sep = ",", quote = FALSE, col.names = TRUE, row.names=FALSE, append=FALSE)


#Try to count for all Co-morbidity
Data_Comorbidity <- fread(paste('D:/BIG DATA/Prof Yehh/DOC/Result/3.patientResultComorbidity.csv', sep=""), header = T, sep = ',', verbose=F, colClasses = "character")

sum(as.integer(Data_Comorbidity$Diabetes))
sum(as.integer(Data_Comorbidity$Stroke))
sum(as.integer(Data_Comorbidity$MI))
sum(as.integer(Data_Comorbidity$DiabetesMI))
sum(as.integer(Data_Comorbidity$DiabetesStroke))
sum(as.integer(Data_Comorbidity$StrokeMI))
sum(as.integer(Data_Comorbidity$DiabetesStrokeMI))


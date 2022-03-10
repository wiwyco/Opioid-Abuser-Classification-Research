# Winslow Conneen
# This R script is intended to be used in tandem with the MIMIC-IV EHR database
# For the prediction of opioid abuse using historical patient medical data

#much of the below information has been redacted to retain credentialed access to the database on the 
#basis of doctor-patient confidentiality and for lab security.
library(RMariaDB)
pswd <- ""
thehost <- ''
mimicDb <- dbConnect(RMariaDB::MariaDB(), user='', password=, dbname='', host=thehost)
dbListTables(mimicDb)

#creates a distinct list of all subject IDs for addicted patients
addictedSubjectIDs <- dbGetQuery(mimicDb, 'SELECT DISTINCT subject_id
  FROM hosp_diagnoses_icd
  WHERE icd_code like \'3055%\';')

#creates a distinct list off all ICD 9 codes
ICD9List <- dbGetQuery(mimicDb, 'SELECT DISTINCT icd_code
  FROM hosp_diagnoses_icd
  WHERE icd_version = 9;')

#initializes list to store prescrption and diagnoses history for abusers
addicts_with_diagnoses_and_prescription <- list()

#index incrementor for the addicted_with_diagnoses_and_prescription list
list_index <- 0 

#grabs subject_IDs of addicts who have both a prescription and diagnoses history
for(i in 1:nrow(addictedSubjectIDs)){
  print(i)
  individual_prescription_history <- dbGetQuery(mimicDb, paste('SELECT distinct drug
    FROM hosp_prescriptions natural join core_admissions
    WHERE subject_id = ', addictedSubjectIDs[i,1], ' AND admittime < (SELECT min(admittime)	
    FROM core_admissions natural join hosp_diagnoses_icd
    WHERE subject_id = ', addictedSubjectIDs[i,1], ' AND icd_code like \'3055%\');'), sep = ' ')
  
  if(!(nrow(individual_prescription_history)) == 0)
  {
    list_index = list_index + 1
    individual_diagnoses_history <- dbGetQuery(mimicDb, paste('SELECT distinct icd_code
                                                                FROM hosp_diagnoses_icd natural join core_admissions
                                                                WHERE subject_id = ', addictedSubjectIDs[i,1], ' AND admittime < (SELECT min(admittime)	
                                                                FROM core_admissions natural join hosp_diagnoses_icd
                                                                WHERE subject_id = ', addictedSubjectIDs[i,1], ' AND icd_code like \'3055%\');'), sep = ' ')
    
    addicts_with_diagnoses_and_prescription[[list_index]] <- list(addictedSubjectIDs[i,1], individual_prescription_history, individual_diagnoses_history)
  }
}

#stores icd code along with the number of addicts that have the diagnoses
most_shared_diagnoses <- data.frame(Icd_Code = "IGNORE", Patients_with_Icd_Code = 0)
new_diagnoses <- FALSE

#Lists icd_code (diagnoses), and lists how many patients had the drug in their history.
for(i in 1:566){
  print(i)
  individual_diagnoses_history <- addicts_with_diagnoses_and_prescription[[i]][[3]]
  length_keeper <- nrow(most_shared_diagnoses)
  
  
  for(j in 1:nrow(individual_diagnoses_history)){
    new_diagnoses <- TRUE
    for(x in 1:length_keeper){
      if(individual_diagnoses_history[j,1] == most_shared_diagnoses[x,1]){
        most_shared_diagnoses[x,2] <- most_shared_diagnoses[x,2] + 1
        new_diagnoses <- FALSE
        
        break
      }
    }
    if(new_diagnoses){
      new_entry <- data.frame(Icd_Code = individual_diagnoses_history[j,1], Patients_with_Icd_Code = 1)
      most_shared_diagnoses <- rbind(most_shared_diagnoses, new_entry)
    }
  }
}

most_shared_prescriptions <- data.frame(Prescription = "IGNORE", Patients_with_Prescription = 0)

#Lists prescriptions, and counts how many patients had the drug in their history.
for(i in 1:566){
  print(i)
  individual_prescription_history <- addicts_with_diagnoses_and_prescription[[i]][[2]]
  length_keeper <- nrow(most_shared_prescriptions)
  
  
  for(j in 1:nrow(individual_prescription_history)){
    new_prescription <- TRUE
    for(x in 1:length_keeper){
      if(individual_prescription_history[j,1] == most_shared_prescriptions[x,1]){
        most_shared_prescriptions[x,2] <- most_shared_prescriptions[x,2] + 1
        new_prescriptions <- FALSE
        
        break
      }
    }
    if(new_prescription){
      new_entry <- data.frame(Prescription = individual_prescription_history[j,1], Patients_with_Prescription = 1)
      most_shared_prescriptions <- rbind(most_shared_prescriptions, new_entry)
    }
  }
}

#Checks for opioid abuse in most_shared_diagnoses
#Why are we doing this? >>> Opioid dependence was found in most_shared_diagnoses. This dataframe should only contain diagnoses PRECEEDING an abuse diagnoses.
#Now, dependency could certainly be diagnosed before abuse. But we still want to make sure most_shared_diagnoses doesn't contain diagnoses that occured during or after the abuse diganoses.
#UPDATE: opioid abuse was not found in most_shared_diagnoses. Dependency must be commonly diagnosed before abuse.
opioid_abuse_icd_code <- data.frame(Icd_Code = "IGNORE")

for(i in 1:nrow(most_shared_diagnoses)){
  diagnoses <- most_shared_diagnoses[i,1]
  if((substr(diagnoses, 1,4)) == "3055"){
    df <- data.frame(Icd_Code = most_shared_diagnoses[i,1])
    icd_codes_start_with_3 <- rbind(icd_codes_start_with_3, df)
  }
}
nrow(most_shared_diagnoses)

#here I converted what was in the findings from the previous section into a parseable vector
top_diags <- c('4019', '311', '3051', '07054', '33829', 'V6284', '53081', '07070', '30000', '30500')
top_scripts <- c("Acetaminophen", "Heparin", "Docusate Sodium", "Senna", "Lorazepam", "Ondansetron", "OxycoDONE (Immediate Release) ", "HYDROmorphone (Dilaudid)", "Potassium Chloride", "Influenza Virus Vaccine")

#this section fetches IDs of all addicted patients to initialize the below dataframe properly
addict_ids <- addicts_with_diagnoses_and_prescription[[1]][[1]]
for(i in 2:566) {
  addict_ids <- c(addict_ids, addicts_with_diagnoses_and_prescription[[i]][[1]])
}

#initalizing the addicted patient dataframe
inputs_with_nulls <- data.frame(SubjectID=addict_ids,
                                Hypertension=rep(c(0), each=566),
                                Depression=rep(c(0), each=566),
                                TobaccoUse=rep(c(0), each=566),
                                ChronicHepC=rep(c(0), each=566),
                                OtherChronicPain=rep(c(0), each=566),
                                SuicidalIdeation=rep(c(0), each=566),
                                EsophagealReflux=rep(c(0), each=566),
                                UnspecHepC=rep(c(0), each=566),
                                AnxietyState=rep(c(0), each=566),
                                AlchoholAbuse=rep(c(0), each=566),
                                Acetaminophen = rep(c(0), each=566), 
                                Heparin = rep(c(0), each=566), 
                                DocusateSodium = rep(c(0), each=566), 
                                Senna = rep(c(0), each=566), 
                                Lorazepam = rep(c(0), each=566), 
                                Ondansetron = rep(c(0), each=566), 
                                OxycoDONE = rep(c(0), each=566), 
                                HYDROmorphone = rep(c(0), each=566), 
                                PotassiumChloride = rep(c(0), each=566), 
                                InfluenzaVirusVaccine = rep(c(0), each=566), 
                                OpioidAbuse=rep(c(1), each=566))

#This populates the dataframe using the diagnosis and prescription lists created above
for(i in 1:566) {
  for(j in 1:10) {
    if(top_diags[j] %in% addicts_with_diagnoses_and_prescription[[i]][[3]][,1]) {
      inputs_with_nulls[i, (j+1)] <- 1
    }
    if(top_scripts[j] %in% addicts_with_diagnoses_and_prescription[[i]][[2]][,1]) {
      inputs_with_nulls[i, (j+11)] <- 1
    }
  }
}

#this eliminates null rows in our data to appropriately train the model
cohort <- inputs_with_nulls[1,]
for(i in 2:566) {
  if(!(setequal(as.numeric(inputs_with_nulls[i,2:11]), c(0,0,0,0,0,0,0,0,0,0))) && !(setequal(as.numeric(inputs_with_nulls[i,12:21]), c(0,0,0,0,0,0,0,0,0,0)))) {
    cohort <- rbind(cohort, inputs_with_nulls[i,])
  }
}

#Now we need to find a unique list of non-addicts that have inputs into our network in both the prescription and diagnosis networks
NASubjectIDs <- dbGetQuery(mimicDb, 'Select distinct subject_id from
  (SELECT subject_id, drug
  FROM hosp_prescriptions
  NATURAL JOIN hosp_diagnoses_icd
  WHERE subject_id not in (SELECT subject_id from addicted)
  AND drug in (\'Acetaminophen\', \'Heparin\', \'Docusate Sodium\', \'Senna\', \'Lorazepam\', \'Ondansetron\',
  \'OxycoDONE (Immediate Release) \', \'HYDROmorphone (Dilaudid)\', \'Potassium Chloride\', \'Influenza Virus Vaccine\')
  AND icd_code in (\'4019\', \'311\', \'3051\', \'07054\', \'33829\', \'V6284\',
  \'53081\', \'07070\', \'30000\', \'30500\')) AS T1;')

#creates random list of subject IDs to match size of cohort
x <- sample(74541, 517, replace = F)

NA_inputs <- data.frame(SubjectID = NASubjectIDs[x,],
                               Hypertension=rep(c(0), each=517),
                               Depression=rep(c(0), each=517),
                               TobaccoUse=rep(c(0), each=517),
                               ChronicHepC=rep(c(0), each=517),
                               OtherChronicPain=rep(c(0), each=517),
                               SuicidalIdeation=rep(c(0), each=517),
                               EsophagealReflux=rep(c(0), each=517),
                               UnspecHepC=rep(c(0), each=517),
                               AnxietyState=rep(c(0), each=517),
                               AlchoholAbuse=rep(c(0), each=517),
                               Acetaminophen = rep(c(0), each=517), 
                               Heparin = rep(c(0), each=517), 
                               DocusateSodium = rep(c(0), each=517), 
                               Senna = rep(c(0), each=517), 
                               Lorazepam = rep(c(0), each=517), 
                               Ondansetron = rep(c(0), each=517), 
                               OxycoDONE = rep(c(0), each=517),
                               HYDROmorphone = rep(c(0), each=517), 
                               PotassiumChloride = rep(c(0), each=517), 
                               InfluenzaVirusVaccine = rep(c(0), each=517), 
                               isAddict = rep(c(0), each=517))

#This portion populates the above dataframe
for(i in 69:517) {
  print(i)
  
  individual_prescription_history <- dbGetQuery(mimicDb, paste('SELECT distinct drug
                                                  FROM hosp_prescriptions
                                                  WHERE subject_id = ', NASubjectIDs[x[i],1],';'), sep="")
  
  individual_diagnoses_history <- dbGetQuery(mimicDb, paste('SELECT distinct icd_code
                                                                FROM hosp_diagnoses_icd natural join core_admissions
                                                                WHERE subject_id = ', addictedSubjectIDs[i,1], ';'), sep = ' ')
  
  for(j in 1:10) {
    if(top_scripts[j] %in% individual_prescription_history[,1]) {
      NA_inputs[i,j+11] <- 1
    }
    if(top_diags %in% individual_diagnoses_history[,1]) {
      NA_inputs[i,j+1] <- 1
    }
  }
}

#define a dataframe with all inputs
all_data <- rbind(NA_inputs, cohort)

#just diagnosis inputs
diagnosis_training <- rbind(NA_inputs[1:311, 2:11], cohort[1:311, 2:11])
diagnosis_testing <- rbind(NA_inputs[312:414, 2:11], cohort[312:414, 2:11])
diagnosis_validation <- rbind(NA_inputs[415:517, 2:11], cohort[415:517, 2:11])

#just prescription inputs
script_training <- rbind(NA_inputs[1:311, 12:21], cohort[1:311, 12:21])
script_testing <- rbind(NA_inputs[312:414, 12:21], cohort[312:414, 12:21])
script_validation <- rbind(NA_inputs[415:517, 12:21], cohort[415:517, 12:21])

#all data inputs
training <- rbind(NA_inputs[1:311, 12:21], cohort[1:311, 12:21])
testing <- rbind(NA_inputs[312:414, 12:21], cohort[312:414, 12:21])
validation <- rbind(NA_inputs[415:517, 12:21], cohort[415:517, 12:21])

#diagnosis neural net
{
diag_nn = neuralnet(isAddict~Hypertension+Depression+TobaccoUse+ChronicHepC+OtherChronicPain+SuicidalIdeation+EsophagealReflux+UnspecHepC+AnxietyState+AlchoholAbuse,
                data=diagnosis_training, hidden=5, learningrate=0.01, algorithm="backprop", act.fct = "logistic", linear.output = FALSE, exclude=c(1,12,23,34,45), constant.weights=c(1,1,1,1,1))

Predict=predict(diag_nn,diagnosis_testing)

#implements a step function activated at 0.5
pred <- ifelse(Predict>0.5, 1, 0)
pred

#counts errors in the test
sumNA <- 0
sumA <- 0
for(i in 1:103) {
  if(pred[i,1] == 1) {
    sumNA <- sumNA + 1
  }
  if(pred[i+103,1] == 0) {
    sumA <- sumA + 1
  }
}

#computes accuracy of model on testing set
sumErrTest <- sumA + sumNA
testAccuracy <- 100 - ((sumErrTest*100)/206)

#runs the validation vectors through the model and catalogs and prints its results
Predict=compute(diag_nn,diagnosis_validation)

#implements a step function activated at 0.5
pred <- ifelse(Predict>0.5, 1, 0)
pred

#counts errors in the validation
sumNA <- 0
sumA <- 0
for(i in 1:103) {
  if(pred[i,1] == 1) {
    sumNA <- sumNA + 1
  }
  if(pred[i+103,1] == 0) {
    sumA <- sumA + 1
  }
}

#computes accuracy of model on the validation set
sumErrVal <- sumA + sumNA
validationAccuracy <- 100 - ((sumErrVal*100)/206)
}

#prescription neural net
{
  script_nn = neuralnet(isAddict~Acetaminophen+Heparin+DocusateSodium+Senna+Lorazepam+Ondansetron+OxycoDONE+HYDROmorphone+PotassiumChloride+InfluenzaVirusVaccine,
                      data=script_training, hidden=5, learningrate=0.01, algorithm="backprop", act.fct = "logistic", linear.output = FALSE, exclude=c(1,12,23,34,45), constant.weights=c(1,1,1,1,1))
  
  Predict=predict(script_nn,script_testing)
  
  #implements a step function activated at 0.5
  pred <- ifelse(Predict>0.5, 1, 0)
  pred
  
  #counts errors in the test
  sumNA <- 0
  sumA <- 0
  for(i in 1:103) {
    if(pred[i,1] == 1) {
      sumNA <- sumNA + 1
    }
    if(pred[i+103,1] == 0) {
      sumA <- sumA + 1
    }
  }
  
  #computes accuracy of model on testing set
  sumErrTest <- sumA + sumNA
  testAccuracy <- 100 - ((sumErrTest*100)/206)
  
  #runs the validation vectors through the model and catalogs and prints its results
  Predict=compute(script_nn,script_validation)
  
  #implements a step function activated at 0.5
  pred <- ifelse(Predict>0.5, 1, 0)
  pred
  
  #counts errors in the validation
  sumNA <- 0
  sumA <- 0
  for(i in 1:103) {
    if(pred[i,1] == 1) {
      sumNA <- sumNA + 1
    }
    if(pred[i+103,1] == 0) {
      sumA <- sumA + 1
    }
  }
  
  #computes accuracy of model on the validation set
  sumErrVal <- sumA + sumNA
  validationAccuracy <- 100 - ((sumErrVal*100)/206)
}

set_analysis <- lm(isAddict~Hypertension+Depression+TobaccoUse+ChronicHepC+OtherChronicPain+SuicidalIdeation+EsophagealReflux+UnspecHepC+AnxietyState+AlchoholAbuse
                   +Acetaminophen+Heparin+DocusateSodium+Senna+Lorazepam+Ondansetron+OxycoDONE+HYDROmorphone+PotassiumChloride+InfluenzaVirusVaccine, data=all_data)

#full neural net
{
  nn = neuralnet(isAddict~Hypertension+Depression+TobaccoUse+ChronicHepC+OtherChronicPain+SuicidalIdeation+EsophagealReflux+UnspecHepC+AnxietyState+AlchoholAbuse+
                          Acetaminophen+Heparin+DocusateSodium+Senna+Lorazepam+Ondansetron+OxycoDONE+HYDROmorphone+PotassiumChloride+InfluenzaVirusVaccine,
                        data=training, hidden=5, learningrate=0.01, algorithm="backprop", act.fct = "logistic", linear.output = FALSE, exclude=c(1,12,23,34,45), constant.weights=c(1,1,1,1,1))
  
  Predict=predict(nn,testing)
  
  #implements a step function activated at 0.5
  pred <- ifelse(Predict>0.5, 1, 0)
  pred
  
  #counts errors in the test
  sumNA <- 0
  sumA <- 0
  for(i in 1:103) {
    if(pred[i,1] == 1) {
      sumNA <- sumNA + 1
    }
    if(pred[i+103,1] == 0) {
      sumA <- sumA + 1
    }
  }
  
  #computes accuracy of model on testing set
  sumErrTest <- sumA + sumNA
  testAccuracy <- 100 - ((sumErrTest*100)/206)
  
  #runs the validation vectors through the model and catalogs and prints its results
  Predict=compute(nn,validation)
  
  #implements a step function activated at 0.5
  pred <- ifelse(Predict>0.5, 1, 0)
  pred
  
  #counts errors in the validation
  sumNA <- 0
  sumA <- 0
  for(i in 1:103) {
    if(pred[i,1] == 1) {
      sumNA <- sumNA + 1
    }
    if(pred[i+103,1] == 0) {
      sumA <- sumA + 1
    }
  }
  
  #computes accuracy of model on the validation set
  sumErrVal <- sumA + sumNA
  validationAccuracy <- 100 - ((sumErrVal*100)/206)
}
library(readr)
library(ggfortify)
library(survival)
library(suvminer)

dataset <- read_csv(NULL)
route <- read_csv("Documents/R_project/coronavirusdataset/route.csv")
time <- read_csv("Documents/R_project/coronavirusdataset/time.csv")
trend <- read_csv("Documents/R_project/coronavirusdataset/trend.csv")
patient <- read_csv("Documents/R_project/coronavirusdataset/patient.csv")


patients<-patient[as.vector(!is.na(patient[,"state"])),]
rel<-patients[,"state"]=="released"
dec<-patients[,"state"]=="deceased"
released.patrients<-patients[rel,]
deceased.patients<-patients[dec,]
released.patrients[,"state"]<-0
deceased.patients[,"state"]<-1
patients.rd<-rbind(released.patrients, deceased.patients)
patients.rd[is.na(patients.rd[,"released_date"]),"released_date"]<-patients.rd[is.na(patients.rd[,"released_date"]),"deceased_date"]
patients.rd[,"time"]<-patients.rd[,"confirmed_date"]-patients.rd[,"released_date"]

fit_sex <- survfit(Surv(time, state) ~ sex, data = patients.rd)
ggsurvplot(fit_sex)

patients.rd[,"age_group"]<-patien
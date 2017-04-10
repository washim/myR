library(tidyverse)
library(ggplot2)
allpatient <- read.csv("~/kaggle/cancer/PatientData.csv", stringsAsFactors = FALSE)
patientdata <- allpatient[,c("PatientID","ImagePositionPatient","ImageOrientationPatient","PixelData","SliceLocation","RescaleIntercept","RescaleSlope")]

patientdatamutate <- patientdata %>%
  mutate(ImagePositionPatient = strsplit(ImagePositionPatient, " ")) %>%
  unnest(ImagePositionPatient) %>%
  mutate(ImageOrientationPatient = strsplit(ImageOrientationPatient, " ")) %>%
  unnest(ImageOrientationPatient)

str(patientdatamutate)
patientdatamutate$ImagePositionPatient <- as.numeric(patientdatamutate$ImagePositionPatient)
patientdatamutate$ImagePositionPatient <- as.numeric(patientdatamutate$ImagePositionPatient)
patientdatamutate$ImageOrientationPatient <- as.numeric(patientdatamutate$ImageOrientationPatient)

ggplot(data = patientdatamutate, aes(x = ImagePositionPatient, fill = PatientID)) +
  geom_histogram(bins = 1,position = "dodge")

table(patientdatamutate[patientdatamutate$PatientID == "00cba091fa4ad62cc3200a657aeb957e",c("RescaleIntercept")])




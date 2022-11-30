#Import the excel "courses"

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library("readxl")
library("writexl")

df_courses <- read_excel("courses.xlsx")


#Data cleaning
library(tidyverse)

#Clean the data frame
cleaned_courses <- select(df_courses, id, course_number, semester, title_en, organisation_en, content_en)

#Remove duplicates based on content_en column
cleaned_courses <- cleaned_courses[!duplicated(df_courses$content_en), ]

#Remove rows with NA
na.omit(cleaned_courses)

#Remove rows with less than 8 characters in content_en
cleaned_courses <- subset(cleaned_courses, nchar(as.character(content_en)) > 8)

#Create excel
write_xlsx(cleaned_courses,"cleaned_courses.xlsx")

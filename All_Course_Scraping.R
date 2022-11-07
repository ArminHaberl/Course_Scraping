setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("https://raw.githubusercontent.com/ArminHaberl/Course_Scraping/main/One_Course_Scraping.R")

require(httr)
library('xml2')
library("writexl")

get_term_IDs <- function(){
  terms_url <- "https://online.uni-graz.at/kfu_online/ee/rest/slc.lib.tm/terms?$language=de"
  terms_data <- httr::GET(url = terms_url)
  terms_json <- content(terms_data)
  term_ids <- c()
  for (term in terms_json[["resource"]]) {
    new_id <- term[["content"]][["semesterLibDto"]][["id"]]
    term_ids <- c(term_ids, new_id)
  }
  return(term_ids)
}

get_course_IDs <- function(term_ID){
  courses_base_url <- "https://online.uni-graz.at/kfu_online/ee/rest/slc.tm.cp/student/courses?" 
  filter_string <- paste("$filter=courseNormKey-eq=LVEAB;orgId-eq=1;termId-eq=",term_ID,"&$orderBy=title=ascnf&",sep="")
  skip_string_init <- "$skip=0&$top=20"
  courses_url_init <- paste(courses_base_url,filter_string,skip_string_init,sep="")
  courses_data_init <- httr::GET(url = courses_url_init)
  courses_xml_init <- content(courses_data_init)
  total_course_count <- courses_xml_init[["totalCount"]]
  skip_values <- seq(0, total_course_count, by=100)
  course_IDs <- c()
  for (skip_value in skip_values) {
    skip_string <- paste("$skip=",skip_value,"&$top=100",sep="")
    course_url <- paste(courses_base_url,filter_string,skip_string,sep="")
    course_data <- httr::GET(url = course_url)
    course_xml <- content(course_data)
    for (course in course_xml[["resource"]]){
      if (is.null(course[["content"]][["cpCourseDto"]][["foreignInstanceDto"]])){
        course_IDs <-c(course_IDs,course[["content"]][["cpCourseDto"]][["id"]])}
    }
  }
  return(course_IDs)
}


# RUN Code

term_IDs <- get_term_IDs()
all_course_IDs <- c()
for (term_ID in term_IDs){
  course_IDs <- get_course_IDs(term_ID)
  all_course_IDs <<- c(all_course_IDs,course_IDs)
}

course_information <- tibble()

for (course_ID in all_course_IDs){
  new_course <- scrape_data(course_ID)
  course_information <<- bind_rows(course_information,new_course)
  Sys.sleep(0.1)
}
  
write_xlsx(course_information,"courses.xlsx")

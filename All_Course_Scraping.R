setwd("/Users/arminhaberl/Documents/Privat/Uni/MASTER/Data Management") #Adapt accordingly
source("One_Course_Scraping.R")

require(httr)
library('xml2')

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
  skip_value_init <- 0
  skip_string <- paste("$skip=",skip_value_init,"&$top=100",sep="")
  courses_url <- paste(courses_base_url,filter_string,skip_string,sep="")
  courses_data <- httr::GET(url = courses_url)
  courses_xml <- content(courses_data)
  total_course_count <- courses_xml[["totalCount"]]
  skip_values <- seq(0, total_course_count, by=100)
  course_IDs <- c()
  for (skip_value in skip_values) {
    skip_string <- paste("$skip=",skip_value,"&$top=100",sep="")
    course_url <- paste(courses_base_url,filter_string,skip_string,sep="")
    course_data <- httr::GET(url = course_url)
    course_xml <- content(course_data)
    for (course in courses_xml[["resource"]]){
      if (is.null(course[["content"]][["cpCourseDto"]][["foreignInstanceDto"]])){
        course_IDs <-c(course_IDs,course[["content"]][["cpCourseDto"]][["id"]])}
    }
  }
  return(course_IDs)
}


# RUN Code

term_IDs <- get_term_IDs()
course_IDs <- get_course_IDs(term_IDs[3])

course_information <- tibble()

for (course_ID in course_IDs){
  new_course <- scrape_data(course_ID)
  course_information <<- bind_rows(course_information,new_course)
  Sys.sleep(0.1)
}
  



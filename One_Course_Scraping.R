require(httr)
library(tidyverse)

scrape_data <- function(course_id){

  course_url <- paste("https://online.uni-graz.at/kfu_online/ee/rest/slc.tm.cp/student/courses/",course_id,sep="")    
  course_data <- httr::GET(url = course_url)
  course_json <- content(course_data)
  course_details <- course_json[["resource"]][[1]][["content"]][["cpCourseDetailDto"]]
  
  id <- course_details[["cpCourseDto"]][["id"]]
  if (is.null(id)) { id <- NA}
  
  course_number <- course_details[["cpCourseDto"]][["courseNumber"]][["databaseValue"]]
  if (is.null(course_number)) { course_number <- NA}
  
  semester <- course_details[["cpCourseDto"]][["semesterDto"]][["key"]]
  if (is.null(semester)) { semester <- NA}
  
  title_ge <- course_details[["cpCourseDto"]][["courseTitle"]][["translations"]][["translation"]][[1]][["value"]]
  if (is.null(title_ge)) { title_ge <- NA}
  
  title_en <- course_details[["cpCourseDto"]][["courseTitle"]][["translations"]][["translation"]][[2]][["value"]]
  if (is.null(title_en)) { title_en <- NA}
  
  ECTS <- course_details[["cpCourseDto"]][["ectsCredits"]]
  if (is.null(ECTS)) { ECTS <- NA}
  
  organisation_ge <- course_details[["cpCourseDto"]][["organisationResponsibleDto"]][["name"]][["translations"]][["translation"]][[1]][["value"]]
  if (is.null(organisation_ge)) { organisation_ge <- NA}
  
  organisation_en <- course_details[["cpCourseDto"]][["organisationResponsibleDto"]][["name"]][["translations"]][["translation"]][[2]][["value"]]
  if (is.null(organisation_en)) { organisation_en <- NA}
  
  type <- course_details[["cpCourseDto"]][["courseTypeDto"]][["key"]]
  if (is.null(type)) { type <- NA}
  
  language <- course_details[["cpCourseDto"]][["courseLanguageDtos"]][[1]][["languageDto"]][["key"]]
  if (is.null(language)) { language <- NA}
  
  semesterhours <- course_details[["cpCourseDto"]][["courseNormConfigs"]][[1]][["value"]]
  if (is.null(semesterhours)) { semesterhours <- NA}
  
  teacher_list <- course_details[["cpCourseDto"]][["lectureships"]]
  teachers <- ""
  for (teacher in teacher_list) {
    teacher_first_name <- teacher[["identityLibDto"]][["firstName"]]
    teacher_last_name <- teacher[["identityLibDto"]][["lastName"]]
    teacher_full_name <- paste(teacher_first_name,teacher_last_name,sep=" ")
    teachers <- paste(teachers,teacher_full_name,", ", sep="")
  } 
  
  content_ge <- course_details[["cpCourseDescriptionDto"]][["courseContent"]][["translations"]][["translation"]][[1]][["value"]]
  if (is.null(content_ge)) { content_ge <- NA}
  
  content_en <- course_details[["cpCourseDescriptionDto"]][["courseContent"]][["translations"]][["translation"]][[2]][["value"]]
  if (is.null(content_en)) { content_en <- NA}
  
  previous_knowledge_ge <- course_details[["cpCourseDescriptionDto"]][["previousKnowledge"]][["translations"]][["translation"]][[1]][["value"]]
  if (is.null(previous_knowledge_ge)) { previous_knowledge_ge <- NA}
  
  previous_knowledge_en <- course_details[["cpCourseDescriptionDto"]][["previousKnowledge"]][["translations"]][["translation"]][[2]][["value"]]
  if (is.null(previous_knowledge_en)) { previous_knowledge_en <- NA}
  
  objective_ge <- course_details[["cpCourseDescriptionDto"]][["courseObjective"]][["translations"]][["translation"]][[1]][["value"]]
  if (is.null(objective_ge)) { objective_ge <- NA}
  
  objective_en <- course_details[["cpCourseDescriptionDto"]][["courseObjective"]][["translations"]][["translation"]][[2]][["value"]]
  if (is.null(objective_en)) { objective_en <- NA}
  
  teaching_method_ge <- course_details[["cpTeachingMethodDto"]][["name"]][["translations"]][["translation"]][[1]][["value"]]
  if (is.null(teaching_method_ge)) { teaching_method_ge <- NA}
  
  teaching_method_en <- course_details[["cpTeachingMethodDto"]][["name"]][["translations"]][["translation"]][[2]][["value"]]
  if (is.null(teaching_method_en)) { teaching_method_en <- NA}
  
  assessment_method_ge <- course_details[["cpCourseDescriptionDto"]][["assessmentScheme"]][["translations"]][["translation"]][[1]][["value"]]
  if (is.null(assessment_method_ge)) { assessment_method_ge <- NA}
  
  assessment_method_en <- course_details[["cpCourseDescriptionDto"]][["assessmentScheme"]][["translations"]][["translation"]][[2]][["value"]]
  if (is.null(assessment_method_en)) { assessment_method_en <- NA}
  
  recommended_literature_ge <- course_details[["cpCourseDescriptionDto"]][["additionalInformation"]][["recommendedLiterature"]][["translations"]][["translation"]][[1]][["value"]]
  if (is.null(recommended_literature_ge)) { recommended_literature_ge <- NA}
  
  recommended_literature_en <- course_details[["cpCourseDescriptionDto"]][["additionalInformation"]][["recommendedLiterature"]][["translations"]][["translation"]][[2]][["value"]]
  if (is.null(recommended_literature_en)) { recommended_literature_en <- NA}
  
  additional_comments_ge <- course_details[["cpCourseDescriptionDto"]][["additionalInformation"]][["comments"]][["translations"]][["translation"]][[1]][["value"]]
  if (is.null(additional_comments_ge)) { additional_comments_ge <- NA}
  
  additional_comments_en <- course_details[["cpCourseDescriptionDto"]][["additionalInformation"]][["comments"]][["translations"]][["translation"]][[2]][["value"]]
  if (is.null(additional_comments_en)) { additional_comments_en <- NA}
  
  new_course <<- tibble(id, course_number, semester, title_ge, title_en, ECTS, organisation_ge, organisation_en, type, language, semesterhours, teachers, content_ge, content_en, previous_knowledge_ge,  previous_knowledge_en, objective_ge, objective_en, teaching_method_ge, teaching_method_en, assessment_method_ge,  assessment_method_en, recommended_literature_ge, recommended_literature_en,additional_comments_ge, additional_comments_en)
  
  return(new_course)
  }

#Test code here

#asdf <- scrape_data("753136")

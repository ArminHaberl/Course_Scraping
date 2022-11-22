require(httr)
library(tidyverse)

scrape_data <- function(course_id){

  course_url <- paste("https://online.uni-graz.at/kfu_online/ee/rest/slc.tm.cp/student/courses/",course_id,sep="")    
  course_data <- httr::GET(url = course_url)
  course_json <- content(course_data)
  course_details <- course_json[["resource"]][[1]][["content"]][["cpCourseDetailDto"]]
  
  tryCatch(id <- course_details[["cpCourseDto"]][["id"]],error=function(err){id <- NA} ,warning=function(err){id <- NA})
  if (is.null(id)) { id <- NA}
  
  tryCatch(course_number <- course_details[["cpCourseDto"]][["courseNumber"]][["databaseValue"]],error=function(err){course_number <- NA} ,warning=function(err){course_number <- NA})
  if (is.null(course_number)) { course_number <- NA}
  
  tryCatch(semester <- course_details[["cpCourseDto"]][["semesterDto"]][["key"]],error=function(err){semester <- NA} ,warning=function(err){semester <- NA})
  if (is.null(semester)) { semester <- NA}
  
  tryCatch(title_ge <- course_details[["cpCourseDto"]][["courseTitle"]][["translations"]][["translation"]][[1]][["value"]],error=function(err){title_ge <- NA} ,warning=function(err){title_ge <- NA})
  if (is.null(title_ge)) { title_ge <- NA}
  
  tryCatch(title_en <- course_details[["cpCourseDto"]][["courseTitle"]][["translations"]][["translation"]][[2]][["value"]],error=function(err){title_en <- NA} ,warning=function(err){title_en <- NA})
  if (is.null(title_en)) { title_en <- NA}
  
  tryCatch(ECTS <- course_details[["cpCourseDto"]][["ectsCredits"]],error=function(err){ECTS <- NA} ,warning=function(err){ECTS <- NA})
  if (is.null(ECTS)) { ECTS <- NA}
  
  tryCatch(organisation_ge <- course_details[["cpCourseDto"]][["organisationResponsibleDto"]][["name"]][["translations"]][["translation"]][[1]][["value"]],error=function(err){organisation_ge <- NA} ,warning=function(err){organisation_ge <- NA})
  if (is.null(organisation_ge)) { organisation_ge <- NA}
  
  tryCatch(organisation_en <- course_details[["cpCourseDto"]][["organisationResponsibleDto"]][["name"]][["translations"]][["translation"]][[2]][["value"]],error=function(err){organisation_en <- NA} ,warning=function(err){organisation_en <- NA})
  if (is.null(organisation_en)) { organisation_en <- NA}
  
  tryCatch(type <- course_details[["cpCourseDto"]][["courseTypeDto"]][["key"]],error=function(err){type <- NA} ,warning=function(err){type <- NA})
  if (is.null(type)) { type <- NA}
  
  tryCatch(language <- course_details[["cpCourseDto"]][["courseLanguageDtos"]][[1]][["languageDto"]][["key"]],error=function(err){language <- NA} ,warning=function(err){language <- NA})
  if (is.null(language)) { language <- NA}
  
  tryCatch(semesterhours <- course_details[["cpCourseDto"]][["courseNormConfigs"]][[1]][["value"]],error=function(err){semesterhours <- NA} ,warning=function(err){semesterhours <- NA})
  if (is.null(semesterhours)) { semesterhours <- NA}
  
  tryCatch(teacher_list <- course_details[["cpCourseDto"]][["lectureships"]],error=function(err){teacher_list <- NA} ,warning=function(err){teacher_list <- NA})
  teachers <- ""
  for (teacher in teacher_list) {
    teacher_first_name <- teacher[["identityLibDto"]][["firstName"]]
    teacher_last_name <- teacher[["identityLibDto"]][["lastName"]]
    teacher_full_name <- paste(teacher_first_name,teacher_last_name,sep=" ")
    teachers <- paste(teachers,teacher_full_name,", ", sep="")
  } 
  
  tryCatch(content_ge <- course_details[["cpCourseDescriptionDto"]][["courseContent"]][["translations"]][["translation"]][[1]][["value"]],error=function(err){content_ge <- NA} ,warning=function(err){content_ge <- NA})
  if (is.null(content_ge)) { content_ge <- NA}
  
  tryCatch(content_en <- course_details[["cpCourseDescriptionDto"]][["courseContent"]][["translations"]][["translation"]][[2]][["value"]],error=function(err){content_en <- NA} ,warning=function(err){content_en <- NA})
  if (is.null(content_en)) { content_en <- NA}
  
  tryCatch(previous_knowledge_ge <- course_details[["cpCourseDescriptionDto"]][["previousKnowledge"]][["translations"]][["translation"]][[1]][["value"]],error=function(err){previous_knowledge_ge <- NA} ,warning=function(err){previous_knowledge_ge <- NA})
  if (is.null(previous_knowledge_ge)) { previous_knowledge_ge <- NA}
  
  tryCatch(previous_knowledge_en <- course_details[["cpCourseDescriptionDto"]][["previousKnowledge"]][["translations"]][["translation"]][[2]][["value"]],error=function(err){previous_knowledge_en <- NA} ,warning=function(err){previous_knowledge_en <- NA})
  if (is.null(previous_knowledge_en)) { previous_knowledge_en <- NA}
  
  tryCatch(objective_ge <- course_details[["cpCourseDescriptionDto"]][["courseObjective"]][["translations"]][["translation"]][[1]][["value"]],error=function(err){objective_ge <- NA} ,warning=function(err){objective_ge <- NA})
  if (is.null(objective_ge)) { objective_ge <- NA}
  
  tryCatch(objective_en <- course_details[["cpCourseDescriptionDto"]][["courseObjective"]][["translations"]][["translation"]][[2]][["value"]],error=function(err){objective_en <- NA} ,warning=function(err){objective_en <- NA})
  if (is.null(objective_en)) { objective_en <- NA}
  
  tryCatch(teaching_method_ge <- course_details[["cpTeachingMethodDto"]][["name"]][["translations"]][["translation"]][[1]][["value"]],error=function(err){teaching_method_ge <- NA} ,warning=function(err){teaching_method_ge <- NA})
  if (is.null(teaching_method_ge)) { teaching_method_ge <- NA}
  
  tryCatch(teaching_method_en <- course_details[["cpTeachingMethodDto"]][["name"]][["translations"]][["translation"]][[2]][["value"]],error=function(err){teaching_method_en <- NA} ,warning=function(err){teaching_method_en <- NA})
  if (is.null(teaching_method_en)) { teaching_method_en <- NA}
  
  tryCatch(assessment_method_ge <- course_details[["cpCourseDescriptionDto"]][["assessmentScheme"]][["translations"]][["translation"]][[1]][["value"]],error=function(err){assessment_method_ge <- NA} ,warning=function(err){assessment_method_ge <- NA})
  if (is.null(assessment_method_ge)) { assessment_method_ge <- NA}
  
  tryCatch(assessment_method_en <- course_details[["cpCourseDescriptionDto"]][["assessmentScheme"]][["translations"]][["translation"]][[2]][["value"]],error=function(err){assessment_method_en <- NA} ,warning=function(err){assessment_method_en <- NA})
  if (is.null(assessment_method_en)) { assessment_method_en <- NA}
  
  tryCatch(recommended_literature_ge <- course_details[["cpCourseDescriptionDto"]][["additionalInformation"]][["recommendedLiterature"]][["translations"]][["translation"]][[1]][["value"]],error=function(err){recommended_literature_ge <- NA} ,warning=function(err){recommended_literature_ge <- NA})
  if (is.null(recommended_literature_ge)) { recommended_literature_ge <- NA}
  
  tryCatch(recommended_literature_en <- course_details[["cpCourseDescriptionDto"]][["additionalInformation"]][["recommendedLiterature"]][["translations"]][["translation"]][[2]][["value"]],error=function(err){recommended_literature_en <- NA} ,warning=function(err){recommended_literature_en <- NA})
  if (is.null(recommended_literature_en)) { recommended_literature_en <- NA}
  
  tryCatch(additional_comments_ge <- course_details[["cpCourseDescriptionDto"]][["additionalInformation"]][["comments"]][["translations"]][["translation"]][[1]][["value"]],error=function(err){additional_comments_ge <- NA} ,warning=function(err){additional_comments_ge <- NA})
  if (is.null(additional_comments_ge)) { additional_comments_ge <- NA}
  
  tryCatch(additional_comments_en <- course_details[["cpCourseDescriptionDto"]][["additionalInformation"]][["comments"]][["translations"]][["translation"]][[2]][["value"]],error=function(err){additional_comments_en <- NA} ,warning=function(err){additional_comments_en <- NA})
  if (is.null(additional_comments_en)) { additional_comments_en <- NA}
  
  new_course <- tibble(id, course_number, semester, title_ge, title_en, ECTS, organisation_ge, organisation_en, type, language, semesterhours, teachers, content_ge, content_en, previous_knowledge_ge,  previous_knowledge_en, objective_ge, objective_en, teaching_method_ge, teaching_method_en, assessment_method_ge,  assessment_method_en, recommended_literature_ge, recommended_literature_en,additional_comments_ge, additional_comments_en)
  
  return(new_course)
  }

#some_course <- scrape_data("747861")

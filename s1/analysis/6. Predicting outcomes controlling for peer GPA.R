mean_analysis_data = analysis_data %>% 
  group_by(StudyID,school,eth) %>% 
  summarise_all_bf_nominee_gpa + pn_scw_nominee_gpa,mean_analysis_data),
  type = "latex"
  )
(mean)

stargazer::stargazer(
  lm(coregpa ~ scw_s,mean_analysis_data),
  lm(coregpa ~ scw_s  + pn_scw_nominee_gpa,mean_analysis_data),
  lm(coregpa ~ scw_s + pdfdfn_bf_nominee_gpa,mean_analysis_data),
  lm(coregpa ~ scw_s + pn
full <- lm(coregpa ~ scw_s + pn_bf_nominee_gpa + pn_scw_nominee_gpa,mean_analysis_data)

library(survey)
#regTermTest function comes from survey package
regTermTest(full, ~pn_bf_nominee_gpa + pn_scw_nominee_gpa, method="Wald")
regTermTest(full, ~pn_bf_nominee_gpa + pn_scw_nominee_gpa, method="LRT")

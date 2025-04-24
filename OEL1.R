library(tidyverse)
View(student)

avg <- mutate(student, Average = (Math_Score + Chemistry_Score + Physics_Score)/3)
view(avg)

fil <- avg %>% filter(Gender == 'Female') %>% mutate(Average > 75 & Attendance_Percentage > 80)
view(fil)

dis <- avg %>% select(Department, Math_Score, Chemistry_Score, Physics_Score) %>%
               group_by(Department) %>% summarise(
                                      Math_Score = mean(Math_Score, na.rm = TRUE),
                                     Physics_Score = mean(Physics_Score, na.rm = TRUE),
                                Chemistry_Score = mean(Chemistry_Score, na.rm = TRUE)
                                )
view(dis)

des <- avg %>% arrange(desc(Average)) %>% slice(1:5)
view(des)

PC <- avg %>% 
  mutate(Performance_Category = ifelse(Average >= 85,'Excellent',
                                ifelse(Average >= 70 & Average < 80,'Good','Needs_Improvement')))
view(PC)

c <- PC %>% group_by(Performance_Category) %>% summarise(n())
view(c)

wh <- PC %>% select(Name, Department, Performance_Category, Attendance_Percentage) %>% 
             filter(Attendance_Percentage < 60)
view(wh)

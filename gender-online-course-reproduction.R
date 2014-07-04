library(dplyr)

data <- droplevels(tbl_df(read.csv("HMXPC13_DI_v2_5-14-14.csv", header=T)))

### Need to extract course code from course ID to compress courses with multiple semesters

data <- data %>% mutate(
  course_id = as.character(lapply(as.character(course_id), function(x) { 
    return (strsplit(x,"/")[[1]][2])
  }))
)

course_names <- data.frame(course_id=c("CB22x", "CS50x", "ER22x", "PH207x","PH278x","14.73x","2.01x","3.091x","6.002x","6.00x","7.00x","8.02x","8.MReV"),
                          school=c(rep("Harvard",5), rep("MIT",8)),
                          name=c("The Ancient Greek Hero", "Intro to Computer Science", "Justice", "Health in Numbers", "Human Health & Environment", "Challenges of Global Poverty", "Elements of Structures", "Intro to Solid State Chemistry","Circuits and Electronics","Intro to CS/Programming","Intro to Biology","Electricity and Magnetism","Mechanics Review"))

by_course_gender <- data %>%
  filter(gender=='m' | gender=='f') %>%
  group_by(course_id) %>%
  summarize(perc_f = sum(gender=='f') / (sum(gender=='m') + sum(gender=='f')))

by_course_gender <- merge(by_course_gender, course_names)
by_course_gender <- tbl_df(by_course_gender) %>% arrange(desc(perc_f))

print(by_course_gender)

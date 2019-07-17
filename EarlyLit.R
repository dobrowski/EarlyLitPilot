# Present September 19 


library(tidyverse)
library(ggthemes)


cols <- c("Timestamp", "FormalAssessment", "Difference", "Role", "Assessment", "Grouping", "Tailor", "OftenGroup", "OftenAssess", "DescribeGroup", "Changed", "Expand", "test" )
cols2 <- c("Timestamp", "FormalAssessment", "Difference", "Role", "Assessment", "Grouping", "Tailor", "OftenGroup", "OftenAssess", "DescribeGroup", "hope", "test" )
levs <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
stud.levs <- c("Pre-A", "A",     "B",     "C",     "D",     "E",     "F",     "G",    
                "H",     "I",     "J",     "K" ,    "K+",    "L",     "M",     "N",    
                "O",     "P",     "Q",     "R" ,    "S" ,    "T",     "U",     "V")


Tpost <- read_csv("MC, BF Goal 3 Early Literacy Pilot (Post) (Responses) - Form Responses 1.csv") %>%
    mutate(test = "post") 

Tpre <- read_csv("MC, BF Goal 3 Early Literacy Pilot (Pre) (Responses) - Form Responses 1.csv") %>%
    mutate(test = "pre")


colnames(Tpost) <- cols 
colnames(Tpre) <- cols2 



Student <- read_csv("StudentScores.csv") %>%
    gather() %>%
    mutate( value =  factor(value, levels = stud.levs))  


Student.change <- read_csv("StudentScores.csv") %>%
    mutate_all( list(~ factor(., levels = stud.levs)) )  %>% 
    mutate(change = as.numeric(POST) - as.numeric(PRE))



teacher <- Tpost %>%
    bind_rows(Tpre) %>%
    mutate_at(vars(FormalAssessment:Tailor), funs( str_to_title(.)  ) ) %>%
    mutate_at(vars(FormalAssessment:Tailor), funs(factor(., levels = levs)) )


ggplot(Student) +
    geom_histogram(stat="count", aes(value, fill = key), position = position_dodge2(preserve = "single", reverse = TRUE)) +
    scale_fill_manual(values=c( "#E69F00", "#999999")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    geom_vline( aes(xintercept = 15)) +
    labs(title = "Students Showed Growth on the Fountas-Pinnell",
         x = "",
         y = "Number of Students",
         fill = "") +
    theme_hc()

ggsave("student.png")

ggplot(Student.change) +
    geom_histogram(stat="count", aes(change, fill = "#E69F00"), position = position_dodge2(preserve = "single", reverse = TRUE)) +
    scale_fill_manual(values=c( "#E69F00", "#999999")) +
    guides(fill = FALSE) +
#    geom_vline( aes(xintercept = 15)) +
    labs(title = "The Median and Average Student Grew 6 Levels \non the Fountas-Pinnell",
         x = "Levels of Growth",
         y = "Number of Students",
         fill = "") +
    theme_hc()

ggsave("studentchange.png")



tit <- "More teachers better understand the role formal \nassessments play in instructional decision making."

ggplot(teacher) +
    geom_histogram(stat="count", aes(Difference, fill = test), position = position_dodge2(preserve = "single", reverse = TRUE)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = tit,
         x = "",
         y = "Number of Teachers",
         fill = "") +
    theme_hc()


teacher.plot <- function(var,tit){
   var <- sym(var)
    
     ggplot(teacher) +
        geom_histogram(stat="count",
                       aes(!!var, fill = test),
                       position = position_dodge2(preserve = "single", reverse = TRUE)) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values=c( "#E69F00", "#999999")) +
        labs(title = tit,
             x = "",
             y = "Number of Teachers",
             fill = "") +
        theme_hc()
    
ggsave(paste0(var,".png"))     
}

teacher.plot("FormalAssessment", "Teachers better understand the role formal \nassessments play in instructional decision making.")
teacher.plot("Difference", "Teachers better understand the difference between \nwhole and small group instruction.")
teacher.plot("Role", "Teachers better understand the role of small \ngroup instruction.")
teacher.plot("Assessment", "Teachers are more confident in using formal \nassessments to make instructional decisions.")
teacher.plot("Grouping", "Teachers are more confident in grouping students \nfor small group instruction.")
teacher.plot("Tailor", "Teachers are more confident in their ability to \ntailor small group lessons based on student need.")




teacher$Expand[c(3,9)]

teacher$Changed[c(3,5,9)]




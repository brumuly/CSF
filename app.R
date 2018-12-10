#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(plotly)
library(ggplot2)
library(Rcpp)
d1 <- read.delim("CSFData1.csv")
d1 <- as.data.frame(d1)
d2 <- select(d1, 13, 3, 7)
d3 <- reshape(d2, dir = "wide", idvar = "InstitutionStudentSurveys__student_id", timevar = "SurveyQuestions__code")
d3 <-d3[,c(1,6,4,10,7, 2, 9, 11, 8, 3, 5)]
colnames(d3) <- c("ID", "Survey_Question1", "Survey_Question2", "Survey_Question3", "Survey_Question4", "Survey_Question5", "Survey_Question6", "Survey_Question7", "Survey_Question8", "Survey_Question9", "Survey_Question10")

# reassign terminology
d3$Survey_Question2[d3$Survey_Question2 == "96"] <- "Male"
d3$Survey_Question2[d3$Survey_Question2 == "97"] <- "Female"
d3$Survey_Question3[d3$Survey_Question3 == "98"] <- "Grade 1"
d3$Survey_Question3[d3$Survey_Question3 == "99"] <- "Grade 2"
d3$Survey_Question3[d3$Survey_Question3 == "100"] <- "Grade 3"
d3$Survey_Question3[d3$Survey_Question3 == "101"] <- "Grade 4"
d3$Survey_Question3[d3$Survey_Question3 == "102"] <- "Grade 5"
d3$Survey_Question3[d3$Survey_Question3 == "103"] <- "Grade 6"
d3$Survey_Question4[d3$Survey_Question4 == "106"] <- "Advanced"
d3$Survey_Question4[d3$Survey_Question4 == "105"] <- "Intermediate"
d3$Survey_Question4[d3$Survey_Question4 == "104"] <- "Beginner"
d3$Survey_Question5[d3$Survey_Question5 == "107"] <- "Once a week"
d3$Survey_Question5[d3$Survey_Question5 == "108"] <- "Once every two weeks"
d3$Survey_Question5[d3$Survey_Question5 == "109"] <- "Once a month"
d3$Survey_Question5[d3$Survey_Question5 == "110"] <- "Once every two months"
d3$Survey_Question5[d3$Survey_Question5 == "111"] <- "Almost never"
d3$Survey_Question6[d3$Survey_Question6 == "112"] <- "High"
d3$Survey_Question6[d3$Survey_Question6 == "113"] <- "Middle"
d3$Survey_Question6[d3$Survey_Question6 == "114"] <- "Low"
d3$Survey_Question7[d3$Survey_Question7 == "115"] <- "Never"
d3$Survey_Question7[d3$Survey_Question7 == "116"] <- "1-30 minutes"
d3$Survey_Question7[d3$Survey_Question7 == "117"] <- "30-60 minutes"
d3$Survey_Question7[d3$Survey_Question7 == "118"] <- "60-90 minutes"
d3$Survey_Question7[d3$Survey_Question7 == "119"] <- "More than 90 minutes"
d3$Survey_Question8[d3$Survey_Question8 == "120"] <- "Yes"
d3$Survey_Question8[d3$Survey_Question8 == "121"] <- "No"
d3$Survey_Question9[d3$Survey_Question9 == "122"] <- "Never attended"
d3$Survey_Question9[d3$Survey_Question9 == "123"] <- "< than = to 3 months"
d3$Survey_Question9[d3$Survey_Question9 == "124"] <- "4-6 months"
d3$Survey_Question9[d3$Survey_Question9 == "125"] <- "7-9 months"
d3$Survey_Question9[d3$Survey_Question9 == "126"] <- "> than = to 10 months"

c8 <- table(d3$Survey_Question9, useNA = "ifany")
c8 <- as.data.frame(table(d3$Survey_Question9, useNA = "ifany"))
colnames(c8)[1] <- "Months"

d4 <- d3
d4$Survey_Question10[d4$Survey_Question10 > "89"] <- "A"
d4$Survey_Question10[d4$Survey_Question10 > "79" & d4$Survey_Question10 < "90"] <- "B"
d4$Survey_Question10[d4$Survey_Question10 > "69" & d4$Survey_Question10 < "80"] <- "C"
d4$Survey_Question10[d4$Survey_Question10 > "59" & d4$Survey_Question10 < "70"] <- "D"
d4$Survey_Question10[d4$Survey_Question10 < "59"] <- "F"

d4$Survey_Question1 <- as.factor(d4$Survey_Question1)
d4$Survey_Question2 <- as.factor(d4$Survey_Question2)
d4$Survey_Question3 <- as.factor(d4$Survey_Question3)
d4$Survey_Question4 <- as.factor(d4$Survey_Question4)
d4$Survey_Question5 <- as.factor(d4$Survey_Question5)
d4$Survey_Question6 <- as.factor(d4$Survey_Question6)
d4$Survey_Question7 <- as.factor(d4$Survey_Question7)
d4$Survey_Question8 <- as.factor(d4$Survey_Question8)
d4$Survey_Question9 <- as.factor(d4$Survey_Question9)
d4$Survey_Question10 <- as.factor(d4$Survey_Question10)
d5 <- subset(d3, select=c("Survey_Question1", "Survey_Question10"))
d5$Survey_Question1 <- as.numeric(d5$Survey_Question1)
d5$Survey_Question10 <- as.numeric(d5$Survey_Question10)
colnames(d5)[1] <- ("Age")
colnames(d5)[2] <- ("Math Score")

ui <- fluidPage( titlePanel("CSF Survey Data Analysis"),
  plotlyOutput("plot"), plotlyOutput("plot2"),
  verbatimTextOutput("event")
)

server <- function(input, output) {
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    v <- plot_ly(x = d5$Age, y = d5$`Math Score`, mode = "markers")  %>%
      layout(title = 'Scatterplot of Math Score and Age',
             xaxis = list(title = 'Age',
                          zeroline = TRUE,
                          range = c(6, 14)),
             yaxis = list(title = 'Math Score',
                          range = c(45, 105)))
  })
  output$plot2 <- renderPlotly({
    T8 <- ggplot(c8, aes(Months, Freq)) +   
      geom_bar(aes(fill = Months, color=Months), position = "dodge", stat="identity")+ theme(axis.text.x=element_text(angle=90, hjust=1))
    t8=ggplotly(T8 + ggtitle("Frequency of Months Students Attended Extra Mathematics classes"), tooltip = c("x","y"))})
  
  output <- renderPrint({
    
  }) 

}


shinyApp(ui = ui, server = server)
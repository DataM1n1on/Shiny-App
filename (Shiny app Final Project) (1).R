setwd("C:\\Users\\DELL\\Desktop\\stats 206 datasets")

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("shiny")) install.packages("shiny")
if (!require("DT")) install.packages("DT")
if (!require("readxl")) install.packages("readxl")
if (!require("janitor")) install.packages("janitor")
if (!require("corrplot")) install.packages("corrplot")

library(readxl)
library(shiny)
library(tidyverse)
library(DT)
library(janitor)
library(corrplot)

data_1<-read_excel("Counties Data Set 1.xlsx")  
data_2<-read_excel("Counties Data Set 2.xlsx") 
data_3<-read_excel("Counties Data Set 3.xlsx") 

data_1<-clean_names(data_1)
data_2<-clean_names(data_2)
data_3<-clean_names(data_3)

data_2<-data_2%>%dplyr::rename(
  democrat=democrate)
data_3<-data_3%>%dplyr::rename(
  pop_density=population_density)

complete_data<-bind_rows(data_1,data_2,data_3)

complete_data<-na.omit(complete_data)

state_sd_mean<-complete_data%>%group_by(state)%>%
  summarize(mean_pop_density=round(mean(pop_density)),sd_pop_density=round(sd(pop_density)),
            mean_pop=round(mean(pop)),sd_pop=round(sd(pop)),mean_dem=round(mean(democrat)),
            sd_dem=round(sd(democrat)),mean_repub=round(mean(republican)),
            sd_repub=round(sd(republican)),mean_white=round(mean(white)),
            sd_white=round(sd(white)),mean_black=round(mean(black)),
            sd_black=round(sd(black)),mean_turnout=round(mean(turnout)),
            sd_turnout=round(sd(turnout)))

state_sd_mean<-complete_data%>%group_by(state)%>%
  summarize(mean_pop_density=round(mean(pop_density)),sd_pop_density=round(sd(pop_density)),
            mean_pop=round(mean(pop)),sd_pop=round(sd(pop)),mean_dem=round(mean(democrat)),
            sd_dem=round(sd(democrat)),mean_repub=round(mean(republican)),
            sd_repub=round(sd(republican)),mean_white=round(mean(white)),
            sd_white=round(sd(white)),mean_black=round(mean(black)),
            sd_black=round(sd(black)),mean_turnout=round(mean(turnout)),
            sd_turnout=round(sd(turnout)))
           
state_sd_mean<-state_sd_mean%>%
  fill(c(sd_pop_density,sd_pop,sd_dem,sd_repub,sd_white, sd_black, sd_turnout), 
                     .direction="updown")

mean_bar<-complete_data%>%group_by(state)%>%summarize(mean_white=mean(white), mean_black=mean(black))%>%
  gather(average, value, -c(state))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel( numericInput(inputId =  "num",
                               label = "No. of Rows",
                               value = NROW(state_sd_mean),
                               min = 0),
                  selectInput("variable", "Select Variable (Boxplot):", 
                              c("Population Size" = "pop", 
                                "Population Density" = "pop_density", 
                                "Democrats" = "democrat",
                                "Republicans" = "republican", 
                                "White Population" = "white", 
                                "Black Population" = "black", 
                                "Turnout" = "turnout"),"white"),
                  selectInput("density", "Select Variable (Density Plot):",
                              c("Population Change" = "pop_change",
                                "Age 65-74" = "age6574",
                                "Age75" = "age75", 
                                "College" = "college",
                                "Farm" = "farm",
                                "Democrats" = "democrat",
                                "Republicans" = "republican",
                                "Black Population" = "black",
                                "Turnnout" = "turnout", 
                                "White Population" = "white")),
                  selectInput("corr1", "Select Continuous Variable 1 (Corrplot):",
                              c("Population Change" = "pop_change",
                                "Age 65-74" = "age6574",
                                "Age75" = "age75", 
                                "College" = "college",
                                "Farm" = "farm",
                                "Democrats" = "democrat",
                                "Republicans" = "republican",
                                "Black Population" = "black",
                                "Turnnout" = "turnout", 
                                "White Population" = "white"),"black"),
                  selectInput("corr2", "Select Continuous Variable 2 (Corrpot):",
                             c("Population Change" = "pop_change",
                               "Age 65-74" = "age6574",
                               "Age75" = "age75", 
                               "College" = "college",
                               "Farm" = "farm",
                               "Democrats" = "democrat",
                               "Republicans" = "republican",
                               "Black Population" = "black",
                               "Turnnout" = "turnout", 
                               "White Population" = "white"),"white"),
                  selectInput("cluster", "Select State (Bar Chart):", 
                              choices = levels(as.factor(mean_bar$state)))),
               
    mainPanel(tabsetPanel(
      tabPanel(h5("Rounded off to the Nearest Unit"), title="Data",
               downloadButton(outputId = "download",label = "Download"),
               DT :: DTOutput(outputId = "mytable")),
      
      tabPanel(title="Boxplot",plotOutput("plot")),
      tabPanel(title="Continuous Variable Analysis",plotOutput("densityPlot"),  
               plotOutput("correlationPlot")),
      tabPanel(title="Bar Plot", plotOutput("bar_chart"))
   )
 ))
)

# Define server logic required
server <- function(input, output) {
  output$mytable<- DT :: renderDT({
    head(state_sd_mean,n=input$num)
  })
  
  output$plot <- renderPlot({
    ggplot(complete_data, aes_string(input$variable)) + 
      geom_boxplot(fill = "light blue", lwd = 1)
  })
  
  output$densityPlot <- renderPlot({
    ggplot(complete_data, aes_string(input$density)) + 
      geom_density(fill = "light blue", lwd = 1.15) + 
      ggtitle(paste0("Density Plot for ", input$density))
  })
  
  output$correlationPlot <- renderPlot({
    cor_matrix <- cor(complete_data[, c(input$corr1, input$corr2)])
    corrplot(cor_matrix, method = "number", order="alphabet")
  })
  
  filtered_data <- reactive({
    mean_bar %>% filter(state == input$cluster)
  })
  output$bar_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = input$cluster, y = value, fill = average)) +
      geom_bar(stat = "identity", position = "dodge") +
      ggtitle(paste0( input$cluster,"State Bar Chart for mean black and white population comparision")) +
      xlab("state") +
      ylab("Value")
  })
  output$download<- downloadHandler(filename = "American Voting Statistics.csv",
                                    content = function(file){
                                      write.csv( head(state_sd_mean,n=input$num),file)
                                    })  
}

shinyApp(ui = ui, server = server)





# set up working directory, change as needed
#setwd("//homerfp01/FolderRedirection/AlexC/Documents/work/connect/")


# load what we need --------------------------


# load packages
library(shiny)
library(tidyverse)
library(tidybayes)
library(rstantools)
library(plotly)
library(shinydashboard)
library(rstanarm)



# load info needed
load("m_smfq_8_stan_nodata.RData")
avg_pred_data <- read_csv("avg_pred_data_smfq.csv")
m8_stan_sum_results <- read_csv("m8_stan_results.csv") %>% 
    select(term, odds, odds_lci, odds_uci)

valid_ages <- c(10, 12, 13, 17, 17, 19, 21, 22, 23)

age_df <- tibble(age = rep(valid_ages, 2),
                 uniqid = rep(c("Scenario 1", "Scenario 2"), 
                              each = length(valid_ages)))

text_page1 <- read_lines("text_page1.txt") %>% 
    str_c(collapse = "\n")

text_results <- read_lines("text_results.txt") %>% 
    str_c(collapse = "\n")

# dashboard ---------------------------


ui <- dashboardPage(
    dashboardHeader(title = "Wellcome social connection dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "intro"),
            menuItem("Analysis results", tabName = "analysis", 
                     icon = icon("book")),
            menuItem("Scenario exploration", tabName = "scenario",
                     icon = icon("dashboard"))
        )
    ),
    
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "intro",
                    HTML(text_page1)
            ),
            tabItem(tabName = "analysis",
                    
                    fluidRow(
                        box(
                            plotlyOutput("fig", 
                                         height = 500,
                                         width = 700), 
                            width = 12
                        )
                    ),
                    fluidRow(HTML(text_results), width = 12),
                    fluidRow(
                        box(dataTableOutput("m8fit"), width = 12),
                    )
            ),
            
            tabItem(tabName = "scenario",
                    
                    fluidRow(
                        box(
                            selectInput("female1", "Sex", 
                                        c("Female", "Male"), 
                                        selected = "Female"),
                            selectInput("white1", "Ethnicity", 
                                        c("White", "Other"), 
                                        selected = "White"),
                            selectInput("healthy_very_81", "Very healthy (age 8)", 
                                        c("Yes", "No"), selected = "Yes"),
                            selectInput("mom_edu_fct1", "Mother education", 
                                        c("Degree", "A level", "Other"), 
                                        selected = "Degree"),
                            selectInput("siblings1", "Has siblings", 
                                        c("Yes", "No"), "Yes"),
                            
                            
                            
                            checkboxGroupInput("events1", "Event happened", 
                                               c("Grandparents looked after (age 0)",
                                                 "Mother had depression",
                                                 "Bullied (age 8)",
                                                 "Bullied (age 10)",
                                                 "Romantic relationship (age 13)",
                                                 "Romantic relationship (age 17)"), 
                                               selected = 
                                                   c("Grandparents looked after (age 0)",
                                                     "Romantic relationship (age 13)",
                                                     "Romantic relationship (age 17)")),
                            
                            checkboxGroupInput("school1", "School connections", 
                                               c("Always likes teacher (ages 4-8)",
                                                 "Always get on with classmates",
                                                 "Never feel lonely at school (ages 11-14)"), 
                                               selected = 
                                                   c("Always likes teacher (ages 4-8)",
                                                     "Always get on with classmates",
                                                     "Never feel lonely at school (ages 11-14)")),
                            
                            
                            selectInput("friends_score_avg1", 
                                        "Friends score", 
                                        c("High", "Medium", "Low"), 
                                        selected = "High"),
                            selectInput("peer_problem_score_avg1", 
                                        "Peer problem score", 
                                        c("High", "Medium", "Low"), 
                                        selected = "Low"),
                            selectInput("social_cohesion_avg1", 
                                        "Social cohesion score", 
                                        c("High", "Medium", "Low"), 
                                        selected = "High"),
                            selectInput("parent_networks_mother_social_support_score_avg1", 
                                        "Mother social support score", 
                                        c("High", "Medium", "Low"), 
                                        selected = "High"),
                            
                            width = 3,
                            title = "Scenario 1", status = "primary", 
                            solidHeader = TRUE,
                        ),
                        
                        box(
                            selectInput("female2", "Sex", 
                                        c("Female", "Male"), 
                                        selected = "Male"),
                            selectInput("white2", "Ethnicity", 
                                        c("White", "Other"), 
                                        selected = "Other"),
                            selectInput("healthy_very_82", "Very healthy (age 8)", 
                                        c("Yes", "No"), selected = "No"),
                            selectInput("mom_edu_fct2", "Mother education", 
                                        c("Degree", "A level", "Other"), 
                                        selected = "Other"),
                            selectInput("siblings2", "Has siblings", 
                                        c("Yes", "No"), "No"),
                            
                            
                            
                            checkboxGroupInput("events2", "Event happened", 
                                               c("Grandparents looked after (age 0)",
                                                 "Mother had depression",
                                                 "Bullied (age 8)",
                                                 "Bullied (age 10)",
                                                 "Romantic relationship (age 13)",
                                                 "Romantic relationship (age 17)"), 
                                               selected = 
                                                   c("Mother had depression",
                                                     "Bullied (age 8)",
                                                     "Bullied (age 10)")),
                            
                            checkboxGroupInput("school2", "School connections", 
                                               c("Always likes teacher (ages 4-8)",
                                                 "Always get on with classmates",
                                                 "Never feel lonely at school (ages 11-14)")),
                            
                            
                            selectInput("friends_score_avg2", 
                                        "Friends score", 
                                        c("High", "Medium", "Low"), 
                                        selected = "Low"),
                            selectInput("peer_problem_score_avg2", 
                                        "Peer problem score", 
                                        c("High", "Medium", "Low"), 
                                        selected = "High"),
                            selectInput("social_cohesion_avg2", 
                                        "Social cohesion score", 
                                        c("High", "Medium", "Low"), 
                                        selected = "Low"),
                            selectInput("parent_networks_mother_social_support_score_avg2", 
                                        "Mother social support score", 
                                        c("High", "Medium", "Low"), 
                                        selected = "Low"),
                            
                            width = 3,
                            title = "Scenario 2", status = "primary", solidHeader = TRUE,
                        ),
                        box(
                            plotOutput("plot2"), 
                            plotOutput("plot3"),
                            width = 5,
                            title = "Visualization", status = "warning", solidHeader = TRUE,
                        )
                        
                        
                    ),
                    fluidRow(box(actionButton("estimate", "Calculate trajectories",
                                              class="btn btn-success", width = "270"), 
                                 width = 3),
                             box(
                                 title = "Warning", width = 3, background = "maroon",
                                 "Takes ~ 2 minutes to estimate. ",
                                 "New graph should appear on the right."
                             ))
            )
        )))



server <- function(input, output) {
    
    output$fig <- renderPlotly({
        fig_test <- m8_stan_sum_results %>% 
            mutate(term = factor(term, levels = m8_stan_sum_results$term) %>%
                       fct_rev()) %>% 
            ggplot(aes(odds, term, xmin = odds_lci, xmax = odds_uci,
                       text = paste("variable:", term, "\n",
                                    "odds:", round(odds, 2), "\n",
                                    "lci:", round(odds_lci, 2), "\n",
                                    "uci:", round(odds_lci, 2)))) +
            geom_pointrange() +
            theme_bw() +
            geom_vline(xintercept = 1) +
            labs(x = "Odds", y = "Predictors")
        
        
        ggplotly(fig_test, tooltip = "text")
    })
    
    output$m8fit <- renderDataTable({
        mutate_if(m8_stan_sum_results, is.numeric, ~round(., 2))
    })
    
    
    scenario1_react <- reactive({
        
        tibble(
            uniqid = "Scenario 1",
            female = ifelse(input$female1 == "Female", 1, 0),
            white = ifelse(input$white1 == "White", 1, 0),
            mom_edu_fct = input$mom_edu_fct1,
            healthy_very_8 = ifelse(input$healthy_very_81 == "Yes", 1, 0),
            mom_depression = ifelse("Mother had depression" %in% 
                                        input$events1,
                                    1, 0), 
            siblings = ifelse(input$siblings1 == "Yes", 1, 0),
            bullied_8 = 
                ifelse("Bullied (age 8)" %in% 
                           input$events1,
                       1, 0),
            bullied_10 = 
                ifelse("Bullied (age 10)" %in% 
                           input$events1,
                       1, 0),
            school_likes_teacher_always = 
                ifelse("Always likes teacher (ages 4-8)" %in% input$school1,
                       1, 0), 
            school_get_on_with_classmates_always = 
                ifelse("Always get on with classmates" %in% input$school1,
                       1, 0),  
            school_feel_lonely_never =  
                ifelse("Never feel lonely at school (ages 11-14)" %in% 
                           input$school1,
                       1, 0), 
            romantic_13 = 
                ifelse("Romantic relationship (age 13)" %in% 
                           input$events1,
                       1, 0), 
            romantic_17 = 
                ifelse("Romantic relationship (age 14)" %in% 
                           input$events1,
                       1, 0), 
            childcare_grandparent_looks_after_ch_0 = 
                ifelse("Grandparents looked after (age 0)" %in% 
                           input$events1,
                       1, 0),
            friends_score_avg = 
                case_when(input$friends_score_avg1 == "High" ~ 11.25,
                          input$friends_score_avg1 == "Medium" ~ 8,
                          input$friends_score_avg1 == "Low" ~ 6), 
            peer_problem_score_avg = 
                case_when(input$peer_problem_score_avg1 == "High" ~ 3.6,
                          input$peer_problem_score_avg1 == "Medium" ~ 0.833,
                          input$peer_problem_score_avg1 == "Low" ~ 0),
            social_cohesion_avg = 
                case_when(input$social_cohesion_avg1 == "High" ~ 20,
                          input$social_cohesion_avg1 == "Medium" ~ 14.28,
                          input$social_cohesion_avg1 == "Low" ~ 9),
            parent_networks_mother_social_support_score_avg = 
                case_when(input$parent_networks_mother_social_support_score_avg1 == "High" ~ 26.16,
                          input$parent_networks_mother_social_support_score_avg1 == "Medium" ~ 20.37,
                          input$parent_networks_mother_social_support_score_avg1 == "Low" ~ 12.6)
        )
    })
    
    scenario2_react <- reactive({
        tibble(
            uniqid = "Scenario 2",
            female = ifelse(input$female2 == "Female", 1, 0),
            white = ifelse(input$white2 == "White", 1, 0),
            mom_edu_fct = input$mom_edu_fct2,
            healthy_very_8 = ifelse(input$healthy_very_82 == "Yes", 1, 0),
            mom_depression = ifelse("Mother had depression" %in% 
                                        input$events2,
                                    1, 0), 
            siblings = ifelse(input$siblings2 == "Yes", 1, 0),
            bullied_8 = 
                ifelse("Bullied (age 8)" %in% 
                           input$events2,
                       1, 0),
            bullied_10 = 
                ifelse("Bullied (age 10)" %in% 
                           input$events2,
                       1, 0),
            school_likes_teacher_always = 
                ifelse("Always likes teacher (ages 4-8)" %in% input$school2,
                       1, 0), 
            school_get_on_with_classmates_always = 
                ifelse("Always get on with classmates" %in% input$school2,
                       1, 0),  
            school_feel_lonely_never =  
                ifelse("Never feel lonely at school (ages 11-14)" %in% 
                           input$school2,
                       1, 0), 
            romantic_13 = 
                ifelse("Romantic relationship (age 13)" %in% 
                           input$events2,
                       1, 0), 
            romantic_17 = 
                ifelse("Romantic relationship (age 14)" %in% 
                           input$events2,
                       1, 0), 
            childcare_grandparent_looks_after_ch_0 = 
                ifelse("Grandparents looked after (age 0)" %in% 
                           input$events2,
                       1, 0),
            friends_score_avg = 
                case_when(input$friends_score_avg2 == "High" ~ 11.25,
                          input$friends_score_avg2 == "Medium" ~ 8,
                          input$friends_score_avg2 == "Low" ~ 6), 
            peer_problem_score_avg = 
                case_when(input$peer_problem_score_avg2 == "High" ~ 3.6,
                          input$peer_problem_score_avg2 == "Medium" ~ 0.833,
                          input$peer_problem_score_avg2 == "Low" ~ 0),
            social_cohesion_avg = 
                case_when(input$social_cohesion_avg2 == "High" ~ 20,
                          input$social_cohesion_avg2 == "Medium" ~ 14.28,
                          input$social_cohesion_avg2 == "Low" ~ 9),
            parent_networks_mother_social_support_score_avg = 
                case_when(input$parent_networks_mother_social_support_score_avg2 == "High" ~ 26.16,
                          input$parent_networks_mother_social_support_score_avg2 == "Medium" ~ 20.37,
                          input$parent_networks_mother_social_support_score_avg2 == "Low" ~ 12.6)
        )
    })
    
    
    output$plot2 <- renderPlot({
        ggplot(avg_pred_data, aes(age, .epred, group = uniqid, 
                                  color = uniqid)) +
            geom_smooth(data = avg_pred_data, 
                        aes(age, .epred, 
                            group = as.character("Predicted mean\n probability"),
                            linetype = as.character("Predicted mean\n probability")), 
                        se = F, color = "black", size = 1.5) +
            theme_bw() +
            labs(x = "Age", y = "Predicted probability of depression (SMFQ)",
                 color = "", linetype = "",
                 fill = "Confidence level") +
            scale_y_continuous(limits = c(0, 0.3)) 
        
        
    })
    
    
    
    
    scenario1_pred <- eventReactive(input$estimate, {
        left_join(scenario1_react(), age_df, "uniqid") %>% 
            mutate(age0 = age - 8) %>% 
            
            epred_draws(object = m_smfq_8_stan, 
                        ndraws = 50,
                        allow_new_levels = TRUE,
                        re_formula = NA)
    })
    
    
    scenario2_pred <- eventReactive(input$estimate, {
        left_join(scenario2_react(), age_df, "uniqid") %>% 
            mutate(age0 = age - 8) %>% 
            
            epred_draws(
                object = m_smfq_8_stan, 
                ndraws = 50,
                allow_new_levels = TRUE,
                re_formula = NA)
    })
    
    output$scenario1_pred_tab <- renderDataTable({
        scenario1_pred()
    })
    output$scenario2_pred_tab <- renderDataTable({
        scenario2_pred()
    })
    
    
    output$plot3 <- renderPlot({
        
        ggplot(avg_pred_data, aes(age, .epred, group = uniqid, 
                                  color = uniqid)) +
            stat_lineribbon(data = rbind(scenario1_pred(), scenario2_pred()), 
                            .width = c(0.95, 0.5)) +
            scale_fill_brewer(palette = "Greys") +
            geom_smooth(data = avg_pred_data, 
                        aes(age, .epred, 
                            group = as.character("Predicted mean\n probability"),
                            linetype = as.character("Predicted mean\n probability")), 
                        se = F, color = "black", size = 1.5) +
            theme_bw() +
            labs(x = "Age", y = "Predicted probability of depression (SMFQ)",
                 color = "", linetype = "",
                 fill = "Confidence level") +
            guides(linetype = guide_legend(order = 1),
                   colour = guide_legend(order = 2), 
                   fill = guide_legend(order = 3))
        
    })
    
}

shinyApp(ui, server)










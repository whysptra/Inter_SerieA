library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(htmltools)
library(lubridate)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(data.table)
library(shinyjs)
library(DT)
library(here)
library(RColorBrewer)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
#==============================================================================================#
##                                             Global                                         ##
#==============================================================================================#

seriea <- read_csv(here::here("data/seriea.csv"))
summary_all_team_final <- read_csv(here::here("data/summary_all_team_final.csv"))
summary_longer <- summary_all_team_final %>%
    pivot_longer(cols=c(5:8),
                 names_to="Result",
                 values_to ="Total")

seriea_line <- seriea %>%
    select(Season, Team, MP, Win, Draw, Lose, GF, GA, GD, Points)  %>%
    group_by(Season, Team) %>%
    summarise(MP=sum(MP),
              W=sum(Win),
              D=sum(Draw),
              L=sum(Lose),
              GF=sum(GF),
              GA=sum(GA),
              GD=sum(GD),
              P = sum(Points))
seriea_line$Season <- as.factor(seriea_line$Season)

nb.cols <- 35
MyPalette <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
team_factor <- factor(unique(seriea_line$Team))
levels(team_factor) <-
    c(
        "Inter",
        "Juventus",
        "Roma",
        "Milan",
        "Napoli",
        "Lazio" ,
        "Atalanta",
        "Bari",
        "Bologna",
        "Cagliari",
        "Catania",
        "Chievo",
        "Fiorentina",
        "Genoa",
        "Livorno",
        "Palermo",
        "Parma",
        "Sampdoria",
        "Siena",
        "Udinese",
        "Brescia",
        "Cesena",
        "Lecce",
        "Novara",
        "Pescara",
        "Torino",
        "Sassuolo",
        "Verona",
        "Empoli",
        "Carpi",
        "Frosinone",
        "Crotone",
        "Benevento",
        "Spal"
    )
names(MyPalette) <- team_factor

our_theme <- theme_classic() + theme(
    rect = element_rect(fill = NA),
    text = element_text(color = "white", size = 10),
    panel.background = element_rect(fill = NA),
    axis.text = element_text(color = "white"),
    panel.border = element_rect(color = "white", size = 0.8),
    plot.title = element_text(
        size = 12,
        face = "bold",
        colour = "white"
    ),
    plot.subtitle = element_text(
        size = 15,
        face = "italic",
        colour = "white"
    ),
    axis.ticks = element_line(colour = "white")
)

home_match <- unique(summary_longer$MatchIndicator)[10:18]
away_match <- unique(summary_longer$MatchIndicator)[1:9]    

#==============================================================================================#
##                                      User Interface                                        ##
#==============================================================================================#

ui <- dashboardPage(skin = "purple",
    dashboardHeader(title = "Serie-A"),
    dashboardSidebar(width = 150,
                     sidebarMenu(
                         menuItem("About",
                                  tabName = "dashboard",
                                  icon = icon("question")),
                         menuItem(
                             "Resources",
                             icon = icon("database"),
                             tabName = "rdb",
                             startExpanded = TRUE,
                             menuSubItem("Table", icon = icon("globe"), tabName = "rdbi"),
                             menuSubItem("Trends", icon = icon("flag"), tabName = "rdbe"),
                             menuSubItem("Stats", icon = icon("book"), tabName = "rdbo")
                         )
                     )),
    dashboardBody(
         # shinyDashboardThemes(theme = "grey_dark"),
        tabItems(
            tabItem(tabName = "rdbo",
                    sidebarPanel(radioButtons(inputId = "format",
                                              label = "Pick one",
                                              choices = c("Home","Away"),
                                              selected = "Home"),
                                 selectInput(inputId = "team_stat",
                                             label = "Select team to compare",
                                             choices = unique(seriea$Team),
                                             selected = "Juventus"),
                                 radioButtons(inputId = "match_result",
                                              label= "Choose the match result",
                                              choices = unique(summary_longer$Result),
                                              selected = "TotalWin"),
                                 selectInput(inputId = "match_stat",
                                             label="Choose one match indicator",
                                             choices = home_match,
                                             selected = "Home_Shots")),
                    mainPanel(tabsetPanel(
                        type = "tabs",
                        tabPanel("Plot", plotlyOutput("stats_plot1", height = 300),
                                  plotlyOutput("stats_plot2", height = 300)),
                        tabPanel("Help", verbatimTextOutput("helptab3")))))
                             ,
                        
            tabItem("dashboard",
                    verbatimTextOutput("about")),
            tabItem(
                "rdbi",
                titlePanel("Table All Season"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            inputId = "season",
                            label = "Select season ",
                            choices = unique(seriea$Season),
                        )
                    ),
                    mainPanel(tabsetPanel(
                        type = "tabs",
                        tabPanel(
                            "Table",
                            DT::dataTableOutput("seriea_table")
                        ),
                        tabPanel("Help", verbatimTextOutput("helptab1"))
                    ))
                )
            ),
            tabItem(
                tabName = "rdbe",
                sidebarLayout(sidebarPanel(
                    selectInput(
                        inputId = "season_line",
                        label = "Select season ",
                        choices = unique(seriea$Season),
                    ),
                    pickerInput(
                        inputId = "team_choice",
                        label = "Select team to compare :",
                        choices = unique(seriea$Team),
                        options = list(
                            `actions-box` = TRUE,
                            `selected-text-format` = "count > 2",
                            `count-selected-text` = "{0}/{1} Team"
                        ),
                        multiple = TRUE,
                        selected = c("Inter")
                    ),
                    width = 4
                ),
                mainPanel(
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Plot", plotlyOutput("plotlyseriea", height = 500)),
                        tabPanel("Help", verbatimTextOutput("helptab2"))
                        # splitLayout(style = "border: 1px solid silver:", cellWidths = c("600","500")
                    )
                ))
            )
         )
    )
)

    
#==============================================================================================#
##                                           Server                                           ##
#==============================================================================================#
server <- function(input, output, session) {
    
    
#==============================================================================================#
##                                      Tab : About                                           ##
#==============================================================================================#

output$about <- renderText({
"This visualisation will tell how Inter Milan performance 
in the highest league in Italy,Serie A in the 2009/2010 to 2018/2019 season.
This report will answer the following questions:

- Does Inter Milan become more competitive team from 2009 season to 2018 season?
- What indicators that affect Inter Milan performance in Serie A?
- How Inter Milan compared to a rival?

The visualisation is aimed at the general public and football fans in particular.
Visualisation is provided on a user-friendly principle with easy-to-understand graphic forms (table, line graph, and bar graph).
By providing analytical data in the form of interactive visualisations, it can help the audience understand the journey of
the Inter Milan team from season to season and compare it with other teams.

Every tab is provided with guide about how to use the app which you can find at the help tab beside the plot tab.
This visualisation is an assignment for FIT5147 Data Exploration and Vizualisation.

Author : Putu Wahyu Saputra (31161278)"})
    
        
#==============================================================================================#
##                                      Tab : Table                                           ##
#==============================================================================================#

    output$seriea_table <- DT::renderDataTable({
        serieatab <- seriea %>%
            select(Season, Team, MP, Win, Draw, Lose, GF, GA, GD, Points) %>%
            filter(Season == input$season) %>%
            group_by(Team) %>%
            summarise(MP=sum(MP),
                      W=sum(Win),
                      D=sum(Draw),
                      L=sum(Lose),
                      GF=sum(GF),
                      GA=sum(GA),
                      GD=sum(GD),
                      P = sum(Points)) %>%
            arrange(desc(P))
        
        my_team = unique(seriea$Team)
        my_colors = ifelse(my_team == "Inter","#4e8de5","NA")
        
          DT::datatable(data = serieatab) %>%
              formatStyle(columns = 'Team', target="row", backgroundColor = styleEqual(my_team,my_colors))
    })
        

output$helptab1 <- renderText({
"* When the application is opened, to access the tab, it is necessary to push the Resources button. After that, a sub-menu will appear. Select Table
* To select season, user can press the drop-down button. In this button, there are ten seasons. Users can choose the season they want.
  When a season is selected, table will change according to the selected season. Inter Milan has a blue highlight.
  The table has information regarding the number of matches (MP), number of wins (W), draws (D) and losses (L), total goals scored (GP),
  number of goals conceded (GA), goal difference (GD), and total points (P).
  
  Only W, D, and L affect the total points. Where the calculations are as follows: 

    - Win = 3 Points
    - Draw = 1 Point
    - Lose = 0 Points
    - Point = Win + Draw + Lose
    
*  The number of rows in the table is 18 because there are 18 teams in one season. The table is shown only at the top 10.
   To view the entire rows in one view, the user can select the number in Show entries.
*  The option to view the next row can be done by pressing the Next button at the bottom of the table.
*  Users can search for their desired team by using the Search box at the top of the table.
   When the user inputs a character, the table will interactively respond to the input.
   The table will display an empty row if the typo or team user are looking for does not exist in that season.
"
})
#==============================================================================================#
##                                      Tab : Line                                            ##
#==============================================================================================#
    
output$plotlyseriea <- renderPlotly({
   req(input$team_choice)
   
     seriea_line %>%
        filter(Team %in% input$team_choice) %>%
        ggplot(aes(x=unclass(Season),
                   y = P,
                   color=Team,
                   ))+
        geom_line() +
        geom_point(aes(text = paste(
            "Season :", Season,"<br>",
            "Team :", Team,"<br>",
            "Total Points :", P)))+
        scale_color_manual(values = MyPalette) +
        labs(x="Season",
             y="Total Points",
             title = "Serie A club trends over ten seasons") +
        scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10"),
                         labels=c("1" = "09/10",
                              "2" = "10/11",
                              "3" = "11/12",
                              "4" = "12/13",
                              "5" = "13/14",
                              "6" = "14/15",
                              "7" = "15/16",
                              "8" = "16/17",
                              "9" = "17/18",
                              "10" = "18/19"))+
         theme_classic()+
         theme(rect = element_rect(fill = NA),
               panel.background = element_rect(fill = NA))

     ggplotly(tooltip = "text") %>% config(displayModeBar = F)
    
})


output$helptab2 <- renderText({
"1. To access the Trends tab, push the Resources button. After that, a sub-menu will appear. Select Trends
2. To select season, user can press the drop-down button. In this button, there are ten seasons.
   Users can choose the season they want. 
3. To select a team, press the drop-down. Inter is by default selected.
   There will be a choice of teams to compare it to Inter. User can select more than one.
   The feature is Select All to show the whole team in the graphic, and Deselect All to clear all selections.
4. Line Plot shows the trend of total points against the season. Each line represents a team.
5. The tooltip feature will appear when the user hovers the mouse into the point.
   information that appears in Season, Team, and Total Points for that season.
6. Legend provides information on the graphic that appears for easy recognition. Legend is interactive.
   Click the legend on a team for highlight. When the legend is clicked, the team will be hidden.
"
})

#==============================================================================================#
##                                      Tab : Stats                                           ##
#==============================================================================================#

output$stats_plot1 <- renderPlotly({
    p1 <- summary_longer %>%
        filter(format == input$format,
            Team %in% c("Inter", input$team_stat),
               MatchIndicator == input$match_stat) %>%
        ggplot(aes(x=Season))+
        geom_col(aes(y=Value,fill=Team, text=paste("Team :", Team,"<br>",
                                                   "Season :", Season,"<br>",
                                                   "Value :", Value,"<br>",
                                                   "Indicator :", input$match_stat)),
                 position = "dodge",stat="identity")+
        ggtitle(paste("Inter vs" ,input$team_stat, "in", input$match_stat))+
        xlab("")+
        theme_classic()+
        theme(rect = element_rect(fill = NA),
              legend.position = "bottom",
              panel.background = element_rect(fill = NA))
    
    ggplotly(p1, tooltip="text") %>% config(displayModeBar = F)

})

# observe({
#     x <- input$format
#     
#     # Can use character(0) to remove all choices
#     if (is.null(x))
#         x <- character(0)
#    
    
choice_match <- summary_longer %>%
    select(format,MatchIndicator) %>%
    unique()

    # Can also set the label and select items
observeEvent(input$format, {
    updateSelectInput(session, "match_stat",
                      label = paste("Select Match Indicator"),
                      choices = choice_match %>% filter(format == input$format) %>% select(MatchIndicator),
                      selected = "none")
})
    
# })
output$stats_plot2 <- renderPlotly({
    
    p2 <- summary_longer %>%
        filter(format == input$format,
            Team %in% c("Inter", input$team_stat),
               Result == input$match_result) %>%
        ggplot(aes(x=unclass(as.factor(Season)),
                             y=Total,
                             color = Team,
                             group=Result))+
        geom_line()+
        geom_point(aes(text=paste("Team :", Team,"<br>",
                                  "Season :", Season,"<br>",
                                  "Type :", Result,"<br>",
                                  "Total :", Total)),size=5) +
        xlab("Season")+
        scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10"),
                         labels=c("1" = "09/10",
                                  "2" = "10/11",
                                  "3" = "11/12",
                                  "4" = "12/13",
                                  "5" = "13/14",
                                  "6" = "14/15",
                                  "7" = "15/16",
                                  "8" = "16/17",
                                  "9" = "17/18",
                                  "10" = "18/19"))+
        theme_classic()+
        ggtitle(paste("Inter vs" ,input$team_stat, "in", input$match_result))+
        theme(rect = element_rect(fill = NA),
              panel.background = element_rect(fill = NA),
              legend.position = "bottom")
    
    ggplotly(p2, tooltip= "text") %>% config(displayModeBar = F)
})
    
output$helptab3 <- renderText({
"1. To access the Stats tab, push the Resources button. After that, a sub-menu will appear. Select Stats
2. To select the game format, the user can press the radio button. On this button, there are two game formats, namely Home and Away
3. To select a team, press the drop-down. Users can only choose one team to compare with Inter Milan.
4. Users can choose one of the four-match results, namely Total Win, Total Lose, Total Draw, and Total Point via the drop-down.
5. Next, the user can select the type of match indicator to be analysed or viewed via the drop-down button.
   There are nine-match indicators for each game format. This button can change according to the input of the Game Format radio button.
6. The first plot displayed is a side by side barplot. This plot shows the comparison of match indicators between Inter and rival teams.
7. The second plot is a line plot with a connected scatter. This plot shows the comparison of match results between Inter and rival teams.
8. The tooltip feature in both the bar chart and line chart will appear when the user hovers the mouse into the point.
   The information that appears is Team, Season, Value, and Total Points. The output graph depends on the input team
9. Legend provides information on the graphic that appears for easy recognition. Legend is interactive.
   Click the legend on a team for highlight. When the legend is clicked, the team will be hidden.
"
})

#==============================================================================================#
##                                      Run the application                                   ##
#==============================================================================================#
}
shinyApp(ui = ui, server = server)

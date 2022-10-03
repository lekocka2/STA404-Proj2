library(shiny)
library(plotly)
library(tidyverse)
library(readxl)
library(data.table)
library(plotly)

## working with plot 2 + 3 data
{
  #reading in data
  data <- data.frame()
  teams <- c("Boston Celtics", "Chicago Bulls", "Dallas Mavericks", "Utah Jazz")
  for (i in 1:length(teams)) {
    teamData <- read_excel("TeamData.xlsx", sheet = 2*i) %>%
      mutate(Team = teams[i])
    data <- bind_rows(data, teamData)
  }
  
  # creating final dataset
  plot2Data <- data %>% 
    filter(G >= 15) %>%
    mutate(Group = case_when(
      Rk >= 0 & Rk <= 5 ~ "Starters",
      Rk >= 6 & Rk <= 10 ~ "Next Off Bench",
      Rk >= 11 ~ "Backup Players"
    )) %>%
    select(Team, Player, Group, `2P%`, `3P%`) %>%
    pivot_longer(cols=c(4,5), values_to="Perc", names_to="Type") %>%
    filter(Perc >= .05)
  
  # reading in next data
  data1 <- data.frame()
  for (i in 1:length(teams)) {
    rosterData <- read_excel("TeamData.xlsx", sheet = 2*i-1) %>%
      mutate(Team = teams[i])
    data1 <- bind_rows(data1, rosterData)
  }
  
  # finalizing next dataset
  plot3Data <- left_join(data1, data, by=c("Player"="Player", "Team"="Team")) %>%
    mutate(Years = as.numeric(ifelse(Exp == "R", 0, Exp))) %>%
    select(Player, Team, Ht, Wt, Age, Years)
  
  # needed for plot 3
  metChoice <- c(
    "Height" = "Ht",
    "Weight" = "Wt",
    "Age" = "Age",
    "Years in NBA" = "Years"
  )
  varChoice <- list(
    "Ht" = "Height",
    "Wt" = "Weight",
    "Age" = "Age",
    "Years" = "Years in NBA"
  )
  
  # data for tab 4
  gamelogs <- read.csv("GameLogs-all.csv", sep = ",", header = T)
  gamelogs <- gamelogs %>%
    filter(!is.na(X) | !is.na(X.1)) %>% # to only keep get scored events
    select(Tot.Time, Score, X.2) %>%
    separate(Score, into = c("visitor", "home"), sep = "-") %>%
    mutate(visitor = as.numeric(visitor),  # separate scores
           home = as.numeric(home),
           diff = as.numeric(home-visitor))
  gamelogs <- gamelogs[-1199,]
  
  # removing unnecessary dfs
  rm(data, data1, rosterData, teamData, i)
}

ui <- fluidPage(
   
  navbarPage("STA 404 Group Project",
             tabPanel("Home",
                      fluidRow(column(12, h3(div("Logan Kocka, Tommy Fowler, Aman Shrestha", style="color: #EE8F20")))), br(), br(),
                      fluidRow(column(12, h4("This dashboard creates interactive visualizations from NBA statistics and history from", uiOutput('url')))),
                      br(), uiOutput("img"), br(), br(), br(),
                      fluidRow(column(10, h5("From a free agent in the draft comparing multiple offers to a basketball fan creating a bracket, basketball
                                             data and visualizations could", br(), "be useful in many different ways.", "The visuals in this dashboard provide insights
                                             into the players and game records of four 2021-2022 NBA teams:", br(), br()),
                                      h4("Chicago Bulls", br(), "Boston Celtics", br(), "Dallas Mavericks", br(), "Utah Jazz")))
                      ),
             
             tabPanel("Player Stats",
                      fluidRow(column(12, strong("Player performance statistics --"), "Here we are interested in looking at individual player performance for each of the
                                      four teams.", br(), "Stats included here are success rates of 2 and 3 pointers, overall field goals, and free throws, as well as assists,
                                      blocks, total points, rebounds, and steals per game.")), br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("team", "Select team", c("Chicago Bulls","Boston Celtics", "Dallas Mavericks", "Utah Jazz"), "Chicago Bulls"),
                          selectInput("stats", "Select data", c("Counts","Rates"), "Counts"),
                          uiOutput("playerinput")),
                        mainPanel(plotlyOutput("plot1"))
                      )
             ),
             
             tabPanel("Shooting Percentages",
                      fluidRow(column(12, strong("Shooting success rates of starters, next off bench players, and backup players --"), "Let's look at
                                      shooting success rate of players for each team grouped by their", br(), "rank within the team. Starters
                                      and next off bench players are going to have more play time than backup players, 
                                      so it's important to consider each player's performance weighted by rank.")), br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("team2", "Choose a team", choices = teams),
                          selectInput("group", "Choose a group of players", choices = unique(plot2Data$Group))),
                        mainPanel(
                          plotOutput(outputId = "plot2")
                        )
                      )
             ),
             
             tabPanel("Player Metric Distribution",
                      fluidRow(column(12, strong("Distributions of player demographics --"), "Player demographics like height, years experience, age, and weight
                                      could have", br(), "significant impact on the team's performance. This visual allows us to see and compare the distributions
                                      of these important metrics.")), br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("team3", "Choose a team", choices = teams, multiple=TRUE, selected="Boston Celtics"),
                          selectInput("metric", "Choose a player metric", choices = metChoice),
                          radioButtons("facet", "Display Grid or Overlay?", choices = list("Overlay", "Grid"))
                        ),
                        mainPanel(
                          plotOutput("plot3")
                        )
                      )
             ),
             
             tabPanel("Game Logs",
                      fluidRow(column(12, strong("Game logs --"), "Looking at a team's performance in past games is perhaps the most indicative of the 
                                      strength of the team. We can see the progression", br(), "of the scores and score differential to get insights on
                                      how the team compares to other teams and the team's resilience over the span of each game.")), br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("game", "Select game", unique(gamelogs$X.2), "Boston Celtics @ New York Knicks"),
                          radioButtons("choice4", "Select plot type", c("Scores", "Point difference")),
                          strong("Game Details"), br(), uiOutput("gameInfo")
                        ),
                        mainPanel(
                          plotlyOutput("plot4")
                          )
                      )
             )
  )
)

# server start
server <- function(input, output) {
  
  # create hyperlink for the citation
  url <- a("www.basketball-reference.com", href="https://www.basketball-reference.com/")
  output$url <- renderUI({
    tagList(url)
  })
    
  # render image
  output$img <- renderUI({
    img(src='images.png', height='90px', width='750px')
  })
  
  # choices for player input first tab
  output$playerinput <- renderUI({
    roster <- read_excel("TeamData.xlsx", sheet=1)
    bullsPlayers <- roster[,2]
    roster2 <- read_excel("TeamData.xlsx", sheet=3)
    celtsPlayers <- roster2[,2]
    roster3 <- read_excel("TeamData.xlsx", sheet=5)
    mavePlayers <- roster3[,2]
    roster4 <- read_excel("TeamData.xlsx", sheet=7)
    jazzPlayers <- roster4[,2]

    switch(input$team,
           'Chicago Bulls' = selectInput('select2',
                                   label = 'Select player:',
                                   choices = bullsPlayers),
           'Boston Celtics' = selectInput('select3',
                                          label = 'Select player:',
                                          choices = celtsPlayers),
           'Dallas Mavericks' = selectInput('select4',
                                            label = 'Select player:',
                                            choices = mavePlayers),
           'Utah Jazz' = selectInput('select5',
                                          label = 'Select player:',
                                          choices = jazzPlayers))
  })
  
  statChoice <- reactive(input$stats)
  
  # render plot 1
  output$plot1 <- renderPlotly({
    
    # get player choice / data
    teamChoice <- reactive(input$team)
    if(teamChoice() == 'Chicago Bulls'){
      playerChoice <- reactive(input$select2)
      bulls_roster <- read_excel("TeamData.xlsx", sheet=1)
      bulls_stats <- read_excel("TeamData.xlsx", sheet=2)
      df <- left_join(bulls_roster, bulls_stats, by="Player")
      rm(bulls_roster, bulls_stats)
    } 
    if(teamChoice() == 'Boston Celtics'){
      playerChoice <- reactive(input$select3)
      celt_roster <- read_excel("TeamData.xlsx", sheet=3)
      celt_stats <- read_excel("TeamData.xlsx", sheet=4)
      df <- left_join(celt_roster, celt_stats, by="Player")
      rm(celt_roster, celt_stats)
    }
    if(teamChoice() == 'Dallas Mavericks'){
      playerChoice <- reactive(input$select4)
      mave_roster <- read_excel("TeamData.xlsx", sheet=5)
      mave_stats <- read_excel("TeamData.xlsx", sheet=6)
      df <- left_join(mave_roster, mave_stats, by="Player")
      rm(mave_roster, mave_stats)
    } 
    if(teamChoice() == 'Utah Jazz'){
      playerChoice <- reactive(input$select5)
      jazz_roster <- read_excel("TeamData.xlsx", sheet=7)
      jazz_stats <- read_excel("TeamData.xlsx", sheet=8)
      df <- left_join(jazz_roster, jazz_stats, by="Player")
      rm(jazz_roster, jazz_stats)
    } # workaround 

    if (statChoice() == "Rates") {
      # rates
      percR <- reactive({
        df[df$Player==playerChoice(), c("Player", "FG%", "3P%", "2P%", "FT%")]
      })
      
      df_filtered <- percR()
      df_filtered <- df_filtered[,-1]
      colnames(df_filtered) <- c("Field Goal", "3-Pointer", "2-Pointer", "Free Throw")
      df_filtered <- pivot_longer(df_filtered, cols=1:4, names_to="stat", values_to="rate")
      df_mutated <- df_filtered %>% mutate(Failure=1-rate) %>% rename(Success=rate)
      df_melted <-  melt(as.data.table(df_mutated), id.vars=1)
      
      plot <- plot_ly(df_melted, x=~stat, y=~as.numeric(value), type='bar', color=~variable, textposition='auto', textfont=list(color="black"), text=~as.numeric(value), 
                      marker=list(line=list(color = 'black', width = 1.2))) %>%
        layout(title=paste("Performance Stats for", playerChoice()), barmode='stack',
               yaxis=list(title="Success Rate"), xaxis=list(title="Stat"))
    }
    
    if (statChoice() == "Counts") {
      # counts
      countR <- reactive({
        df[df$Player==playerChoice(), c("Player", "TRB", "AST", "STL", "BLK", "PTS/G")]
      })
      df_filtered <- countR()
      df_filtered <- df_filtered[,-1]
      colnames(df_filtered) <- c("Rebounds", "Assists", "Steals", "Blocks", "Points")
      df_mutated <- pivot_longer(df_filtered, cols=1:5, names_to="stat", values_to="value")
      
      plot <- plot_ly(df_mutated, x=~stat, y=~value, type='bar', textposition='outside', textfont=list(color="black"), text=~as.numeric(value)) %>%
        layout(title=paste("Performance Stats for", playerChoice()),
               yaxis=list(title="Count"), xaxis=list(title="Stat (per game)"))
    }
    plot
  })
  
  ###########################################################################################
  # render plot 2
  output$plot2 <- renderPlot({
    
    # plot it
    ggplot(filter(plot2Data, Team == input$team2, Group == input$group)) +
      geom_col(aes(x=Perc, y=Player, fill=Type), position="dodge") +
      geom_text(aes(x=Perc, y=Player, fill=Type, label=Perc),
                position=position_dodge(width=.8), hjust=1) +
      scale_fill_manual(values=c("firebrick1", "dodgerblue"),
                        labels = c("2-Pointer %", "3-Pointer %")) +
      theme_bw() +
      labs(title=paste("2 and 3 Pointer Shooting Percentages for", input$group)) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.title.y = element_blank())
  })

  ###########################################################################################
  # render plot 3
  
  # set team colors for fill on plot
  teamColors <- data.frame(
    row.names = teams,
    Color = c("#007A33", "#CE1141", "#00538C", "#F9A01B")
  )
  
  # function to determine team colors to choose
  colorFun <- function(teamsSel) {
    t <- str_sort(teamsSel)
    if (length(teamsSel) == 4) {
      value <- c(teamColors[t[1], 1], teamColors[t[2], 1],
                 teamColors[t[3], 1], teamColors[t[4], 1])
    }
    if (length(teamsSel) == 3) {
      value <- c(teamColors[t[1], 1], teamColors[t[2], 1],
                 teamColors[t[3], 1])
    }
    if (length(teamsSel) == 2) {
      value <- c(teamColors[t[1], 1], teamColors[t[2], 1])
    }
    if (length(teamsSel) == 1) {
      value <- c(teamColors[t[1], 1])
    }
    value
  } 
  
  output$plot3 <- renderPlot({
    
    plotted <- ggplot(data=filter(plot3Data, Team %in% input$team3), aes(color=Team)) +
      geom_density(aes_string(x=input$metric)) +
      theme_bw() +
      scale_color_manual(values=colorFun(input$team3)) +
      labs(x=varChoice[[input$metric]], 
           y="Density", 
           title=paste("Distribution of Selected Teams Players'", varChoice[[input$metric]]))
    
    if (input$facet == "Grid") {
      plotted + facet_wrap(~Team, nrow=2) + theme(legend.position = "none")
    }
    else plotted
    
  })

  
  ###########################################################################################
  # render plot 4
  
  output$plot4 <- renderPlotly({
    if(input$choice4 == "Scores"){
      plot4 <- plot_ly( filter(gamelogs, X.2 == input$game) ) %>%
        add_lines(y=~visitor, x=~Tot.Time, name="Visitor") %>%
        add_lines(y=~home, x=~Tot.Time, name="Home") %>%
        layout(xaxis=list(autorange="reversed", title="Time (min:sec)"),
               yaxis=list(title="Points Scored"), title=paste(input$game, "Score"))
    } else {
      plot4 <- plot_ly( filter(gamelogs, X.2 == input$game) ) %>%
        add_lines(y=~diff, x=~Tot.Time) %>%
        layout(xaxis=list(autorange="reversed", title="Time (min:sec)"),
               yaxis=list(title="Points Difference"), title=paste(input$game, "Score Difference"))
    }
    plot4
    
  })
  
  output$gameInfo <- renderUI({
    gamelogs <- filter(gamelogs, X.2 == input$game)
    if(input$game == "Boston Celtics @ New York Knicks")
      inf <- HTML("Boston Celtics vs New York Knicks<br>Away<br>Regular Season<br>Wed, Oct 20, 2021<br>Loss, 2OT, 134-138")
    if(input$game == "Milwaukee Bucks @ Boston Celtics")
      inf <- HTML("Boston Celtics vs Milwaukee Bucks<br>Home<br>Playoffs<br>Sun, May 1, 2022<br>Loss, 89-101")
    if(input$game == "Boston Cetlics @ Houston Rockets")
      inf <- HTML("Boston Cetlics vs Houston Rockets<br>Away<br>Regular Season<br>Sun, Oct 24, 2021<br>Win, 107-97")
    if(input$game == "Chicago Bulls @ Detroit Pistons")
      inf <- HTML("Chicago Bulls vs Detroit Pistons<br>Away<br>Regular Season<br>Wed, Oct 20, 2021<br>Win, 94-88")
    if(input$game == "New Orleans Pelicans @ Chicago Bulls")
      inf <- HTML("Chicago Bulls vs New Orleans Pelicans<br>Home<br>Regular Season<br>Fri, Oct 22, 2021<br>Win, 128-112")
    if(input$game == "Chicago Bulls @ Toronto Raptors")
      inf <- HTML("Chicago Bulls vs Toronto Raptors<br>Away<br>Regular Season<br>Mon, Oct 25, 2021<br>Win, 111-108")
    if(input$game == "Oklahoma City Thunder @ Utah Jazz")
      inf <- HTML("Utah Jazz vs Oklahoma City Thunder<br>Home<br>Regular Season<br>Wed, Oct 20, 2021<br>Win, 107-86")
    if(input$game == "Utah Jazz @ Sacramento Kings")
    inf <- HTML("Utah Jazz vs Sacramento Kings<br>Away<br>Regular Season<br>Fri, Oct 22, 2021<br>Win, 110-101")
    if(input$game == "Denver Nuggets @ Utah Jazz")
      inf <- HTML("Utah Jazz vs Denver Nuggets<br>Home<br>Regular Season<br>Tue, Oct 26, 2021<br>Win, 122-110")
    if(input$game == "Dallas Mavericks @ Atlanta Hawks")
      inf <- HTML("Dallas Mavericks vs Atlanta Hawks<br>Away<br>Regular Season<br>Thu, Oct 21, 2021<br>Loss, 87-113")
    if(input$game == "Dallas Mavericks @ Toronto Raptors")
      inf <- HTML("Dallas Mavericks vs Toronto Raptors<br>Away<br>Regular Season<br>Sat, Oct 23, 2021<br>Win, 103-95")
    if(input$game == "San Antonio Spurs @ Dallas Mavericks")
      inf <- HTML("Dallas Mavericks vs San Antonio Spurs<br>Home<br>Regular Season<br>Thu, Oct 28, 2021<br>Win, 104-99")
    
    inf
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


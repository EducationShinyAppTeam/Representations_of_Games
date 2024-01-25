# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

# Define global constants and load question banks ----
questionBank1 <- read.csv("questionBank1.csv", header = TRUE)
questionBank1 <- na.omit(questionBank1)
questionBank2 <- read.csv("questionBank2.csv", header = TRUE)
questionBank2 <- na.omit(questionBank2)
questionBank3 <- read.csv("questionBank3.csv", header = TRUE)
questionBank3 <- na.omit(questionBank3)

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "Representations",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Representations_of_Games")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ## Sidebar menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "challenge", icon = icon("gears")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Repersentations of Games"),
          p("This is an app that plays extensive form and normal form representations
            of the games to explore how decision markers such as firms or players
            will choose the strategies in a conflict situaion and react to the strategy
            of their rivals."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review key topics of components and consumption of a game."),
            tags$li("Explore the differences of games in extensive form and normal
                    form"),
            tags$li("Challenge yourself by play the game using the mixed strategy method."),
          ),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToExplore",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Luqi Jiao Emanuele.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 1/20/2024 by LJE.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Three elements of a game:"),
          tags$ul(
            tags$li("A set of players."),
            tags$li("A set of strategies for each player."),
            tags$li("A payoff function with a payoff to each player with all possible
                    strategies.")
          ),
          br(),
          p("Assumptions of payoff matrix:"),
          tags$ul(
            tags$li("The players will play their strategies simultaneously in Normal form."),
            tags$li("This is one-shot game, meaning all players paly only once.")
          ),
          box(
            title = strong("Extensive Form"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "We will use game tree in extensive form.",
            tags$figure(
              class = "leftFigure",
              tags$img(
                src = "prere1.jpg",
                width = 400,
                alt = "Example of extensive form in game tree."
              )
            ),
            br(),
            "There are three sets of objects in extensive form:",
            tags$ol(
              tags$li("Nodes - Players will be located"),
              tags$li("Branches - Strategies of each Players"),
              tags$li("Information sets - information that a Player knows about
                      what his/her opponent will do")
            ),
            br(),
            "From the above example, the initial node is started by Player A to
            make a decision whether to play or not play a game. Next, Player B at
            the second and third nodes will observe Player A's decision and then
            decide to either play or not play a game.",
            br(),
            "The values represent the utilities of each decision. For example, if:",
            tags$ol(
              tags$li("Player A chooses to play the game and Player B chooses
                      to play, A will get a utility of 5."),
              tags$li("Player A chooses to play the game and Player B chooses
                      to not play, he will get a utility of 10."),
              tags$li("Player A chooses to not play the game and Player B chooses
                      to play, A will get a utility of 9."),
              tags$li("Player A chooses to not play the game and Player B chooses
                      to not play, he will get a utility of 14.")
            ),
            "This is the same situation with Player B."
          ),
          box(
            title = strong("Normal Form"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "We will use payoff matrix in normal form.",
            tags$figure(
              class = "leftFigure",
              tags$img(
                src = "prere2.jpg",
                width = 400,
                alt = "Example of normal form in payoff matrix."
              )
            ),
            br(),
            "There are three sets of objects in normal form:",
            tags$ol(
              tags$li("Players - Player A and Player B"),
              tags$li("Strategies - Play or not play"),
              tags$li("Payoffs")
            ),
            "From the above example, we have the same set up as in the Extensive form.
            However, this time the players will play the game simultaneously. In this case,
            the best strategy for Player A is to chooses not play no matter what
            Palyer B plays, because by comparing the payoffs:",
            tags$ol(
              tags$li("If B chooses to play, then A's utility function of not play
                      (9) is high than play (5)."),
              tags$li("If B chooses to not play, then A's utility function of not
                      play (14) is high than play (10).")
            ),
            "For Player B, there is no best strategy, because:",
            tags$ol(
              tags$li("If A chooses to play, then B's utility function of not play
                      (6) is high than play (3)."),
              tags$li("If A chooses to not play, then B's utility function of
                      play (11) is high than not play (7).")
            ),
            "Neither of the strategies of Player B is consistently better than the
            other."
          )
        ),
        ### Explore Page ----
        tabItem(
          tabName = "explore",
          h2("What is this page about?"),
          p("State what this page is about"),
          tabsetPanel(
            id = "exploreTabs",
            type = "tabs",
            #### First Explore Tab ----
            tabPanel(
              title = "Normal Form",
              br(),
              p("Neil wants more text on the page to help a user know what they
                are supposed to do."),
              p("Given the size of your image, I don't think the two-column
                layout is going to be useful."),
              h3("Payoff Matrix "),
              fluidRow(
                column(
                  width = 6,
                  uiOutput(outputId = "questionPlot1"),
                  br(),
                  h4("Answer Choices:"),
                  p("Use a selectInput call that you update instead of your
                    current approach."),
                  p("A:"),
                  uiOutput("choiceA"),
                  br(),
                  p("B:"),
                  uiOutput("choiceB"),
                  br(),
                  p("C:"),
                  uiOutput("choiceC"),
                  br()
                ),
                column(
                  width = 6,
                  p("Test your understanding by trying out these questions."),
                  wellPanel(
                    selectInput(
                      inputId = "response1",
                      label = "Select your answer",
                      choices = list("A", "B", "C", ""),
                      selected = ""
                    ),
                    bsButton(
                      inputId = "submit1",
                      label = "Submit",
                      size = "large",
                      style = "default"
                    ),
                    br(),
                    p("Feedback"),
                    uiOutput("icon1"),
                    uiOutput("answer1"),
                    br(),
                    bsButton(
                      inputId = "newChallenge1",
                      label = "New Challenge",
                      size = "large",
                      style = "default"
                    )
                  )
                )
              )
            ),
            #### Second Explore Tab ----
            tabPanel(
              title = "Extensive Form",
              br(),
              h3("Game Tree"),
              br(),
              p("According to the situation, input payoffs in the table below
                      for each player to create the game tree."),
              fluidRow(
                column(
                  width = 6,
                  uiOutput(outputId = "questionPlot2"),
                  br(),
                  h4("Answer Choices:"),
                  p("A:"),
                  ## All inputs and outputs MUST BE uniquely named
                  uiOutput("choiceA2"),
                  br(),
                  p("B:"),
                  uiOutput("choiceB2"),
                  br(),
                  p("C:"),
                  uiOutput("choiceC2"),
                  br()
                ),
                column(
                  width = 6,
                  p("Test your understanding by trying out these questions."),
                  wellPanel(
                    selectInput(
                      inputId = "response2",
                      label = "Select your answer",
                      choices = list("A", "B", "C", ""),
                      selected = ""
                    ),
                    bsButton(
                      inputId = "submit2",
                      label = "Submit",
                      size = "large",
                      style = "default"
                    ),
                    br(),
                    p("Feedback"),
                    uiOutput("icon2"),
                    uiOutput("answer2"),
                    br(),
                    bsButton(
                      inputId = "newChallenge2",
                      label = "New Challenge",
                      size = "large",
                      style = "default"
                    )
                  )
                )
              )
            )
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToChallenge",
              label = "Challenge!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        ### Challenge Page ----
        tabItem(
          tabName = "challenge",
          fluidPage(
            tabsetPanel(
              #### Second tab ----
              #### Where's the first tab?
              tabPanel(
                title = "Mixed Strategy",
                br(),
                p("The following questions will test your knowledge of two player
                    games with a mixed strategy.."),
                fluidRow(
                  column(
                    width = 6,
                    uiOutput(outputId = "questionPlot4"),
                    br(),
                    h4("Answer Choices:"),
                    p("A:"),
                    uiOutput("choiceA3"),
                    br(),
                    p("B:"),
                    uiOutput("choiceB3"),
                    br(),
                    p("C:"),
                    uiOutput("choiceC3"),
                    br()
                  ),
                  column(
                    width = 6,
                    p("Test your understanding by trying out these questions."),
                    wellPanel(
                      selectInput(
                        inputId = "response4",
                        label = "Select your answer",
                        choices = list("A", "B", "C", ""),
                        selected = ""
                      ),
                      bsButton(
                        inputId = "submit4",
                        label = "Submit",
                        size = "large",
                        style = "default"
                      ),
                      br(),
                      p("Feedback"),
                      uiOutput("icon4"),
                      uiOutput("answer4"),
                      br(),
                      bsButton(
                        inputId = "newChallenge4",
                        label = "New Challenge",
                        size = "large",
                        style = "default"
                      )
                    )
                  )
                )
              ),
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
              (v 0.61.1). [R package]. Available from
              https://CRAN.R-project.org/package=shinyBS"
          ),
          p(class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST Utilities.
              (v 0.1.12.3). [R package]. Available from
              https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create
              dashboards with 'Shiny'. (v. 0.7.2). [R package]. Available from
              https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
              Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
              Web application framework for R, R package. (v 1.7.1). [R package].
              Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(class = "hangingindent",
            "Polak, B. (2007), midterm exam solutions - Yale University. Open Yale Courses.
              Available from https://oyc.yale.edu/sites/default/files/midterm-exam-solutions-pdf.pdf"
          ),
          p(class = "hangingindent",
            "Ishii, Y. (n.d.), Introductory Lecture. Canvas.
              Available from https://psu.instructure.com/courses/2212963/files/folder/Lectures"
          ),
          p(class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2022), shinyWidgets: Custom
              Inputs Widgets for Shiny. (v 0.7.0). [R package]. Available from
              https://CRAN.R-project.org/package=shinyWidgets"
          ),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App will help you to pracite the knowledge of game theory by using the
        Normal form and Extensive form of game."
      )
    }
  )

    ## Move to Explore page ----
    observeEvent(
      eventExpr = input$goToExplore,
      handlerExpr = {
        updateTabItems(
          session = session,
          inputId = "pages",
          selected = "explore"
        )
      }
    )

    ## Move to Challenge page ----
    observeEvent(
      eventExpr = input$goToChallenge,
      handlerExpr = {
        updateTabItems(
          session = session,
          inputId = "pages",
          selected = "challenge"
        )
      }
    )

    ## Explore Page Buttons ----
    ### Payoff Matrix ----
    challengeElements1 <- reactiveValues(
      # Creates a vector of shuffled integers which we can use for the id column
      promptIds1 = sample(1:nrow(questionBank1), size = nrow(questionBank1), replace = FALSE),
      # Create current index
      currentIndex1 = 1,
      # Create a flag if current question has been answered
      answered1 = FALSE,
      # Create a flag for first visit to page
      firstTime1 = TRUE
    )

    ### Set watcher to iterate current question
    observeEvent(
      eventExpr = c(input$pages, input$newChallenge1),
      handlerExpr = {
        if (input$pages == "explore" & challengeElements1$firstTime1) {
          challengeElements1$firstTime1 <- FALSE
        } else if (challengeElements1$currentIndex1 == nrow(questionBank1)) {

        } else {
          challengeElements1$currentIndex1 <- challengeElements1$currentIndex1 + 1
        }
      }
    )


    ### Display challenge plot and question ----
    output$questionPlot1 <- renderUI(
      expr = {
        questionId1 <- challengeElements1$promptIds1[challengeElements1$currentIndex1]
        tagList(
          HTML(questionBank1$extraOutput[questionId1]),
          p(questionBank1$question[questionId1])
        )
      }
    )

    random_order1 <- reactiveVal()

    random_choice1 <- function() {
      choices <- c("1", "2", "3")
      random_order1 <- sample(choices)
      return(random_order1)
    }

    random_choice1()

    observe({
      random_order_val1 <- random_order1()

      output$choiceA <- renderUI({
        questionId1 <- challengeElements1$promptIds1[challengeElements1$currentIndex1]
        tagList(
          HTML(questionBank1[[paste0("A")]][questionId1])
        )
      })

      output$choiceB <- renderUI({
        questionId1 <- challengeElements1$promptIds1[challengeElements1$currentIndex1]
        tagList(
          HTML(questionBank1[[paste0("B")]][questionId1])
        )
      })

      output$choiceC <- renderUI({
        questionId1 <- challengeElements1$promptIds1[challengeElements1$currentIndex1]
        tagList(
          HTML(questionBank1[[paste0("C")]][questionId1])
        )
      })
    })

    current_question1 <- reactiveVal()

    random_question1 <- function() {
      random_index1 <- sample(nrow(questionBank1), 1)
      current_question1(questionBank1[random_index1, ])
    }

    random_question1()

    output$question1 <- renderText({
      current_question1()$question1
    })

    output$extraOutput1 <- renderImage({
      if (!is.null(current_question1()$extraOutput1) && nchar(current_question1()$extraOutput1) > 0)
      {
        return(list(src = current_question1()$extraOutput1))
      }
    },
    deleteFile = FALSE)

    output$A <- renderText({
      if (!is.null(current_question1()$A) && nchar(current_question1()$A) > 0)
      {
        return(list(src = current_question1()$A))
      }
    })

    output$B <- renderText({
      if (!is.null(current_question1()$B) && nchar(current_question1()$B) > 0)
      {
        return(list(src = current_question1()$B))
      }
    })

    output$C <- renderText({
      if (!is.null(current_question1()$C) && nchar(current_question1()$C) > 0)
      {
        return(list(src = current_question1()$C))
      }
    })

    observeEvent(
      eventExpr = input$submit1,
      handlerExpr = {
        user_answer1 <- input$response1
        correct_answer1 <- current_question()$answer1

        if (user_answer1 == correct_answer1) {
          output$icon1 <- renderIcon("correct", width = 40)
        } else {
          output$icon1 <- renderIcon("incorrect", width = 40)
        }

      }
    )

    ### Get new challenge and reset feedback
    observeEvent(
      eventExpr = input$newChallenge1,
      handlerExpr = {
        random_choice1()
        random_question1()

        updateSelectInput(
          session = session,
          inputId = "answer1",
          selected = ""
        )

        output$icon1 <- renderIcon()
        output$response1 <- renderUI(NULL)

      }
    )

    ### Game Tree ----
    challengeElements2 <- reactiveValues(
      # Creates a vector of shuffled integers which we can use for the id column
      promptIds2 = sample(1:nrow(questionBank2), size = nrow(questionBank2), replace = FALSE),
      # Create current index
      currentIndex2 = 1,
      # Create a flag if current question has been answered
      answered2 = FALSE,
      # Create a flag for first visit to page
      firstTime2 = TRUE
    )

    ### Set watcher to iterate current question
    observeEvent(
      eventExpr = c(input$pages, input$newChallenge2),
      handlerExpr = {
        if (input$pages == "explore" & challengeElements2$firstTime2) {
          challengeElements2$firstTime2 <- FALSE
        } else if (challengeElements2$currentIndex2 == nrow(questionBank2)) {

        } else {
          challengeElements2$currentIndex2 <- challengeElements2$currentIndex2 + 1
        }
      }
    )

    ### Display challenge plot and question
    output$questionPlot2 <- renderUI(
      expr = {
        questionId2 <- challengeElements2$promptIds2[challengeElements2$currentIndex2]
        tagList(
          HTML(questionBank2$extraOutput[questionId2]),
          p(questionBank2$question[questionId2])
        )
      }
    )

    random_order2 <- reactiveVal()

    random_choice2 <- function() {
      choices <- c("1", "2", "3")
      random_order2 <- sample(choices)
      return(random_order2)
    }

    random_choice2()

    observe({
      random_order_val2 <- random_order2()

      output$choiceA <- renderUI({
        questionId2 <- challengeElements2$promptIds2[challengeElements2$currentIndex2]
        tagList(
          HTML(questionBank2[[paste0("A")]][questionId2])
        )
      })

      output$choiceB <- renderUI({
        questionId2 <- challengeElements2$promptIds[challengeElements2$currentIndex2]
        tagList(
          HTML(questionBank2[[paste0("B")]][questionId2])
        )
      })

      output$choiceC <- renderUI({
        questionId2 <- challengeElements2$promptIds[challengeElements2$currentIndex2]
        tagList(
          HTML(questionBank2[[paste0("C")]][questionId2])
        )
      })
    })

    current_question2 <- reactiveVal()

    random_question2 <- function() {
      random_index2 <- sample(nrow(questionBank2), 1)
      current_question2(questionBank2[random_index2, ])
    }

    random_question2()

    output$question2 <- renderText({
      current_question2()$question2
    })

    output$extraOutput2 <- renderImage({
      if (!is.null(current_question2()$extraOutput2) && nchar(current_question2()$extraOutput2) > 0)
      {
        return(list(src = current_question2()$extraOutput2))
      }
    },
    deleteFile = FALSE)

    output$A <- renderText({
      if (!is.null(current_question2()$A) && nchar(current_question2()$A) > 0)
      {
        return(list(src = current_question2()$A))
      }
    })

    output$B <- renderText({
      if (!is.null(current_question2()$B) && nchar(current_question2()$B) > 0)
      {
        return(list(src = current_question2()$B))
      }
    })

    output$C <- renderText({
      if (!is.null(current_question2()$C) && nchar(current_question2()$C) > 0)
      {
        return(list(src = current_question2()$C))
      }
    })

    observeEvent(
      eventExpr = input$submit2,
      handlerExpr = {
        user_answer2 <- input$response2
        correct_answer2 <- current_question2()$answer2

        if (user_answer2 == correct_answer2) {
          output$icon2 <- renderIcon("correct", width = 40)
        } else {
          output$icon2 <- renderIcon("incorrect", width = 40)
        }

      }
    )

    ### Get new challenge and reset feedback
    observeEvent(
      eventExpr = input$newChallenge2,
      handlerExpr = {
        random_choice2()
        random_question2()

        updateSelectInput(
          session = session,
          inputId = "answer2",
          selected = ""
        )

        output$icon2 <- renderIcon()
        output$response2 <- renderUI(NULL)

      }
    )

    ## Challenge ----
    # ###Mixed Strategy ----
    # challengeElements4 <- reactiveValues(
    #   # Creates a vector of shuffled integers which we can use for the id column
    #   promptIds4 = sample(1:nrow(questionBank4), size = nrow(questionBank4), replace = FALSE),
    #   # Create current index
    #   currentIndex4 = 1,
    #   # Create a flag if current question has been answered
    #   answered4 = FALSE,
    #   # Create a flag for first visit to page
    #   firstTime4 = TRUE
    # )
    #
    # ### Set watcher to iterate current question
    # observeEvent(
    #   eventExpr = c(input$pages, input$newChallenge4),
    #   handlerExpr = {
    #     if (input$pages == "example" & challengeElements4$firstTime4) {
    #       challengeElements4$firstTime4 <- FALSE
    #     } else if (challengeElements4$currentIndex4 == nrow(questionBank4)) {
    #
    #     } else {
    #       challengeElements4$currentIndex4 <- challengeElements4$currentIndex4 + 1
    #     }
    #   }
    # )
    #
    # ### Display challenge plot and question
    # output$questionPlot4 <- renderUI(
    #   expr = {
    #     questionId4 <- challengeElements4$promptIds4[challengeElements4$currentIndex4]
    #     tagList(
    #       HTML(questionBank4$extraOutput4[questionId4]),
    #       p(questionBank4$question4[questionId4])
    #     )
    #   }
    # )
    #
    # random_order4 <- reactiveVal()
    #
    # random_choice4 <- function() {
    #   choices <- c("1", "2", "3")
    #   random_order4 <- sample(choices)
    #   return(random_order4)
    # }
    #
    # random_choice4()
    #
    # observe({
    #   random_order_val4 <- random_order4()
    #
    #   output$choiceA <- renderUI({
    #     questionId4 <- challengeElements4$promptIds4[challengeElements4$currentIndex4]
    #     tagList(
    #       HTML(questionBank4[[paste0("A")]][questionId4])
    #     )
    #   })
    #
    #   output$choiceB <- renderUI({
    #     questionId4 <- challengeElements4$promptIds4[challengeElements4$currentIndex4]
    #     tagList(
    #       HTML(questionBank4[[paste0("B")]][questionId4])
    #     )
    #   })
    #
    #   output$choiceC <- renderUI({
    #     questionId4 <- challengeElements4$promptIds4[challengeElements4$currentIndex4]
    #     tagList(
    #       HTML(questionBank4[[paste0("C")]][questionId4])
    #     )
    #   })
    # })
    #
    # current_question4 <- reactiveVal()
    #
    # random_question4 <- function() {
    #   random_index4 <- sample(nrow(questionBank4), 1)
    #   current_question4(questionBank4[random_index4, ])
    # }
    #
    # random_question4()
    #
    # output$question4 <- renderText({
    #   current_question4()$question4
    # })
    #
    # output$extraOutput4 <- renderImage({
    #   if (!is.null(current_question4()$extraOutput4) && nchar(current_question4()$extraOutput4) > 0)
    #   {
    #     return(list(src = current_question4()$extraOutput4))
    #   }
    # },
    # deleteFile = FALSE)
    #
    # output$A <- renderText({
    #   if (!is.null(current_question4()$A) && nchar(current_question4()$A) > 0)
    #   {
    #     return(list(src = current_question4()$A))
    #   }
    # })
    #
    # output$B <- renderText({
    #   if (!is.null(current_question4()$B) && nchar(current_question4()$B) > 0)
    #   {
    #     return(list(src = current_question4()$B))
    #   }
    # })
    #
    # output$C <- renderText({
    #   if (!is.null(current_question4()$C) && nchar(current_question4()$C) > 0)
    #   {
    #     return(list(src = current_question4()$C))
    #   }
    # })
    #
    # observeEvent(
    #   eventExpr = input$submit4,
    #   handlerExpr = {
    #     user_answer4 <- input$response4
    #     correct_answer4 <- current_question()$answer4
    #
    #     if (user_answer4 == correct_answer4) {
    #       output$icon4 <- renderIcon("correct", width = 40)
    #     } else {
    #       output$icon4 <- renderIcon("incorrect", width = 40)
    #     }
    #
    #   }
    # )
    #
    # ### Get new challenge and reset feedback
    # observeEvent(
    #   eventExpr = input$newChallenge4,
    #   handlerExpr = {
    #     random_choice4()
    #     random_question4()
    #
    #     updateSelectInput(
    #       session = session,
    #       inputId = "answer4",
    #       selected = ""
    #     )
    #
    #     output$icon4 <- renderIcon()
    #     output$response4 <- renderUI(NULL)
    #
    #   }
    # )

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

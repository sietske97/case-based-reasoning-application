#------------------------------------------#
## sourcing of files for libraries and functions
source("R/01_libraries.R")
source("R/02_functions.R")
source("R/03_defining_values.R")

#------------------------------------------#
## interface of the app ##
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  column(
    width = 12,
    div(style = "height:50px")
  ),
  # Title of application
  column(
    width = 12,
    style = "font-weight: bold",
    titlePanel(
      title = "Case-Based Reasoning application"
    )
  ),

  # Loading balk if application is loading
  tags$head(tags$style(type = "text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #F5F5F5;
               z-index: 105;
             }
          ")),
  conditionalPanel(
    condition = "$('html').hasClass('shiny-busy')",
    tags$div("Loading...", id = "loadmessage")
  ),
  tabsetPanel(
    # Horizontal tabs
    tabPanel(
      # First horizontal tab with general information
      title = tags$h4(tags$strong("General information")),
      navlistPanel(
        # Vertical tabs
        tabPanel(
          title = tags$h4("Introduction"),
          tags$h2(tags$strong("Introduction")),
          fluidRow(
            column(
              width = 6,
              tags$h4(tags$strong("What is the goal of this application?")),
              tags$p("The goal of this application is to show the operation and associated value of a Case-Based Reasoning system."),
              tags$h4(tags$strong("What does the model predict??")),
              tags$p("The model predicts (classifies) for new cases the type of conviction that the suspect of the new case is likely to receive: detention or non-detention. Detention is mainly imprisonment and non-detention is convictions such as community service. A public dataset is used containing all convictions in a certain district in America. As output, the model generates a class (detention or non-detention) and associated probability, and a list of most-comparable old convictions and a similarity score between the query case and the old case.")
            ),
            column(
              width = 6,
              tags$h4(tags$strong("What are the functionalities of the app")),
              tags$p("This application consists of two tabs: a tab with 'general information' and a tab with 'generate query'"),
              tags$p(""),
              tags$p("Under the 'general information' tab you can find information about CBR, the test data used and the CBR model used. Under the cup
                   'what is CBR' is a general explanation of a CBR system. Under the heading 'used test set'
                   the used test set is described, and the heading 'used CBR model' indicates how the model was created
                   has been established and contains some general data (accuracy, etc.) of the model."),
              tags$p(""),
              tags$p("Under the generate query tab you can try out the CBR model yourself. You have three options here:
                  you can enter values for all variables yourself, you can give the system a random observation
                   generated or you can upload a .csv file containing a new observation. The results
                  of this query also appear on this tab.")
            )
          )
        ),

        tabPanel(
          title = tags$h4("What is Case Based Reasoning?"),
          fluidRow(
            column(
              width = 8,
              tags$h2(tags$strong("Wat is Case-Based Reasoning?")),
              tags$p("Case-Based Reasoning is a method in which new problems are solved based on solutions from
                  similar problems from the past. Based on the similarity between a new 'problem' and
                  an old solution, a solution can be found for the new problem. The CBR model in this one
                  web application is looking for a solution to the problem 'What type of detention does a convicted receive?'.
                  There are two possible solutions: detention and non-detention. Based on old issues and associated
                  solutions (these are old convictions) can provide a solution for the new problem
                  turn into; in other words: what type of detention will a convicted person receive."),
              tags$p(""),
              tags$h4(tags$strong("Which algorithm does the CBR-model use?")),
              tags$p("Underlying the CBR model, the K-nearest-neighbors (KNN) algorithm is used.
                  KNN is a supervised machine learning algorithm and its operation is relatively simple.
                  A supervised algorithm needs labeled data as input to output a label
                  to provide. The algorithm can be used for classification problems and regression problems."),
              tags$p(""),
              tags$h4(tags$strong("How does K-nearest neighbours work?")),
              tags$p("KNN works with a database and a new case. Based on a formula, the distance between
             calculated the new case (also called the query) and the cases in the database. This happens on
             based on the variables of the data. Since KNN calculates the absolute distance, the data is first
             normalized. After the distance has been calculated, a self-selected number of cases are entered
             selected the database closest to the new case. This self-chosen number
             is the 'k' in the nearest neighbors algorithm. Then, in the case of classification,
             of the selected cases in the database looked at which class they have. Thereafter
             it is simply 'voted': the class that most of the selected cases have
             is assigned to the new case."),
              tags$p(""),
              tags$h4(tags$strong("Advantages of Case-Based Reasoning")),
              tags$p("Case-Based Reasoning is based on the value of solutions to old problems. These solutions can
                   namely also reused for new problems. This makes CBR different from other types of classification
                   methods such as logistic regression and random forest models. These models must be trained in advance,
                   and do not only generate a solution when new data is available. A CBR model is also pre-trained,
                   but only to determine the ideal k. When new data is available, a CBR model searches the case base for similar cases. Thereafter
                   the 'solution' of the most similar problem, or problems, is presented as the solution for the new case."),
              tags$h4(tags$strong("Phases of a CBR-system")),
              img(src = "cbr_cyclus.png"),
              tags$p("A CBR system consists of four different phases; these are depicted in the image above The different phases are described below."),
              tags$strong("1. Retrieve phase: inside the system"),
              tags$p("The cycle of CBR starts with a new problem, the ‘current problem’. Subsequently, the case base, which contains all old cases, is searched for similar cases. This is done using an algorithm that calculates the distance and similarity between the new and old case with a distance measure. Various algorithms can be used for this, but a KNN algorithm is used in the prototype that has been developed. Next, the closest old cases are retrieved and displayed to the user.

"),
              tags$strong("2. Reuse phase: in and outside the system"),
              tags$p("Based on the “retrieved case”, a solution for the new case is proposed. This can be done in various ways. The simplest method is to apply the “retrieved case” solution directly to the “new case”. However, it is also possible to choose not to return one old case, but several. In that case, by means of a vote, the solution that most old cases had, it can be determined which solution is applied to the new case. Here too, there are many more options, which depend on the available data and the domain of the CBR prototype. The solution is then applied to the new case outside the system.

"),
              tags$strong("3. Revise phase: outside the system"),
              tags$p("This step is optional in a CBR system. After the solution has been applied to the new case, it becomes clear over time whether the solution works. In the event that the solution did not work, the case can go through the CBR cycle again. The CBR system can then provide a revised solution for the failed case. This can then be applied to the case outside the system. Whether this step actually takes place depends on the context of the CBR system.

"),
              tags$strong("4. Retain phase: inside the system"),
              tags$p("As a final step in the CBR system, the new case, with accompanying solution, can be added to the case base if this provides value for the system. If the new case is added, cases looking for a solution in the future can also learn from the solution of the new case, which was successful or not. In this way, a CBR system can learn from its own advice. It’s important to make sure the system doesn’t create its own bias by storing its own stuff. The risk of this differs per system.

")
            ),
            column(
              width = 4,
              column(
                width = 12,
                div(style = "height:50px")
              ),
              column(
                width = 2
              ),
              column(
                width = 10,
                style = "border-style: ridge;width:400px;",
                tags$h4(tags$strong("What's the difference between classification and regression?")),
                tags$p("For classification the model must predict a class and for regression the model must predict a
                    number."),
                tags$strong("Examples of classification problems:"),
                tags$li("Identifying spam mails. Two labels: spam and non-spam"),
                tags$li("Identifying a disease. Two labels: sick and non-sick"),
                tags$li("Identifying certain groups of customers. Two labels: buy and do not buy after advertising"),
                tags$p(""),
                tags$strong("Examples of regression problems:"),
                tags$li("Predicting a company's currency turnover"),
                tags$li("Predicting ice cream sales in a given year: quantity in currency"),
                tags$li("Predicting the temperature in a given month: quantity in temperature"),
                tags$p("")
              ),
            )
          )
        ),
        tabPanel(
          tags$h4("The 'sentence' dataset"),
          column(
            width = 11,
            tags$h2(tags$strong("The 'sentence' dataset")),
            tags$p("For this CBR model an open dataset has been used by the Cook County Government. This dataset contains more than 250,000 convictions of thousands of people convicted in Cook County for various types of crimes. "),
            tags$a(href = "https://datacatalog.cookcountyil.gov/Courts/Sentencing", "The dataset can be downloaded via this link"),
            tags$h4(tags$strong("Aanpassingen aan data")),
            tags$p("Various adjustments have been made to be able to use the data for this CBR system.
                  The modifications have been applied to the functions read_and_clean, found in the functions.R file, but the most important modifications are listed below:"),
            tags$li("The original SENTENCE_TYPE column contained different types of detention. These have been adjusted as stated in
                   the explanation of the data on the Cook County website, by two categories: detention and non-detention"),
            tags$li("The database contained many duplicate observations, both duplicate charge_id and person_ids. Therefore
                   these observations have been removed from the dataset."),
            tags$li("Several variables have been removed from the dataset. In the end there will be eight
                   predictive variables included in the model, and there are three variables that provide more info
                   give about the case. These variables are shown in the tab 'used CBR model'."),
            tags$li("A sample of 1/100 of the entire database has been taken because of a large number of observations
                   slow down the CBR model. It has been ensured that all groups are equally represented
                   are in the model."),
            tags$li("This sample was then divided into a test set and a train set. The trainset is used
                   as a case base, and the test set was used only to test the model's operation."),
            tags$h4(tags$strong("Impression of the sentence dataset")),
            tags$p("Below you can see the first ten observations of the trainset. The total trainset exists
                  of 2791 observations of 12 different variables."),
            DT::dataTableOutput("head_trainset")
          )
        ),
        tabPanel(
          tags$h4("The developed CBR-model"),
          column(
            width = 11,
            tags$h2(tags$strong("The developed CBR-model")),
            tags$p("The model is built in R, and no updated Case-Based Reasoning package is available in this programming language. That is why I wrote code for a CBR model myself, based on an existing KNN package. Below I briefly explain how I used the FNN package to build a CBR model. Then I will briefly discuss how the most ideal k for the sentence data was found using a test and train set."),
            tags$h4(tags$strong("Project workflow based on targets")),
            tags$p("I made this project using the targets package. This is a package with which you can easily create a clear project workflow. This means that you write your code in functions, so that it is clear to others what is happening in your code. In addition, the package has the advantage that it is very time-saving: if you have only changed one step of your workflow, only the modified step will be executed."),
            tags$p("The targets package is also used to generate a clear workflow of your code, the workflow of my project can be found below. Click on the steps of the process to read a brief description of the step. The final CBR model can be found under 'results_cbr' and the generated query is the step 'random_query'"),
            includeHTML("www/workflow.html"),
            tags$h4(tags$strong("From a KNN package to a CBR Model")),
            tags$p("Many KNN packages are available in R, but no CBR package. That's why I used the FNN (Fast Nearest Neighbors) package to build a CBR model. The FNN package has several options for KNN, and in addition to the predicted class / value also generates a distance matrix and an index table, containing the nearest neighbors. I have used these two tables to build the CBR system."),
            tags$p(""),
            tags$code("similarity = 1 - (afstand tussen twee punten / maximiale afstand)"),
            tags$p("A distance matrix is a table containing the distance between the new case and the cases in the database. At KNN, the k-nearest neighbors of the new case are selected: these are the cases with the lowest distance. For a CBR model you do not want to generate a distance, but a similarity percentage. The code contains a piece that contains the maximum distance based on can generate a dataset. This calculates the distance between two cases: one of the cases has all zeros as an observation, and another case has all ones as an observation. The result of this is the maximum distance between two points in a data set. The following formula is used to calculate the similarity percentage between the query and the old cases:"),
            tags$p("Then I used the index table to generate a table with the k-nearest neighbors. The model simply displays the k-nearest neighbors based on the similarity percentage. The query case class is generated based on the voting principle: the predicted class is the class that has the most of the nearest neighbors. However, the probability of voting is shown here. This is the percentage of the nearest neighbors that also has this class."),
            tags$h4(tags$strong("KNN-methode en ideale K bepalen op basis van test- en train set")),
            tags$p("The KNN algorithm of the FNN package has two important settings: the number of nearest neighbors that are selected and the method of the KNN: 'kd-tree', 'cover_tree', or 'brute'. In addition, several predictive variables can be used: all predictor variables or only predictor variables that have a certain significance level. This significance can be calculated using logistic regression."),
            tags$p("The ideal K, method of KNN and which predictor variables should be included can be calculated by running the KNN algorithm on a test set for each of these possibilities (the three methods with the two types of predictor variables). The data is split into a train set of 70% and a test set of 30%. The KNN algorithm is then applied to the test set, with k from 1 to 300. Thus, the class is predicted for each case in the test set. Subsequently, this predicted class can be compared with the real class. This results in an 'accuracy level'. This is the percentage of class of the test set that the model predicted correctly."),
            tags$p("The graph below shows the accuracy of the model for each of the combinations, up to and including k = 300. As can be seen, the model performs better when all predictors are included and this is also incorporated in the model. The model generates the highest accuracy, 64.9%, with a k-value of 15 and the brute method with the method. These are the default settings for the CBR model. The accuracy is not very high, but that is also because this model was built to illustrate the value of the CBR method with a test database and not to generate the highest possible accuracy. It should be noted, however, that the percentage of convictions in the database with a 'sentence' is only 54.9%. This means that the model is better able to predict convictions than if all predictions are labeled 'detention'. In that case the accuracy will only be 54.9%.")
          ),
          column(
            width = 10,
            offset = 1,
            img(
              src = "plot_knn.png",
              style = "display: block; margin-left: auto; margin-right: auto;"
            )
          )
        )
      )
    ),
    tabPanel(
      title = tags$h4(tags$strong("CBR Model")),
      navlistPanel(
        tabPanel(
          title = tags$h4("Generate query"),
          fluidPage(
            column(
              width = 12,
              style = "text-align: center",
              tags$h2(tags$strong("Generate query"))
            ),
            column(
              width = 10,
              offset = 1,
              tags$p("Choose a method here to generate a new query for the CBR model. There are three options: upload a .csv file in the correct format, generate a random query, or enter a new query manually. Below each method is a separate submit button: upload query for the first method, generate random query for the second method, and submit query for the third method. The generated query will appear a few seconds after you press the button at the bottom of the page. When you are satisfied with the query, click on 'accept' query. The query is now being run. The output of the model can be found under the tab 'CBR model results'.")
            ),
            column(
              width = 1
            ),
            column(
              width = 12,
              #  hr(style = "border-top: 1px solid #606060;")
            ),
            column(
              width = 3,
              style = ("text-align: center;height: 950px"),
              tags$h3(tags$strong("1. Upload bestand")),
              fileInput("csv_cbr",
                label = "Upload a .csv file"
              ),
              actionButton(
                inputId = "upload_file",
                label = "Upload query"
              )
            ),
            column(
              width = 3,
              offset = 0.5,
              style = ("text-align: center;height: 950px"),
              tags$h3(tags$strong("2. Generate random query")),
              column(
                width = 12,
                div(style = "height:72px;"),
                actionButton(
                  inputId = "random_query",
                  label = "Generate random query"
                ),
                tags$p(""),
                tags$strong(textOutput("random_query_submit"))
              )
            ),
            column(
              width = 6,
              tags$h3(tags$strong("3. Manual query inputquery")),
              style = ("text-align: center;height: 950px"),
              column(
                width = 6,
                style = "text-align: center",
                inputPanel(
                  tags$strong("Non-predictive variables"),
                  helpText("These are variables that are not included by the model as predictive variables, but are presented purely as additional information."),
                  numericInput(
                    inputId = "een",
                    label = "case_id",
                    value = NULL,
                    min = min_value[[1]],
                    max = max_value[[1]]
                  ),
                  selectInput(
                    inputId = "zeven",
                    label = "gender",
                    choices = levels(database[[7]])
                  )
                )
              ),
              column(
                width = 6,
                style = "text-align: center",
                inputPanel(
                  tags$strong("Predicting variables"),
                  helpText("These are predictive variables used by the model to predict the query case class."),
                  numericInput(
                    inputId = "twee",
                    label = "charge count",
                    value = NULL,
                    min = min_value[[2]],
                    max = max_value[[2]]
                  ),
                  selectInput("drie",
                    label = "charge disposition",
                    choices = levels(database[[3]])
                  ),
                  selectInput(
                    inputId = "vier",
                    label = "sentence court name",
                    choices = levels(database[[4]])
                  ),
                  numericInput(
                    inputId = "vijf",
                    label = "length of case in days",
                    value = NULL,
                    min = min_value[[5]],
                    max = max_value[[5]]
                  ),
                  numericInput(
                    inputId = "zes",
                    label = "age at incident",
                    value = NULL,
                    min = min_value[[6]],
                    max = max_value[[6]]
                  ),
                  selectInput(
                    inputId = "acht",
                    label = "offense category",
                    choices = levels(database[[8]])
                  ),
                  numericInput(
                    inputId = "negen",
                    label = "days incident arrest",
                    value = NULL,
                    min = min_value[[9]],
                    max = max_value[[9]]
                  ),
                  selectInput(
                    inputId = "elf",
                    label = "convicted chicago",
                    choices = levels(database[[11]])
                  ),
                  actionButton(
                    inputId = "submit",
                    label = "Submit query"
                  ),
                  tags$p(""),
                  tags$strong(textOutput("submit_message"))
                )
              )
            ),
            column(
              width = 12,
              style = "text-align: center",
              uiOutput("gegenereerde_query")
            )
          )
        ),
        tabPanel(
          # Tweede verticale tab op CBR model horizontale tab
          # Tab om het CBR model te runnen met de gegenereerde query
          title = tags$h4("Run CBR-model"),
          column(
            width = 12,
            tags$h2(tags$strong("Run CBR-model")),
            style = "text-align: center",
            column(
              width = 10,
              style = "text-align: left",
              offset = 1,
              tags$p("The KNN algorithm, which runs under the CBR model, has two additional settings: the number of nearest neighbors that are selected (K) and the method by which they are found. The settings that provide the highest accuracy are selected, but these settings can be adjusted here.")
            ),
            column(
              width = 1
            ),
            column(
              width = 8,
              style = "text-align: center",
              inputPanel(
                numericInput("k",
                  label = "Number for K",
                  value = 15
                ),
                selectInput("cbr_method",
                  label = "CBR-methode",
                  choices = c("kd_tree", "cover_tree", "brute"),
                  selected = "brute"
                )
              )
            ),
            column(
              width = 4,
              inputPanel(
                column(
                  width = 12,
                  style = "height: 10px"
                ),
                column(
                  width = 12,
                  actionButton(
                    inputId = "run_cbr_model",
                    label = "Run CBR-model"
                  )
                ),
                column(
                  width = 12,
                  style = "height: 25px"
                )
              )
            )
          ),
          column(
            width = 12,
            tags$h2(tags$strong(textOutput("resultaten_cbr_model"))),
            style = "text-align: center",
            column(
              width = 6,
              tags$h3(tags$strong(textOutput("voorspelde_klasse_text"))),
              style = "text-align: right",
            ),
            column(
              width = 6,
              h3(strong(textOutput("voorspelde_klasse_query"))),
              style = "text-align: left"
            )
          ),
          column(
            width = 12,
            column(
              width = 6,
              tags$h3(tags$strong(textOutput("percentage_neighbours_text")),
                style = "text-align: right"
              )
            ),
            column(
              width = 6,
              h3(strong(textOutput("percentage_neighbours")),
                style = "text-align: left"
              ),
            )
          ),
          column(
            width = 12,
            uiOutput("streep")
          ),
          column(
            width = 10,
            offset = 1,
            tags$h3(tags$strong(textOutput("nearest_neighbours_query")),
              style = "text-align: center"
            )
          ),
          column(width = 1),
          column(
            width = 12,
            DT::dataTableOutput("query_cbr_results")
          ),
          column(
            width = 12,
            uiOutput("accepting_query")
          ),
          column(
            width = 12,
            uiOutput("nieuwe_case_base"),
          ),
          column(
            width = 12,
            dataTableOutput("update_casebase")
          )
        )
      )
    )
  )
)

#------------------------------------------#
# server = code om content in de UI te genereren
server <- function(input, output) {
  
  reactive <- reactiveValues(value = "leeg")
  observe({
    input$random_query
    reactive$value <- "random_query"
  })
  observe({
    input$submit
    reactive$value <- "manual_query"
  })
  observe({
    input$upload_file
    reactive$value <- "upload_file"
  })

  output$head_trainset <- DT::renderDataTable(
    {
      database %>%
        relocate(sentence, gender, case_id)
    },
    options = list(scrollX = TRUE)
  )

    output$accept_query_message <- renderText({
    if (input$run_cbr_model > 0) {
      return("")
    } else if (input$accept_query > 0) {
      return("The query is accepted")
    } else {
      return("")
    }
  })

  output$submit_message <- renderText({
    if (input$run_cbr_model > 0) {
      return("")
    } else if (input$submit == 1) {
      return("The query is generated")
    } else if (input$submit > 1) {
      return("The new query is generated")
    }
    else {
      return("")
    }
  })

  output$random_query_submit <- renderText({
    if (input$run_cbr_model > 0) {
      return("")
    } else if (input$random_query == 1) {
      return("The random query is generated")
    } else if (input$random_query > 1) {
      return("The new random query is generated")
    }
    else {
      return("")
    }
  })

  output$gegenereerde_query <- renderUI({
    if (reactive$value == "random_query") {
      display_random_query()
      taglist <- list(fluidPage(
        column(
          width = 12,
          tags$h2(tags$strong("The generated query")),
          DT::DTOutput("query_display", width = "100%"),
          DT::DTOutput("random_query_display"),
          hr(style = "border-top: 1px solid #606060;"),
          tags$h2(tags$strong("Accepting of query")),
          style = "text-align: center",
          column(
            width = 12,
            style = "height: 20px"
          )
        ),
        column(
          width = 8,
          style = "text-align: left",
          tags$p("Click on 'accept query' to accept the query. Then go to the tab 'run CBR model', where you can run the query.")
        ),
        column(
          width = 4,
          actionButton(
            inputId = "accept_query",
            label = "Accept query"
          ),
          tags$p(""),
          tags$strong(textOutput("accept_query_message"))
        )
      ))
    }
    else if (reactive$value == "manual_query") {
      display_query()
      list(fluidPage(
        column(
          width = 12,
          tags$h2(tags$strong("The generated query")),
          DT::DTOutput("query_display", width = "100%"),
          DT::DTOutput("random_query_display"),
          hr(style = "border-top: 1px solid #606060;"),
          tags$h2(tags$strong("Accepting of query")),
          style = "text-align: center",
          column(
            width = 12,
            style = "height: 20px"
          )
        ),
        column(
          width = 8,
          style = "text-align: left",
          tags$p("Click on 'accept query' to accept the query. Then go to the tab 'run CBR model', where you can run the query.")
        ),
        column(
          width = 4,
          actionButton(
            inputId = "accept_query",
            label = "Accept query"
          ),
          tags$p(""),
          tags$strong(textOutput("accept_query_message"))
        )
      ))
    }
    else if (reactive$value == "upload_file") {
      display_query_manual()
      list(fluidPage(
        column(
          width = 12,
          tags$h2(tags$strong("The generated query")),
          DT::DTOutput("query_display", width = "100%"),
          DT::DTOutput("random_query_display"),
          hr(style = "border-top: 1px solid #606060;"),
          tags$h2(tags$strong("Accepting of query")),
          style = "text-align: center",
          column(
            width = 12,
            style = "height: 20px"
          )
        ),
        column(
          width = 8,
          style = "text-align: left",
          tags$p("Click on 'accept query' to accept the query. Then go to the tab 'run CBR model', where you can run the query.")
        ),
        column(
          width = 4,
          actionButton(
            inputId = "accept_query",
            label = "Accept query"
          ),
          tags$p(""),
          tags$strong(textOutput("accept_query_message"))
        )
      ))
    }
  })

  display_query <- eventReactive(input$submit, {
    query <- c(
      input$een, input$twee, input$drie, input$vier, input$vijf, input$zes, input$zeven, input$acht,
      input$negen, "", input$elf
    )
    query_data <- database %>%
      rbind(query) %>%
      tail(1) %>%
      mutate_if(is.character, as.numeric)
  })

  display_query_manual <- eventReactive(input$upload_file, {
    query <- read_delim(input$csv_cbr$datapath,
      ";",
      escape_double = FALSE, trim_ws = TRUE
    )
    query_data <- database %>%
      rbind(query) %>%
      tail(1) %>%
      mutate_if(is.character, as.numeric)
    return(query_data)
  })
  display_random_query <- eventReactive(input$random_query, {
    min_value <- NULL
    max_value <- NULL
    levels <- NULL
    random_level <- NULL
    value <- NULL
    for (i in 1:ncol(database)) {
      if (!(is.numeric(database[[i]]))) {
        min_value[[i]] <- NA
        max_value[[i]] <- NA
        levels[[i]] <- levels(database[[i]])
      } else {
        min_value[[i]] <- min(database[[i]])
        max_value[[i]] <- max(database[[i]])
        levels[[i]] <- levels(database[[i]])
      }
    }
    for (i in 1:ncol(database)) {
      if (is.na(min_value[i])) {
        all_level <- levels[[i]]
        random_level[[i]] <- sample(all_level, 1)
      } else {
        a <- c(min_value[[i]], max_value[[i]])
        random_level[[i]] <- sample(a, 1)
      }
    }
    query_data <- database %>%
      rbind(random_level) %>%
      tail(1) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(sentence = "")
    return(query_data)
  })

  output$random_query_display <- DT::renderDataTable(
    {
      if (reactive$value == "random_query") {
        query <- display_random_query()
        query <- query %>%
          relocate(sentence, gender, case_id)
        return(query)
      }
      if (reactive$value == "manual_query") {
        query <- display_query()
        query <- query %>%
          relocate(sentence, gender, case_id)
        return(query)
      }
      if (reactive$value == "upload_file") {
        query <- display_query_manual()
        query <- query %>%
          relocate(sentence, gender, case_id)
        return(query)
      }
    },
    options = list(scrollX = TRUE)
  )

  run_cbr_model <- eventReactive(input$run_cbr_model, {
    if (reactive$value == "random_query") {
      random_query_data <- display_random_query()
      display_the_results <- cbr_model(
        database = database,
        query = random_query_data,
        methode = input$cbr_method,
        k = input$k
      )
      return(display_the_results)
      break()
    }
    if (reactive$value == "manual_query") {
      custom_query_data <- display_query()
      display_the_results <- cbr_model(
        database = database,
        query = custom_query_data,
        methode = input$cbr_method,
        k = input$k
      )
      return(display_the_results)
      break()
    }
    if (reactive$value == "upload_file") {
      manual_query_data <- display_query_manual()
      display_the_results <- cbr_model(
        database = database,
        query = manual_query_data,
        methode = input$cbr_method,
        k = input$k
      )
      return(display_the_results)
      break()
    }
  })

  output$voorspelde_klasse_text <- renderText({
    run_cbr_model()
    return("The predicted class, the proposed solution, of the query case is:")
  })
  output$voorspelde_klasse_query <- renderText({
    results <- run_cbr_model()
    return(results$class)
  })
  output$percentage_neighbours_text <- renderText({
    run_cbr_model()
    return("This is suggested by the following percentage of the nearest neighbors:")
  })
  output$percentage_neighbours <- renderText({
    results <- run_cbr_model()
    text <- round((results$probability * 100), digits = 2)
    text <- paste(text, "%")
    return(text)
  })

  output$resultaten_cbr_model <- renderText({
    run_cbr_model()
    return("Resultaten CBR model")
  })
  output$nearest_neighbours_query <- renderText({
    run_cbr_model()
    return("Nearest neighbours of the query case:")
  })
  output$streep <- renderUI({
    run_cbr_model()
    hr(style = "border-top: 1px solid #606060;")
  })

  output$query_cbr_results <- DT::renderDataTable(
    {
      results <- run_cbr_model()
      data <- results$results_dataframe
      data <- data %>%
        mutate(similarity = round(similarity, digits = 4)) %>%
        mutate(similarity = paste(similarity * 100, "%", sep = ""))
    },
    options = list(
      scrollX = TRUE,
      digits = 3
    )
  )

  add_query_to_casebase <- eventReactive(input$toevoegen_casebase, {
    if (reactive$value == "random_query") {
      add_row <- display_random_query()
      results <- run_cbr_model()
      add_row <- add_row %>%
        mutate(sentence = results$class)
      database <- database %>%
        full_join(add_row)
      return(database)
      break()
    }
    if (reactive$value == "manual_query") {
      add_row <- display_query()
      results <- run_cbr_model()
      add_row <- add_row %>%
        mutate(sentence = results$class)
      database <- database %>%
        full_join(add_row)
      return(database)
      break()
    }
    if (reactive$value == "upload_file") {
      add_row <- display_query_manual()
      results <- run_cbr_model()
      add_row <- add_row %>%
        mutate(sentence = results$class)
      database <- database %>%
        full_join(add_row)
      return(database)
      break()
    }
  })

  output$accepting_query <- renderUI({
    run_cbr_model()
    tagList(column(
      width = 12,
      hr(style = "border-top: 1px solid #606060;"),
      tags$h2(tags$strong("Adding to the case base?")),
      style = "text-align: center",
      column(
        width = 12,
        style = "height: 20px"
      ),
      column(
        width = 8,
        style = "text-align: left",
        tags$p("Add the query containing the proposed solution to the case base?")
      ),
      column(
        width = 4,
        actionButton("toevoegen_casebase",
          label = "Add to the case base"
        ),
        tags$p("")
      )
    ))
  })


  # Renderen van nieuwe kolom onder pagina met daarin nieuwe case base
  output$nieuwe_case_base <- renderUI({
    add_query_to_casebase()
    tagList(column(
      width = 12,
      style = "text-align: center",
      p(""),
      hr(style = "border-top: 1px solid #606060;"),
      tags$h2(tags$strong("New case base")),
      p("This is the new case-base with the runned query case")
    ))
  })

  # Datatable met nieuwe case base
  output$update_casebase <- renderDataTable(
    {
      data <- add_query_to_casebase()
      data <- data %>%
        relocate(sentence, gender, case_id)
      return(data)
    },
    options = list(
      scrollX = TRUE,
      pageLength = 10
    )
  )
}

#------------------------------------------#
## Runnen van applicatie ##
shinyApp(ui = ui, server = server)

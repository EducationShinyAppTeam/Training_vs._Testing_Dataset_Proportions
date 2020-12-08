# The core to this concept is that the less data in testing the more variance in
# performance statistic.
# Then less in-training data the parameter estimates have greater variance.
# https://stackoverflow.com/questions/13610074/
# The [name] package does have , "Resistant Regression",
# "multiple Correspondence Analysis", "Quadratic Discriminant Analysis", "
# predict.lqs, predict.mca, predict.qda, predict.profile.glm

# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(caret)
library(kernlab)
library(ggplot2)
library(palmerpenguins)
library(MASS)
library(glmnet)
library(lars)
library(DT)

# Packages for Future Versions
# library(ellipse)
# library(e1071)
# library(randomForest)
# library(profvis)

# Define global functions and constants, load data ----
data(iris)
data("penguins")
marketing <- read.csv("marketing.csv")
heartDisease <- read.csv("HeartDiseaseData.csv")

scaleFUN <- function(x){ sprintf("%.1f", x) }

#Input: data set, the testingPercent (average .8), method used
#output: returns the percent accuracy of the model.
#percent <- calculateAccuracy(dataset, input$testingPercent, input$theMethod, predictor, predictionVariable)
#predictor replaces theDataSet$Species, predictionVariable is just 'variableString'
calculateAccuracy <- function(theDataSet, testingPercent, method, predictor, predictionVariable){
  validation_index <- createDataPartition(
    y = predictor,
    p = testingPercent,
    list = FALSE
  ) #p usually input$testingPercent
  #dataset <- dataset(na.action=na.exclude)
  validation <- theDataSet[-validation_index,]

  trainingDataset <- theDataSet[validation_index,]
  sapply(trainingDataset, class)

  if (method == 'Linear Discriminant Analysis') {
    fit.lda <- lda(
      eval(parse(text = paste(predictionVariable,'~.'))),
      data = trainingDataset,
      na.action = "na.omit"
    )
    predictions <- predict(object = fit.lda, newdata = validation)
    finalPredictions <- predictions$class
    outputType <- "categorical"
  } else if (method == 'Multiple Linear Regression') {
    fit.lm <- lm(
      eval(parse(text = paste(predictionVariable, '~.'))),
      data = trainingDataset
    )
    finalPredictions <- predict(fit.lm, validation)
    outputType <- "continuous"
  } else if (method == 'Logistic Regression') {
    fit.lr <- glm(
      eval(parse(text = paste(predictionVariable, '~.'))),
      data = trainingDataset,
      family = "binomial"
    )
    probabilities <- predict(fit.lr, validation, type = "response")
    if (predictionVariable ==  "sex") {
      finalPredictions <- ifelse(probabilities < 0.5, "female", "male")
    } else if (predictionVariable == "target") {
      finalPredictions <- ifelse(probabilities > 0.5, 1, 0) #1 unhealthy and 2 healthy
    } else {
      print("")
    }
    #fit.lr <- glm(target ~ ., data = trainingDataset, family = "binomial")
    #probabilities <- predict(fit.lr, validation, type = "response")
    #finalPredictions <- ifelse(probabilities > 0.5, "Unhealthy", "Healthy")
    outputType <- "categorical"
  } else if (method == "Ridge Regression") {
    #x <- model.matrix( ~ ., trainingDataset)
    x <- as.matrix(trainingDataset[, names(trainingDataset) != predictionVariable])
    y <- as.matrix(trainingDataset[, predictionVariable])

    fit.glm <- glmnet(x, y, family = "gaussian", alpha = 0, lambda = 0.001)
    finalPredictions <- predict(
      fit.glm,
      data.matrix(validation[, names(trainingDataset) != predictionVariable]),
      type = "response" #type used to = "link"
    )
    outputType <- "continuous"
  } else if (method == "LASSO") {
    # fit model
    #print(theDataSet[,1:3])
    x <- data.matrix(trainingDataset[, names(trainingDataset) != predictionVariable])
    y <- data.matrix(trainingDataset[, predictionVariable])
    fit.lars <- lars(x, y, type = "lasso")
    # select a step with a minimum error
    best_step <- fit.lars$df[which.min(fit.lars$RSS)]
    # make predictions
    finalPredictions <- predict(
      fit.lars,
      data.matrix(validation[,names(trainingDataset) != predictionVariable]),
      s = best_step,
      type = "fit"
    )$fit
    outputType <- "continuous"
  } else {

  }

  #6 Make predictions
  #Estimate accuracy of LDA on validation dataset
  if (outputType == "categorical") {
    count <- 0
    correct <- 0
    for (word in finalPredictions) {
      count <- count + 1
      if (word == eval(parse(text = paste('validation$',predictionVariable,'[',count,']'))))
        correct <- correct + 1
    }
    percentCorrect <- correct / count
    percentCorrect <- percentCorrect * 100
    return(percentCorrect)
  } else {# For continuous
    count <- 0
    MSE <- 0
    for (number in finalPredictions) {
      count <- count + 1
      MSE = MSE + ((number - eval(parse(text = paste('validation$',predictionVariable,'[',count,']'))))^2)
    }
    MSE <- MSE / count
    return(MSE)
  }
}

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "green",
    ## Header ----
    dashboardHeader(
      title = "Training vs. Testing", # You may use a shortened form of the title here
      tags$li(class = "dropdown",
              tags$a(href = 'https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Training_vs._Testing_Dataset_Proportions',
                     icon("comments"))),
      tags$li(class = "dropdown",
              tags$a(href = 'https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ## Sidebar ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "expl", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
        # Future development idea
        # menuItem("ML Challenge", tabName = "challenge", icon = icon("gamepad"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Training vs. Testing Dataset Proportions"),
          p('In this app you will study how the accuracy of a machine learning
            algorithm is affected by the proportion of the cases in the training
            dataset vs the testing dataset. As you explore, pay attention to how
            the accuracy of the estimates along with their standard deviation
            varies with the proportion of cases in the training set.'),
          h2("Instructions"),
          tags$ol(
            tags$li("Look over the prerequisites."),
            tags$li("Choose a Dataset and Variable to Predict"),
            tags$li("Examine the effect of proportion of training data on
                    variance of the accuracy."),
            tags$li("Examine the effect of proportion of training data on the
                    accuracy mean.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Ethan Wright.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 12/08/2020 by NJH")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2('Prerequisites'),
          p(
            'Please refer to the',
            a(
              href = 'https://towardsdatascience.com/workflow-of-a-machine-learning-project-ec1dba419b94',
              'Machine Learning Cheatsheet',
              target = "_blank",
              class = "bodylinks"
            ),
            'for all the information needed.'
          ),
          p('MSE: Mean Square Error. This is used often for statistics and here
            is used to measure how far off a prediction is from the true value.
            The formula is
            \\(\\frac{1}{n}\\sum_{i=1}^n \\left(x_i- \\widehat{x}\\right)^2\\).'),
        ),
        ### Explore Page ----
        tabItem(
          tabName = "expl",
          withMathJax(),
          h2("Explore Testing Proportion"),
          p("Select a dataset and a variable to predict."),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = 'theDataSet',
                label = 'Dataset',
                choices = list(
                  'Heart Disease',
                  'Palmer Penguins',
                  'Marketing',
                  'Iris'
                ),
                selected = 'Heart Disease'
              ),
              selectInput(
                inputId = 'theVariable',
                label = 'Variable to predict',
                choices = c("filler")
              ),
              uiOutput("dataTableInfo")
            ),
            column(
              width = 8,
              p(
                "Variable Names and Descriptions--variables in bold indicate
                which you can select to predict."
              ),
              uiOutput("dataTableVariables")
            )
          ),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                selectInput(
                  inputId = 'theMethod',
                  label = 'Select a method',
                  choices = c("filler"),
                ),
                selectInput(
                  inputId = 'repetitions',
                  label = 'Number of training sets to generate',
                  choices = list(5, 10, 20, 30, 40, 50, 70, 100),
                  selected = 20
                ),
                sliderInput(
                  inputId = 'testingPercent',
                  label = 'Proportion of data in training set',
                  min = 0.2,
                  max = 0.95,
                  value = 0.80,
                  step = 0.05
                ),
                bsButton(
                  inputId = 'newGraphOutput',
                  label = 'Run method',
                  style = "default",
                  size = "large",
                  icon = icon("bolt")
                )
              )
            ),
            column(
              width = 8,
              tabsetPanel(
                id = "results",
                type = "tabs",
                tabPanel(
                  title = "Plots",
                  plotOutput(outputId = "variancePlot", width = "100%"),
                  plotOutput(outputId = "AccuracyPlot", width = "100%"),
                ),
                tabPanel(
                  title = "Data",
                  br(),
                  DT::dataTableOutput("dataTable") #Shows first 10 cases in table
                )
              )
            )
          )
        ),
        ### References ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities.
            (v0.1.10). [R Package]. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2020).
            shiny: Web application framework for R. (v1.5.0) [R Package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Friedman, J., Hastie, T., and Tibshirani, R. (2010). Regularization Paths for
            Generalized Linear Models via Coordinate Descent. Journal of Statistical Software
            33(1), 1-22. Available at http://www.jstatsoft.org/v33/i01/"
          ),
          p(
            class = "hangingindent",
            "Hastie, T., and Efron, B. (2013). lars: Least Angle Regression, Lasso, and
            Forward Stagewise. (v 1.2) [R Package]. Available from
            https://CRAN.R-project.org/package=lars"
          ),
          p(
            class = "hangingindent",
            "Horst, A. M., Hill, A. P., and Gorman, K. B. (2020). palmerpenguins: Palmer
            Archipelago (Antarctica) penguin data. (v 0.1.0) [R Package]. Available
            from https://allisonhorst.github.io/palmerpenguins/"
          ),
          p(
            class = "hangingindent",
            "Karatzoglou, A., Smola, A., Hornik, K. and Zeileis, A. (2004).
            kernlab - An S4 package for kernal methods in R. Journal of Statistical Software
            11(9), 1-20.  Available at http://www.jstatsoft.org/v11/i09/"
          ),
          p(
            class = "hangingindent",
            "Kuhn, M. (2020). caret: Classification and Regression Training (v 6.0-86)
            [R Package]. Available from https://CRAN.R-project.org/package=caret"
          ),
          p(
            class = "hangingindent",
            "Venables, W., N., and Ripley, B., D. (2002). Modern Applied Statistics with
            S. Fourth Edition. Springer, New York. [MASS Package]"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. ggplot2: Elegant Graphics for Data Analysis.
            Springer-Verlag New York, 2016. Available at https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2020). DT: A Wrapper of the
            JavaScript Library 'DataTables'. R package version 0.16.
            https://CRAN.R-project.org/package=DT"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        ) #,
        # Challenge Page (Future Development?) ----
        # tabItem(
        #   tabName = "challenge",
        #   withMathJax(),
        #   h2(inputId = "Now do it yourself",label = "test"),
        #   h2('Observations'),
        #   p('Everything is a trade off between accuracy and consistency.
        #     The more of the data is used for training the more accurate
        #     (unless oversampling occurs) the estimate. More of the data used for
        #     testing you will get a result that more accuratetly measures how good
        #     the data algorithm is'),
        #   p('If the percent correct is very high you may as well just
        #     go with low training and high testing to make sure you get an accurate result')
        # )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  ## Define what GO button does
  observeEvent(input$go1, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "expl"
    )
  })

  tracking <- reactiveValues()
  tracking$DT <- data.frame(
    percentTraining = numeric(),
    accuracyValue = numeric()
  )
  #----New reactives----
  yLabel <- reactive({
    yLabel <- input$theVariable
    if(input$theVariable == "Species" || input$theVariable == "species" || input$theVariable == "island" || input$theVariable == "sex" || input$theVariable == "target")
    {
      yLabel <- "Percentage of Correct Predictions"
    }
    else
    {
      yLabel <- "MSE (the lower, the better)"
    }
    yLabel
  })

  # What are these for?
  maxYAxis <- eventReactive(
    eventExpr = input$theVariable,
    valueExpr = {
      switch(
        EXPR = input$theVariable,
        "Sepal.Length" = 0.2,
        "Species" = 0.95,
        "species" = 100,
        "island" = 75,
        "body_mass_g" = 120000,
        "sex" = 100,
        "youtube" = 4000,
        "sales" = 20,
        "target" = 100,
        "trestbps" = 400
      )
    }
  )

  minYAxis <- eventReactive(
    eventExpr = input$theVariable,
    valueExpr = {
      switch(
        EXPR = input$theVariable,
        "Sepal.Length" = 0,
        "Species" = 90,
        "species" = 90,
        "island" = 65,
        "body_mass_g" = 0,
        "sex" = 50,
        "youtube" = 0,
        "sales" = 0,
        "target" = 55,
        "trestbps" = 150
      )
    }
  )

  meanMaxYAxis <- eventReactive(
    eventExpr = input$theVariable,
    valueExpr = {
      switch(
        EXPR = input$theVariable,
        "Sepal.Length" = 0.2,
        "Species" = 100,
        "species" = 100,
        "island" = 75,
        "body_mass_g" = 100000,
        "sex" = 100,
        "youtube" = 2000,
        "sales" = 6,
        "target" = 85,
        "trestbps" = 400
      )
    }
  )

  meanMinYAxis <- eventReactive(
    eventExpr = input$theVariable,
    valueExpr = {
      switch(
        EXPR = input$theVariable,
        "Sepal.Length" = 0,
        "Species" = 0.95,
        "species" = 99,
        "island" = 65,
        "body_mass_g" = 75000,
        "sex" = 64,
        "youtube" = 1400,
        "sales" = 3,
        "target" = 75,
        "trestbps" = 250
      )
    }
  )

  ## Set the data set ----
  chosenData <- eventReactive(
    eventExpr = input$theDataSet,
    valueExpr = {
      list(
        data = switch(
          EXPR = input$theDataSet,
          "Iris" = iris,
          "Palmer Penguins" = penguins,
          "Marketing" = marketing,
          "Heart Disease" = heartDisease
        ),
        info = switch(
          EXPR = input$theDataSet,
          "Iris" = "Machine learning dataset containing various measurments of
                    different species of iris flowers",
          "Palmer Penguins" = "Machine learning algorithm to try and learn the
                               type of penguins",
          "Marketing" = "Simple dataset to predict continuous values most notably
                         the overall sales",
          "Heart Disease" = "This dataset adds extra challenge with its numerous
                             continuous and categorical variables to be predicted."
        ),
        variables = switch(
          EXPR = input$theDataSet,
          "Iris" =  "<ul>
                     <li><strong>Sepal.Length: Length of the sepal in cm</strong></li>
                     <li>Sepal.Width: Length of the sepal in cm</li>
                     <li>Petal.Length: Length of the sepal in cm</li>
                     <li>Petal.Width: Length of the sepal in cm</li>
                     <li><strong>Species: Either Setosa, Versicolour or Virginica</strong></li>
                    </ul>",
          "Palmer Penguins" = "<ul>
                               <li><strong>Species: Species of penguin</strong></li>
                               <li><strong>island: Which island in Palmer Archipelago penguin was found on</strong></li>
                               <li>bill_length_mm: bill length in milimeters</li>
                               <li>bill_depth_mm: bill depth in milimeters</li>
                               <li>flipper_length_mm: flipper length in milimeters</li>
                               <li><strong>body_mass_g: Total mass of penguin in grams</strong></li>
                               <li><strong>sex: (Male or Female)</strong></li>
                              </ul>",
          "Marketing" = "<ul>
                         <li><strong>youtube: Youtube advertising spent in thousands of dollars</strong></li>
                         <li>facebook: Facebook advertising spent in thousands of dollars</li>
                         <li>newspaper: Newspaper advertising spent in thousands of dollars</li>
                         <li><strong>sales: Total sales made in thousands of units</strong></li>
                        </ul>",
          "Heart Disease" = "<ul>
                             <li>Age: in years</li>
                             <li>cp: chest pain type</li>
                             <li><strong>sex: male or female</strong></li>
                             <li><strong>trestbps: resting blood pressure</strong></li>
                             <li>chol: serum cholestoral in mg/dl, fasting blood sugar (yes or no)</li>
                             <li>restecg: resting electrocardiographic results</li>
                             <li>thalach: maximum heart rate</li>
                             <li>exang: exercise induced (yes or no)</li>
                             <li>oldpeak: ST depression induced by exercise relative to rest</li>
                             <li>Slope: of peak exercise ST segment,</li>
                             <li>ca: number of vessels colored by flourosopy (0-3) </li>
                             <li>thal: Thalassemia level (blood disorder) the lower the better</li>
                             <li><strong>target: heart disease (healthy or unhealthy)</strong></li>
                            </ul>"
        )
      )
    }
  )

  ## Display data information ----
  observeEvent(input$theDataSet, {
    ### Data Table
    output$dataTable <- DT::renderDataTable(
      expr = chosenData()$data,
      style = "bootstrap4",
      rownames = TRUE,
      options = list(
        responsive = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = 1:ncol(chosenData()$data))
        )
      )
    )
    ### Data info
    output$dataTableInfo <- renderText({
      chosenData()$info
    })

    ### Variable explanation
    output$dataTableVariables <- renderText({
      chosenData()$variables
    })

    ### Update variable selection
    if (input$theDataSet == "Iris") {
      updateSelectInput(
        session = session,
        inputId = "theVariable",
        choices = list('Sepal.Length', 'Species')
      )
    } else if (input$theDataSet == "Palmer Penguins") {
      updateSelectInput(
        session = session,
        inputId = "theVariable",
        choices = list('species','island', 'body_mass_g', 'sex')
      )
    } else if (input$theDataSet == "Marketing") {
      updateSelectInput(
        session = session,
        inputId = "theVariable",
        choices = list('youtube', 'sales')
      )
    } else if (input$theDataSet == "Heart Disease") {
      updateSelectInput(
        session = session,
        inputId = "theVariable",
        choices = list('sex', 'trestbps', 'target')
      )
    }
  })

  ## Change method depending on selected variable ----
  observeEvent(input$theVariable, {
    if (input$theVariable %in% c("Sepal.Length", "body_mass_g", "trestbps")) {
      updateSelectInput(
        session = session,
        inputId = "theMethod",
        choices = list("Multiple Linear Regression")
      )
    } else if (input$theVariable %in% c("Species", "species", "island")) {
      updateSelectInput(
        session = session,
        inputId = "theMethod",
        choices = list("Linear Discriminant Analysis")
      )
    } else if (input$theVariable %in% c("sex", "target")) {
      updateSelectInput(
        session = session,
        inputId = "theMethod",
        choices = list("Linear Discriminant Analysis", "Logistic Regression")
      )
    } else if (input$theVariable %in% c("youtube", "sales")) {
      updateSelectInput(
        session = session,
        inputId = "theMethod",
        choices = list("Multiple Linear Regression", "Ridge Regression", "LASSO")
      )
    }

    tracking$DT <- data.frame(
      percentTraining = numeric(),
      accuracyValue = numeric()
    )
  })

  ## Run Method button ----
  observeEvent(input$newGraphOutput, {
    if (input$theDataSet == 'Iris') {
      dataset <- na.omit(chosenData()$data)
      #predictor is the variable that we are going to try to predict
      predictor <- iris$Species
      predictionVariable <- input$theVariable
      #outputType <- "categorical"
    } else if (input$theDataSet == 'Marketing') {
      dataset <- na.omit(chosenData()$data)
      predictor <- dataset$sales
      predictionVariable <- input$theVariable
      #outputType <- "continuous"
    } else if (input$theDataSet == "Heart Disease") {
      #https://www.youtube.com/watch?v=C4N3_XJJ-jU was important
      dataset <- chosenData()$data
      #Cleaning
      dataset[dataset == "?"] <- NA
      dataset <- na.omit(dataset)
      #test
      dataset[dataset$sex == 0,]$sex <- "female"
      dataset[dataset$sex == 1,]$sex <- "male"
      dataset$sex <- as.factor(dataset$sex)
      dataset$fbs <- as.factor(dataset$fbs)
      dataset$exang <- as.factor(dataset$exang)
      dataset$ca <- as.integer(dataset$ca)
      dataset$thal <- as.integer(dataset$thal) # "thal" also had "?"s in it.
      ## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
      #dataset$target <- ifelse(test=dataset$target == 0, yes="Healthy", no="Unhealthy") #I might remove
      dataset$target <- as.factor(dataset$target) # Now convert to a factor
      predictor <- dataset$target
      predictionVariable <- input$theVariable
    } else if (input$theDataSet == 'Palmer Penguins') {
      dataset <- na.omit(chosenData()$data)
      predictor <- dataset$species
      predictionVariable <- input$theVariable
      #outputType <- "categorical"
    }
    # Future development?
    # else if(input$theDataSet == "House Data"){
    #   dataset <- read.csv("HouseTrain.csv")
    #   predictor <- dataset$SalePrice
    #   predictionVariable <- input$theVariable
    #   #outputType <- "categorical"
    # } else {
    #   print('ERROR')
    # }

    #consistency50 <- vector(mode = "list", length = totalRuns)
    count <- 1
    totalRuns <- as.numeric(input$repetitions)

    while (count <= totalRuns) {
      percentCorrect <- calculateAccuracy(
        dataset,
        input$testingPercent,
        input$theMethod,
        predictor,
        predictionVariable
      )
      percentCorrect <- signif(percentCorrect,4)
      currentPoints <- data.frame(
        percentTraining = input$testingPercent,
        accuracyValue = percentCorrect
      )

      count <- count + 1
      ###---- Store new values ----
      tracking$DT <- rbind(tracking$DT, currentPoints)
    }
  })

  basePerformPlot <- ggplot(
    data = data.frame(
      x = seq(from = 0.2, to = 1),
      y = seq(from = 0, to = 1)
    ),
    mapping = aes(x = x, y = y)
  ) +
    theme_bw() +
    xlab("Proportion in Training Set") +
    ylab("MSE error") +
    labs(title = "The Accuracy of Every Test") +
    theme(
      text = element_text(size = 18)
    ) +
    scale_x_continuous(
      limits = c(.2, 1),
      expand = expansion(mult = 0.01, add = 0)
    )

  output$variancePlot <- renderPlot({
    basePerformPlot +
      scale_y_continuous(
        limits = c(minYAxis(), maxYAxis()),
        expand = expansion(mult = 0.05, add = 0),
        labels = scaleFUN
      ) +
      geom_point(
        data = tracking$DT,
        alpha = 0.33,
        mapping = aes(
          x = percentTraining,
          y = accuracyValue
        ),
        size = 4
      ) +
      ylab(yLabel()) +
      theme(
        legend.position = "bottom"
      )
    #geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96))
  })

  output$AccuracyPlot <- renderPlot({
    #Compute the average of the points at each location X
    subset20 <- mean(subset(tracking$DT, (percentTraining == .2))$accuracyValue)
    subset25 <- mean(subset(tracking$DT, (percentTraining == .25))$accuracyValue)
    subset30 <- mean(subset(tracking$DT, (percentTraining == .3))$accuracyValue)
    subset35 <- mean(subset(tracking$DT, (percentTraining == .35))$accuracyValue)
    subset40 <- mean(subset(tracking$DT, (percentTraining == .4))$accuracyValue)
    subset45 <- mean(subset(tracking$DT, (percentTraining == .45))$accuracyValue)
    subset50 <- mean(subset(tracking$DT, (percentTraining == .5))$accuracyValue)
    subset55 <- mean(subset(tracking$DT, (percentTraining == .55))$accuracyValue)
    subset60 <- mean(subset(tracking$DT, (percentTraining == .6))$accuracyValue)
    subset65 <- mean(subset(tracking$DT, (percentTraining == .65))$accuracyValue)
    subset70 <- mean(subset(tracking$DT, (percentTraining == .7))$accuracyValue)
    subset75 <- mean(subset(tracking$DT, (percentTraining == .75))$accuracyValue)
    subset80 <- mean(subset(tracking$DT, (percentTraining == .8))$accuracyValue)
    subset85 <- mean(subset(tracking$DT, (percentTraining == .85))$accuracyValue)
    subset90 <- mean(subset(tracking$DT, (percentTraining == .9))$accuracyValue)
    subset95 <- mean(subset(tracking$DT, (percentTraining == .95))$accuracyValue)
    #Put these values into a list along with their corresponding percents in a parellel vector
    averagesList <- c(subset20, subset25, subset30, subset35, subset40, subset45,
                      subset50, subset55, subset60, subset65, subset70, subset75,
                      subset80, subset85, subset90, subset95)
    percents <- c(.2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95)
    meanPoints <- data.frame(averagesList, percents)
    meanPoints <- subset(meanPoints, (averagesList != 'NaN'))
    ggplot(
      data = meanPoints,
      mapping = aes(x = percents, y = averagesList)
    ) +
      geom_point(
        data = meanPoints,
        mapping = aes(
          x = percents,
          y = averagesList
        ),
        size = 4
      ) +
      theme_bw() +
      theme(
        text = element_text(size = 18)
      ) +
      xlab("Proportion in Training Set") +
      ylab(paste('Mean', yLabel())) +
      labs(title = "Mean Accuracy") +
      stat_smooth(method = "lm", formula = y ~ poly(x, 1), size = 1, se = FALSE) +
      scale_y_continuous(
        limits = c(meanMinYAxis(), meanMaxYAxis()),
        expand = expansion(mult = 0.05, add = 0),
        labels = scaleFUN
      ) +
      scale_x_continuous(
        limits = c(.2, 1),
        expand = expansion(mult = 0.01, add = 0)
      )
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
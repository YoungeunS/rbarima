#'Rbarima
#' @param df A dataframe input
#' @param Measure Specify value measures such as Revenue and Shipments
#' @param Time Specify time variables such as Year, Quarter, and Month
#' @param Pred.time Specify a number of years or quarters to predict
#' @export

rbarima <- function(df=.Last.value,
                    Measure = "",
                    Time = "",
                    Pred.time = "",
                    launch.browser = False) {

  ################################################################################################################
  #Non-reactive functions
  ################################################################################################################

  options(scipen=999)

  #Combine Time columns to one
  if(length(Time) > 1){

    df_Time <- df %>% dplyr::select(Time)
    df_excl_Time <- df %>% dplyr::select(-one_of(Time))
    colnames(df_Time) <- c("Time_1", "Time_2")

    df_Time <- df_Time %>%
      dplyr::mutate(Time_1 = as.numeric(gsub("[[:alpha:][:punct:]]","", Time_1)),
                    Time_2 = as.numeric(gsub("[[:alpha:][:punct:]]","", Time_2))) %>%
      dplyr::mutate(Time = paste(Time_1, Time_2, sep = "_")) %>%
      dplyr::select(Time)

    df <-cbind(df_Time, df_excl_Time)

  }else{

    df_Time <- df %>% dplyr::select(Time)
    df_excl_Time <- df %>% dplyr::select(-one_of(Time))
    colnames(df_Time) <- c("Time")

    df_Time <- df_Time %>%
      dplyr::mutate(Time = as.numeric(gsub("[[:alpha:][:punct:]]","", Time)))

    df <-cbind(df_Time, df_excl_Time)

  }

  #Gather values
  if(length(Measure) > 1){
    df0 <- df %>%
      tidyr::gather(Measures, Value, Measure) %>%
      dplyr::mutate_if(is.factor, as.character)
  }else{
    df0 <- df %>%
      dplyr::mutate_if(is.factor, as.character)

    names(df0)[names(df0) == Measure] <- "Value"
  }

  #Make names
  names(df0) <- make.names(names(df0))
  all.elements <- "Show All"

  #Remove variables with a unique value.
  unique.df0 <- df0 %>%
    summarise_all(funs(n_distinct(.))) %>%
    gather() %>%
    filter(value>1)
  dim_names <-unique.df0$key


  #select dimension to run the model
  df0a <- df0[, dim_names]
  dim_select <- names(df0a)[!(names(df0a) %in% c("Measures", "Value"))]

  if(length(Measure) > 1){
    df1 <- df0 %>% dplyr::select(Time, Measures, Value, dim_select)
  }else{
    df1 <- df0 %>% dplyr::select(Time, Value, dim_select)
  }

  dim_names <- names(df1)[!(names(df1) %in% c("Value"))]



  ################################################################################################################
  #UI.R
  ################################################################################################################


  ui <- dashboardPage(

    dashboardHeader(title = "Rforcasting"),
    dashboardSidebar(
      br(),
      fluidRow(
        column(
          width = 12,
          uiOutput("ui_update_data")
        ),
        column(
          div(
            style = "padding: 15px",
            h4("Data Filter: ")
          ),
          width = 12,
          uiOutput("selects")
        )
      ),
      br(),
      hr(),
      fluidRow(
        column(
          div(
            style = "padding: 15px",
            h4("Model Parameter: ")
          ),
          width = 12,
          sliderInput('ABInput', 'ARIMA+BASS Fcst Input', 4, min = 1, max = 20)
        )
      )
    ),

    dashboardBody(
      tags$style(HTML(".box.box-info {background: #ffffff}
                      .box.box-primary {background: rgba(236,240,245,1)}")),
      tags$style(".span12 {color: black;}"),
      tags$head(tags$style(HTML(".skin-blue .main-sidebar label{color: white;}
                                .skin-blue .main-header .navbar {background-color: #006791;}
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #006791;}
                                .skin-blue .main-header .logo:hover {background-color: #006791;}
                                .skin-blue .main-header .logo {background-color: #006791;}
                                .skin-blue .main-sidebar {background-color:  #434b4e;}
                                "))),

      tabsetPanel(
        tabPanel("Rforecasting",
                 icon = shiny::icon("cog"),
                 br(),
                 box(status = "info", title = "Dashboard", height = "1170",width = "12",solidHeader = F,
                     fluidRow(
                       box(height = "650px",width = "12",solidHeader = T,
                           column(width = 12,
                                  plotlyOutput("plotly", width = "100%", height = "600px")
                           )
                       )
                     ),
                     br(),
                     fluidRow(
                       box(title = "Data Output", height = "350",width = "7",solidHeader = T,
                           column(
                             width = 12,
                             list(
                               div(style = 'overflow-x: scroll',DT::dataTableOutput("df.dt",width = "100%"))
                             )
                           )
                       ),
                       box(title = "Model Evalutation", height = "350",width = "5",solidHeader = T,
                           column(
                             width = 12,
                             list(
                               div(DT::dataTableOutput("Accuracy",width = "100%"))
                             ),
                             br(),
                             div(
                               p(strong(code("Best Model:"))),
                               textOutput("Best.Model")
                             )
                           )
                       ),
                       br(),
                       br(),
                       div(
                         style = "padding: 15px",
                         downloadButton(outputId = "df.dt_mydownload", label = "Download Data")
                       )
                     )),
                 br(),
                 br(),
                 fluidPage(
                   tags$style(HTML(".tabbable > .nav > li > a                           {background-color: #006791; color: white}
                                   .tabbable > .nav > li > a[data-value='Time.Series'] {background-color: #006791; color: white}
                                   .tabbable > .nav > li > a[data-value='Bass.Diffusion'] {background-color: #006791; color: white}
                                   ")),
                   tabsetPanel(
                     tabPanel("Time.Series",
                              br(),
                              h4(strong("Time Series: ")),
                              br(),
                              h4("1. Decomposition"),
                              br(),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("Decomposition")),
                                  p("The building blocks of a time series analysis are seasonality, trend, and cycle.
                                    These intuitive components capture the historical patterns in the series.
                                    Not every series will have all three (or any) of these components, but if they are present, deconstructing the series can help you understand its behavior and prepare a foundation for building a forecasting model.
                                    Seasonal component refers to fluctuations in the data related to calendar cycles.
                                    ")
                                  ),
                              box(height = "500px",width = "8",solidHeader = F,
                                  plotOutput("decomp.plot", width = "100%", height = "400px")
                              ),
                              br(),
                              h4("2. Stationarity"),
                              br(),
                              box(status = "primary", height = "750px",width = "6",solidHeader = F,
                                  p("Fitting an ARIMA model requires the series to be stationary.
                                    A series is said to be stationary when its mean, variance, and autocovariance are time invariant. "),
                                  br(),
                                  h5(strong("KPSS")),
                                  p("The Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test figures out if a time series is stationary around a mean or linear trend, or is non-stationary due to a unit root. A stationary time series is one where statistical properties - like the mean and variance - are constant over time.In this case, the null-hypothesis is that the data are stationary.  In this case, p-value less than 0.05 indicates non-stationary series and p-value greater than 0.05 indicates stationary series."),
                                  h5(strong(verbatimTextOutput("kpss.text"))),
                                  textOutput("kpss.i"),
                                  br(),
                                  h5(strong("BOX-Ljung Test")),
                                  p("The p-values for the modified Box-Ljung test all are well above .05, indicating that the residuals are independent (white noise)."),
                                  h5(strong(verbatimTextOutput("ARIMA.Box"))),
                                  br(),
                                  h5(strong("ACF/PACF")),
                                  p("Autocorrelation plots (also known as ACF or the auto correlation function) are a useful visual tool in determining whether a series is stationary.
                                    These plots can also help to choose the order parameters for ARIMA model.
                                    ACF plots display correlation between a series and its lags.
                                    In addition to suggesting the order of differencing, ACF plots can help in determining the order of the M A (q) model.
                                    Partial autocorrelation plots (PACF), as the name suggests, display correlation between a variable and its lags that is not explained by previous lags.
                                    PACF plots are useful when determining the order of the AR(p) model.")
                                  ),
                              box(height = "750px",width = "6",solidHeader = F,
                                  plotOutput("smr.plot", width = "100%", height = "650px")
                              ),
                              h4("3. Auto Arima"),
                              br(),
                              box(status = "primary", height = "700px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary")),
                                  h5(strong(verbatimTextOutput("arima.model")))
                              ),
                              box(status = "primary", height = "700px",width = "4",solidHeader = F,
                                  h5(strong("Arima Model")),
                                  p("ARIMA stands for auto-regressive integrated moving average and is specified by these three order parameters: (p, d, q).
                                    The process of fitting an ARIMA model is sometimes referred to as the Box-Jenkins method. "),
                                  br(),
                                  h5(strong("Seasonal Arima")),
                                  p("The seasonal ARIMA model incorporates both non-seasonal and seasonal factors in a multiplicative model.  One shorthand notation for the model is"),
                                  h5(strong("ARIMA(p, d, q) x (P, D, Q)S,")),
                                  p("with p = non-seasonal AR order, d = non-seasonal differencing, q = non-seasonal MA order, P = seasonal AR order, D = seasonal differencing, Q = seasonal MA order, and S = time span of repeating seasonal pattern."),
                                  p("[1]Simple Exponential Smoothing ARIMA(0,1,1)"),
                                  p("[2]Holt's Exponential Smoothing  ARIMA(0,2,2)"),
                                  p("[3]White noise ARIMA(0,0,0)"),
                                  p("[4]Random walk ARIMA(0,1,0) with no constant"),
                                  p("[5]Random walk with drift ARIMA(0,1,0) with a constant"),
                                  p("[6]Autoregression ARIMA(p,0,0)"),
                                  p("[7]Moving average ARIMA(0,0,q)")
                                  )
                                  ),


                     tabPanel("Bass.Diffusion",
                              br(),
                              h4(strong("Bass Diffusion: ")),
                              br(),
                              h4("1. Bass Diffusion Model"),
                              br(),
                              box(status = "primary", height = "1000px",width = "6",solidHeader = F,
                                  p("The Bass Model or Bass Diffusion Model was developed by Frank Bass. It consists of a simple differential equation that describes the process of how
                                    new products get adopted in a population. The model presents a rationale of how current adopters and potential adopters of a new product interact.
                                    The basic premise of the model is that adopters can be classified as innovators or as imitators and the speed and timing of adoption depends on
                                    their degree of innovativeness and the degree of imitation among adopters. The Bass model has been widely used in forecasting, especially new products'
                                    sales forecasting and technology forecasting. Mathematically, the basic Bass diffusion is a Riccati equation with constant coefficients."),
                                  br(),
                                  plotOutput("New.BD.plot", width = "100%", height = "300px"),
                                  plotOutput("Cu.BD.plot", width = "100%", height = "300px")
                                  ),
                              box(status = "primary", height = "1000px",width = "6",solidHeader = F,
                                  h5(strong("Model Summary")),
                                  p("MPQ: M is the ultimate market potential. The coefficient P is called the coefficient of innovation, external influence or advertising effect.
                                    The coefficient Q is called the coefficient of imitation, internal influence or word-of-mouth effect. #Function nlsLM - estimates parameters using the method of nonlinear least squares."),
                                  br(),
                                  h5(strong(verbatimTextOutput("BD.model")))
                                  ),
                              h4("2. Forecast Accuracy Check"),
                              br(),
                              box(height = "650px",width = "6",solidHeader = F,
                                  plotOutput("BD.residual.plot", width = "100%", height = "600px")
                              ),
                              box(status = "primary", height = "650px",width = "6",solidHeader = F,
                                  h5(strong("Shapiro-walk normality test")),
                                  p("From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution.
                                    In other words, we can assume the normality."),
                                  br(),
                                  h5(strong(verbatimTextOutput("BD.shapiro.test"))),
                                  br(),
                                  h5(strong("Model Accuracy")),
                                  br(),
                                  h5(strong(verbatimTextOutput("BD.accuracy")))
                                  )
                              )))),

        tabPanel("Link",
                 br(),
                 h4("Link"),
                 icon = icon("link"),
                 br(),
                 a(href ="https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials", "[1][1] Introduction to Forecasting with ARIMA in R"),
                 br(),
                 a(href ="https://onlinecourses.science.psu.edu/stat510/node/67", "[1][2] Seasonal ARIMA models "),
                 br(),
                 a(href ="https://www.otexts.org/fpp/8/7", "[1][3] ARIMA modelling in R "),
                 br(),
                 a(href ="https://robjhyndman.com/hyndsight/forecast3/", "[1][4] Major changes to the forecast package "),
                 br(),
                 a(href ="http://www.statisticshowto.com/kpss-test/", "[1][5] KPSS Test: Definition and Interpretation "),
                 br(),
                 a(href ="https://www.listendata.com/2015/10/arima-modeling-with-r.html", "[1][6] ARIMA MODELING WITH R "),
                 br(),
                 a(href ="https://www.quantstart.com/articles/Autoregressive-Integrated-Moving-Average-ARIMA-p-d-q-Models-for-Time-Series-Analysis", "[1][7] Autoregressive Integrated Moving Average ARIMA(p, d, q) Models for Time Series Analysis"),
                 br(),
                 a(href ="http://www.uvm.edu/pdodds/files/papers/others/everything/bass1969a.pdf", "[2][1] F. Bass, A New Product Growth for Model Consumer Durables, Management Science, Vol. 15 (January 1969)"),
                 br(),
                 a(href ="https://rpubs.com/chengjun/bass_model", "[2][2] Bass diffusion model chengjun"),
                 br(),
                 a(href ="https://www.r-bloggers.com/a-better-nls/", "[2][3] A better 'nls' (?)"),
                 br(),
                 a(href ="https://journal.r-project.org/archive/2017/RJ-2017-006/RJ-2017-006.pdf", "[2][4] Implementing a Metapopulation Bass Diffusion Model using the R Package deSolve"),
                 br(),
                 a(href ="http://www.iam.fmph.uniba.sk/institute/stehlikova/ts16/ex/ex02_bass.pdf", "[2][5] Application of nonlinear least squares: Estimating parameters of the Bass model"),
                 br(),
                 a(href ="http://people.duke.edu/~rnau/testing.htm", "[2][6] Regression diagnostics:  testing the assumptions of linear regression"),
                 br(),
                 a(href = "http://shishirshakya.blogspot.com/", "[3][1] Economic Modeling"),
                 br(),
                 a(href = "https://hbr.org/1971/07/how-to-choose-the-right-forecasting-technique", "[3][2] How to Choose the Right Forecasting Technique")

        ))))





  ################################################################################################################
  #Server.R
  ################################################################################################################

  server <- function(input, output, session) {

    dat0 <- reactive({
      dat <- df1
      return(dat)
    })

    pred.time.input <- reactive({
      h <- Pred.time
      return(h)
    })

    #Series filtering
    output$selects <- renderUI({
      req(dat0())
      #Loop through each dimension to build a filter
      lapply(seq_along(dim_names), function(i){
        dat <- dat0()

        #Treat all series the same... time, nonagg, etc...
        choice.list <- c(all.elements, unique(dat0()[, dim_names[i]]))
        #Choose that total
        choice.selected <- choice.list[1]
        #Multiple allowed
        choice.mult <- TRUE


        # Build the Menu for each dimension
        selectInput(
          inputId = paste0("Sel", i),
          label = paste0(dim_names[i]),
          choices = choice.list,
          selected = choice.selected,
          multiple = choice.mult
        )
      })
    })

    ###
    # Action Button
    ###
    output$ui_update_data <- renderUI({
      actionButton("update_data", label = "Refresh Data", icon = shiny::icon("refresh"),
                   style = "background-color:#17B3D1; color:#ffffff;")
    })


    ###
    # Edit table
    ###

    #Filter
    dat.filtered <- eventReactive(input$update_data,
                                  ignoreNULL = FALSE, {
                                    req(dat0())


                                    dat <- dat0()
                                    datF <- dat

                                    for(i in seq_along(dim_names)){
                                      get_input <- eval(parse(text=paste0("input$Sel", i))) #Which filter to check


                                      #If no items are selected or the Select All is selected, show ALL items
                                      if(length(get_input) == 0 || all.elements %in% get_input){
                                        get_series <- tibble::as.tibble(dat[, dim_names[i]]) %>% dplyr::distinct() %>% dplyr::pull()

                                        filter_criteria_T <- lazyeval::interp( ~ which_column %in% get_series, which_column = as.name(dim_names[i])) #If a Filter is selected....
                                      } else {
                                        get_series <- as.character(get_input)
                                        filter_criteria_T <- lazyeval::interp( ~ which_column %in% get_series, which_column = as.name(dim_names[i])) #If a Filter is selected....
                                      }

                                      #.... Do this
                                      datF <- datF %>%
                                        dplyr::filter_(filter_criteria_T)

                                    } #End for

                                    return(as.data.frame(datF))
                                  })



    dat.value <- reactive({
      dat <- dat.filtered()
      dat <- dat %>%
        dplyr::select(Time, Value) %>%
        dplyr::group_by(Time) %>%
        dplyr::summarize(Value = sum(Value)) %>%
        dplyr::filter(!grepl("NA",Time))

      return(as.data.frame(dat))
    })


    ###
    #Pred.time.frame
    ###

    #Time frame, Time_1: Year, Time_2: Quarter or Month
    frame.output <- reactive({

      dat <- dat.value()
      h <- pred.time.input()

      dat <- dat %>%
        tidyr::separate(Time, c("Time_1", "Time_2"), "_", remove = FALSE)

      this.year <- max(as.numeric(dat$Time_1))

      if(is.na(dat$Time_2)){

        pred.time <- as.data.frame(rep((this.year + 1):(this.year + h), each = 1))
        colnames(pred.time) <- "Time"

      }else{

        this.time_2 <- as.numeric(tail(dat$Time_2, n=1))
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        # For a full year or Non full year data
        if(as.numeric(max(dat$Time_1)) == this.year && as.numeric(max(dat$Time_2)) == this.time_2){
          pred.time <- as.data.frame(rep((this.year + 1):(this.year + h/Freq), each = Freq))
          colnames(pred.time) <- "Time_1"
          pred.time$Time_2 <- rep(1:Freq, h/Freq)
        }else{
          pred.time <- as.data.frame(rep((this.year):(this.year + h/Freq), each = Freq))
          colnames(pred.time) <- "Time_1"
          pred.time <- pred.time %>%
            slice(this.time_2+1:n())
          Time_2 <- rep(1:Freq, (h+Freq)/Freq)
          pred.time$Time_2 <- Time_2[-(1:this.time_2)]
        }


        pred.time <- as.data.frame(paste(pred.time$Time_1, pred.time$Time_2, sep="_"))
        colnames(pred.time) <- "Time"

      }

      df.time <- dat %>%
        dplyr::select(Time) %>%
        unique()

      frame.output <- rbind(df.time, pred.time)

      return(as.data.frame(frame.output))
    })


    ##########################################################################################################
    #Models
    ##########################################################################################################




    ###
    #Linear Regression
    ###

    Linear.Regression <- reactive({
      dat <- dat.value()
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)

      lm.dat <- dat %>%
        dplyr::select(Value)
      lm.dat$x <- 1:nrow(lm.dat)


      possibleError <- tryCatch(
        lm <- glm(Value ~ x , data=lm.dat, family=gaussian(link = "identity")),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        pred.time <- data.frame(x = 1:(nrow(lm.dat)+h), Linear.Regression=0)
        Linear.Regression<- pred.time$Linear.Regression

      }else{

        lm <- glm(Value ~ x , data=lm.dat, family=gaussian(link = "identity"))
        pred.time <- data.frame(x = 1:(nrow(lm.dat)+h), Value=0)
        Linear.Regression <- predict.lm(lm, pred.time)
        #Linear.Regression[Linear.Regression < 0] <- 0

      }
      return(as.data.frame(Linear.Regression))

    })


    ###
    #Time Series
    ###

    ts.arima <- reactive({

      dat <- dat.value()
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)

      dat <- dat %>%
        tidyr::separate(Time, c("Time_1", "Time_2"), "_", remove = FALSE)

      if(is.na(dat$Time_2)){
        Freq <- 1
        dat$ts.time <- as.numeric(dat$Time_1)
      }else{
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time_2)))
        dat$ts.time <- as.numeric(dat$Time_1) + as.numeric(dat$Time_2)/Freq - 1/Freq
      }

      start.time = min(dat$ts.time)
      end.time = max(dat$ts.time)

      ts.arima <- ts(data = dat$Value, start = c(start.time) , end = c(end.time), frequency = Freq)

      return(ts.arima)
    })




    ## Data Validation
    #Decomposition
    ARIMA.decomp <- reactive({

      ts <- ts.arima()
      decomp = stl(ts, s.window = "periodic")

      return(decomp)
    })

    output$decomp.plot <- renderPlot({
      decomp <- ARIMA.decomp()
      plot(decomp)
    })



    #Stationary test: Augmented Dickey-Fuller Test

    kpss.i <- reactive({

      ts <- ts.arima()
      D <- nsdiffs(ts, test = c("ocsb"))

      if(D>0){
        ts <- diff(ts, lag = 4, D)
        d <- ndiffs(ts, test = "kpss")
      }else if(D==0){
        d <- ndiffs(ts, test = "kpss")
      }else{
        d <- NA
      }

      kpss.i <-rbind(d, D)
      kpss.i <- paste("(p,", d, ",q)(P,", D, ",Q)")

      return(kpss.i)

    })


    kpss <- reactive({

      ts <- ts.arima()
      D <- nsdiffs(ts, test = c("ocsb"))

      if(D>0){
        ts <- diff(ts, lag = 4, D)
        d <- ndiffs(ts, test = "kpss")
      }else {
        d <- ndiffs(ts, test = "kpss")
      }



      if(D>0){
        diffed <- diff(ts, lag = 4, D)
        diffed <- diff(diffed, d)
        kpss.test <- kpss.test(diffed)
      }else{
        diffed <- diff(ts, d)
        kpss.test <- kpss.test(diffed)
      }

      return(kpss.test)

    })


    output$kpss.i <- renderPrint({
      kpss <- kpss.i()
      kpss
    })

    output$kpss.text <- renderPrint({
      kpss <- kpss()
      kpss
    })



    #Time Series Model
    arima.model <- reactive({

      dat <- dat.value()
      ts <- ts.arima()
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)

      arima.model <- auto.arima(ts, seasonal=TRUE, trace= TRUE, allowdrift = FALSE, allowmean = FALSE)

      return(arima.model)
    })

    #Model Summary
    output$arima.model <- renderPrint({
      arima.model <- arima.model()
      summary(arima.model)
    })

    ARIMA.Forecast <- reactive({

      dat <- dat.value()
      ts <- ts.arima()
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)

      #Fit AR(2) model
      arima.model <- auto.arima(ts, seasonal=TRUE,  trace= TRUE, allowdrift = FALSE, allowmean = FALSE)
      ARIMA.Forecast <- forecast(arima.model, h=h)

      return(ARIMA.Forecast)
    })


    #Time Series Result
    ARIMA <- reactive({


      ARIMA <- ARIMA.Forecast()

      #ts.fitted with forecast
      ts.fitted <- data.frame(ARIMA = c(ARIMA$fit), time = c(time(ARIMA$fitted)))
      ts.mean <-data.frame(ARIMA = c(ARIMA$mean), time = c(time(ARIMA$mean)))
      ARIMA <- rbind(ts.fitted, ts.mean) %>%
        dplyr::select(ARIMA)

      #merge
      return(as.data.frame(ARIMA))

    })


    #SMR tsdisplay
    ARIMA.SMR <- reactive({

      dat <- dat.value()
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)
      ARIMA <- ARIMA()
      arima.model <- arima.model()

      smr <- tsdisplay(residuals(arima.model), lag.max=length(ARIMA[[1]])-h, main='Model Residuals')
      return(smr)

    })

    output$smr.plot <- renderPlot({
      smr <- ARIMA.SMR()
      smr

    })

    #Box,test
    ARIMA.Box <- reactive({

      arima.model <- arima.model()

      box <-Box.test(arima.model$residuals, type = "Ljung-Box")
      return(box)

    })

    output$ARIMA.Box <- renderPrint({
      box <- ARIMA.Box()
      box

    })


    ###
    #Time.Series + Bass.diffusion
    ###

    Arima.Bass <- reactive({

      ts <- ARIMA()

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)-input$ABInput


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))

      dm.dat <- rbind(0, dm.dat)


      #Time Series Forecast
      ts.dat<-ts[(nrow(dat)+1):(nrow(dat)+input$ABInput),]

      #Cumulative MAU, Max MAU, MAX Cumulative MAU
      cu.dat <- cumsum(dm.dat)
      cu.dat <- unlist(append(list(cu.dat), list(ts.dat))) #Combined with Time Series
      dm.dat <- diff(cu.dat)
      dm.dat <- unlist(append(0, list(dm.dat)))
      t <- 1:length(dm.dat)

      #Find Optimal Start MPQ
      #BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2

      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }



      #Bass.Diffision
      possibleError <- tryCatch(
        #NLS on Bass General CDF
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Arima.Bass = 0)
        Arima.Bass <- Pred.time$Arima.Bass

      }else{
        #NLS on Bass General CDF
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]

        if(max(cu.dat)>M){


          ###
          #Time.Series + Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))

          dm.dat <- rbind(0, dm.dat)


          #Time Series Forecast
          ts.dat<-ts[(nrow(dat)+1):(nrow(dat)+input$ABInput),]

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)
          cu.dat <- unlist(append(list(cu.dat), list(ts.dat))) #Combined with Time Series
          dm.dat <- diff(cu.dat)
          dm.dat <- unlist(append(0, list(dm.dat)))
          t <- 1:length(dm.dat)

          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Arima.Bass = 0)
            Arima.Bass <- Pred.time$Arima.Bass

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)

            Arima.Bass <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative
            ab.df <- as.data.frame(Arima.Bass)

            delta.ab.ts <- ts[(nrow(dat)+1):(nrow(dat)+h),] - ab.df[(nrow(dat)+1):(nrow(dat)+h),]
            sum.delta.ab.ts <- sum(na.omit(as.data.frame(delta.ab.ts)))

            Arima.Bass

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{

          Bass.nls <- Bass.nls
          #Predict
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          Pred.Bass<-predict(Bass.nls, newdata=Pred.time)

          bdf <- M * ((P + Q)^2/P) * exp(-(P + Q) * Pred.time)/(1 + (Q/P) * exp(-(P + Q) * Pred.time))^2
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Arima.Bass <- cdf + dat$Value[[1]] - cdf[[1]][1]
          ab.df <- as.data.frame(as.numeric(unlist(Arima.Bass)))

          delta.ab.ts <- ts[(nrow(dat)+1):(nrow(dat)+h),] - ab.df[(nrow(dat)+1):(nrow(dat)+h),]
          sum.delta.ab.ts <- sum(na.omit(as.data.frame(delta.ab.ts)))

          Arima.Bass <- as.numeric(unlist(Arima.Bass))

        }
      }



      return(as.data.frame(Arima.Bass))

    })




    ###
    #Bass.diffusion
    ###


    Bass.Diffusion <- reactive({

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, fit the data to the cumulative curve.
        if(max(cu.dat)>M){


          ###
          #Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))
          dm.dat <- rbind(0, dm.dat)
          t <- 1:length(dm.dat)

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)


          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
            Bass.Diffusion <- Pred.time$Bass.Diffusion

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)
            #Bass.Diffusion<-c(Pred.Bass[1],diff(Pred.Bass)) #Not-cumulative
            Bass.Diffusion <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }

      return(as.data.frame(Bass.Diffusion))

    })

    #BD Model Summary
    Bass.Diffusion.Summary <- reactive({

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, fit the data to the cumulative curve.
        if(max(cu.dat)>M){


          ###
          #Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))
          dm.dat <- rbind(0, dm.dat)
          t <- 1:length(dm.dat)

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)


          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
            Bass.Diffusion <- Pred.time$Bass.Diffusion

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)
            #Bass.Diffusion<-c(Pred.Bass[1],diff(Pred.Bass)) #Not-cumulative
            Bass.Diffusion <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }
      overview(Bass.nls)

    })

    output$BD.model <- renderPrint({
      BD.model <- Bass.Diffusion.Summary()
      BD.model
    })


    #Non-Cumulative Total/Innovator/Immitator

    New.BD.total<- reactive({

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, fit the data to the cumulative curve.
        if(max(cu.dat)>M){


          ###
          #Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))
          dm.dat <- rbind(0, dm.dat)
          t <- 1:length(dm.dat)

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)


          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
            Bass.Diffusion <- Pred.time$Bass.Diffusion

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)
            #Bass.Diffusion<-c(Pred.Bass[1],diff(Pred.Bass)) #Not-cumulative
            Bass.Diffusion <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }
      Total <- M * ((P + Q)^2/P) * exp(-(P + Q) * Pred.time)/(1 + (Q/P) * exp(-(P + Q) * Pred.time))^2
      Total

    })


    New.Innovator <- reactive({

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, fit the data to the cumulative curve.
        if(max(cu.dat)>M){


          ###
          #Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))
          dm.dat <- rbind(0, dm.dat)
          t <- 1:length(dm.dat)

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)


          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
            Bass.Diffusion <- Pred.time$Bass.Diffusion

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)
            #Bass.Diffusion<-c(Pred.Bass[1],diff(Pred.Bass)) #Not-cumulative
            Bass.Diffusion <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }
      # when q=0, only Innovators without Immitators.
      Innovator <- M * ((P + 0)^2/P) * exp(-(P + 0) * Pred.time)/(1 + (0/P) * exp(-(P + 0) * Pred.time))^2

      Innovator

    })

    output$New.BD.plot <- renderPlot({

      Total <- New.BD.total()
      Innovator <- New.Innovator()
      Immitator <- Total - Innovator

      x <- data.frame(x=1:(lengths(Total)))

      par(bg = '#ECF0F5')
      plot(x[[1]], Total[[1]], xlab = "Time", ylab = "New users", type = "l", col = "red")
      lines(Innovator[[1]], col = "green")
      lines(Immitator[[1]], col = "blue")
      legend("topleft",  inset=.02, legend=c("Total", "Innovator", "Immitator"),
             col=c("red","green", "blue"), lty=1, box.lty=0, cex=0.8)


    })




    #Cumulative Total/Innovator/Immitator

    Cu.BD.total<- reactive({

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, fit the data to the cumulative curve.
        if(max(cu.dat)>M){


          ###
          #Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))
          dm.dat <- rbind(0, dm.dat)
          t <- 1:length(dm.dat)

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)


          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
            Bass.Diffusion <- Pred.time$Bass.Diffusion

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)
            #Bass.Diffusion<-c(Pred.Bass[1],diff(Pred.Bass)) #Not-cumulative
            Bass.Diffusion <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }
      Total <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))
      Total

    })


    Cu.Innovator <- reactive({

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, fit the data to the cumulative curve.
        if(max(cu.dat)>M){


          ###
          #Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))
          dm.dat <- rbind(0, dm.dat)
          t <- 1:length(dm.dat)

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)


          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
            Bass.Diffusion <- Pred.time$Bass.Diffusion

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)
            #Bass.Diffusion<-c(Pred.Bass[1],diff(Pred.Bass)) #Not-cumulative
            Bass.Diffusion <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }
      # when q=0, only Innovators without Immitators.
      Innovator <- (M * (1 - (exp(-(P + 0) * Pred.time)))/(1 + (0/P) * (exp(-(P + 0) * Pred.time))))
      Innovator

    })

    output$Cu.BD.plot <- renderPlot({

      Total <- Cu.BD.total()
      Innovator <- Cu.Innovator()
      Immitator <- Total - Innovator

      x <- data.frame(x=1:(lengths(Total)))

      par(bg = '#ECF0F5')
      plot(x[[1]], Total[[1]], xlab = "Time", ylab = "Cumulative", type = "l", col = "red")
      lines(Innovator[[1]], col = "green")
      lines(Immitator[[1]], col = "blue")
      legend("topleft",  inset=.02, legend=c("Total", "Innovator", "Immitator"),
             col=c("red","green", "blue"), lty=1, box.lty=0, cex=0.8)


    })




    #BD Accuracy
    Bass.Diffusion.Accuracy <- reactive({

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, fit the data to the cumulative curve.
        if(max(cu.dat)>M){


          ###
          #Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))
          dm.dat <- rbind(0, dm.dat)
          t <- 1:length(dm.dat)

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)


          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
            Bass.Diffusion <- Pred.time$Bass.Diffusion

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)
            #Bass.Diffusion<-c(Pred.Bass[1],diff(Pred.Bass)) #Not-cumulative
            Bass.Diffusion <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }
      pred = predict(Bass.nls)
      real = as.vector(dm.dat)
      accuracy <- accuracy(real, pred)
      accuracy
    })

    output$BD.accuracy <- renderPrint({
      BD.accuracy <- Bass.Diffusion.Accuracy()
      BD.accuracy
    })


    #BD Residual

    Bass.Diffusion.Residual <- reactive({

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, fit the data to the cumulative curve.
        if(max(cu.dat)>M){


          ###
          #Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))
          dm.dat <- rbind(0, dm.dat)
          t <- 1:length(dm.dat)

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)


          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
            Bass.Diffusion <- Pred.time$Bass.Diffusion

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)
            #Bass.Diffusion<-c(Pred.Bass[1],diff(Pred.Bass)) #Not-cumulative
            Bass.Diffusion <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }
      BD.residual <- nlsResiduals(Bass.nls)
      BD.residual

    })

    output$BD.residual.plot <- renderPlot({
      BD.residual <- Bass.Diffusion.Residual()
      plot(BD.residual)

    })

    #BD Shapiro test
    Bass.Diffusion.Shapiro.test <- reactive({

      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, fit the data to the cumulative curve.
        if(max(cu.dat)>M){


          ###
          #Bass.diffusion
          ###


          #Data processing for dm
          dm.dat <- as.numeric(as.matrix(dat$Value))
          dm.dat <- as.matrix(diff(dm.dat))
          dm.dat <- rbind(0, dm.dat)
          t <- 1:length(dm.dat)

          #Cumulative MAU, Max MAU, MAX Cumulative MAU
          cu.dat <- cumsum(dm.dat)


          #Find Optimal Start MPQ
          BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))

          possibleError <- tryCatch(
            #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
            error=function(e) e
          )

          if(inherits(possibleError, "error")){
            mpq.start <- c(M = 0,
                           P = 0,
                           Q = 0)
          }else{
            Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
            mpq.start <- update(Bass.nlsLM)

            mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                           P = summary(mpq.start)$coefficients[2],
                           Q = summary(mpq.start)$coefficients[3])
          }



          #Bass.Diffision
          possibleError <- tryCatch(
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
            error=function(e) e
          )


          if(inherits(possibleError, "error")){

            Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = 0)
            Bass.Diffusion <- Pred.time$Bass.Diffusion

          }else{
            #NLS on Bass General CDF
            Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
            Bass.nls.parameters=as.matrix(coef(Bass.nls))

            #Predict
            Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
            Pred.Bass<-predict(Bass.nls, newdata=Pred.time)
            #Bass.Diffusion<-c(Pred.Bass[1],diff(Pred.Bass)) #Not-cumulative
            Bass.Diffusion <- Pred.Bass + dat$Value[[1]] - Pred.Bass[[1]] #Cumulative

            M = Bass.nls.parameters[1,]
            P = Bass.nls.parameters[2,]
            Q = Bass.nls.parameters[3,]
          }



        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }
      BD.residual <- nlsResiduals(Bass.nls)
      Shapiro.test <- test.nlsResiduals(BD.residual)


    })

    output$BD.shapiro.test <- renderPrint({
      Shapiro.test <- Bass.Diffusion.Shapiro.test()
      Shapiro.test

    })



    ###
    #Output
    ###

    df <- reactive({

      frame.outout <- frame.output()
      dat.value <- dat.value()
      ARIMA <- ARIMA()
      Bass.Diffusion <- Bass.Diffusion()
      Linear.Regression <- Linear.Regression()
      Arima.Bass <- Arima.Bass()

      df <- cbind(frame.output(), ARIMA(), Bass.Diffusion(), Arima.Bass(), Linear.Regression())
      dat.value <- dat.value %>% dplyr::select(Time, Value)
      df <- df %>%
        dplyr::left_join(dat.value) %>%
        dplyr::select(Time, Value, ARIMA, Bass.Diffusion, Arima.Bass, Linear.Regression)

      return(df)
    })

    #df datatable
    df.dt <- reactive({
      df <- df()
      df.dt <- setNames(as.data.frame(t(df[,-1])), df[,1])
      return(df.dt)

    })

    #Output DataTable
    output$df.dt <- renderDataTable({
      df.dt <- df.dt()
      datatable(round(df.dt, digits =2),
                options = list(searching = FALSE, paging = FALSE))
    })

    output$df.dt_mydownload <-downloadHandler(
      filename = "Data_Output.csv",
      content = function(file){
        write.csv(df.dt(), file)
      })



    #Output - Accuracy table

    #Output - Accuracy table
    Accuracy <-reactive({

      df <-df()

      Abs.Error <- df %>%
        dplyr::mutate(ts.abs.error=abs(df$Value - df$ARIMA),
                      dm.abs.error=abs(df$Value - df$Bass.Diffusion),
                      tb.abs.error=abs(df$Value - df$Arima.Bass),
                      lm.abs.error=abs(df$Value - df$Linear.Regression))%>%
        dplyr::select(Time, ts.abs.error, dm.abs.error, tb.abs.error, lm.abs.error) %>%
        dplyr::rename(ARIMA = ts.abs.error, Bass.Diffusion = dm.abs.error, Arima.Bass = tb.abs.error, Linear.Regression = lm.abs.error) %>%
        dplyr::filter(complete.cases(.))

      MAE <- colSums(Abs.Error[2:5])/nrow(Abs.Error)

      Sqr.Error <- df %>%
        dplyr::mutate(ts.sqr.error=(df$Value - df$ARIMA)^2,
                      dm.sqr.error=(df$Value - df$Bass.Diffusion)^2,
                      tb.sqr.error=(df$Value - df$Arima.Bass)^2,
                      lm.sqr.error=(df$Value - df$Linear.Regression)^2)%>%
        dplyr::select(Time, ts.sqr.error, dm.sqr.error,tb.sqr.error, lm.sqr.error) %>%
        dplyr::rename(ARIMA = ts.sqr.error, Bass.Diffusion = dm.sqr.error, Arima.Bass = tb.sqr.error, Linear.Regression = lm.sqr.error) %>%
        dplyr::filter(complete.cases(.))

      RMSE <- sqrt(colSums(Sqr.Error[2:5])/nrow(Sqr.Error))
      Accuracy <- data.frame(t(rbind(MAE, RMSE)))


      if(length(Time)>1 && nrow(Abs.Error)>4){

        df.1yr <- df[(nrow(Abs.Error)-3):nrow(Abs.Error),]

        Abs.Error.1yr <- df.1yr %>%
          dplyr::mutate(ts.abs.error=abs(df.1yr$Value - df.1yr$ARIMA),
                        dm.abs.error=abs(df.1yr$Value - df.1yr$Bass.Diffusion),
                        tb.abs.error=abs(df.1yr$Value - df.1yr$Arima.Bass),
                        lm.abs.error=abs(df.1yr$Value - df.1yr$Linear.Regression))%>%
          dplyr::select(Time, ts.abs.error, dm.abs.error, tb.abs.error, lm.abs.error) %>%
          dplyr::rename(ARIMA = ts.abs.error, Bass.Diffusion = dm.abs.error,Arima.Bass = tb.abs.error, Linear.Regression = lm.abs.error) %>%
          dplyr::filter(complete.cases(.))

        Accuracy$MAE.1yr <- colSums(Abs.Error.1yr[2:5])/nrow(Abs.Error.1yr)

        Sqr.Error.1yr <- df.1yr %>%
          dplyr::mutate(ts.sqr.error=(df.1yr$Value - df.1yr$ARIMA)^2,
                        dm.sqr.error=(df.1yr$Value - df.1yr$Bass.Diffusion)^2,
                        tb.sqr.error=(df.1yr$Value - df.1yr$Arima.Bass)^2,
                        lm.sqr.error=(df.1yr$Value - df.1yr$Linear.Regression)^2)%>%
          dplyr::select(Time, ts.sqr.error, dm.sqr.error, tb.sqr.error, lm.sqr.error) %>%
          dplyr::rename(ARIMA = ts.sqr.error, Bass.Diffusion = dm.sqr.error, Arima.Bass = tb.sqr.error, Linear.Regression = lm.sqr.error) %>%
          dplyr::filter(complete.cases(.))

        Accuracy$RMSE.1yr <- sqrt(colSums(Sqr.Error.1yr[2:5])/nrow(Sqr.Error.1yr))

      }else{
        Accuracy$MAE.1yr <- 0
        Accuracy$RMSE.1yr <- 0
      }


      return(as.data.frame(Accuracy))

    })


    output$Accuracy <- renderDataTable({

      Accuracy <- Accuracy()
      Accuracy <- round(Accuracy, 2)


      formattable(Accuracy, list(
        MAE = formatter("span", style = ~ style(color = ifelse(MAE == min(MAE), "blue", NA))),
        RMSE =  formatter("span", style = ~ style(color = ifelse(RMSE == min(RMSE), "blue", NA))),
        MAE.2yrs =  formatter("span", style = ~ style(color = ifelse(MAE.2yrs == min(MAE.2yrs), "blue", NA))),
        RMSE.2yrs =  formatter("span", style = ~ style(color = ifelse(RMSE.2yrs == min(RMSE.2yrs), "blue", NA)))
      )) %>%
        as.datatable(.,
                     options = list(searching = FALSE, paging = FALSE), rownames = TRUE)
    })


    #Output - Best.Model
    Best.Model <-reactive({
      Accuracy <- Accuracy()
      Accuracy

      Best.MAE <- row.names(Accuracy)[(which(Accuracy$MAE==min(Accuracy$MAE)))]
      Best.RMSE <- row.names(Accuracy)[(which(Accuracy$RMSE==min(Accuracy$RMSE)))]
      Best.MAE.2yrs <- row.names(Accuracy)[(which(Accuracy$MAE.2yrs==min(Accuracy$MAE.2yrs)))]
      Best.RMSE.2yrs <- row.names(Accuracy)[(which(Accuracy$RMSE.2yrs==min(Accuracy$RMSE.2yrs)))]

      Best.Model <- rbind(Best.MAE, Best.RMSE, Best.MAE.2yrs, Best.RMSE.2yrs)
      Best.Model <- unique(Best.Model[,1])

      return(Best.Model)

    })

    output$Best.Model <- renderText({
      Best.Model <-Best.Model()
    })


    #plotly Output
    output$plotly <-renderPlotly({

      dat <- dat.value()
      df <- df()
      df$Time <- as.character(df$Time)


      plotly <- plot_ly(df, x=~Time) %>%
        add_trace(y = ~Linear.Regression, name = 'Linear Trend', type = 'scatter', mode = 'lines', width = 2, line = list(dash = 3, color = '#919EAD'))%>%
        add_trace(y = ~Arima.Bass, name = 'ARIMA + Bass', type = 'scatter', mode = 'lines+markers', width = 3)%>%
        add_trace(y = ~Bass.Diffusion, name = 'Bass Diffusion', type = 'scatter', mode = 'lines+markers', line = list(color = 'orange'), marker = list(color = 'orange', width = 3))%>%
        add_trace(y = ~ARIMA, name = 'ARIMA', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'green'), marker = list(color = 'green', width = 3)) %>%
        add_trace(y= ~Value, name = 'Value', type = 'scatter', mode = 'lines+markers', line = list(color = '#1f78b4'), marker = list(color = '#1f78b4', width = 3))%>%
        add_trace(y = ~Value*0.9, name = 'Value_Low', type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor='rgba(35,145,254,0.1)', line = list(color = 'transparent')) %>%
        add_trace(y = ~Value*1.1, name = 'Value_High', type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor='rgba(35,145,254,0.1)', line = list(color = 'transparent')) %>%
        layout(title = "",
               xaxis = list(title = ""), yaxis = list (title = ""))


      plotly


    })



  }

  runGadget(ui, server, viewer = browserViewer())

}

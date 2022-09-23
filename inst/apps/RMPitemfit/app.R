library(shiny)
library(DT)
library(ggplot2)
library(eRm)
library(TAM)
library(sirt)




ui <- pageWithSidebar(
  headerPanel('Item Statistics for polytomous items'),
  sidebarPanel(
    helpText("Parameter etimation method for:"),
    fluidRow(
      column(4,
             radioButtons(inputId = "method",
                          label = "Items",
                          choices = list(#"Simulation" = "sim",
                            "PCML" = "pcml.ip",
                            "JML" = "jml.ip",
                            "CML" = "cml.ip",
                            "MML" = "mml.ip"))
      ),
      column(2,
             radioButtons(inputId = "methodpp",
                          label = "Persons",
                          choices = list(#"Simulation" = "sim",
                            "WML" = "wml.pp",
                            "ML" = "ml.pp"))
      )
    ),
    tags$head(tags$style(".progress-bar{background-color:#8B8682;}",
                         type = "text/css", "a{color: #8B8682;}",
                         HTML('#go{background-color:#8B8682}'))),
    # Input: Select a file ----
    fileInput('beta', 'Choose CSV File with Item Parameters',
              accept=c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')),
    # Input: Checkbox if file has header ----
    checkboxInput("headerBeta", "Header", TRUE),
    fileInput('theta', 'Choose CSV File with Person Parameters',
              accept=c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')),
    # Input: Checkbox if file has header ----
    checkboxInput("headerTheta", "Header", TRUE),
    # Input: Control number of simulations ----
    numericInput('B', 'Number of simulations', 50, min = 0, max = 1000, step = 50),
    #conditionalPanel(
    #  condition = "input.method == 'jml.ip'",
    #  numericInput(inputId = "adj", label = "Adjustment for extremes in JML", 
    #               value = 0.3, min = 0, max = 1, step = "any")
    #),
    actionButton("go", "Go")
  ),
  mainPanel(
    #conditionalPanel(
    #  condition = "input.stats == 'Positive/Negative Count'",
    #  plotOutput("sidebarplot")
    #  ),
    #dataTableOutput("table1"),
    tabsetPanel(type = "tabs", id = "inTabset",
                tabPanel(
                  title = "Input", value = "Input",
                  fluidRow(
                    column(width = 4,
                           h3("Item parameters"),
                           DT::dataTableOutput('tbl1')),
                    column(width = 4,
                           h3("Person parameters"),
                           DT::dataTableOutput('tbl2'))
                  )),
                tabPanel("Outfit", plotOutput("plot2")),
                tabPanel("Infit", plotOutput("plot3")),
                tabPanel("FitResidual", plotOutput("plot4")))
  )
)

server <- function(input, output, session) {
  
  idb <- NULL
  
  betaRea <- reactive({
    
    validate(
      need(input$beta != "", "Please select CSV file with item parameters")
    )
    
    beta0 <- input$beta
    if (is.null(beta0)) return(NULL)
    
    beta1 <- read.csv(beta0$datapath,
                      header = input$headerBeta#,
                      #sep = input$sepBeta,
                      #quote = input$quoteBeta
    )
    
    if (is.integer(beta1[,1])) {
      beta <- apply(beta1[, -1], 2, as.numeric)
    } else {
      beta <- apply(beta1, 2, as.numeric)
    }
    
    
    validate(
      need(!is.null(beta), "Cannot identify item parameters from input file.")
    )
    
    idb <<- showNotification(paste("Number of items: ", ncol(beta)), 
                     duration = NULL, type = "warning")
    
    beta
    
  })
  
  thetaRea <- reactive({
    
    validate(
      need(input$theta != "", "Please select CSV file with person parameters")
    )
    
    theta0 <- input$theta
    if (is.null(theta0)) return(NULL)
    
    #theta <- suppressWarnings(as.numeric(data))
    #validate(
    #  need(anyNA(theta), "All values of person parameters can not be converted to numeric")
    #)
    
    theta1 <- read.csv(theta0$datapath,
                       header = input$headerTheta#,
                       #sep = input$sepTheta,
                       #quote = input$quoteTheta
                       )
    
    if (any(class(theta1) %in% c("matrix", "data.frame", "array"))) {
      if (any(colnames(theta1) == "theta")) {
        theta <- as.numeric(theta1[,"theta"])
      } else if (is.integer(theta1[, 1])) {
        theta <- as.numeric(theta1[, 2]) 
      } else if (ncol(theta1) == 1) {
        theta <- as.numeric(theta1[,1])
      }
    } else if (any(class(theta1) == "numeric")) {
      theta <- theta1
    } else {
      theta <- NULL
    }
    
    validate(
      need(!is.null(theta), "Cannot identify person parameters from input file.")
    )
    
    theta
    
  })
  
  observeEvent(betaRea(), {
    if(input$method == "jml.ip") {
      beta <- betaRea()
      if(!dplyr::near(mean(beta), 0, tol = .Machine$double.eps^0.5)) {
        showModal(zeromean_confirm)
      } 
    } 
  })
  
  observeEvent(input$ok, {
    beta <- betaRea()
    showNotification(paste("Continue without zero-mean constraint:", 
                           "mean = ", signif(mean(beta), digits = 1), collapse = "\n"), 
                     duration = NULL)
    removeModal()
  })
  observeEvent(input$cancel, {
    removeModal()
  })
  
  #toListen <- reactive({
  #  list(thetaRea(),betaRea())
  #})
  
  #observeEvent(toListen(), {
  #  beta <- as.numeric(betaRea()[, 2]) #betaRea()
  #  theta <- as.numeric(thetaRea()[, 2]) #thetaRea()
  #  if (length(beta) == length(theta)) {
  #    if (all(beta == theta)) {
  #      showModal(nobsnitms_confirm)
  #    } 
  #  }
  #})
  
  #observeEvent(input$okn, {
  #  showNotification("Files of item parameters and person parameters are the same", 
  #                   duration = NULL)
  #  removeModal()
  #})
  #observeEvent(input$canceln, {
  #  removeModal()
  #})
  
  #output$table1 <- renderDataTable({
  #  beta <- as.numeric(betaRea()[,"beta"])
  #  theta <- as.numeric(thetaRea()[,"theta"])
  #  
  #  data.frame("Item paramters" = beta,
  #             "Person parameters" = theta)
  #})
  
  output$tbl1 <- renderDataTable(data.frame(betaRea()),
                                 options = list(searching = FALSE, scrollX = T),
                                 rownames= FALSE
  )
  
  output$tbl2 <- renderDataTable(data.frame(thetaRea()),
                                 options = list(searching = FALSE, scrollX = T),
                                 rownames= FALSE
  )
  
  observeEvent(input$go, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Outfit")
  })
  
  
  #============ Simulate statistics =======================
  
  selectedData <- eventReactive(input$go, {
    
    removeNotification(idb)
    
    withProgress(message = 'Simulating', value = 0, {
      
      beta <- betaRea()
      theta <- thetaRea()
      
      
      method.item <- input$method
      if (input$methodpp == "ml.pp") {
        method.person <- "MLE"
      } else if (input$methodpp == "wml.pp") {
        method.person <- "WML"
      }
      #if (input.method == "jml.ip") {
      #  jlm.adj <- input$adj
      #}
      
      N <- length(theta)
      K <- ncol(beta)
      M <- nrow(beta)
      mi <- rep(nrow(beta), K) #apply(beta, 2, function(x) sum(!is.na(x)))
      set.seed(1)
      
      #-------------------- Compute probabilities ----------------------------
      probs <- vector(mode = "list", length = K)
      for (i in 1:K) {
        probs[[i]] <- pcmfct(theta = theta, b = beta, ii = i)
      }
      
      #-------------------- Simulate -----------------------------------------
      B <- input$B
      P0 <- matrix(runif(n = N * K * B, 0, 1), ncol = B)
      P1 <- split(P0, rep(1:ncol(P0), each = nrow(P0)))
      PP <- lapply(P1, FUN = function(x) matrix(x, nrow = N, ncol = K))
      
      if (method.item %in% c("cml.ip", "jml.ip", "mml.ip", "pcml.ip")) {
        
        X <- vector(mode = "list", length = B)
        X <- lapply(X, FUN = function(x) matrix(nrow = N, ncol = K))
        outfitsim <- matrix(nrow = K, ncol = B)
        infitsim <- matrix(nrow = K, ncol = B)
        fitresid <- matrix(nrow = K, ncol = B)
        
        for (b in 1:B) {
          
          #------------- Simulate polytomous item responses ------------------
          for (i in 1:K) {
            for (j in 1:N) {
              cumprop <- cumsum(probs[[i]][j, ])
              U <- PP[[b]][j, i] 
              for (x in 1:(mi[i]+1)) {
                if (U <= cumprop[x]) {
                  X[[b]][j,i] <- x-1
                  break
                }
              }
            }
          }
          #testfit <- PCM(X[[b]], sum0 = TRUE); coef(testfit); betavec
          #testpers <- person.parameter(testfit); testtheta <- ppar$thetapar$NAgroup1
          
          #------------- Remove obs. with extreme scores ---------------------
          test <- which(rowSums(X[[b]]) %in% c(0,M*K))
          if (identical(test, integer(0))) {
            newX <- X[[b]]
          } else {
            newX <- X[[b]][-test,]
          }
          colnames(newX) <- paste0("I", 1:K)
          
          #============= Fit item parameters =================================
          
          if (method.item == "cml.ip") {
            fit <- eRm::PCM(newX, sum0 = TRUE)
            beta.vec <- - fit$betapar 
            rows <- do.call(c, lapply(1:K, function(i) 1:mi[i]))
            cols <- do.call(c, lapply(1:K, function(i) rep(i, mi[i])))
            beta.sim <- matrix(NA, nrow = max(newX), ncol = ncol(newX))
            for (i in 1:length(beta.vec)) {
              beta.sim[rows[i], cols[i]] <- beta.vec[i]
            }
          }
          if (method.item == "jml.ip") {
            fit <- TAM::tam.jml(resp = newX, 
                                #adj = jml.adj,
                                constraint = "items",
                                verbose = FALSE)
            beta.sim <- t(fit$item[, grep("AXsi", colnames(fit$item))])
            beta.sim <- beta.sim - mean(beta.sim, na.rm = TRUE) #---------------------------?
          }
          if (method.item == "mml.ip") {
            #library(ltm)
            #fit <- ltm::gpcm(newX, constraint = "rasch")
            #beta.tmp <- lapply(fit$coefficients, function(i) i[-length(i)])
            #beta.sim <- do.call(cbind, beta.tmp)
            #beta.sim <- beta.sim
            fit <- TAM::tam.mml(resp = newX, 
                                #constraint = "items", #------------------------------------?
                                irtmodel = "PCM", #-----------------------------------------?
                                verbose = FALSE)
            beta.vec <- fit$xsi$xsi #- mean(fit$xsi$xsi, na.rm = TRUE) 
            rows <- do.call(c, lapply(1:K, function(i) 1:mi[i]))
            cols <- do.call(c, lapply(1:K, function(i) rep(i, mi[i])))
            beta.sim <- matrix(NA, nrow = max(newX), ncol = ncol(newX))
            for (i in 1:length(beta.vec)) {
              beta.sim[rows[i], cols[i]] <- beta.vec[i]
            }
          }
          if (method.item == "pcml.ip") {
            fit <- sirt::rasch.evm.pcm(dat = newX)
            beta.vec <- fit$item$est 
            if (any(is.na(beta.vec))) {
              outfitsim[,b] <- rep(NA, K)
              infitsim[, b] <- rep(NA, K)
              fitresid[, b] <- rep(NA, K)
              next
            }
            rows <- do.call(c, lapply(1:K, function(i) 1:mi[i]))
            cols <- do.call(c, lapply(1:K, function(i) rep(i, mi[i])))
            beta.sim <- matrix(NA, nrow = max(newX), ncol = ncol(newX))
            for (i in 1:length(beta.vec)) {
              beta.sim[rows[i], cols[i]] <- beta.vec[i]
            }
          }
          
          #============= End fit item parameters =============================
          
          #============= Fit person parameters: WML/MLE ======================
          
          #------------- Define item response function -----------------------
          arg.list <- list("b" = beta.sim)
          abil <- sirt::IRT.mle(data = newX, irffct = pcmfct, arg.list = arg.list,
                                type = method.person, progress = FALSE)
          theta.sim <- abil$est
          theta.sim[theta.sim %in% c(Inf, -Inf)] <- NA
          
          #============= End fit person parameters ===========================
          
          #============= Compute infit/outfit/fitresid =======================
          
          statobj <- outfitfct(beta = beta.sim, theta = theta.sim, 
                               data = newX, N = nrow(newX), K)
          outfitsim[,b] <- statobj$outfitsim
          infitsim[, b] <- statobj$infitsim
          fitresid[, b] <- statobj$fitresid
          
          #============= End compute infit/outfit ============================
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/B, detail = paste("Iteration", b))
          
        }
      }
      
      zmin <- apply(outfitsim, 2, min)
      zmin <- zmin[!is.na(zmin)]
      zmax <- apply(outfitsim, 2, max)
      zmax <- zmax[!is.na(zmax)]
      
      imin <- apply(infitsim, 2, min)
      imin <- imin[!is.na(imin)]
      imax <- apply(infitsim, 2, max)
      imax <- imax[!is.na(imax)]
      
      fmin <- apply(fitresid, 2, min)
      fmin <- fmin[!is.na(fmin)]
      fmax <- apply(fitresid, 2, max)
      fmax <- fmax[!is.na(fmax)]
      
      selectedData <- list(zmin = zmin, zmax = zmax, 
                           imin = imin, imax = imax,
                           fmin = fmin, fmax = fmax)
      
      
    })
  })
  
  output$plot2 <- renderPlot({
    
    my_colors <- c("seashell3", "seashell2", "seashell")
    names(my_colors) <- c("2.5%", "5%", "other")
    
    z <- selectedData()$zmin 
    p1 <- plotfct(z = z, probs = c(0.025, 0.05), 
                  breaks = c("2.5%", "5%"), labels = c("2.5%", "5%", "other"),
                  colours = my_colors, xtitle = "Outfit", 
                  title = "Minimal outfit distribution")
    
    z <- selectedData()$zmax 
    p2 <- plotfct(z = z, probs = c(0.95, 0.975), 
                  breaks = c("5%", "2.5%"), labels = c("other", "5%", "2.5%"),
                  colours = my_colors, xtitle = "Outfit", 
                  title = "Maximal outfit distribution")
    
    ggpubr::ggarrange(plotlist = list(p1, p2), 
                      common.legend = TRUE, legend = "bottom")
    
  })
  
  output$plot3 <- renderPlot({
    
    my_colors <- c("seashell3", "seashell2", "seashell")
    names(my_colors) <- c("2.5%", "5%", "other")
    
    z <- selectedData()$imin 
    p1 <- plotfct(z = z, probs = c(0.025, 0.05), 
                  breaks = c("2.5%", "5%"), labels = c("2.5%", "5%", "other"),
                  colours = my_colors, xtitle = "Infit", 
                  title = "Minimal infit distribution")
    
    z <- selectedData()$imax 
    p2 <- plotfct(z = z, probs = c(0.95, 0.975), 
                  breaks = c("5%", "2.5%"), labels = c("other", "5%", "2.5%"),
                  colours = my_colors, xtitle = "Infit", 
                  title = "Maximal infit distribution")
    
    ggpubr::ggarrange(plotlist = list(p1, p2), 
                      common.legend = TRUE, legend = "bottom")
    
  })
  
  output$plot4 <- renderPlot({
    
    my_colors <- c("seashell3", "seashell2", "seashell")
    names(my_colors) <- c("2.5%", "5%", "other")
    
    z <- selectedData()$fmin 
    p1 <- plotfct(z = z, probs = c(0.025, 0.05), 
                  breaks = c("2.5%", "5%"), labels = c("2.5%", "5%", "other"),
                  colours = my_colors, xtitle = "FitResidual", 
                  title = "Minimal FitResidual distribution")
    
    z <- selectedData()$fmax 
    p2 <- plotfct(z = z, probs = c(0.95, 0.975), 
                  breaks = c("5%", "2.5%"), labels = c("other", "5%", "2.5%"),
                  colours = my_colors, xtitle = "FitResidual", 
                  title = "Maximal FitResidual distribution")
    
    ggpubr::ggarrange(plotlist = list(p1, p2), 
                      common.legend = TRUE, legend = "bottom")
    
  })
  
}
  
shinyApp(ui, server)
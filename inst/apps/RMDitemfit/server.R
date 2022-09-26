function(input, output, session) {
  
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
    
    if (any(class(beta1) %in% c("matrix", "data.frame", "array"))) {
      if (any(colnames(beta1) == "beta")) {
        beta <- as.numeric(beta1[,"beta"])
      } else if (is.integer(beta1[, 1])) {
        beta <- as.numeric(beta1[, 2]) 
      } else if (ncol(beta1) == 1) {
        beta <- as.numeric(beta1[,1])
      }
    } else if (any(class(beta1) == "numeric")) {
      beta <- beta1
    } else {
      beta <- NULL
    }
    
    validate(
      need(!is.null(beta), "Cannot identify item parameters from input file.")
    )
    
    beta
    
  })
  
  thetaRea <- reactive({
    
    validate(
      need(input$theta != "", "Please select CSV file with person parameters")
    )
    
    theta0 <- input$theta
    if (is.null(theta0)) return(NULL)
    
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
      if(!dplyr::near(mean(betaRea()), 0, tol = .Machine$double.eps^0.5)) {
        showModal(zeromean_confirm)
      } 
    } 
    
  })
  
  observeEvent(input$ok, {
    
    showNotification(paste("Continue without zero-mean constraint:", 
                           "mean = ", signif(mean(betaRea()), digits = 1), collapse = "\n"), 
                     duration = NULL)
    removeModal()
    
  })
  
  observeEvent(input$cancel, {
    removeModal()
  })
  
  toListen <- reactive({
    list(thetaRea(),betaRea())
  })
  
  observeEvent(toListen(), {
    beta <- betaRea()
    theta <- thetaRea()
    if (length(beta) == length(theta)) {
      if (all(beta == theta)) {
        showModal(nobsnitms_confirm)
      } 
    }
  })
  
  observeEvent(input$okn, {
    showNotification("Files of item parameters and person parameters are the same", 
                     duration = NULL)
    removeModal()
  })
  observeEvent(input$canceln, {
    removeModal()
  })
  
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
      
      B <- input$B
      
      set.seed(1)
      N <- length(theta)
      K <- length(beta)
      
      #-------------------- Compute probabilities ----------------------------
      probs <- sapply(1:K, function(ii) irffct(theta = theta, b = beta, ii)[, 2])
      
      #-------------------- Simulate -----------------------------------------
      
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
          
          #------------- Simulate item responses (0/1) -----------------------
          for (i in 1:N) {
            for (j in 1:K) {
              X[[b]][i,j] <- ifelse(PP[[b]][i,j] > probs[i,j], 1, 0)
            }
          }
          
          #------------- Remove obs. with extreme scores ---------------------
          test <- which(rowSums(X[[b]]) %in% c(0,ncol(X[[b]])))
          if (identical(test, integer(0))) {
            newX <- X[[b]]
          } else {
            newX <- X[[b]][-test,]
          }
          colnames(newX) <- paste0("I", 1:K)
          
          #============= Fit item parameters =================================
          
          if (method.item == "cml.ip") {
            fit <- eRm::RM(newX, sum0 = TRUE)
            beta.sim <- - fit$betapar 
          }
          if (method.item == "jml.ip") {
            fit <- TAM::tam.jml(resp = newX, 
                                #adj = jml.adj,
                                constraint = "items",
                                verbose = FALSE)
            beta.sim <- c(fit$xsi, -sum(fit$xsi))
          }
          if (method.item == "mml.ip") {
            fit <- TAM::tam.mml(resp = newX, verbose = FALSE)
            beta.sim <- fit$xsi$xsi - mean(fit$xsi$xsi) 
          }
          if (method.item == "pcml.ip") {
            fit <- sirt::rasch.pairwise(dat = newX, zerosum = TRUE)
            tmp <- which(!colnames(newX) %in% fit$item$item)
            if (identical(tmp, integer(0))) {
              beta.sim <- fit$item$b   # extract item difficulties
            } else {
              nottmp <- (1:K)[-tmp]
              beta.sim <- rep(NA, K)
              beta.sim[nottmp] <- fit$item$b   # extract item difficulties
            }
            
          }
          
          #============= End fit item parameters =============================
          
          #============= Fit person parameters: WML/MLE ======================
          
          arg.list <- list("b" = beta.sim)
          abil <- sirt::IRT.mle(data = newX, irffct = irffct, arg.list = arg.list,
                                type = method.person, progress = FALSE)
          theta.sim <- abil$est
          theta.sim[theta.sim %in% c(Inf, -Inf)] <- NA
          
          #============= End fit person parameters ===========================
          
          #============= Compute infit/outfit/fitresid =======================
          
          statobj <- outfitfct(beta = beta.sim, theta = theta.sim, 
                               data = newX)
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
                  colours = my_colors, xtitle = "Outfit", title = "Minimal outfit distribution")
    
    z <- selectedData()$zmax 
    p2 <- plotfct(z = z, probs = c(0.95, 0.975), 
                  breaks = c("5%", "2.5%"), labels = c("other", "5%", "2.5%"),
                  colours = my_colors, xtitle = "Outfit", title = "Maximal outfit distribution")
    
    ggpubr::ggarrange(plotlist = list(p1, p2), common.legend = TRUE, legend = "bottom")
    
  })
  
  output$plot3 <- renderPlot({
    
    my_colors <- c("seashell3", "seashell2", "seashell")
    names(my_colors) <- c("2.5%", "5%", "other")
    
    z <- selectedData()$imin 
    p1 <- plotfct(z = z, probs = c(0.025, 0.05), 
                  breaks = c("2.5%", "5%"), labels = c("2.5%", "5%", "other"),
                  colours = my_colors, xtitle = "Infit", title = "Minimal infit distribution")
    
    z <- selectedData()$imax 
    p2 <- plotfct(z = z, probs = c(0.95, 0.975), 
                  breaks = c("5%", "2.5%"), labels = c("other", "5%", "2.5%"),
                  colours = my_colors, xtitle = "Infit", title = "Maximal infit distribution")
    ggpubr::ggarrange(plotlist = list(p1, p2), common.legend = TRUE, legend = "bottom")
    
  })
  
  output$plot4 <- renderPlot({
    
    my_colors <- c("seashell3", "seashell2", "seashell")
    names(my_colors) <- c("2.5%", "5%", "other")
    
    z <- selectedData()$fmin 
    p1 <- plotfct(z = z, probs = c(0.025, 0.05), 
                  breaks = c("2.5%", "5%"), labels = c("2.5%", "5%", "other"),
                  colours = my_colors, xtitle = "FitResidual", title = "Minimal FitResidual distribution")
    
    z <- selectedData()$fmax 
    p2 <- plotfct(z = z, probs = c(0.95, 0.975), 
                  breaks = c("5%", "2.5%"), labels = c("other", "5%", "2.5%"),
                  colours = my_colors, xtitle = "FitResidual", title = "Maximal FitResidual distribution")
    ggpubr::ggarrange(plotlist = list(p1, p2), common.legend = TRUE, legend = "bottom")
    
  })
  
}
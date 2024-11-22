source("global.R")
source("ui.R")

server <- function(input, output, session) {
  # Base reactive data
  data <- reactive({
    preprocess_data()
  })
  
  # Reactive data processing for each tab
  provincial_schools <- reactive({
    schools <- data()$schools
    
    if (input$provincialSchoolType != "All") {
      schools <- schools %>%
        filter(school_type == input$provincialSchoolType)
    }
    
    if (length(input$provincialSchoolLevels) > 0) {
      schools <- schools %>%
        filter(school_level %in% input$provincialSchoolLevels)
    }
    
    # Ensure schools are within administrative boundaries
    admin_boundaries <- data()$admin
    
    schools_in_bounds <- st_intersection(
      schools,
      st_union(admin_boundaries)  # Use st_union to get single geometry
    )
    
    return(schools)
  })
  
  city_schools <- reactive({
    req(input$citySelect)
    
    admin_filtered <- data()$schools %>%
      filter(city_name == input$citySelect)
    
    # Then verify spatial intersection
    city_boundary <- data()$admin %>%
      filter(city == input$citySelect)
    
    schools <- st_intersection(
      admin_filtered,
      st_union(city_boundary)
    )
    
    # Apply filters
    if (input$citySchoolType != "All") {
      schools <- schools %>%
        filter(school_type == input$citySchoolType)
    }
    
    if (length(input$citySchoolLevels) > 0) {
      schools <- schools %>%
        filter(school_level %in% input$citySchoolLevels)
    }
    
    return(schools)
  })
  
  stats_schools <- reactive({
    schools <- data()$schools
    
    if (input$statsSelect != "All") {
      schools <- schools %>%
        filter(city_name == input$statsSelect)
    }
    
    if (input$statsSchoolType != "All") {
      schools <- schools %>%
        filter(school_type == input$statsSchoolType)
    }
    
    if (length(input$statsSchoolLevels) > 0) {
      schools <- schools %>%
        filter(school_level %in% input$statsSchoolLevels)
    }
    
    return(schools)
  })
  
  # Static data for public school ratio
  public_ratio <- reactive({
    data()$schools %>%
      group_by(city_name) %>%
      summarise(
        total_schools = n(),
        public_schools = sum(school_type == "Public"),
        public_ratio = public_schools / total_schools,
        .groups = "drop"
      )
  })
  
  # Provincial Overview outputs
  output$schoolDistPlot <- renderPlotly({
    ggplotly(
      ggplot(provincial_schools()) +
        geom_bar(aes(x = city_name, fill = city_name)) +
        scale_fill_viridis_d(option = "plasma") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 12, face = "bold"),
          legend.position = "none"
        ) +
        labs(x = "City",
             y = "Number of Schools")
    )
  })
  
  output$levelDistPlot <- renderPlotly({
    ggplotly(
      ggplot(provincial_schools()) +
        geom_bar(aes(x = city_name, fill = school_level)) +
        scale_fill_viridis_d(option = "plasma") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "City",
             y = "Number of Schools",
             fill = "School Level")
    )
  })
  
  output$densityMap <- renderPlot({
    req(provincial_schools())
    
    kde_result <- calculate_kde(
      provincial_schools(),
      data()$boundaries,
      bandwidth = input$provincialBandwidth
    )
    
    if (!is.null(kde_result)) {
      kde_df <- as.data.frame(kde_result$kde)
      
      ggplot() +
        geom_sf(data = data()$boundaries, fill = "transparent") +
        geom_tile(data = kde_df, aes(x = x, y = y, fill = value)) +
        scale_fill_viridis_c(option = "plasma") +
        theme_minimal() +
        labs(
          title = "Kernel Density Estimation of Schools",
          subtitle = paste0("Based on ", nrow(provincial_schools()), " school locations"),
          fill = "Density"
        )
    }
  })
  
  # Add Population Density map
  output$populationMap <- renderPlot({
    ggplot() +
      geom_sf(data = data()$admin, aes(fill = popn_density)) +
      scale_fill_viridis_c(
        option = "plasma",
        labels = scales::comma_format()
      ) +
      theme_minimal() +
      labs(fill = "Population\nDensity\n(per km²)")
  })
  
  output$cityKDE <- renderPlot({
    req(city_schools())
    req(input$citySelect)
    
    # Get city boundary
    city_boundary <- data()$admin %>%
      filter(city == input$citySelect)
    
    schools_in_boundary <- st_intersection(
      city_schools(),
      st_union(city_boundary)
    )
    
    if(nrow(schools_in_boundary) > 0) {
      ggplot() +
        geom_sf(data = city_boundary, 
                aes(fill = popn_density)) +
        geom_sf(data = schools_in_boundary,
                aes(color = school_level),
                size = 2,
                alpha = 0.6) +
        scale_fill_viridis_c(
          option = "plasma",
          name = "Population\nDensity",
          labels = scales::comma_format()
        ) +
        scale_color_viridis_d(
          option = "plasma",
          name = "School Level"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          plot.margin = margin(10, 10, 10, 10)
        ) +
        labs(
          title = paste("Schools Distribution in", input$citySelect),
          subtitle = paste("Number of schools:", nrow(schools_in_boundary))
        )
    } else {
      ggplot() +
        geom_sf(data = city_boundary) +
        theme_minimal() +
        labs(title = "No schools match the selected criteria")
    }
  })
  
  output$typeRatio <- renderPlot({
    ggplot(public_ratio()) +
      geom_bar(aes(x = reorder(city_name, -public_ratio), 
                   y = public_ratio, 
                   fill = city_name),
               stat = "identity") +
      scale_fill_viridis_d(option = "plasma") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      labs(
        title = "Public School Ratio by City",
        x = "City",
        y = "Proportion of Public Schools"
      )
  })
  
  # Add Nearest Neighbor Analysis
  output$nnPlot <- renderPlot({
    req(city_schools())
    nn_stats <- calculate_nn_stats(city_schools(), data()$boundaries)
    
    if(!is.null(nn_stats)) {
      nn_df <- data.frame(
        Type = c("Observed", "Expected"),
        Distance = c(nn_stats$observed, nn_stats$expected)
      )
      
      ggplot(nn_df, aes(x = Type, y = Distance, fill = Type)) +
        geom_bar(stat = "identity") +
        scale_fill_viridis_d(option = "plasma") +
        theme_minimal() +
        labs(
          title = paste("Nearest Neighbor Distances -", input$citySelect),
          subtitle = paste("R statistic:", round(nn_stats$r_statistic, 3)),
          y = "Distance (meters)"
        )
    }
  })
  
  output$cityStats <- renderDT({
    # Get schools data
    schools_data <- city_schools() %>%
      st_set_geometry(NULL)
    
    # Create base columns
    base_cols <- c("city_name")
    
    # Add school level columns based on filter
    level_cols <- NULL
    if(length(input$citySchoolLevels) == 0) {
      level_cols <- c(
        "Elementary" = "Elementary",
        "Junior High" = "Junior High",
        "Senior High" = "Senior High",
        "Special Education" = "Special Education"
      )
    } else {
      level_cols <- setNames(input$citySchoolLevels, input$citySchoolLevels)
    }
    
    # Add school type columns based on filter
    type_cols <- NULL
    if(input$citySchoolType == "All") {
      type_cols <- c("Public", "Private")
    } else {
      type_cols <- input$citySchoolType
    }
    
    # Create summary
    stats_df <- schools_data %>%
      group_by(city_name) %>%
      summarise(
        `Total Schools` = n(),
        !!!lapply(level_cols, function(x) 
          quo(sum(school_level == !!x))),
        !!!lapply(type_cols, function(x) 
          quo(sum(school_type == !!x))),
        .groups = "drop"
      ) %>%
      rename(City = city_name)
    
    # Add column labels
    colnames(stats_df) <- c(
      "City", 
      "Total Schools",
      paste(names(level_cols), "Schools"),
      paste(type_cols, "Schools")
    )
    
    datatable(
      stats_df,
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        pageLength = 5
      ),
      rownames = FALSE
    )
  })
    
  output$quadratPlot <- renderPlot({
    req(stats_schools())
    req(nrow(stats_schools()) >= 4)
    
    # Get appropriate boundary
    if (input$statsSelect == "All") {
      boundary <- data()$boundaries
    } else {
      boundary <- data()$boundaries %>%
        filter(shapeName == input$statsSelect)
    }
    
    window <- as.owin(st_union(boundary))
    coords <- st_coordinates(stats_schools())
    
    # Remove duplicates
    coords_df <- as.data.frame(coords)
    coords_unique <- coords_df[!duplicated(coords_df), ]
    
    # Filter points within window
    inside <- inside.owin(
      x = coords_unique[,1],
      y = coords_unique[,2],
      w = window
    )
    valid_coords <- coords_unique[inside,]
    
    if(nrow(valid_coords) >= 4) {
      ppp_points <- ppp(
        x = valid_coords[,1],
        y = valid_coords[,2],
        window = window
      )
      
      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(window, main = "Quadrat Analysis", col = "lightgray")
      points(ppp_points, col = "blue", pch = 16)
      
      density_overlay <- density.ppp(ppp_points, 
                                   sigma = bw.scott(ppp_points),
                                   edge = TRUE)
      plot(density_overlay, add = TRUE, col = viridis(100, alpha = 0.4))
      
      # Adjust number of quadrats based on number of points
      n_points <- nrow(valid_coords)
      n_quadrats <- min(max(floor(sqrt(n_points/5)), 2), 10)
      
      q <- quadratcount(ppp_points, nx = n_quadrats, ny = n_quadrats)
      plot(q, add = TRUE, col = "red", cex = 0.8)
      
    } else {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Insufficient data for quadrat analysis\n(Need at least 4 points)")
    }
  })
  
  output$quadratStats <- renderPrint({
    req(stats_schools())
    req(nrow(stats_schools()) >= 4)
    
    # Get appropriate boundary based on selection
    if (input$statsSelect == "All") {
      boundary <- data()$boundaries
    } else {
      boundary <- data()$boundaries %>%
        filter(shapeName == input$statsSelect)
    }
    
    window <- as.owin(st_union(boundary))
    coords <- st_coordinates(stats_schools())
    
    # Remove duplicates
    coords_df <- as.data.frame(coords)
    coords_unique <- coords_df[!duplicated(coords_df), ]
    
    # Filter points within window
    inside <- inside.owin(
      x = coords_unique[,1],
      y = coords_unique[,2],
      w = window
    )
    valid_coords <- coords_unique[inside,]
    
    if(nrow(valid_coords) >= 4) {
      ppp_points <- ppp(
        x = valid_coords[,1],
        y = valid_coords[,2],
        window = window
      )
      
      # Adjust number of quadrats based on point density
      area <- area.owin(window)
      n_points <- nrow(valid_coords)
      expected_per_quadrat <- 5  # aim for at least 5 points per quadrat
      n_quadrats <- min(max(floor(sqrt(n_points/expected_per_quadrat)), 2), 10)
      
      tryCatch({
        qt <- quadrat.test(
          ppp_points,
          nx = n_quadrats,
          ny = n_quadrats,
          nsim = input$simulations
        )
        print(qt)
        
        # Add additional information
        cat("\nAnalysis Details:\n")
        cat("Number of points:", n_points, "\n")
        cat("Number of quadrats:", n_quadrats * n_quadrats, "\n")
        cat("Average points per quadrat:", 
            round(n_points/(n_quadrats * n_quadrats), 2), "\n")
        cat("Number of simulations:", input$simulations, "\n")
      }, error = function(e) {
        cat("Error in quadrat test calculation\n")
        cat("Error message:", e$message, "\n")
      })
    } else {
      cat("Insufficient data for quadrat analysis (Need at least 4 points)\n")
    }
  })
  
  # Cross K Function Analysis
  output$crossK <- renderPlot({
    req(stats_schools())
    req(nrow(stats_schools()) >= 4)
    
    # Get appropriate boundary based on selection
    if (input$statsSelect == "All") {
      boundary <- data()$boundaries
    } else {
      boundary <- data()$boundaries %>%
        filter(shapeName == input$statsSelect)
    }
    
    # Always include both public and private schools for comparison
    all_schools <- data()$schools
    if (input$statsSelect != "All") {
      all_schools <- all_schools %>%
        filter(city_name == input$statsSelect)
    }
    
    # Filter by school level if selected
    if (length(input$statsSchoolLevels) > 0) {
      all_schools <- all_schools %>%
        filter(school_level %in% input$statsSchoolLevels)
    }
    
    window <- as.owin(st_union(data()$boundaries))
    
    # Separate public and private schools
    public_schools <- all_schools %>% 
      filter(school_type == "Public")
    private_schools <- all_schools %>% 
      filter(school_type == "Private")
    
    if(nrow(public_schools) >= 2 && nrow(private_schools) >= 2) {
      coords <- st_coordinates(all_schools)
      marks <- factor(all_schools$school_type)
      
      # Remove duplicates and filter points
      coords_df <- data.frame(coords, mark = marks)
      coords_unique <- coords_df[!duplicated(coords_df[,1:2]), ]
      
      inside <- inside.owin(
        x = coords_unique[,1],
        y = coords_unique[,2],
        w = window
      )
      valid_coords <- coords_unique[inside,]
      
      if(nrow(valid_coords) >= 4) {
        ppp_marked <- ppp(
          x = valid_coords[,1],
          y = valid_coords[,2],
          window = window,
          marks = factor(valid_coords$mark)
        )
        
        cross_k <- Kcross(ppp_marked, i = "Public", j = "Private", correction = "border")
        
        # Calculate theoretical envelope if simulations are requested
        if(input$simulations > 0) {
          cross_k_env <- envelope(ppp_marked, 
                                  fun = Kcross,
                                  i = "Public",
                                  j = "Private",
                                  nsim = input$simulations,
                                  correction = "border")
          
          plot(cross_k_env, 
               main = paste("Cross K Function: Public vs Private Schools\n",
                            sprintf("Based on %d simulations", input$simulations)),
               legend = TRUE)
        } else {
          plot(cross_k, 
               main = "Cross K Function: Public vs Private Schools",
               legend = TRUE)
        }
      }
    } else {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Insufficient data for Cross K analysis\n(Need at least 2 schools of each type)")
    }
  })
  
  output$kFunction <- renderPlot({
    req(stats_schools())
    req(nrow(stats_schools()) >= 4)
    
    # Get appropriate boundary
    if (input$statsSelect == "All") {
      boundary <- data()$boundaries
    } else {
      boundary <- data()$boundaries %>%
        filter(shapeName == input$statsSelect)
    }
    
    window <- as.owin(st_union(boundary))
    coords <- st_coordinates(stats_schools())
    
    # Remove duplicates
    coords_df <- as.data.frame(coords)
    coords_unique <- coords_df[!duplicated(coords_df), ]
    
    # Filter points within window
    inside <- inside.owin(
      x = coords_unique[,1],
      y = coords_unique[,2],
      w = window
    )
    valid_coords <- coords_unique[inside,]
    
    if(nrow(valid_coords) >= 4) {
      ppp_points <- ppp(
        x = valid_coords[,1],
        y = valid_coords[,2],
        window = window
      )
      
      k_func <- Kest(ppp_points, correction = "border")
      
      # Add envelope if simulations are requested
      if(input$simulations > 0) {
        k_env <- envelope(ppp_points, 
                          fun = Kest,
                          nsim = input$simulations,
                          correction = "border")
        
        plot(k_env,
             main = paste("K Function Analysis", 
                          if(input$statsSelect != "All") paste("-", input$statsSelect) else "",
                          sprintf("\nBased on %d simulations", input$simulations)),
             legend = TRUE)
      } else {
        plot(k_func,
             main = paste("K Function Analysis", 
                          if(input$statsSelect != "All") paste("-", input$statsSelect) else ""),
             legend = TRUE)
      }
    }
  })
  
  # Update L Function to show single plot with envelope
  output$lFunction <- renderPlot({
    req(stats_schools())
    req(nrow(stats_schools()) >= 4)
    
    # Get appropriate boundary
    if (input$statsSelect == "All") {
      boundary <- data()$boundaries
    } else {
      boundary <- data()$boundaries %>%
        filter(shapeName == input$statsSelect)
    }
    
    window <- as.owin(st_union(boundary))
    coords <- st_coordinates(stats_schools())
    
    # Remove duplicates
    coords_df <- as.data.frame(coords)
    coords_unique <- coords_df[!duplicated(coords_df), ]
    
    # Filter points within window
    inside <- inside.owin(
      x = coords_unique[,1],
      y = coords_unique[,2],
      w = window
    )
    valid_coords <- coords_unique[inside,]
    
    if(nrow(valid_coords) >= 4) {
      ppp_points <- ppp(
        x = valid_coords[,1],
        y = valid_coords[,2],
        window = window
      )
      
      l_func <- Lest(ppp_points, correction = "border")
      
      # Add envelope if simulations are requested
      if(input$simulations > 0) {
        l_env <- envelope(ppp_points, 
                          fun = Lest,
                          nsim = input$simulations,
                          correction = "border")
        
        plot(l_env,
             main = paste("L Function Analysis", 
                          if(input$statsSelect != "All") paste("-", input$statsSelect) else "",
                          sprintf("\nBased on %d simulations", input$simulations)),
             legend = TRUE,
             ylab = "L(r) - r")
      } else {
        plot(l_func,
             main = paste("L Function Analysis", 
                          if(input$statsSelect != "All") paste("-", input$statsSelect) else ""),
             legend = TRUE,
             ylab = "L(r) - r")
      }
    }
  })
  
  ## LISA Analysis
  weights <- reactive({
    if(input$weight_type == "queen") {
      nb <- poly2nb(district_metrics, queen = TRUE)
      weights <- nb2listw(nb, style = "W")
    } else {
      coords <- st_centroid(district_metrics) %>% st_coordinates()
      nb <- knearneigh(coords, k = input$k_neighbors) %>% knn2nb()
      weights <- nb2listw(nb, style = "W")
    }
    return(weights)
  })
  
  lisa_results <- reactive({
    x <- district_metrics[[input$metric]]
    district_weights <- weights()
    
    if(input$analysis_type == "lisa") {
      local_moran <- localmoran(x, district_weights)
      lag_var <- lag.listw(district_weights, x)
      z_var <- scale(x)
      z_lag <- scale(lag_var)
      
      clusters <- rep("Not Significant", length(z_var))
      sig_indices <- which(local_moran[, "Pr(z != E(Ii))"] < input$significance)
      
      for(i in sig_indices) {
        if(z_var[i] > 0 && z_lag[i] > 0) clusters[i] <- "High-High"
        else if(z_var[i] < 0 && z_lag[i] < 0) clusters[i] <- "Low-Low"
        else if(z_var[i] > 0 && z_lag[i] < 0) clusters[i] <- "High-Low"
        else if(z_var[i] < 0 && z_lag[i] > 0) clusters[i] <- "Low-High"
      }
      
      district_metrics$cluster_type <- clusters
      district_metrics$statistic <- local_moran[, "Ii"]
      district_metrics$p_value <- local_moran[, "Pr(z != E(Ii))"]
      
    } else {
      g_star <- lag.listw(district_weights, x) / scale(x)[,1]
      
      n <- length(x)
      W <- listw2mat(district_weights)
      W2 <- W * W
      S1 <- sum(W2)
      mean_g <- sum(W %*% x) / n
      var_g <- S1 * (n - 1) / (n * n - 1)
      z_scores <- (g_star - mean_g) / sqrt(var_g)
      
      p_values <- 2 * pnorm(-abs(z_scores))
      
      clusters <- rep("Not Significant", length(z_scores))
      clusters[z_scores > 1.96 & p_values < input$significance] <- "Hot Spot (95%)"
      clusters[z_scores > 2.58 & p_values < input$significance] <- "Hot Spot (99%)"
      clusters[z_scores < -1.96 & p_values < input$significance] <- "Cold Spot (95%)"
      clusters[z_scores < -2.58 & p_values < input$significance] <- "Cold Spot (99%)"
      
      district_metrics$cluster_type <- clusters
      district_metrics$statistic <- z_scores
      district_metrics$p_value <- p_values
    }
    
    return(district_metrics)
  })
  
  # LISA Analysis outputs
  output$analysis_map <- renderPlot({
    res <- lisa_results()
    
    colors <- if(input$analysis_type == "lisa") {
      c(
        "High-High" = "#FF0000",
        "Low-Low" = "#0000FF",
        "High-Low" = "#FF69B4",
        "Low-High" = "#87CEEB",
        "Not Significant" = "#CCCCCC"
      )
    } else {
      c(
        "Hot Spot (99%)" = "#FF0000",
        "Hot Spot (95%)" = "#FF6666",
        "Cold Spot (99%)" = "#0000FF",
        "Cold Spot (95%)" = "#6666FF",
        "Not Significant" = "#CCCCCC"
      )
    }
    
    ggplot() +
      geom_sf(data = res, aes(fill = cluster_type)) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      labs(
        title = paste(
          if(input$analysis_type == "lisa") {
            sprintf("LISA Analysis (%s): ",
                    if(input$weight_type == "queen") "Queen Contiguity" else 
                      sprintf("Adaptive Distance Weight (k=%d)", input$k_neighbors))
          } else "Hot Spot Analysis:",
          gsub("_", " ", input$metric)
        ),
        subtitle = paste("Significance Level:", input$significance),
        fill = if(input$analysis_type == "lisa") "LISA Cluster Type" else "G* Cluster Type"
      )
  })
  
  output$cluster_stats <- renderPrint({
    res <- lisa_results()
    
    cat("Cluster Distribution:\n")
    cat("---------------------------------------------------------------------\n")
    cluster_counts <- table(res$cluster_type)
    cluster_percentages <- prop.table(cluster_counts) * 100
    
    for(i in 1:length(cluster_counts)) {
      cat(sprintf("%s:\n  %.1f%% (%d districts)\n",
                  names(cluster_counts)[i],
                  cluster_percentages[i],
                  cluster_counts[i]))
    }
  })
  
  # === GWR Analysis Functions ===
  current_results <- reactive({
    switch(input$education_level,
           "elementary" = elementary_results,
           "junior" = junior_results,
           "senior" = senior_results)
  })
  
  get_values <- reactive({
    results <- current_results()
    model_type <- input$model_type
    
    if(input$coefficient == "r2") {
      return(results[[model_type]]$local_R2)
    }
    
    level_prefix <- switch(input$education_level,
                           "elementary" = "elem",
                           "junior" = "junior",
                           "senior" = "senior")
    
    coef_name <- paste0(level_prefix, "_", input$coefficient)
    values <- results[[model_type]]$model$SDF@data[[coef_name]]
    
    return(as.numeric(values))
  })
  
  # GWR outputs
  output$map <- renderLeaflet({
    results <- current_results()
    values <- get_values()
    
    pal <- if(input$coefficient == "r2") {
      colorNumeric("viridis", domain = range(values, na.rm = TRUE))
    } else {
      colorNumeric("RdBu", domain = range(values, na.rm = TRUE), reverse = TRUE)
    }
    
    leaflet(results$spatial$batas) %>%
      addTiles() %>%
      setView(lng = 106.8456, lat = -6.2088, zoom = 11) %>%
      addPolygons(
        fillColor = ~pal(values),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste(
          "District:", district, "<br>",
          "Subdistrict:", subdistrict, "<br>",
          if(input$coefficient == "r2") "Local R²: " else "Coefficient: ",
          round(values, 4)
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = values,
        title = if(input$coefficient == "r2") "Local R²" else "Coefficient"
      )
  })
  
  output$model_summary <- renderTable({
    results <- current_results()
    
    data.frame(
      `Model Type` = c("Fixed", "Adaptive"),
      Bandwidth = c(
        sprintf("%.2f", results$fixed$bandwidth),
        sprintf("%d neighbors", results$adaptive$bandwidth)
      ),
      AICc = c(
        sprintf("%.2f", results$fixed$diagnostics$AICc),
        sprintf("%.2f", results$adaptive$diagnostics$AICc)
      ),
      `R²` = c(
        sprintf("%.4f", results$fixed$diagnostics$gw.R2),
        sprintf("%.4f", results$adaptive$diagnostics$gw.R2)
      )
    )
  })
  
  output$results_table <- renderDT({
    results <- current_results()
    model_type <- input$model_type
    level_prefix <- switch(input$education_level,
                           "elementary" = "elem",
                           "junior" = "junior",
                           "senior" = "senior")
    
    coefficients <- data.frame(
      District = as.character(results$spatial$batas$district),
      Subdistrict = as.character(results$spatial$batas$subdistrict),
      Education_Ratio = as.numeric(results$data[[paste0(level_prefix, "_ed_ratio")]]),
      Local_R2 = as.numeric(results[[model_type]]$local_R2)
    )
    
    coefficients$Count_Public <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_count_public")]])
    coefficients$Count_Private <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_count_private")]])
    coefficients$Nearest_Public <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_nearest_public")]])
    coefficients$Nearest_Private <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_nearest_private")]])
    coefficients$KDE <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_kde")]])
    
    numeric_cols <- names(coefficients)[!names(coefficients) %in% c("District", "Subdistrict")]
    coefficients[numeric_cols] <- lapply(coefficients[numeric_cols], round, 4)
    
    datatable(
      coefficients,
      options = list(
        scrollX = TRUE,
        pageLength = 10
      )
    )
  })
  
  # Download handler for GWR results
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$education_level, "_gwr_", input$model_type, "_",
             format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      results <- current_results()
      model_type <- input$model_type
      level_prefix <- switch(input$education_level,
                             "elementary" = "elem",
                             "junior" = "junior",
                             "senior" = "senior")
      
      coefficients <- data.frame(
        District = as.character(results$spatial$batas$district),
        Subdistrict = as.character(results$spatial$batas$subdistrict),
        Education_Ratio = as.numeric(results$data[[paste0(level_prefix, "_ed_ratio")]]),
        Local_R2 = as.numeric(results[[model_type]]$local_R2)
      )
      
      coefficients$Count_Public <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_count_public")]])
      coefficients$Count_Private <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_count_private")]])
      coefficients$Nearest_Public <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_nearest_public")]])
      coefficients$Nearest_Private <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_nearest_private")]])
      coefficients$KDE <- as.numeric(results[[model_type]]$model$SDF@data[[paste0(level_prefix, "_kde")]])
      
      numeric_cols <- names(coefficients)[!names(coefficients) %in% c("District", "Subdistrict")]
      coefficients[numeric_cols] <- lapply(coefficients[numeric_cols], round, 4)
      
      write.csv(coefficients, file, row.names = FALSE)
    }
  )
}


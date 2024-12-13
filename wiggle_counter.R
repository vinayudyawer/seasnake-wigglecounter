## Wiggle counter
## custom function to calculate amplitude and wiggles per dive_id

# Parameters
## df: Data frame with dive data containing at minimum depth, time and dive_id
## w: length of window for rolling analysis
## smooth: width of span in data to consider maximum value to identify peak of wiggle
## plot: should the output be plotted (TRUE or FALSE)

wiggle_counter <-
  function(df, w = 10, smooth = 0.1, plot = TRUE) {
    x <- as.numeric(df$X)
    y <- df$Y
    
    argmax <- function(x, y, w=1, ...) {
      require(zoo)
      n <- length(y)
      y.smooth <- loess(y ~ x, ...)$fitted
      y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
      delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
      i.max <- which(delta <= 0) + w
      list(x=x[i.max], i=i.max, y.hat=y.smooth)
    }
    
    peaks <- argmax(x, y, w = w, span = smooth)
    
    wiggle_dat <- tibble(X = as_datetime(df$X), y.hat = peaks$y.hat)
    smooth_dat <- tibble(X = as_datetime(x), Y = loess(y ~ x)$fitted)
    
    residuals <- 
      smooth_dat %>% 
      mutate(residuals = -Y+y,
             residual_smooth = -Y+wiggle_dat$y.hat)
    
    plot_dat <- tibble(X = as_datetime(df$X), y.hat = peaks$y.hat)
    seg_dat <- tibble(x = as_datetime(peaks$x), ymin = -Inf, ymax = peaks$y.hat[peaks$i])
    
    n_wiggles <- length(peaks$i)
    
    if(n_wiggles > 0){
      amplitude_dat <- tibble(mean = mean(abs(residuals$residuals)),
                              max = quantile(abs(residuals$residuals), probs = 0.95)) 
    } else {
      amplitude_dat <- tibble(mean = NA,
                              max = NA) 
    }
    
    plt1 <-
      ggplot(df, aes(x = as_datetime(X), y = Y)) +
      geom_point(col = "grey") +
      geom_line(data = smooth_dat, aes(x = X, y = Y)) +
      geom_line(data = plot_dat, aes(x = X, y = y.hat), col = "steelblue") +
      geom_segment(data = seg_dat, aes(x = x, y = ymin, yend = ymax), 
                   col = "red", lty = 2) +
      geom_point(data = seg_dat, aes(x = x, y = ymax),
                 col = "red", pch = 19, cex = 2.5) +
      scale_x_datetime(date_labels = "%H:%M:%S\n%Y-%m-%d") +
      labs(subtitle = paste("Number of wiggles =", n_wiggles),
           x = NULL, y = "Depth (m)") +
      theme_bw() 
    
    plt2 <-
      ggplot(residuals, aes(x = X, y = residuals)) +
      geom_point(col = "grey", alpha = 0.5) +
      geom_line(aes(x = X, y = residual_smooth), col = "steelblue") +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = c(amplitude_dat$mean, -amplitude_dat$mean), col = "firebrick", lty = 3) +
      geom_hline(yintercept = c(amplitude_dat$max, -amplitude_dat$max), col = "firebrick", lty = 2) +
      scale_x_datetime(date_labels = "%H:%M:%S\n%Y-%m-%d") +
      labs(subtitle = paste("Mean amplitude = ", round(amplitude_dat$mean, digits = 3), "m",
                            "\nMax amplitude = ", round(amplitude_dat$max, digits = 3), "m", sep = ""),
           x = NULL, y = "Residuals") +
      theme_bw() 
    
    plt <- plt1/plt2
    
    out_dat <- 
      df %>% 
      mutate(n_wiggles = n_wiggles,
             mean_amp = amplitude_dat$mean,
             max_amp = amplitude_dat$max) %>% 
      dplyr::select(-c(X, Y))
    
    
    if(plot){
      plt
    } else {
      return(out_dat) 
    }
  }



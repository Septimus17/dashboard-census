load("data.RData")

print.f <- function(f) { 
  cat(paste(deparse(f, width.cutoff=getOption("width")))) 
}

density_apply <- function(dat) {
  dat <- select_if(dat, is.numeric)
  for(i in 1:length(dat)) {
    p <- ggplotly(density_plot(dat, names(dat[i])))
  }
}

smooth_plot <- function(dati, x1, x2) {
  ggplot(dati, aes(dati[[x1]], dati[[x2]])) +
    geom_smooth(method= "auto",
                formula = y ~ splines::bs(x, 3),
                fill = "#afadd3", 
                color = "#605ca8") +
    labs(x = x1,
         y = x2) +
    theme_minimal() +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(limits = c(min(dati[[x2]]), max(dati[[x2]])))
}


smooth_apply <- function(dat) {
  dat <- select_if(dat, is.numeric)
  p <- as.list(NA, dim = length(dat)^2)
  for(i in 1:length(dat)) {
    for (j in 1:length(dat)){
        if (i >= j)
          p[[j+(i-1)*length(dat)]] <- rectGrob(gp = gpar(fill = "grey90"))
        else 
          p[[j+(i-1)*length(dat)]] <- smooth_plot(dat, names(dat[i]), names(dat[j]))
    }
  }
  do.call("grid.arrange", c(p, ncol = length(dat)))
}

per_plot <- function(dati, var) {
  x <- dati[[var]]
  ggplotly(
    ggplot(dati, aes(x)) +
      geom_bar(aes(y = (..count..)/sum(..count..)), 
               fill = "#605ca8",
               color = "#4c4986") +
      labs(x = var,
           y = "rate") +
      theme_minimal() +
      scale_y_continuous(labels = percent,
                         limits = c(0, 1)) +
      theme(text=element_text(family="Segoe UI", size=12),
            axis.text.x = element_text(angle=45, 
                                       hjust=1))
  ) %>% 
    style(text = "rate", 
          hoverinfo = "y+text")
}

density_plot <- function(dati, var) {
  x <- dati[[var]]
  ggplotly(
    ggplot(dati, aes(x)) +
      geom_density(fill = "#605ca8" , 
                   adjust = 5, 
                   color = "#4c4986") +
      labs(x = var) +
      theme_minimal() +
      scale_x_continuous(labels = comma) +
      theme(text=element_text(family="Segoe UI", size=12))
  )
}

dens_plot <- function(var, checkrate, checkimp, checktest) {
  if (checkimp == T && checktest == T)
    dati <- testimp
  else {
    if (checkimp == F && checktest == F)
      dati <- new_adult_data
    else 
      if (checkimp == F && checktest == T)
        dati <- new_adult_test
      else dati <- dataimp
  }
  x <- dati[[var]]
  if(is.integer(x) == T)
    density_plot(dati, var) %>% 
    style(text = var, 
          hoverinfo = "x+text")
    else {
      if (checkrate == T)
        per_plot(dati, var)
      else
        ggplotly(
          ggplot(dati, aes(x)) +
            geom_bar(fill = "#605ca8",
                     color = "#4c4986") +
            labs(x = var) +
            theme_minimal() +
            theme(text=element_text(family="Segoe UI", size=12),
                  axis.text.x = element_text(angle=45, 
                                             hjust=1))
        ) %>% 
        style(text = "count", 
              hoverinfo = "y+text")
    }
}

two_plot <- function(x1, x2, checkimp, checktest) {
  if (checkimp == T && checktest == T)
    dati <- testimp
  else {
    if (checkimp == F && checktest == F)
      dati <- new_adult_data
    else 
      if (checkimp == F && checktest == T)
        dati <- new_adult_test
      else dati <- dataimp
  }
  x <- dati[[x1]]
  y <- dati[[x2]]
  if (x1 != x2){
    if (is.character(x) == F && 
        is.character(y) == F){
      ggplotly(smooth_plot(dati, x1, x2)) %>% 
      style(text = paste(x1,",",x2), 
            hoverinfo = "x + y + text")
    } else {
      if (is.character(x) == T && 
          is.character(y) == F) {
        ggplotly(box_plot(dati, x1, x2)) %>% 
        style(text = x2, 
              hoverinfo = "y + text")
      }
      else {
        if (is.character(x) == F && 
            is.character(y) == T) {
          ggplotly(box_plot(dati, x2, x1)) %>% 
          style(text = x1, 
                hoverinfo = "y + text")
        }
        else { 
          p1 <- ggplotly(bar_dodge_plot(dati, x1, x2)) %>% 
            style(text = "count", 
                  hoverinfo = "y+text")
          p2 <- ggplotly(bar_fill_plot(dati, x1, x2)) %>% 
            style(showlegend = F,
                  text = "cumulative rate", 
                  hoverinfo = "y+text") %>% 
            layout(yaxis = list(title = "rate"))
          subplot(p1,p2,nrows = 2, shareX = T, titleY = T)
        }
      }
    }
  }
}

bar_dodge_plot <- function(dati, x1, x2) {
  ggplot(dati, aes(dati[[x1]], fill = dati[[x2]])) +
    geom_bar(position = "dodge",
             color = "black") +
    labs(x = x1,
         fill = x2) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45, 
                                   hjust=1)) +
    scale_y_continuous(labels = comma)
}

bar_fill_plot <- function(dati, x1, x2) {
  ggplot(dati, aes(dati[[x1]], fill = dati[[x2]])) +
    geom_bar(position = "fill",
             color = "black") +
    labs(x = x1,
         fill = x2) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45, 
                                   hjust=1),
          legend.title = element_blank())
}

bar_apply <- function(dat) {
  dat <- select_if(dat, is.character)
  for(i in 1:length(dat)) {
    print(bar_plot(dat, names(dat[i])))
  }
}

box_plot <- function(dati, x1, x2) {
  mycols <- c("#4c4986", "#565297", "#605ca8", "#6f6cb0", "#7f7cb9", "#8f8cc2", "#9f9dca", 
              "#afadd3", "#bfbddc", "#cfcee4", "#dfdeed", "#efeef6", "#ffffff")
  ggplot(dati, aes(dati[[x1]], dati[[x2]])) +
    geom_boxplot(aes(fill = dati[[x1]])) +
    scale_fill_manual(values = mycols) +
    labs(y = x2,
         x = x1) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none") +
    scale_y_continuous(labels = comma)
}

boxplot_apply <- function(dat) {
  facto <- select_if(dat, is.character)
  inter <- select_if(dat, is.numeric)
  for(i in 1:length(inter)) {
    p <- as.list(NA, dim = length(facto))
    n <- 0
    form <- as.formula(
      paste(names(inter[i]), 
            paste(names(facto), collapse = " + "), 
            sep = " ~ "))
    test <- summary(aov(form, dat))
    Var <- P.value <- Fstatistics <- NULL
    for (j in 1:length(facto)){
      if (test[[1]][["Pr(>F)"]][j] < 0.05) {
        n <- n+1
        Var <- c(Var,names(facto)[j])
        P.value <- c(P.value,test[[1]][["Pr(>F)"]][j])
        Fstatistics <- c(Fstatistics, test[[1]][["F value"]][j])
        p[[n]] <- box_plot(dat, names(facto[j]), names(inter[i]))
      }
    }
    P.value <- round(P.value, digits=3)
    Fstatistics <- round(Fstatistics, digits=3)
    df <- data.frame(Var, Fstatistics, P.value)
    print(df, quote = F, row.names=F)
    do.call("grid.arrange", c(p, ncol = n/2))
    invisible(readline(prompt = "Press [enter] to continue"))
  }
}

encoding <- getOption("shiny.site.encoding", default = "UTF-8")

inclRmd <- function(path, r_env = parent.frame()) {
  paste(
    readLines(path, warn = FALSE, encoding = encoding),
    collapse = '\n'
  ) %>%
    knitr::knit2html(
      text = .,
      fragment.only = TRUE,
      envir = r_env,
      options = "",
      stylesheet = "",
      encoding = encoding
    ) %>%
    gsub("&lt;!--/html_preserve--&gt;","",.) %>%  ## knitr adds this
    gsub("&lt;!--html_preserve--&gt;","",.) %>%   ## knitr adds this
    HTML
}

three_plot <- function(x1, x2, x3, checkimp, checktest) {
  if (checkimp == T && checktest == T)
    dati <- testimp
  else {
    if (checkimp == F && checktest == F)
      dati <- new_adult_data
    else 
      if (checkimp == F && checktest == T)
        dati <- new_adult_test
      else dati <- dataimp
  }
  x <- dati[[x1]]
  y <- dati[[x2]]
  z <- dati[[x3]]
  if (is.integer(y) == T){
    if (is.integer(z) == T)
      z<- cut(z, 4)
    ggplot(dati,aes(x = x, y = y)) +
      geom_smooth(aes(colour = z), se = F) +
      labs(x = x1,
           y = x2,
           colour = x3) +
      theme_minimal() +
      scale_y_continuous(labels = comma)
   }
   else {
     if (is.integer(z) == T)
      ggplot(dati,aes(x = x, y = z)) +
       geom_smooth(aes(colour = y), se = F) +
       labs(x = x1,
            y = x2,
            colour = x3) +
       theme_minimal() +
       scale_y_continuous(labels = comma)
     else
       ggplotly(
          ggplot(dati,aes(x = y, y = x)) +
          geom_boxplot(aes(fill = z)) +
          labs(x = x2,
               y = x1,
               fill = x3) +
          theme_minimal() +
          theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
          scale_y_continuous(labels = comma)) %>%
       layout(boxmode = "group") %>% 
       style(text = x1, 
             hoverinfo = "y + text")
   }
}  

prog <- function() {
  withProgress(message = 'Loading', {
    # Number of times we'll go through the loop
    n <- 100
    for (i in 1:n) {
      # Increment the progress bar, and update the detail text.
      incProgress(amount = 0.1, detail = paste(i,"%"))
      # Pause for 0.01 seconds to simulate a long computation.
      Sys.sleep(0.01)
    }
  })
}


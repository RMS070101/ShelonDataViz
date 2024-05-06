#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(ggplot2)
# Define UI for the Shiny application
ui <- fluidPage(
  titlePanel("Confidence Interval App"),
  tabsetPanel(
    tabPanel("Table of Contents",
             fluidPage(
               h3("Lesson Overview"),
               tags$hr(), # Add a horizontal line for separation
               # Use list to organize content with custom styling
               list(
                 tags$style(HTML("
            .toc-item {
              margin-bottom: 10px;
              color: #333;
            }
            .toc-item:hover {
              background-color: #F0F0F0;
              cursor: pointer;
            }
          ")),
                 tags$div(class = "toc-item", "1. Introduction to Confidence Intervals"),
                 tags$div(class = "toc-item", "(a) Population Distribution"),
                 tags$div(class = "toc-item", "(b) Sample Distribution"),
                 tags$div(class = "toc-item", "(c) Sampling Distribution"),
                 tags$div(class = "toc-item", "2. Confidence Interval Example"),
                 tags$div(class = "toc-item", "3. Effect of Different Confidence Levels on Interval Width"),
                 tags$div(class = "toc-item", "4. Effect of Different Confidence Levels on Interval Accuracy")
               )
             )
    ),
    tabPanel("1. Introduction to Confidence Intervals",
             fluidPage(
               h3("Confidence Intervals"),
               h4("Why are they useful?"),
               p("Confidence intervals provide a range of possible values for a population parameter using sample data. Usually,
the true mean of a population are used within CIs but it can also apply to other estimates like a correlation
coefficient or odds ratios."),
               p("These intervals also require a confidence level, usually in percentages like 95%,
because we need to have a degree of certainty in the interval estimate. If we were to choose to calculate our
interval with a 95% confidence level, we are saying that if we repeat the sampling process multiple times with
the same sample size, we would expect about 95% of the resulting confidence intervals to contain the true
population parameter."),
               br(),
               h4("Definition of Formula"),
               p("To get the actual intervals, we need to look and break down the formula. The overall formula comes in
                 three parts: the sample statistic, the critical value, and the margin of error. "),
               h5("Formula:"),
               p(HTML("(sample statistic)  &plusmn; (critical value)(margin of error)")),
               p("Lets examine each part:"),
               p(HTML("<strong>Sample Statistic:</strong> The sample statistic is used as a point estimate for the population parameter.
              If we are using a CI to estimate the population mean, we would take a sample from the population and
              use its mean. (i.e.$bar{x}$)")),
               p(HTML("<strong>Critical Value:</strong> This critical value is taken from its respective probability distribution and is
              consistent with the chosen level of confidence. If we have a normal population, our critical value is a
              z-value derived from the standard normal distribution that corresponds to our level of confidence. For
              instance, if we want a 95% confidence interval, our critical value, &plusmn; Z, is a z-score for a 95%
              probability of 0.025 on each tail of the standard normal distribution.")),
               p(HTML("<strong>Margin of Error:</strong> This is our measure of uncertainty when estimating a population parameter based on
              our sample. For example, if our population standard deviation is known and we are calculating the true mean,
              the margin of error would be the standard error denoted by &sigma;/&radic;n.")),
               br(),
               h5("When we explain that repeated sampling is used within confidence intervals, we can take a closer look
                 at the relationship with a review on sampling distribution. For now we will focus on estimating the true mean of a population...")
             )
    ),
    tabPanel("(a) Population Distribution",
             sidebarLayout(
               sidebarPanel(
                 h3("Population Distribution:"),
                 p("First, let’s create and look at a normal population. As you adjust it, the population is bell shaped and normal."),
                 sliderInput("num1", label = h4("Size of Population"), value = 10000, min = 1, max = 10000),
                 sliderInput("num2", label = h4("Mean"), value = 50, min = 0, max = 1000),
                 sliderInput("num3", label = h4("Standard Deviation"), value = 5, min = 1, max = 10)
               ),
               mainPanel(plotOutput("populationPlot"),
                         textOutput("populationDetails"))
             )
    ),
    tabPanel("(b) Sample Distribution",
             sidebarLayout(
               sidebarPanel(
                 h3("Sample Distribution:"),
                 p("Using our population distribution from the previous tab, if we take one sample from this population, the graph shows us
the distribution of all the sample values"),
                 sliderInput("samp_size", "Sample Size (n):", value = 1000, min = 10, max = 8000)
               ),
               mainPanel(plotOutput("samplePlot"),
                         textOutput("sampleDetails"))
             )
    ),
    tabPanel("(c) Sampling Distribution",
             sidebarLayout(
               sidebarPanel(
                 h3("Sampling Distribution:"),
                 p("Now lets imagine we repeat taking samples of your selected size and adjust the number of samples you take..."),
                 sliderInput("num_samples", h2("Number of Samples"), value = 1000, min = 10, max = 10000)
               ),
               mainPanel(plotOutput("samplingPlot"),
                         textOutput("samplingDetails"))
             )
    ),
    tabPanel("2. Confidence Interval Example",
             fluidPage(
               h3("Relationship between Repeated Sampling and Confidence Intervals"),
               p("Now we can apply the same concept to confidence intervals when
estimating a population parameters. When we use a single sample statistic to estimate a parameter, such as
the true mean, it results in a single interval. Replicating this process yields multiple intervals. If we were
to choose a 95% confidence level when doing the repeated sampling process, approximately 95% of the
calculated intervals would contain the true mean of the population. "),
               p("Additonally, as our sample size and number of samples taken increases, the sampling distribution of the sample mean (or other sample statistic)
                will be approximately normally distributed. Similarily, when calculating multiple intervals, there will be more accuracy in estimating the population parameter."),
               br(),
               h4("Confidence Interval for Estimating
                  True Mean"),
               p("Let's say I took a sample from a normal population with a mean of 30 and standard deviation of 5. The resulting sample mean would be 30.06."),
               HTML("I can then use this value and plug it into our formula: CI = x-bar &plusmn; Z* &sigma;/&radic;n."),
               p("80% CI: [29.97895, 30.14105]"),
               p("99% CI:[29.89709, 30.22291]"),
               p("A typical intepretation for calculating a confidence interval will look like this: We are 99% confident that the true population mean
falls within [29.89709, 30.22291]."),
               p("Note that the 80% Confidence Interval has a smaller range than the 99% CI. We will focus on why that is through the effects of different confidence levels on width and accuracy next.")
             )),
    tabPanel("3. Effect on Interval Width",
             sidebarLayout(
               sidebarPanel(
                 h3("Effect of Different Confidence Levels on Interval Width:"),
                 p("As we increase the confidence level, we’re essentially expressing greater assurance in our estimation. Consequently, this larger confidence prompts intervals to widen. Picture it as boosting your belief in the reliability
of your findings—the more confident you become, the broader the range of possible values for the true population parameter becomes. Really, confidence levels directly influence the width of intervals. Here is an
example from a sample of the population distribution. One can see that the lower confidence levels have smaller intervals, and as the confidence level increases, so does the interval length."),
                 sliderInput("sample_size", "Sample Size:", min = 10, max = 1000, value = 100),
                 checkboxGroupInput("confidence_levels", "Select Confidence Levels:",
                                    choices = seq(0.8, 0.99, by = 0.02),
                                    selected = .8)
               ),
               mainPanel(plotOutput("interval_plot"))
             )
    ),
    tabPanel("4. Effect on Interval Accuracy",
             sidebarLayout(
               sidebarPanel(
                 h3("Effect of Different Confidence Levels on Interval Accuracy:"),
                 p("If we took repeated random samples from the population's distribution, we would see that the confidence
                 level directly determines how many of those samples intervals would contain the true mean parameter. In this
                 example we are working with the same population distribution that you set before. You can see, when selecting a
                 higher confidence level, more intervals seem to contain the population mean value. When selecting a lower confidence level,
                 you can see less intervals contain the true mean. Please note that the proportion of intervals that contain the true population parameter
                 is not exactly equal to the confidence level. However, we do note that as more samples are taken, the closer the proportions should become."),
                 sliderInput("ci",
                             "Confidence Level:",
                             min = 0,
                             max = .99,
                             value = 0.8, step = 0.01),
                 sliderInput("num_intervals",
                             "Number of Intervals:",
                             min = 5,
                             max = 50,
                             value = 20, step = 1)
               ),
               mainPanel(plotOutput("accuracyPlot"))
             )
    )
  )
)
1:34
# Define server logic for the Shiny application
server <- function(input, output) {
  # Render population distribution plot
  output$populationPlot <- renderPlot({
    set.seed(23)
    pop <- rnorm(input$num1, input$num2, input$num3)
    ggplot(data.frame(population = pop), aes(x = population)) +
      geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "purple3", alpha = 0.75) +
      geom_density(col = "#003865") +
      geom_vline(xintercept = mean(pop), col = "#003865") +
      xlab("Population Values") +
      ggtitle("Distribution of Population")
  })
  output$populationDetails <- renderText({
    paste("Your current population distribution has a mean of", input$num2, "and a standard deviation of", input$num3)
  })
  # Sample distribution plot
  output$samplePlot <- renderPlot({
    set.seed(40)
    pop <- rnorm(input$num1, input$num2, input$num3)
    samp <- sample(pop, input$samp_size)
    ggplot(data.frame(sample = samp), aes(x = sample)) +
      geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "pink3", alpha = 0.75) +
      geom_density(col = "#003865") +
      geom_vline(xintercept = mean(samp), col = "#003865") +
      xlab("Sample Values") +
      ggtitle("Distribution of Sample")
  })
  output$sampleDetails <- renderText({
    paste("You are taking samples of size", input$samp_size, "from a population that has a mean of",input$num2, "and a standard deviation of",input$num3, ". What happens when we take multiple samples of size", input$samp_size ,"and take the mean of each one? We can look at the distribution of all the mean values through the samplng distribution.")
  })
  # Sampling distribution plot
  output$samplingPlot <- renderPlot({
    set.seed(24)
    pop <- rnorm(input$num1, input$num2, input$num3)
    samp_means <- replicate(input$num_samples, mean(sample(pop, input$samp_size)))
    ggplot(data.frame(sample_mean = samp_means), aes(x = sample_mean)) +
      geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "pink3", alpha = 0.75) +
      geom_density(col = "#003865") +
      geom_vline(xintercept = mean(pop), col = "#003865") +
      xlab("Sampling Distribution Values") +
      ggtitle("Sampling Distribution of Means")
  })
  output$samplingDetails <- renderText({
    paste("You are taking", input$num_samples, "samples of size (n)",input$samp_size, ". What we see here is a sampling distribution; the values represent the means taken from", input$num_samples, "samples of
size", input$samp_size, "from your original population.")
  })
  #Function to calculate CI
  calculate_ci <- function(sample, alpha) {
    n <- length(sample)
    mean_val <- mean(sample)
    std_err <- sd(sample) / sqrt(n)
    z_val <- qnorm(1 - (1 - alpha) / 2)
    lower <- mean_val - z_val * std_err
    upper <- mean_val + z_val * std_err
    return(data.frame(lower = lower, upper = upper))
  }
  #Interval width plot
  output$interval_plot <- renderPlot({
    # Pulling population data based on user inputs
    pop <- rnorm(input$num1, input$num2, input$num3)  # Population based on user inputs
    # Sample from the population
    samp <- sample(pop, input$sample_size)
    # Function to calculate CI
    calculate_ci <- function(sample, alpha) {
      n <- length(sample)
      mean_val <- mean(sample)
      std_err <- sd(sample) / sqrt(n)
      z_val <- qnorm(1 - (1 - alpha) / 2)
      lower <- mean_val - z_val * std_err
      upper <- mean_val + z_val * std_err
      return(data.frame(conf_lvl = alpha, interval = c(lower, upper)))
    }
    # Selected confidence levels from user input
    selected_conf_levels <- as.numeric(input$confidence_levels)
    # Calculate confidence intervals for selected confidence levels
    interval_results <- lapply(selected_conf_levels, function(alpha) {
      calculate_ci(samp, alpha)
    })
    # Putting interval results into a single data frame
    interval_data <- do.call(rbind, interval_results)
    # Custom color palette for groups
    custom_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                        "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5")
    # Interval Plot in ggplot
    ggplot(interval_data, aes(x = conf_lvl, y = interval, color = as.factor(conf_lvl), group = conf_lvl)) +
      geom_point() +
      geom_line() +
      labs(
        title = "Confidence Intervals for Selected Confidence Levels",
        x = "Confidence Level",
        y = "Interval"
      ) +
      coord_flip() +
      theme_minimal() +
      scale_color_manual(values = custom_palette) +  # Set custom color palette
      guides(color = guide_legend(title = "Confidence Level"))    # Hide color legend since it represents confidence levels
  })
  # Accuracy plot
  output$accuracyPlot <- renderPlot({
    confidence_level <- input$ci
    population_mean <- input$num2  # Mean entered by user
    population_sd <- input$num3    # Standard Deviation entered by user
    num_intervals <- input$num_intervals  # Number of intervals entered by user
    # Generate multiple random samples and calculate confidence intervals
    sample_results <- lapply(1:num_intervals, function(i) {
      # Draw a random sample from the population distribution
      sample_data <- rnorm(input$num1, population_mean, population_sd)
      # Calculate confidence interval
      ci <- calculate_ci(sample_data, confidence_level)
      # Check if the confidence interval contains the population mean
      contains_mean <- (ci$lower <= population_mean) & (ci$upper >= population_mean)
      # Determine if the interval is correct
      is_correct <- ifelse(contains_mean, "Captured", "Not Captured")
      # Create data frame for sample results
      data.frame(
        interval_id = i,
        lower = ci$lower,
        upper = ci$upper,
        is_correct = is_correct
      )
    })
    # Combine results into a single data frame
    interval_df <- do.call(rbind, sample_results)
    # Determine colors based on correctness
    interval_df$color <- ifelse(interval_df$is_correct == "Captured", "green", "red")
    # Plotting
    ggplot(interval_df, aes(x = interval_id, ymin = lower, ymax = upper, color = color)) +
      geom_linerange() +
      geom_hline(yintercept = population_mean, linetype = "dashed", color = "red") +
      scale_color_identity(guide = "legend", labels = c("Captured", "Not Captured")) +
      labs(
        title = paste("Confidence Intervals for", num_intervals, "Random Samples"),
        x = "Number of Intervals",
        y = "Confidence Interval",
        color = "Intervals that Capture Population Mean"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
}
# Run the Shiny application
shinyApp(ui = ui, server = server)
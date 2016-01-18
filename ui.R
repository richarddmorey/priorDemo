require(shiny)


reactiveSvg <- function (outputId) 
{
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
}

textInput3<-function (inputId, label, value = "",...) 
{
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "text", value = value,...))
}


shinyUI(
  navbarPage(
    title="BF t test prior demo",
    tabPanel(title="Figures",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          HTML('<h4>Prior</h4>'),
          selectInput(inputId="whichPrior", label="Prior Type:", 
            choices = c("point","normal","cauchy","gamma")),
          conditionalPanel(
            condition = "input.whichPrior != 'point'",
            selectInput(inputId="whichRange", label="Prior range:", 
              choices=c("Full","Positive","Negative","Custom")),
            conditionalPanel(
              condition = "input.whichRange == 'Custom'",
                textInput3("range.lower", "Lower:", value = "-Inf", class="input-mini"),
                textInput3("range.upper", "Upper:", value = "Inf", class="input-mini")
            )
          ),
          conditionalPanel(
            condition = "input.whichPrior == 'point'",
            textInput("point.delta", "Point Alternative", value = ".707")
          ),
          conditionalPanel(
            condition = "input.whichPrior == 'normal'",
            textInput("normal.mu", "location", value = "0"),
            textInput("normal.r", "rscale", value = ".707")
          ),
          conditionalPanel(
            condition = "input.whichPrior == 'cauchy'",
            textInput("cauchy.mu", "location", value = "0"),
            textInput("cauchy.r", "rscale", value = ".707")
          ), 
          conditionalPanel(
            condition = "input.whichPrior == 'gamma'",
            textInput("gamma.shape", "shape", value = "3"),
            textInput("gamma.r", "rscale", value = ".707")
          ),
          textOutput(outputId = "priorString"),
          HTML('<hr><h4>Data</h4>'),
          selectInput(inputId="whichData", label="Data type:", 
            choices = c("t statistic","Cohen's d")),
          textInput("data.stat", "Statistic", value = "1"),
          textInput("data.N", "Sample size", value = "25"),
          HTML('<hr><h4>Output</h4>'),
          textOutput(outputId = "statString"),
          textOutput(outputId = "bfString")
        ),
        mainPanel(
          htmlOutput(outputId="svg.grid")
        )
      )
    ),
    tabPanel(title="Instructions",
      withMathJax(),
      HTML('<h4>Bayes factor t test prior explanation</h4>'),
      helpText('The top figure shows the two hypotheses: the null (red) and the chosen alternative (blue). 
        Select from the four different kinds of priors to see how they distribute density across the effect sizes. 
        The normal, Cauchy, and Gamma all have parameters selected so that when the location parameter is 0 and
        the range is unrestricted,
        $$\\begin{eqnarray}
          \\mbox{Pr}(|\\delta|<r) &=& .5
          \\end{eqnarray}
        $$'),
      uiOutput(outputId="prior.out"),
      helpText('The bottom figure shows observed data predicted by the two hypotheses. You can select what 
              kind of data is shown by toggling the "Data type". The Bayes factor is the ratio of these 
              prior predictive distributions.')

    )
  )
)

shinyUI(fluidPage(
        headerPanel("Hypothesis Testing of the Mean"),
        h5("The application below conducts a hypothesis test for a mean given some inputs.
           The graph shows the sampling distribution of the means based upon the proposed mean and standard deviation. The
           blue area represents the 'non-rejection region' whereas the red represents the 'rejection region.'
           If the standard deviation is estimated (comes from a sample) the entire application will automatically update
           to a t distribution assuming you select 'sample' from the inputs below.  The hypotheses are determined by the type
           of test selected.  The next section 'Inferential Statistics' lists 3 sets of scores commonly used
           to compare whether the sample mean is significantly different from the proposed mean.  Finally the conclusion
           gives the result of the test which should correspond to the graph and inferential statistics above."),
        sidebarPanel(
                textInput(inputId="xbar",label=HTML("Sample Mean (x&#772)"),value=2),
                textInput(inputId="mu",label=HTML("Proposed Mean (&mu;<sub>0</sub>)"),value=0),
                textInput(inputId="sigma",label=HTML("Standard Deviation (&#963; or s)"),value=1),
                radioButtons(inputId="sampleorpopstd",label="Where did your standard deviation come from?",
                             choices=c("population" ="popstdev",
                               "sample"="samplestdev"),
                             inline=TRUE),
                textInput(inputId="n",label="Sample Size (n)",value=1),
                numericInput('alpha',HTML('Alpha (&#945;)'),0.05,min=0.01,max=.99,step=.01),
                radioButtons(inputId="testtype",label="What kind of test?",
                                   choices=c("Two-Tailed"="twotailed",
                                     "Left-Tailed"="lefttailed",
                                     "Right-Tailed"="righttailed"))
        ),
        mainPanel(
                plotOutput('normaldist'),
                h4('Hypothesis Testing'),
                tags$table(tags$tr(tags$td(HTML("H<sub>0</sub>: &nbsp;")),tags$td(HTML("&mu; &nbsp;")),tags$td(htmlOutput("nullhypothesis")),tags$td(textOutput("mu1")))),
                tags$table(tags$tr(tags$td(HTML("H<sub>1</sub>: &nbsp;")),tags$td(HTML("&mu; &nbsp;")),tags$td(htmlOutput("althypothesis")),tags$td(textOutput("mu2")))),
                h4('Inferential Statistics'),
                tags$table(border="0",cellspacing="2",cellpadding="10",width="80%",
                           tags$tr(tags$td(" "),tags$td("Raw Score",align="center"),tags$td("Standardized Score",align="center"),tags$td("Percentile Score",align="center")),
                           tags$tr(tags$td(HTML("Threshold Value(s) &nbsp;"),align="right"),tags$td(verbatimTextOutput("id1"),align="center"),tags$td(verbatimTextOutput("id2"),align="center"),tags$td(verbatimTextOutput("id3"),align="center")),
                           tags$tr(tags$td(HTML("Sampling Value &nbsp;"),align="right"),tags$td(verbatimTextOutput("id4"),align="center"),tags$td(verbatimTextOutput("id5"),align="center"),tags$td(verbatimTextOutput("id6"),align="center"))
                ),
                h4('Conclusion'),
                verbatimTextOutput("conclusion")

        )
))

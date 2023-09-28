library(shiny)
library(shinymanager)
library(shinyBS)
library(dplyr)


# Setup basic log in
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer;
window.onmousedown = resetTimer;
window.onclick = resetTimer;
window.onscroll = resetTimer;
window.onkeypress = resetTimer;
function logout() {
window.close();
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);
}
}
idleTimer();"

credentials = data.frame(
    user = "mimi",
    password = "c2NyeXB0ABEAAAAIAAAAARMnMC1QW3EGlj4+ECmYEM2MOkOQWEXMOE/Q/ozcmV7Pn9yNGdl61pLDfoyZv4rIMpvixCiJiJG315UQ98D6LNyUCzlhdGyXhR8LaqwBm8qG",
    is_hashed_password = TRUE,
    permissions = "standard",
    stringsAsFactors = FALSE
)


# App user interface defining the form where new values are entered
ui <-
secure_app(head_auth = tags$script(inactivity),
    fluidPage(
        titlePanel("Imminent myocardial infarction prediction model"),

        # Layout: left sidebar panel with input, main (right) panel with output
        
        # Input sidebar
        sidebarLayout(

            # Sidebar panel with required input
            sidebarPanel(
                tags$style(HTML(".tooltip > .tooltip-inner {background-color: red;}")),
                           
                sliderInput("age", "Age (years)", min = 18, max = 99, value = 63, step = 1, ticks = FALSE),
                selectInput("sex", "Sex", choices = list("Female", "Male"), selected = "Male", multiple = FALSE),
                selectInput("diabetes", tags$span("Diabetes", bsButton("diab_info", label = "", icon = icon("info"), style = "info", size = "extra-small")), choices = list("No", "Yes"), selected = "No", multiple = FALSE),
                selectInput("edu", tags$span("Education", bsButton("edu_info", label = "", icon = icon("info"), style = "info", size = "extra-small")), choices = list("Low", "Middle", "High"), selected = "Middle", multiple = FALSE),
                sliderInput("height", "Height (centimeters)", min = 120, max = 220, value = 171, step = 1, ticks = FALSE),
                sliderInput("waist", "Waist (centimeters)", min = 50, max = 250, value = 96, step = 1, ticks = FALSE),
                sliderInput("ldl", "LDL-C (mmol/L)", min = 0.1, max = 25.0, value = 3.8, step = 0.1, ticks = FALSE),
                sliderInput("hdl", "HDL-C (mmol/L)", min = 0.1, max = 25.0, value = 1.3, step = 0.1, ticks = FALSE),
                selectInput("smok", "Smoking status", choices = list("Never", "Former", "Current"), selected = "Former", multiple = FALSE),

                
            # Section with with optional/advanced settings.
            hr(style = "border-top: 1px solid #000000;"),

            selectInput("custom", "Advanced settings", choices = list("Do not show", "MIMI/UKBB values", "Custom values"), selected = "Do not show", multiple = FALSE),

            conditionalPanel(
                condition = "input.custom == 'MIMI/UKBB values'",
                selectInput("s0", tags$span("S0[t=182]", bsButton("s0_info", label = "", icon = icon("info"), style = "info", size = "extra-small")), choices = list("MIMI", "UKBB"), selected = "MIMI", multiple = FALSE),
                selectInput("m", "Population means", choices = list("MIMI", "UKBB"), selected = "MIMI", multiple = FALSE)

            ),
            conditionalPanel(
                condition = "input.custom == 'Custom values'",
                sliderInput("custom_s0", "S0[t=182]", min = 0.7, max = 1, value = 0.999, step = 0.001, ticks = FALSE),
                sliderInput("m_age", "Mean age (years)", min = 18, max = 99, value = 63, step = 1, ticks = FALSE),
                sliderInput("m_sex", "Proportion males", min = 0, max = 1, value = 0.64, step = 0.01, ticks = FALSE),
                sliderInput("m_diabetes", "Proportion with diabetes", min = 0, max = 1, value = 0.1, step = 0.01, ticks = FALSE),
                sliderInput("m_edu_mid", "Proportion with middle education", min = 0, max = 1, value = 0.38, step = 0.01, ticks = FALSE),
                sliderInput("m_edu_high", "Proportion with high education", min = 0, max = 1, value = 0.22, step = 0.01, ticks = FALSE),
                sliderInput("m_height", "Mean height (centimeters)", min = 140, max = 195, value = 171, step = 1, ticks = FALSE),
                sliderInput("m_waist", "Mean waist (centimeters)", min = 50, max = 250, value = 96, step = 1, ticks = FALSE),
                sliderInput("m_ldl", "Mean LDL-C (mmol/L)", min = 0.1, max = 25.0, value = 3.8, step = 0.1, ticks = FALSE),
                sliderInput("m_hdl", "Mean HDL-C (mmol/L)", min = 0.1, max = 25.0, value = 1.3, step = 0.1, ticks = FALSE),
                sliderInput("m_former_smok", "Proportion former smokers", min = 0, max = 1, value = 0.37, step = 0.01, ticks = FALSE),
                sliderInput("m_current_smok", "Proportion current smokers", min = 0, max = 1, value = 0.26, step = 0.01, ticks = FALSE)
            ),

            bsPopover(
                id = "diab_info", placement = "right", trigger = "hover", options = list(container = "body"),
                title = "Definition", content = "Any form of diabetes mellitus. However, it should be noted that in the training of the model, almost all diabetes cases were of type II."
            ),
            bsPopover(
                id = "edu_info", placement = "right", trigger = "hover", options = list(container = "body"),
                title = "Definition", content = "Low education: no education up to primary school.<br />Middle education: e.g. secondary school.<br />High education: e.g. university."
            ),
            bsPopover(
                id = "s0_info", placement = "right", trigger = "hover", options = list(container = "body"),
                title = "Definition", content = "This is the risk of imminent myocardial infarction of an average individual (all covariates set to the mean) at day 182 after baseline."
            ),
            
            bsTooltip(id = "custom", title = "Warning! These advanced settings on population level are not recommended for most users.", placement = "top", trigger = "hover")
            
        ),
        
        # Output area
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Results", 
                    h3("The risk of a heart attack within 6 months is estimated to be:"), 
                    br(), 
                    htmlOutput("pr_imi"),
                    hr(),
                    "A non-technical summary can be found under the ", actionLink("link_to_summary", "Summary tab")
                ),
                tabPanel(
                    "Summary", 
                    h3("About the prediction model:"), 
                    tags$p("Heart attack is a leading cause of death globally and is difficult to predict. In patients free from prior cardiovascular disease, we aimed to identify characteristics of an imminent first heart attack and design a prediction model."),
                    tags$p("This risk equation gives an estimate of how many people with similar answers/measurements will have a heart attack within the next six months. However, it does not predict the future for any one individual; it cannot identify who will be free from disease and who will have a heart attack. As in any statistical analysis, the associations of variables with the risk of heart attack have a degree of uncertainty, so does the point estimate of this risk equation. Further, this prediction model was developed in a population of mostly middle-aged European individuals and how it performs in other populations is unknown."),
                    "If you have any questions or concerns about the results, please contact ", tags$a(href="mailto:johan.sundstrom@uu.se", "Johan Sundström"),
                    br(),
                    tags$p("The full paper (PMID: [to be added]) can be accessed here [to be added].")
                ),
                id = "tabs"
            )
        )
    )
)
)#login

# Server logic for making the output the prediction results
server <- function(input, output, session) {
    
    auth <- secure_server(check_credentials = check_credentials(credentials))
    
    
    # Make sure that the proportions of a factor variable do to go above 100%
    ## Education
    observeEvent(input$m_edu_mid, {
        remaining = 1 - input$m_edu_mid
        if (input$m_edu_high > remaining) {
            updateSliderInput(inputId = "m_edu_high", value = remaining)
        }
    })
    observeEvent(input$m_edu_high, {
        remaining = 1 - input$m_edu_high
        if (input$m_edu_mid > remaining) {
            updateSliderInput(inputId = "m_edu_mid", value = remaining)
        }
    })
    
    ## Smoking
    observeEvent(input$m_former_smok, {
        remaining = 1 - input$m_former_smok
        if (input$m_current_smok > remaining) {
            updateSliderInput(inputId = "m_current_smok", value = remaining)
        }
    })
    observeEvent(input$m_current_smok, {
        remaining = 1 - input$m_current_smok
        if (input$m_former_smok > remaining) {
            updateSliderInput(inputId = "m_former_smok", value = remaining)
        }
    })
    
    # Goto summary tab
    observeEvent(input$link_to_summary, {
        updateTabsetPanel(session, "tabs", "Summary")
    })
    
    
    # data.frame with model S0, pop means, and coefficients from Erik's model
    # (placed together with app.R)
    dms = readRDS("dms.rds")
    
    # Process form data
    get_form_data <- reactive({
        # Build a named vector with form input
        indata = c(
            age = as.double(input$age),
            sex = as.double(ifelse(input$sex == "Male", 1, 0)),
            diabetes = as.double(ifelse(input$diabetes == "Yes", 1, 0)),
            edu_mid = as.double(ifelse(input$edu == "Middle", 1, 0)),
            edu_high = as.double(ifelse(input$edu == "High", 1, 0)),
            height_m = as.double(input$height)/100,
            waist_cm = as.double(input$waist),
            ldl_chol_mmoll = as.double(input$ldl),
            hdl_chol_mmoll = as.double(input$hdl),
            former_smk = as.double(ifelse(input$smok == "Former", 1, 0)),
            current_smk = as.double(ifelse(input$smok == "Current", 1, 0)),
            use_s0_ukbb = 0, # MIMI stats used as default
            use_m_ukbb = 0   # MIMI stats used as default
        )
        
        # Advanced options selected (MIMI/UKBB):
        if (input$custom == "MIMI/UKBB values") {
            indata["use_s0_ukbb"] = as.double(ifelse(input$s0 == "UKBB", 1, 0))
            indata["use_m_ukbb"] = as.double(ifelse(input$m == "UKBB", 1, 0))
        }
        
        # Advanced options selected (custom values):
        if (input$custom == "Custom values") {
            indata["custom_s0"] = as.double(input$custom_s0)
            indata["m_age"] = as.double(input$m_age)
            indata["m_sex"] = as.double(input$m_sex)
            indata["m_diabetes"] = as.double(input$m_diabetes)
            indata["m_edu_mid"] = as.double(input$m_edu_mid)
            indata["m_edu_high"] = as.double(input$m_edu_high)
            indata["m_height_m"] = as.double(input$m_height)/100
            indata["m_waist_cm"] = as.double(input$m_waist)
            indata["m_ldl_chol_mmoll"] = as.double(input$m_ldl)
            indata["m_hdl_chol_mmoll"] = as.double(input$m_hdl)
            indata["m_former_smk"] = as.double(input$m_former_smok)
            indata["m_current_smk"] = as.double(input$m_current_smok)
        }
        
        indata
    })
    
    # Predict
    predict_imi <- reactive({
        # Extract and process input
        form_data = get_form_data()
        
        X = matrix(form_data[dms$var], nrow = 1)
        
        s0 = dms$s0_mimi[1]
        if (form_data["use_s0_ukbb"] == 1) s0 = dms$s0_ukbb[1]
        if ("custom_s0" %in% names(form_data)) s0 = form_data["custom_s0"]
        m = dms$m_mimi
        if (form_data["use_m_ukbb"] == 1) m = dms$m_ukbb
        if ("custom_s0" %in% names(form_data)) m = form_data[paste0("m_", dms$var)]
        
        # Calculate probability of IMI
        lin_pred = (X - m) %*% dms$coef
        pr = 1 - s0^exp(lin_pred)
        
        # HTML output string
        paste0(
            dplyr::case_when(pr > 0.9 ~ "&gt;90", pr < 0.01 ~ "&lt;1", TRUE ~ format(round(100*pr, 2), nsmall = 2)), "%<hr />",
            "This is calculated from ",
            "100*(1 - S<sub>0</sub>[t=182]<sup>exp(lp)</sup>) where<br />S<sub>0</sub>[t=182] = ", s0, " and <br />",
            "lp = (X - means) * beta = ", paste0(paste0("(<strong>", c(X), "</strong>-", m, ")*", dms$coef), collapse = "+"), "<br />"
        )
    })
    
    output$pr_imi <- renderText({predict_imi()})
}

# Run the application 
shinyApp(ui = ui, server = server)

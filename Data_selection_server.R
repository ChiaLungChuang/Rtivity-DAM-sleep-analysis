########################## VARIABLES #################################

#Folder directory
Directory <- reactiveVal()

#Files imported by the folder selection
ImportedFiles <- reactiveVal()

#Choices for Data selection
fileChoice <- reactiveVal()

#Channels variables
Channels <- reactiveValues(df = NULL)

#Data imported as behavr table
damData <- reactiveValues(dt = NULL)

#Conditions variable
Conditions <- reactiveValues(df = NULL)

#Settings of data analysis
settingsTable <- reactiveVal()

#Thresholds of data
thresholds <- reactiveValues(l = c())

#Data extracted from monitor files
myData<- reactiveVal()

#Date and Time from monitor files
# Inside the dateTime reactive expression
# In dateTime reactive
dateTime <- reactive({
  tryCatch({
    # Extract the date components from myData()
    day_col <- myData()[, 2]
    month_col <- myData()[, 3]
    year_col <- myData()[, 4]
    time_col <- myData()[, 5]
    
    # Debugging: Check types
    print(paste("day_col type:", class(day_col)))
    print(paste("month_col type:", class(month_col)))
    print(paste("year_col type:", class(year_col)))
    print(paste("time_col type:", class(time_col)))
    
    # Ensure no functions are present
    if (any(sapply(time_col, is.function))) {
      stop("time_col contains functions")
    }
    
    # Combine the date and time components
    date_str <- paste(day_col, month_col, year_col, sep = "-")
    datetime_str <- paste(date_str, time_col, sep = " ")
    
    # For debugging
    print(paste("DateTime string:", head(datetime_str)))  # Print the first few entries
    
    # Parse the datetime strings
    parsed_dateTime <- lubridate::dmy_hms(datetime_str)
    
    if (any(is.na(parsed_dateTime))) {
      warning("Some date/time values could not be parsed")
    }
    return(parsed_dateTime)
  }, error = function(e) {
    message("Error in dateTime parsing: ", e$message)
    return(NULL)
  })
})



#Data presented in datatable
tableData <- reactiveValues(df = NULL)

#Selected dates
dates <- reactiveValues (startDateTime = NULL, stopDateTime = NULL)


##### Zeitgeber variables
#Light onset time time
# Rename ZT to zeitgeberTimes to avoid conflicts
zeitgeberTimes <- reactiveVal()

#Files added and zeitgeber table
zt_table <- reactiveVal()

#Change minimum time to zeitgeber time
MinTime <- reactiveVal(FALSE)

#LD cycle hours
l_hours <- reactiveVal()

#Light period per LD cycle
l_period <- reactiveVal()
###########################  FUNCTIONS #################################
options(timeFormat = "%H:%M:%S")

observe({
  log_info(paste("Start_date input:", input$Start_date))
  log_info(paste("start_time input:", input$start_time))
  log_info(paste("Finish_date input:", input$Finish_date))
  log_info(paste("finish_time input:", input$finish_time))
})

# helper function for making channel panel layout
shinyInput <- function(FUN, len, id, Val,...) {
  if (is.numeric(Val) ) 
    inputs <- numeric(len)
  else
    inputs <- character(len)
  
  for (i in seq_len(len)) {
    if (length  (Val) > 1)
      v = Val[i]
    else
      v = Val
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, value = v, ...)) 
  } 
  inputs 
} 

# obtain the values of inputs of the checkboxes
shinyValue <- function(id, len) {
  unlist(lapply(seq_len(len), function(i) {
    value = input[[paste0(id, i)]]
    if (is.null(value)) TRUE else value
  }))
}

# Get order number of selected file
getFileNr <- function() {
  log_info(paste("selectedFile:", input$selectedFile))
  log_info(paste("fileChoice:", paste(fileChoice(), collapse = ", ")))
  
  n <- match(input$selectedFile, fileChoice())
  
  log_info(paste("Matched file number:", n))
  
  return(as.numeric(n))
}

#Update channels pannel
updateChannels <- function(){
  
  n <- getFileNr()
  
  #Set all values to non selected
  for (j in 1:4){
    for (i in 1:9){
      col <- paste0("cbox",i)
      shinyjs::enable(paste0(col,j))
      updatePrettyCheckbox(session, paste0(col,j), value = FALSE)
    }
  }
  
  req(Conditions$df)
  
  # If there is any condition selected and the file selected in current analysis
  # has channels  already selected, prevent them to be enabled twice
  if(nrow(Conditions$df)>0){
    for (k in 1:nrow(Conditions$df)){
      if (Conditions$df[k,1] == ImportedFiles()[n]){
        
        ch <- Conditions$df[k,4]
        row <- ceiling(ch/8)
        col <- ch-8*(row-1)
        shinyjs::disable(paste0(paste0("cbox",col),row))
        shinyjs::disable(paste0(paste0("cbox",9),row))}}
  }
}

#Verify dates
verifyDates <- function(){
  
  Files <- unique(Conditions$df[,1]) #Get unique file names
  
  #Get  start and stop dates for each file
  for (i in 1:length(Files)){
    
    indexes <- which(Conditions$df[,1]==unique(Conditions$df[,1])[1])
    start_date <- unique(Conditions$df[indexes,2])
    stop_date <- unique(Conditions$df[indexes,3])
    
    #If the stop date is higher than start date return FALSE
    if (stop_date <= start_date){
      showNotification(paste("Start date is higher than stop date in file",Files[i]),type = "error",duration = 5)
      return(FALSE)
    }
  }
  return(TRUE)
}

#Update dates and zeitgeber time
updateDates <- function(){
  tryCatch({
    log_info("Starting updateDates function")
    
    n <- getFileNr()
    log_info(paste("Updating dates for file number:", n))
    
    # Safely get start and stop datetimes
    startDateTime <- tryCatch_wrapper(dates$startDateTime[n], "Error getting start datetime")
    stopDateTime <- tryCatch_wrapper(dates$stopDateTime[n], "Error getting stop datetime")
    
    if(is.null(startDateTime) || is.null(stopDateTime)) {
      log_error("Start or stop datetime is NULL")
      return()
    }
    
    # Update date inputs
    updateDateInput(session, "Start_date", value = as.Date(startDateTime))
    updateDateInput(session, "Finish_date", value = as.Date(stopDateTime))
    
    # Update time inputs using updateAirDateInput (correct function for airDatepickerInput)
    tryCatch({
      updateAirDateInput(session, "start_time", value = startDateTime)
    }, error = function(e) {
      log_warning(paste("Could not update start_time input:", e$message))
    })
    tryCatch({
      updateAirDateInput(session, "finish_time", value = stopDateTime)
    }, error = function(e) {
      log_warning(paste("Could not update finish_time input:", e$message))
    })

    log_info(paste("Updated start datetime:", format(startDateTime, "%Y-%m-%d %H:%M:%S")))
    log_info(paste("Updated stop datetime:", format(stopDateTime, "%Y-%m-%d %H:%M:%S")))

    # Update shown zeitgeber time using updateAirDateInput
    ZTime <- tryCatch_wrapper(strptime(zeitgeberTimes()[n], '%T'), "Error parsing zeitgeber time")
    if(!is.null(ZTime)) {
      tryCatch({
        updateAirDateInput(session, "zeitgeberTime", value = ZTime)
      }, error = function(e) {
        log_warning(paste("Could not update zeitgeberTime input:", e$message))
      })
    } else {
      log_warning("Could not update zeitgeber time")
    }
    
    log_info("updateDates function completed successfully")
  }, error = function(e) {
    log_error(paste("Error in updateDates:", e$message))
    showNotification(paste("Error updating dates:", e$message), type = "error")
  })
}
tryCatch_wrapper <- function(expr, error_message = "An error occurred") {
  tryCatch(
    expr,
    error = function(e) {
      log_error(paste(error_message, ":", e$message))
      showNotification(paste(error_message, ":", e$message), type = "error")
      return(NULL)
    },
    warning = function(w) {
      log_warning(paste("Warning:", w$message))
      showNotification(paste("Warning:", w$message), type = "warning")
      return(NULL)
    }
  )
}

# Make sure to also include these logging functions if not already defined
log_info <- function(message) {
  cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - INFO: ", message, "\n"), file = "app_log.txt", append = TRUE)
}

log_error <- function(message) {
  cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ERROR: ", message, "\n"), file = "app_log.txt", append = TRUE)
}

log_warning <- function(message) {
  cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - WARNING: ", message, "\n"), file = "app_log.txt", append = TRUE)
}

validate_time <- function(time_str) {
  tryCatch({
    if (is.character(time_str)) {
      # Try parsing as 24-hour time
      parsed_time <- strptime(time_str, format = "%H:%M:%S")
      if (is.na(parsed_time)) {
        # Try parsing as 12-hour time with AM/PM
        parsed_time <- strptime(time_str, format = "%I:%M%p")
      }
    } else if (inherits(time_str, "POSIXt")) {
      parsed_time <- time_str
    } else {
      return(NULL)
    }
    if (is.na(parsed_time)) return(NULL)
    format(parsed_time, "%H:%M:%S")
  }, error = function(e) {
    message(paste("Error validating time:", e$message))
    return(NULL)
  })
}


#############################################################################
############################## SESSION ######################################

#Disable buttons previously to have directory and files
shinyjs::disable("importConditions")
shinyjs::disable("saveMetadata")


###################### GET DATA AND UPDATE FIELDS ###########################

### Get files and create main variables
observeEvent(input$files, {
  tryCatch_wrapper({
    validate(need(nrow(input$files) > 0, "No files selected"))
    
    base_path <- tools::file_path_as_absolute(input$files$datapath[1])
    print(paste("Base path:", base_path))  # Debugging print
    
    split_path <- str_split(base_path, '/')[[1]]
    path <- paste(split_path[-length(split_path)], collapse = "/")
    print(paste("Extracted path:", path))  # Debugging print
    
    validate(need(length(path) > 0, "Invalid path"))
    
    Directory(path) # Attribute files directory to directory variable
    
    # Rename files
    for(i in 1:nrow(input$files)) {
      file.rename(input$files$datapath[i], file.path(path, input$files$name[i]))
    }
    
    #### If the files were imported, reset variables
    ##### RESET variables 
    
    # Reset variables
    Conditions$df <- NULL
    damData$dt <- NULL
    tableData$df <- NULL
    myData(NULL)
    zeitgeberTimes(NULL)  # Ensure zeitgeberTimes is reset
    zt_table(NULL)
    deadTable(NULL)
    
    # Import files  
    withProgress(message = 'Importing files', value = 0, {
      files <- list.files(path = Directory(), pattern = '.txt')
      validate(need(length(files) > 0, "Select a folder with DAM text files"))
      
      import <- c()
      thresholds$l <- c()
      table <- data.frame()
      
      ErrorColumnNumberFile <- c()
      MissingDataFiles <- c()
      
      for (i in 1:length(files)) {
        filename <- files[i]
        datapath <- file.path(Directory(), filename)
        
        data <- tryCatch_wrapper({
          read.table(datapath)
        })
        
        if (is.null(data)) {
          showNotification(paste(filename, "is not a monitor file"), type = "error", duration = 5)
          next
        }
        
        if (ncol(data) == 44) {
          if (is.null(import)) {
            thresholds$l <- c(thresholds$l, nrow(data))
          } else {
            thresholds$l <- c(thresholds$l, tail(thresholds$l, 1) + nrow(data))
          }
          table <- rbind(table, data)
          import <- c(import, filename)
        } else {
          ErrorColumnNumberFile <- c(ErrorColumnNumberFile, filename)
          showNotification(paste(filename, "has a wrong column number"), type = "error", duration = 5)
        }
        
        # Your original data integrity checks
        colTypes <- sapply(data, class)
        if (any(colTypes == "numeric")) {
          showNotification(paste(filename, "has decimal values"), type = "error", duration = 5)
          next
        }
        
        checkColType <- rep("integer", 44)
        checkColType[3] <- "character"
        checkColType[5] <- "character"
        checkColType[10] <- "character"
        
        for (j in 1:12) {
          if (colTypes[j] != checkColType[j]) {
            showNotification(paste("Column", j, "of", filename, "should be", checkColType[j]), type = "error", duration = 5)
            next
          }
        }
        
        # Check for missing data
        activityData <- data[, 13:44]
        for(j in 1:ncol(activityData)) {
          activityData[, j] <- as.numeric(activityData[, j])
        }
        if (any(is.na(activityData))) {
          MissingDataFiles <- c(MissingDataFiles, filename)
          showNotification(paste(filename, "has missing data"), type = "error", duration = 5)
        }
        
        incProgress(1/length(files))
      }
      
      validate(
        need(length(MissingDataFiles) == 0, ""),
        need(length(ErrorColumnNumberFile) == 0, ""),
        need(!is.null(import), "Select a folder with DAM text files")
      )
      
      ImportedFiles(import)
      myData(table)
      
      # Initialize zeitgeberTimes after importing files
      if (length(ImportedFiles()) > 0) {
        initial_times <- rep("08:00:00", length(ImportedFiles()))
        zeitgeberTimes(initial_times)
      }
      
      # Update file choices
      fileChoice(paste(seq_along(ImportedFiles()), "-", tools::file_path_sans_ext(ImportedFiles())))
      updateSelectInput(session, "selectedFile", choices = fileChoice())
    })
  })
})




### Update dates and channels when selected file changes
observeEvent(input$selectedFile,{
  
  req(!is.null(Directory()))
  tryCatch({
    log_info("Calling updateDates after file selection")
    updateDates()
  }, error = function(e) {
    log_error(paste("Error calling updateDates:", e$message))
    showNotification("Error updating dates. Please check the log for details.", type = "error")
  })

  
  #Update dates
  updateDates()
  
  # Update Channels panel
  updateChannels()
})

### Update zeitgeber if field changes
observeEvent(input$zeitgeberTime,{

  # If there are any files, update the zeitgeber time variable according to users change

  req(length(ImportedFiles())>0)

  zts <- zeitgeberTimes()
  new_time <- paste(strftime(input$zeitgeberTime, format="%H:%M:%S"))

  # Check if "Apply to all files" checkbox is checked
  if (!is.null(input$applyZTToAll) && input$applyZTToAll == TRUE) {
    # Apply the same zeitgeber time to all files
    for (i in 1:length(zts)) {
      zts[i] <- new_time
    }
  } else {
    # Only update the currently selected file
    n <- getFileNr() #Get file order
    zts[n] <- new_time
  }
  zeitgeberTimes(zts)
})

### Zeitgeber time data
#If there are any conditions selected create the table of the zeitgeber time to show to user
observe({
  
  req(Conditions$df)

  if(nrow(Conditions$df)>0){
    
    Files <- unique(Conditions$df[,1])
    indexes <- match(Files, Conditions$df[,1])
    Start_Dates <- as.character(Conditions$df[indexes,2])
    Finish_Dates <- as.character(Conditions$df[indexes,3])
    
    zts <- c() #Zeitgebers numbers
    
    for (i in 1:length(Files)){
      n <- match(Files[i],ImportedFiles())
      zts <- c(zts,n)
    }
    
    Light_onset <- zeitgeberTimes()[zts]
    
    df <- data.frame(Files, Light_onset,Start_Dates, Finish_Dates)
    
    zt_table(df)
    
    output$FilesAdded <- renderTable(zt_table())
  }
  else{
    output$FilesAdded <- renderTable(NULL)
  }
})


### Light hours and period variables update
observeEvent(input$l_period,{
  
  updateSliderInput(session,'l_hours',max=input$l_period)
  l_period(input$l_period)
  
  req(damData$dt)
  
  #Activity and bouts variables for statistics plots
  damData$dt[, experimentDay := floor(periodT/(l_period()*3600))] #Experiment day
  damData$dt[, day_night := ((periodT-experimentDay*l_period()*3600)/3600)<l_hours()] #Day or night time
  damData$dt[, 'SleepBoxPlot_time' := floor(damData$dt[,'t']/(input$SleepGroupBoxTime * l_period()*3600))]
  damData$dt[, 'ActivityBoxPlot_time' := floor(damData$dt[,'t']/(input$ActivityGroupBoxTime * l_period()*3600))]
  
  
})
observeEvent(input$l_hours,{
  
  l_hours(input$l_hours)
  
  req(damData$dt)
  
  #Activity and bouts variables for statistics plots
  damData$dt[, experimentDay := floor(periodT/(l_period()*3600))] #Experiment day
  damData$dt[, day_night := ((periodT-experimentDay*l_period()*3600)/3600)<l_hours()] #Day or night time
  damData$dt[, 'SleepBoxPlot_time' := floor(damData$dt[,'t']/(input$SleepGroupBoxTime * l_period()*3600))]
  damData$dt[, 'ActivityBoxPlot_time' := floor(damData$dt[,'t']/(input$ActivityGroupBoxTime * l_period()*3600))]
  
})

#############################################################################
################## DATE AND TIME FIELDS AND VARIABLES ############

### Check for changes in date and time
changeDate <-reactive({
  list(input$Start_date,input$start_time,input$Finish_date,input$finish_time)
})

### Update date and time variable "dates" according to users changes
# Time validation function
validate_time <- function(time_str) {
  tryCatch({
    if (is.character(time_str)) {
      parsed_time <- strptime(time_str, format = "%H:%M:%S")
    } else if (inherits(time_str, "POSIXt")) {
      parsed_time <- time_str
    } else {
      return(NULL)
    }
    if (is.na(parsed_time)) return(NULL)
    format(parsed_time, "%H:%M:%S")
  }, error = function(e) {
    log_error(paste("Error validating time:", e$message))
    return(NULL)
  })
}

# changeDate observer
# Improved changeDate observer
observeEvent(changeDate(), {
  tryCatch({
    log_info("Starting changeDate observer")
    
    req(!is.null(ImportedFiles()))
    req(input$selectedFile)
    
    n <- tryCatch_wrapper(getFileNr(), "Error getting file number")
    if (is.null(n)) return()
    
    log_info(paste("Processing dates for file number:", n))
    
    # Use the date and time inputs directly (with error handling for R 4.5+ compatibility)
    start_date <- input$Start_date
    finish_date <- input$Finish_date

    # Safely format time inputs
    start_time <- tryCatch({
      format(input$start_time, "%H:%M:%S")
    }, error = function(e) {
      # Fallback: try to extract time as string
      if(inherits(input$start_time, "POSIXt")) {
        strftime(input$start_time, "%H:%M:%S")
      } else {
        "00:00:00"
      }
    })

    finish_time <- tryCatch({
      format(input$finish_time, "%H:%M:%S")
    }, error = function(e) {
      if(inherits(input$finish_time, "POSIXt")) {
        strftime(input$finish_time, "%H:%M:%S")
      } else {
        "23:59:59"
      }
    })

    # Combine date and time
    start <- as.POSIXct(paste(start_date, start_time), format = "%Y-%m-%d %H:%M:%S")
    finish <- as.POSIXct(paste(finish_date, finish_time), format = "%Y-%m-%d %H:%M:%S")
    
    if (is.na(start) || is.na(finish)) {
      log_error("Failed to parse start or finish datetime")
      showNotification("Error parsing date/time. Please check the input format.", type = "error")
      return()
    }
    
    log_info(paste("Parsed start date:", start, "Parsed finish date:", finish))
    
    # Get time data
    req(myData())
    time <- myData()[,5]
    
    # Calculate min and max dates
    req(dateTime())
    req(thresholds$l)
    if (n == 1) {
      minDate <- dateTime()[1]
      minTime <- as.POSIXct(time[1], format = "%H:%M:%S")
    } else {
      minDate <- dateTime()[thresholds$l[n-1]+1]
      minTime <- as.POSIXct(time[thresholds$l[n-1]+1], format = "%H:%M:%S")
    }
    maxDate <- dateTime()[thresholds$l[n]]
    maxTime <- as.POSIXct(time[thresholds$l[n]], format = "%H:%M:%S")
    
    log_info(paste("Min date:", minDate, "Max date:", maxDate))
    
    # Validate and adjust dates if necessary
    if (start < minDate) {
      start <- minDate
      updateDateInput(session, "Start_date", value = as.Date(minDate))
      tryCatch({
        updateAirDateInput(session, "start_time", value = minTime)
      }, error = function(e) { })
      showNotification("Start date adjusted to minimum allowed date", type = "warning", duration = 5)
    }
    if (finish > maxDate) {
      finish <- maxDate
      updateDateInput(session, "Finish_date", value = as.Date(maxDate))
      tryCatch({
        updateAirDateInput(session, "finish_time", value = maxTime)
      }, error = function(e) { })
      showNotification("Finish date adjusted to maximum allowed date", type = "warning", duration = 5)
    }
    if (start >= finish) {
      showNotification("Start date must be earlier than finish date", type = "error", duration = 10)
      return()
    }
    
    # Update reactive values
    dates$startDateTime[n] <- start
    dates$stopDateTime[n] <- finish
    
    # Update UI elements based on time difference
    timeDiff <- difftime(finish, start, units = "days")
    log_info(paste("Time difference:", timeDiff, "days"))
    
    if (as.numeric(timeDiff) < 1) {
      updateRadioButtons(session, 'SleepBoxTime', 'Box time', 
                         c("Minutes" = "Min", "Hours" = "Hour"), 
                         selected = "Hour", inline = TRUE)
      updateRadioButtons(session, 'ActivityBoxTime', 'Box time', 
                         c("Minutes" = "Min", "Hours" = "Hour"), 
                         selected = "Hour", inline = TRUE)
    } else {
      updateRadioButtons(session, 'SleepBoxTime', 'Box time', 
                         c("Hours" = "Hour", "Days" = "Day"), 
                         selected = "Day", inline = TRUE)
      updateRadioButtons(session, 'ActivityBoxTime', 'Box time', 
                         c("Hours" = "Hour", "Days" = "Day"), 
                         selected = "Day", inline = TRUE)
    }
    
    # Update Conditions data frame if it exists
    if (!is.null(Conditions$df) && nrow(Conditions$df) > 0) {
      for (i in 1:nrow(Conditions$df)) {
        if (Conditions$df[i,1] == ImportedFiles()[n]) {
          Conditions$df[i,'start_datetime'] <- format(start, "%Y-%m-%d %H:%M:%S")
          Conditions$df[i,'stop_datetime'] <- format(finish, "%Y-%m-%d %H:%M:%S")
        }
      }
    }
    
    log_info("Date change processing completed successfully")
    
  }, error = function(e) {
    log_error(paste("Unhandled error in changeDate observer:", e$message))
    showNotification(paste("Error processing dates:", e$message), type = "error")
  })
})


########################### CHANNELS PANEL ##################

#Create channels datatable
observe({
  
  req(is.null(Channels$df))
  
  #Create dataframe
  df <- data.frame(
    #Creation of checkbox datatable with lower width possible
    Col1 = shinyInput(prettyCheckbox, 4, paste0("cbox1"), FALSE, width = "5%"),
    Col2 = shinyInput(prettyCheckbox, 4, paste0("cbox2"), FALSE, width = "5%"),
    Col3 = shinyInput(prettyCheckbox, 4, paste0("cbox3"), FALSE, width = "5%"),
    Col4 = shinyInput(prettyCheckbox, 4, paste0("cbox4"), FALSE, width = "5%"),
    Col5 = shinyInput(prettyCheckbox, 4, paste0("cbox5"), FALSE, width = "5%"),
    Col6 = shinyInput(prettyCheckbox, 4, paste0("cbox6"), FALSE, width = "5%"),
    Col7 = shinyInput(prettyCheckbox, 4, paste0("cbox7"), FALSE, width = "5%"),
    Col8 = shinyInput(prettyCheckbox, 4, paste0("cbox8"), FALSE, width = "5%"),
    Col9 = shinyInput(prettyCheckbox, 4, paste0("cbox9"), FALSE, width = "5%")
  )
  
  #Column names
  names(df)[1] <- " "
  names(df)[2] <- " "
  names(df)[3] <- " "
  names(df)[4] <- " "
  names(df)[5] <- " "
  names(df)[6] <- " "
  names(df)[7] <- " "
  names(df)[8] <- " "
  names(df)[9] <- "Select All"
  
  Channels$df <- df
  
  #Set channels datatable to channels panel
  output$Channels <- DT::renderDataTable(
    expr = {Channels$df},
    server = FALSE,
    escape = FALSE,
    selection = "none",
    rownames= FALSE,
    options = list(
      stateSave = TRUE,
      ordering = FALSE,
      searching = FALSE,
      paging = TRUE,
      pagingType = "numbers",
      pageLength = 5,
      lengthChange = FALSE,
      info = FALSE,
      preDrawCallback = JS("function() {
          Shiny.unbindAll(this.api().table().node()); }"
      ),
      drawCallback = JS("function() {
          Shiny.bindAll(this.api().table().node());} "))
  )
  
  #Name each checkbox
  for (i in 1:4){
    for (j in seq_len(8)){
      col <- paste0("cbox",j)
      updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),FALSE)
    }
  }
})

#Select All buttons (if press one select All button, select or unselect all)
observeEvent(input[['cbox91']],{
  i=1
  value <- shinyValue(paste0("cbox",9),4)[i]==TRUE
  for (j in seq_len(8)){
    col = paste0("cbox",j)
    updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),value)}
})
observeEvent(input[['cbox92']],{
  
  i=2
  value <- shinyValue(paste0("cbox",9),4)[i]==TRUE
  for (j in seq_len(8)){
    col = paste0("cbox",j)
    updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),value)}
})
observeEvent(input[['cbox93']],{
  
  i=3
  value <- shinyValue(paste0("cbox",9),4)[i]==TRUE
  for (j in seq_len(8)){
    col = paste0("cbox",j)
    updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),value)}
})
observeEvent(input[['cbox94']],{
  
  i=4
  value <- shinyValue(paste0("cbox",9),4)[i]==TRUE
  for (j in seq_len(8)){
    col = paste0("cbox",j)
    updatePrettyCheckbox(session,paste0(col,i),paste(j+8*(i-1)),value)}
})


############################## METADATA #####################################

#Create metadata from conditions added and update conditions addition UI
observeEvent(input$addcondition,{
  
  if (is.null(ImportedFiles())){
    showNotification("No files imported",type  ="error", duration = 5)}
  
  validate (
    need(ImportedFiles(),"")
  )
  
  n<-getFileNr()
  
  #Check if any name was inputed for the condition
  if (input$condition==""){
    showNotification("No name selected for the condition chosen", type = "error", duration = 5)
  }
  validate(
    need(input$condition!="","No name selected for the condition chosen")
  )
  
  #Check if any channel was selected
  region_id <- vector()
  for (j in 1:4){
    for (i in 1:8){
      if(shinyValue(paste0("cbox",i),4)[j])
      { 
        col = paste0("cbox",i)
        shinyjs::disable(paste0(col,j))
        shinyjs::disable(paste0(paste0("cbox",9),j))
        updatePrettyCheckbox(session,paste0(col,j),paste(i+8*(j-1)),FALSE)
        region_id <- c(region_id,i+8*(j-1))}
    }
    updatePrettyCheckbox(session,paste0(paste0("cbox",9),j)," ",FALSE)}
  
  
  if (length(region_id)<1){
    showNotification("No channel selected", type = "error", duration = 5)
  }
  validate(
    need(length(region_id)>0,"No channel selected")
  )
  
  #Reset conditions text field
  updateTextInput(session,"condition",value = "")
  
  ### Order variable
  value <- 1
  if (!is.null(Conditions$df)){
    if (nrow(Conditions$df)>0){
      for (i in 1:length(Conditions$df[,6])){
        if (input$condition == Conditions$df[i,5]){
          value <- Conditions$df[i,6]
          break
        }
        if (i == length(Conditions$df[,6]) && length(Conditions$df[,6]) > 0){
          value <- max(Conditions$df[,6])+1
        }
      }
    }
  }
  
  #Metadata variables
  file <- ImportedFiles()[n]

  # Safely format datetime (handle NULL or invalid values)
  start_dt <- dates$startDateTime[n]
  stop_dt <- dates$stopDateTime[n]

  if(inherits(start_dt, "POSIXt")) {
    start_datetime <- format(start_dt, "%Y-%m-%d %H:%M:%S")
  } else if(is.numeric(start_dt)) {
    # Convert Unix timestamp to formatted datetime
    start_datetime <- format(as.POSIXct(start_dt, origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S")
  } else if(is.character(start_dt) && grepl("^[0-9.]+$", start_dt)) {
    # Character that looks like a Unix timestamp
    start_datetime <- format(as.POSIXct(as.numeric(start_dt), origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S")
  } else if(is.character(start_dt)) {
    start_datetime <- start_dt
  } else {
    start_datetime <- as.character(start_dt)
  }

  if(inherits(stop_dt, "POSIXt")) {
    stop_datetime <- format(stop_dt, "%Y-%m-%d %H:%M:%S")
  } else if(is.numeric(stop_dt)) {
    # Convert Unix timestamp to formatted datetime
    stop_datetime <- format(as.POSIXct(stop_dt, origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S")
  } else if(is.character(stop_dt) && grepl("^[0-9.]+$", stop_dt)) {
    # Character that looks like a Unix timestamp
    stop_datetime <- format(as.POSIXct(as.numeric(stop_dt), origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S")
  } else if(is.character(stop_dt)) {
    stop_datetime <- stop_dt
  } else {
    stop_datetime <- as.character(stop_dt)
  }

  labels <- input$condition
  order <- value

  File <- file
  result <- data.frame(file, start_datetime, stop_datetime,region_id, labels, order, File, stringsAsFactors = FALSE)
  
  Conditions$df <- rbind(Conditions$df,result)
  
})

### Save metadata
observeEvent(input$saveMetadata,{
  
  req(nrow(Conditions$df))
  
  shinySaveButton("save", "Save file", "Save file as ...", filetype=list(xlsx="xlsx"))
})

#Download/Save metadata
observeEvent(Conditions$df,{
  
  if (nrow(Conditions$df)>0){
    shinyjs::enable("saveMetadata")
  }
  else{
    shinyjs::disable("saveMetadata")
  }
  output$saveMetadata <- downloadHandler(
    
    filename = function(){
      paste0('metadata','.xlsx')
    },
    content = function(file){
      
      req(nrow(Conditions$df)>0)
      
      data <- Conditions$df[,1:6]
      
      data[,2] <- as.character(data[,2])
      data[,3] <- as.character(data[,3])
      
      #Create xlsx workbook of conditions and zeitgeber table
      wb<-createWorkbook()
      addWorksheet(wb, "Conditions")
      writeData(wb, "Conditions", data)
      addWorksheet(wb, "zeitgeberTimes")
      writeData(wb, "zeitgeberTimes", zt_table())

      #Settings sheet
      Settings <- c("LD cycle (h)","Light hours per cycle (h)", "Bin size (min)", "Stop threshold (min)",
                    "Sleep threshold (min)", "Periodogram function", "Resolution/Oversampling", "Minimum period (h)",
                    "Maximum period (h)", "Death threshold (h)", "Remove all dead", "Remove dead individually", "Remove last inactivity")

      Values <- c(l_period(), l_hours(), input$movingAverage, input$ActivityBoutWindow,
                  input$sleepTime, input$perFun, input$periodogramValue, input$minPer,
                  input$maxPer,input$deadTime, FALSE, FALSE, FALSE)

      settingsTable(cbind(Settings,Values))

      addWorksheet(wb, "Settings")
      writeData(wb, "Settings", settingsTable())

      #Colors sheet
      if(nrow(graphsAestethics$df)>0 & nrow(damData$dt)>0){
        addWorksheet(wb, "Colors")
        writeData(wb, "Colors", graphsAestethics$df)
      }

      saveWorkbook(wb, file, overwrite = TRUE)
    })
})

### Import analysis metadata
observeEvent(input$importConditions,{
  
  #Get metadata file
  
  #Verify if data is in excel format
  ext <- tools::file_ext(input$importConditions$name)
  
  if (ext != "xlsx"){      
    showNotification(paste("Files must be .xlsx"), type = "error", duration = 10)
  }
  validate(need(ext == "xlsx", ""))
  
  #Verify if the .xlsx have 2 pages
  wb <- loadWorkbook(input$importConditions$datapath)
  sheet <- getSheets(wb)
  if (length(sheet) < 2 | length(sheet) >4){      
    showNotification("Metadata files should have 2 to 4 sheets", type = "error", duration = 10)
  }
  validate(need(length(sheet) >= 2 & length(sheet) <=4, ""))
  
  
  # Get conditions metadata
  table <- read.xlsx(input$importConditions$datapath,1)
  
  error = FALSE
  
  ### Verify if all headers are according to the saved metadata files
  if (names(table)[1] != "file" | names(table)[2] != "start_datetime" |
      names(table)[3] != "stop_datetime" | names(table)[4] != "region_id" |
      names(table)[5] != "labels" | names(table)[6] != "order"){
    showNotification(paste("The headers of the metadata files are not valid"), type = "error", duration = 5)
    error = TRUE
  }
  
  validate(
    need(error == FALSE,""))
  
  File <- table[,1]
  table <- cbind(table,File)
  
  ### Turn dates into date and time format
  table[,2] <- as.POSIXct(table[,2])
  table[,3] <- as.POSIXct(table[,3])
  
  uniqueNames <- unique(table[,'file'])
  
  ### Verify if all files match with the ones present in the selected folder and import data
  import <- TRUE
  for(i in 1:length(uniqueNames)){
    if (is.na(match(uniqueNames[i],ImportedFiles()))){
      import <-  FALSE
      break
    }
    else{
      n <- match(uniqueNames[i],ImportedFiles())
      dateN <- match(uniqueNames[i],File)
      dates$startDateTime[n] <- table[dateN,'start_datetime']
      dates$stopDateTime[n] <- table[dateN,'stop_datetime']
      
    }
  }
  
  #If all verifications were passed, attribute table to metadata variable
  if (import == TRUE){
    Conditions$df <- table
  }
  else{
    showNotification(paste("Selected metadata file is not from files from selected folder"), type = "error", duration = 5)
  }
  
  validate(
    need(import == TRUE,"")
  )
  
  #Update zeitgeber times
  ztSheet <- read.xlsx(input$importConditions$datapath,2)
  
  if(nrow(ztSheet)<length(uniqueNames)){
    showNotification("Corrupted zeitgeberTimes sheet",type = "warning", duration = 5)
  }
  validate(
    need(nrow(ztSheet)>=length(uniqueNames),"")
  )
  
  # Use different variable name to avoid shadowing the reactive zeitgeberTimes
  zt_values <- c()

  for (i in 1:length(ImportedFiles())){
    zt_values <- c(zt_values,"08:00:00")
  }


  for (i in 1:nrow(ztSheet)){
    n <- match(ztSheet[i,1],ImportedFiles())
    if (!is.na(ztSheet[i,2])){
      zt_values[n] <- ztSheet[i,2]
    }
  }

  zeitgeberTimes(zt_values)
  
  
  
  #Update settings
  if (length(sheet) >= 3){
    settings <- read.xlsx(input$importConditions$datapath,3)
    
    updateSliderInput(session,"l_period", value = settings[1,2])
    updateSliderInput(session,"l_hours", value = settings[2,2])
    updateSliderInput(session,"movingAverage", value = settings[3,2])
    updateSliderInput(session,"ActivityBoutWindow", value = settings[4,2])
    updateSliderInput(session,"sleepTime", value = settings[5,2])
    updateSliderInput(session,"sleepTime2", value = settings[5,2])
    updateSliderInput(session,"perFun", value = settings[6,2])
    updateSliderInput(session,"periodogramValue", value = settings[7,2])
    updateSliderInput(session,"minPer", value = settings[8,2])
    updateSliderInput(session,"maxPer", value = settings[9,2])
    updateSliderInput(session,"deadTime", value = settings[10,2])
  }
  
  #Update colors
  if (length(sheet) >= 4){
    graphsAestethics$df <- read.xlsx(input$importConditions$datapath,4)
  }
  
  
  
  #Update Dates
  updateDates()
  
  #Update Channels
  updateChannels()
  
})

######################### CONDITIONS DATATABLE ##################################
#Create data table to present to the user
observeEvent(Conditions$df,{

  req(nrow(Conditions$df)>0)

  #Reorder labels and order number
  if (nrow(Conditions$df)>1){
    Conditions$df <- Conditions$df[order(Conditions$df[,'order']),]
  }


  # Separate data according to labels and files
  # Add other variables
  Channels <- vector()
  Labels <- vector()
  File <- vector()
  First_date <- vector()
  End_date <- vector()
  Order <- vector()

  for (i in 1:length(Conditions$df[,5])){
    if (i == 1){
      channelsPerLabel <- as.character(Conditions$df[i,4])
      Labels <- Conditions$df[i,'labels']
      File <- tools::file_path_sans_ext(Conditions$df[i,1])
      First_date <- as.character(Conditions$df[i,2])
      End_date <- as.character(Conditions$df[i,3])
      Order <- as.numeric(Conditions$df[i,6])
    }
    else{
      if (Conditions$df[i,'labels'] == Labels[length(Labels)] & tools::file_path_sans_ext(Conditions$df[i,1]) == File[length(File)]){
        channelsPerLabel <- paste(channelsPerLabel,as.character(Conditions$df[i,4]))
      }
      else{
        Channels <- c(Channels,channelsPerLabel)
        channelsPerLabel <- as.character(Conditions$df[i,4])
        Labels <- c(Labels,Conditions$df[i,5])
        File <- c(File,tools::file_path_sans_ext(Conditions$df[i,1]))
        First_date <- c(First_date,as.character(Conditions$df[i,2]))
        End_date <- c(End_date,as.character(Conditions$df[i,3]))
        Order <- c(Order,Conditions$df[i,6])
      }
    }
  }

  Channels <- c(Channels,channelsPerLabel)


  tableData$df <- data.frame(File,First_date,End_date,Channels,Labels, Order)

})

#Data panel
output$Data <- DT::renderDataTable(
  expr= {deleteButtonColumn(tableData$df, 'delete_button')}, escape = FALSE, selection = 'none', 
  editable  = list(target = 'cell',disable = list(columns = c(1,2,3,4,7))),
  option = list(ordering = TRUE, order = list(list(6,'asc'))))

###################### UPDATE DATA BY CHANGES IN TABLE 

#Update metadata and labels from cell edit
observeEvent(input$Data_cell_edit, {
  info = input$Data_cell_edit
  str(info)
  i = info$row
  j = info$col
  v = info$value
  
  #Change metadata value
  if (j==5){
    
    index <- which(v == Conditions$df[,5])
    
    for (k in 1:length(Conditions$df[,5])){
      if (Conditions$df[k,5] == tableData$df[i,j]){
        Conditions$df[k,5] <- v
        if(length(index)>0){
          Conditions$df[k,6] <- Conditions$df[index[1],6]
        }
      }
    }
  }
  if (j==6){
    for (k in 1:length(Conditions$df[,6])){
      if (Conditions$df[k,6] == tableData$df[i,j]){
        Conditions$df[k,6] <- as.numeric(v)
      }
      else{
        if (Conditions$df[k,6] == v){
          Conditions$df[k,6] <- as.numeric(tableData$df[i,j])
        }
      }
    }
  }
  
})

#Delete row from metadata
observeEvent(input$deletePressed, {
  rowNum <- parseDeleteEvent(input$deletePressed)
  
  if(input$pages == "Data selection"){
    
    req(nrow(Conditions$df)>0)
    
    withProgress(message = 'Update data', value = 0, {
      for (k in seq(length(Conditions$df[,5]),1,-1)){
        if (Conditions$df[k,5] == tableData$df[rowNum,5] & tools::file_path_sans_ext(Conditions$df[k,1]) == tableData$df[rowNum,1]){
          
          incProgress(1/nrow(Conditions$df)) #Increment of progress bar
          ch <- Conditions$df[k,4]
          row <- ceiling(ch/8)
          col <- ch-8*(row-1)
          shinyjs::enable(paste0(paste0("cbox",col),row))
          
          #Delete value from metadata
          Conditions$df <- Conditions$df[-k,]
          
          if (ch<10)
            paste_channels <- paste0('0',ch)
          else
            paste_channels <- ch
          
          ids <- paste(Conditions$df[row,'start_datetime'],paste0(Conditions$df[row,'file']),paste_channels,sep="|")
          
        }
      }
    })
  
    MinTime(TRUE)
    
    if (nrow(Conditions$df)==0){
      damData$dt <- NULL
      cleanData(NULL)
    }
    else{
      #Dead animals
      damData$dt<-damData$dt[which(damData$dt[,'id']!=ids),]
      cleanData(cleanData()[which(cleanData()[,'id']!=ids),])
    }
    
    
    #Select all ticks
    shinyjs::enable(paste0(paste0("cbox",9),1))
    shinyjs::enable(paste0(paste0("cbox",9),2))
    shinyjs::enable(paste0(paste0("cbox",9),3))
    shinyjs::enable(paste0(paste0("cbox",9),4))
    
    n<- getFileNr()
    
    if(length(Conditions$df[,1]>0)){
      for (k in seq(length(Conditions$df[,1]))){
        if (Conditions$df[k,1] == ImportedFiles()[n]){
          
          ch <- Conditions$df[k,4]
          if (ch>=1 & ch<=8){
            shinyjs::disable(paste0(paste0("cbox",9),1))}
          if (ch>=9 & ch<=16){
            shinyjs::disable(paste0(paste0("cbox",9),2))}
          if (ch>=17 & ch<=24){
            shinyjs::disable(paste0(paste0("cbox",9),3))}
          if (ch>=25 & ch<=32){
            shinyjs::disable(paste0(paste0("cbox",9),4))}
        }}
    }
    
    # Delete the row from the data frame
    tableData$df <- tableData$df[-rowNum,]
  }
})

############################# Start analysis ###################################

#Import and analyse data
# Import and analyse data
observeEvent(input$startanalysis, {
  
  req(nrow(Conditions$df) > 0)
  
  # Verify that dates are correctly inputted
  validate(
    need(verifyDates(), "")
  )
  
  ##### Import data and create behavr tables

  withProgress(message = 'Importing data (1/2)', value = 0, {

    # Fix datetime format before linking - convert Unix timestamps to datetime strings
    # This handles cases where POSIXct gets converted to numeric during data.frame operations
    for(i in 1:nrow(Conditions$df)) {
      # Check if start_datetime looks like a Unix timestamp (numeric or numeric string)
      start_val <- Conditions$df[i, 'start_datetime']
      stop_val <- Conditions$df[i, 'stop_datetime']

      # If it's numeric or a numeric string, convert it
      if(is.numeric(start_val) || (is.character(start_val) && grepl("^[0-9.]+$", start_val))) {
        Conditions$df[i, 'start_datetime'] <- format(as.POSIXct(as.numeric(start_val), origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S")
      }
      if(is.numeric(stop_val) || (is.character(stop_val) && grepl("^[0-9.]+$", stop_val))) {
        Conditions$df[i, 'stop_datetime'] <- format(as.POSIXct(as.numeric(stop_val), origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S")
      }
    }

    # Get DAM data
    data <- link_dam_metadata(Conditions$df, Directory()) # Linking
    damData$dt <- load_dam(data, FUN = sleep_dam_annotation, min_time_immobile = 60 * input$sleepTime) # Load DAM data
    
    incProgress(0.5) # Increment progress bar
    MinTime(TRUE)
    
    disable("Death_graphs")
    
    # Only analyse bouts if more than 60 measurements
    if (nrow(damData$dt) > 60) {
      enable("boutAnalysis")
    }
    
    # Update sliders according to imported data
    if ((max(damData$dt[, 't']) - min(damData$dt[, 't'])) / 60 < 180) {
      updateSliderInput(session, "movingAverage", max = (max(damData$dt[, 't']) - min(damData$dt[, 't'])) / 60)
      updateSliderInput(session, "sleepTime", max = (max(damData$dt[, 't']) - min(damData$dt[, 't'])) / 60)
    } else {
      updateSliderInput(session, "movingAverage", value = 60, max = 180)
      updateSliderInput(session, "sleepTime", value = 5, max = 60)
    }
    
  })
  
  ##### Calculate parameters 
  
  withProgress(message = 'Computing activity and sleep parameters (2/2)', value = 0, {
    
    # Minimum time counted in the Sensor and update sliders accordingly
    damData$dt[, timeDiff := c(NaN, damData$dt[2:nrow(damData$dt), t] - damData$dt[1:(nrow(damData$dt) - 1), t])]
    updateSliderInput(session, "activityValue", min = round(mean(damData$dt[, timeDiff], na.rm = TRUE) / 60))
    updateSliderInput(session, "boutValue", min = round(mean(damData$dt[, timeDiff], na.rm = TRUE) / 60))
    updateSliderInput(session, "movingAverage", min = ceiling(max(damData$dt[, timeDiff], na.rm = TRUE) / 60), step = max(damData$dt[, timeDiff], na.rm = TRUE) / 60)
    updateSliderInput(session, "ActivityBoutWindow", min = ceiling(max(damData$dt[, timeDiff], na.rm = TRUE) / 60), step = max(damData$dt[, timeDiff], na.rm = TRUE) / 60)
    updateRadioButtons(session, "SleepBoxTime", selected = "Day")
    updateRadioButtons(session, "ActivityBoxTime", selected = "Day")
    
    # Files in the metadata
    Files <- damData$dt[, file_info, meta = TRUE]

    # Difference between start time and zeitgeber time for ALL files
    # Build minDiff vector for each imported file
    # IMPORTANT: Truncate seconds from start time to get clean minute alignment
    minDiff <- c()
    zt_times <- zeitgeberTimes()
    for (file_idx in 1:length(ImportedFiles())) {
      # Get start time and truncate to whole minutes (remove seconds)
      start_time_str <- strftime(dates$startDateTime[file_idx], format = "%H:%M:00")
      diff_val <- as.numeric(difftime(
        as.POSIXct(start_time_str, format = "%H:%M:%S"),
        as.POSIXct(zt_times[file_idx], format = "%H:%M:%S"),
        units = "mins"
      ))
      # Round to nearest minute to avoid floating point issues
      diff_val <- round(diff_val)
      minDiff <- c(minDiff, diff_val)
    }

    # Start index
    startIndex <- 1

    # Create moving and AUC variables
    damData$dt[, moving := activity > 0]
    damData$dt[, auc := cumsum(activity)]

    # Get final indexes for each animal
    finalIndexes <- c()
    if (sum(damData$dt[, timeDiff] < 0, na.rm = TRUE) > 0) {
      finalIndexes <- which(damData$dt[, timeDiff] %in% damData$dt[, timeDiff][damData$dt[, timeDiff] < 0]) - 1
    }

    finalIndexes <- c(finalIndexes, nrow(damData$dt))
    startIndexes <- c(1, finalIndexes[1:length(finalIndexes) - 1] + 1)

    # Update time according to the zeitgeber time
    startIndex <- 1
    if (MinTime() == TRUE) {
      for (i in 1:length(damData$dt[, id, meta = TRUE])) {
        n <- match(Files[[i]]$file, ImportedFiles())
        damData$dt[startIndex:finalIndexes[i], t := t + (minDiff[n] * 60)]
        startIndex <- finalIndexes[i] + 1
      }
    }
    
    MinTime(FALSE)
    
    # Periodic time adjustable by the user
    damData$dt[, 'periodT' := (damData$dt[, 't'])]
    
    # Create labels, file, channels, and order 
    damData$dt[, labels := damData$dt[1, labels, meta = TRUE]]
    damData$dt[, file := damData$dt[1, File, meta = TRUE]]
    damData$dt[, channels := damData$dt[1, region_id, meta = TRUE]]
    damData$dt[, order := damData$dt[1, order, meta = TRUE]]
    
    damData$dt[startIndexes, timeDiff := NaN]
    if (length(startIndexes) > 1) {
      for (i in 1:length(startIndexes)) {
        damData$dt[(startIndexes[i]:finalIndexes[i]), auc := cumsum(damData$dt[startIndexes[i]:finalIndexes[i], activity])]
        
        animal <- damData$dt[, , meta = TRUE][i]
        damData$dt[startIndexes[i]:finalIndexes[i], 'labels'] <- rep(animal$labels, (finalIndexes[i] - startIndexes[i]) + 1)
        damData$dt[startIndexes[i]:finalIndexes[i], 'file'] <- rep(animal$File, (finalIndexes[i] - startIndexes[i]) + 1)
        damData$dt[startIndexes[i]:finalIndexes[i], 'channels'] <- rep(animal$region_id, (finalIndexes[i] - startIndexes[i]) + 1)
        damData$dt[startIndexes[i]:finalIndexes[i], 'order'] <- rep(animal$order, (finalIndexes[i] - startIndexes[i]) + 1)
      }
    }
  })
  
  # Data labels for graphs
  damData$dt[, Data_labels := interaction(File, labels, region_id, sep = " - "), meta = TRUE]
  damData$dt[, Data := interaction(labels, order, sep = " - "), meta = TRUE]

  # Also add Data as a regular column for ggetho compatibility with newer R versions
  # Create Data from labels and order columns that were already assigned to each row
  damData$dt[, Data := interaction(labels, order, sep = " - ")]

  # Activity and bouts variables for statistics plots
  damData$dt[, experimentDay := floor(periodT / (l_period() * 3600))] # Experiment day
  damData$dt[, day_night := ((periodT - experimentDay * l_period() * 3600) / 3600) < l_hours()] # Day or night time
  damData$dt[, 'SleepBoxPlot_time' := floor(damData$dt[, 't'] / (input$SleepGroupBoxTime * l_period() * 3600))]
  damData$dt[, 'ActivityBoxPlot_time' := floor(damData$dt[, 't'] / (input$ActivityGroupBoxTime * l_period() * 3600))]
  
  Settings <- c("LD cycle (h)", "Light hours per cycle (h)", "Bin size (min)", "Stop threshold (min)",
                "Sleep threshold (min)", "Periodogram function", "Resolution/Oversampling", "Minimum period (h)",
                "Maximum period (h)", "Death threshold (h)", "Remove all dead", "Remove dead individually", "Remove last inactivity",
                "Fractal time-scale 1 (h)", "Fractal time-scale 2 (h)", "Number of points per time-scale")
  
  Values <- c(l_period(), l_hours(), input$movingAverage, input$ActivityBoutWindow,
              input$sleepTime, input$perFun, input$periodogramValue, input$minPer,
              input$maxPer, input$deadTime, FALSE, FALSE, FALSE,
              paste(input$Fractal_limits[1], input$TimeScale_limits, sep = " - "),
              paste(input$TimeScale_limits, input$Fractal_limits[2], sep = " - "),
              input$nFractal)
  
  settingsTable(cbind(Settings, Values))
})



# ===============================================================================
# SLEEP BOUT ANALYSIS WITH INTEGRATED DEATH DETECTION
# Standalone R script - No UI required
# Compatible with R 4.5.x
# ===============================================================================

################################ REQUIRED PACKAGES ################################

required_packages <- c("data.table", "lubridate", "stringr", "openxlsx")

for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("All packages loaded successfully!\n\n")

################################ DEATH DETECTION FUNCTION ################################

# Detect dead animals based on continuous inactivity
detect_dead_animals <- function(data, death_inactivity_hours = 12) {

  cat("\n=== DEATH DETECTION ===\n")
  cat("Inactivity threshold:", death_inactivity_hours, "hours\n")

  # Get all channel columns
  activity_cols <- grep("^activity_", names(data), value = TRUE)

  dead_info <- data.table()
  alive_channels <- c()

  for(col in activity_cols) {
    channel_num <- as.numeric(sub("activity_", "", col))

    # Check for continuous zero activity
    activity <- data[[col]]
    is_zero <- activity == 0

    # Find runs of zeros
    rle_result <- rle(is_zero)

    # Find the longest continuous zero period (in minutes)
    max_zero_length <- 0
    death_start_idx <- NA

    if(any(rle_result$values)) {
      zero_lengths <- rle_result$lengths[rle_result$values]
      max_zero_length <- max(zero_lengths)

      # Find when this longest period started
      if(max_zero_length >= death_inactivity_hours * 60) {
        cumsum_lengths <- cumsum(rle_result$lengths)
        zero_run_idx <- which(rle_result$values & rle_result$lengths == max_zero_length)[1]

        if(zero_run_idx > 1) {
          death_start_idx <- cumsum_lengths[zero_run_idx - 1] + 1
        } else {
          death_start_idx <- 1
        }
      }
    }

    max_zero_hours <- max_zero_length / 60

    # Determine if dead
    is_dead <- max_zero_hours >= death_inactivity_hours

    if(is_dead) {
      death_time <- data$datetime[death_start_idx]

      dead_info <- rbind(dead_info, data.table(
        channel = channel_num,
        status = "DEAD",
        max_inactivity_hours = round(max_zero_hours, 2),
        death_time = format(death_time, "%Y-%m-%d %H:%M:%S"),
        death_start_index = death_start_idx
      ))

      cat(sprintf("  Channel %2d: DEAD (%.1f hours inactivity starting at %s)\n",
                  channel_num, max_zero_hours, format(death_time, "%Y-%m-%d %H:%M")))
    } else {
      alive_channels <- c(alive_channels, channel_num)
    }
  }

  cat("\nDeath Detection Summary:\n")
  cat("  Total channels checked:", length(activity_cols), "\n")
  cat("  Dead animals found:", nrow(dead_info), "\n")
  cat("  Alive animals:", length(alive_channels), "\n\n")

  return(list(
    dead_info = dead_info,
    alive_channels = alive_channels
  ))
}

################################ CORE FUNCTIONS ################################

# Function to read a single DAM file with time filtering and death detection
read_dam_file_with_death_detection <- function(filepath, start_time = NULL, end_time = NULL,
                                               death_inactivity_hours = 12,
                                               exclude_dead = TRUE) {

  cat("Reading:", basename(filepath), "\n")

  # Read DAM file
  data <- tryCatch({
    read.table(filepath, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop(paste("Error reading file:", e$message))
  })

  # Validate format (DAM files have 42 or 44 columns)
  if(ncol(data) < 42) {
    stop(paste("File has", ncol(data), "columns, expected at least 42"))
  }

  # Create datetime from columns 2-5 (day, month, year, time)
  datetime_str <- paste(data[,2], data[,3], data[,4], data[,5])
  datetime <- dmy_hms(datetime_str, tz = "UTC")

  # Create basic data structure
  result <- data.table(
    datetime = datetime,
    monitor = data[,1],
    day = data[,2],
    month = data[,3],
    year = data[,4],
    time = data[,5]
  )

  # Add activity for each channel (columns 11-42 for standard DAM, or 13-44)
  if(ncol(data) == 42) {
    activity_data <- data[, 11:42]
  } else {
    activity_data <- data[, 13:44]
  }

  for(i in 1:32) {
    result[[paste0("activity_", i)]] <- as.numeric(activity_data[, i])
  }

  # Apply time frame filtering
  original_rows <- nrow(result)

  if(!is.null(start_time)) {
    start_datetime <- as.POSIXct(start_time, tz = "UTC")
    result <- result[datetime >= start_datetime]
    cat("  Filtered to start from:", format(start_datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "\n")
  }

  if(!is.null(end_time)) {
    end_datetime <- as.POSIXct(end_time, tz = "UTC")
    result <- result[datetime <= end_datetime]
    cat("  Filtered to end at:", format(end_datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "\n")
  }

  if(!is.null(start_time) || !is.null(end_time)) {
    cat("  Rows:", original_rows, "->", nrow(result), "(", round(100*nrow(result)/original_rows, 1), "%)\n")
  }

  if(nrow(result) == 0) {
    stop("No data remaining after time filtering!")
  }

  cat("  Successfully read", nrow(result), "rows with 32 activity channels\n")

  # DEATH DETECTION
  death_results <- detect_dead_animals(result, death_inactivity_hours)

  # Store death info with the data
  attr(result, "death_info") <- death_results$dead_info
  attr(result, "alive_channels") <- death_results$alive_channels

  if(exclude_dead && nrow(death_results$dead_info) > 0) {
    cat("\n>>> Excluding", nrow(death_results$dead_info), "dead animals from analysis\n")
  }

  return(result)
}

# Function to detect sleep bouts for one channel
detect_sleep_bouts_simple <- function(activity_vector, sleep_threshold_min = 5) {

  # Convert activity to sleep/wake (0 activity = asleep)
  asleep <- activity_vector == 0

  # Find sleep bout starts (wake to sleep transitions)
  sleep_starts <- which(asleep & !c(FALSE, asleep[-length(asleep)]))

  # Find sleep bout ends (sleep to wake transitions)
  sleep_ends <- which(!asleep & c(asleep[-1], FALSE))

  # Initialize output vectors
  bout_durations <- rep(0, length(activity_vector))
  sleep_bout_markers <- rep(FALSE, length(activity_vector))

  if(length(sleep_starts) > 0 & length(sleep_ends) > 0) {

    # Match starts with ends
    for(i in 1:length(sleep_starts)) {
      start_idx <- sleep_starts[i]

      # Find corresponding end
      end_idx <- sleep_ends[sleep_ends > start_idx]
      if(length(end_idx) > 0) {
        end_idx <- end_idx[1]
        duration_min <- end_idx - start_idx

        # Only count as sleep bout if >= threshold
        if(duration_min >= sleep_threshold_min) {
          sleep_bout_markers[start_idx] <- TRUE
          bout_durations[start_idx] <- duration_min
        }
      }
    }
  }

  return(list(
    asleep = asleep,
    sleep_bout = sleep_bout_markers,
    bout_duration = bout_durations
  ))
}

################################ MAIN ANALYSIS FUNCTION ################################

analyze_one_file_split_conditions <- function(
    # File path
    file_path,

    # Time settings
    start_date = "2024-09-08", start_time = "06:00",
    end_date = "2024-09-09", end_time = "05:59",

    # Channel settings
    condition1_channels = 1:16,
    condition2_channels = 17:32,

    # General settings
    condition_names = c("Control", "Treatment"),
    sleep_threshold_min = 5,
    lights_on_time = "06:00",
    lights_off_time = "18:00",

    # Death detection settings
    death_inactivity_hours = 12,
    exclude_dead = TRUE,

    output_dir = ".") {

  # Parse time strings for light/dark classification
  lights_on_hour <- as.numeric(substr(lights_on_time, 1, 2))
  lights_off_hour <- as.numeric(substr(lights_off_time, 1, 2))

  # Create datetime strings
  start_datetime <- paste(start_date, paste0(start_time, ":00"))
  end_datetime <- paste(end_date, paste0(end_time, ":59"))

  cat("=== 1-FILE SPLIT CONDITIONS ANALYSIS WITH DEATH DETECTION ===\n")
  cat("File:", basename(file_path), "\n")
  cat("  Time period:", start_datetime, "to", end_datetime, "\n")
  cat("  ", condition_names[1], "channels:", paste(condition1_channels, collapse = ", "), "\n")
  cat("  ", condition_names[2], "channels:", paste(condition2_channels, collapse = ", "), "\n")
  cat("Sleep threshold:", sleep_threshold_min, "minutes\n")
  cat("Light phase:", lights_on_time, "to", lights_off_time, "\n")
  cat("Death detection threshold:", death_inactivity_hours, "hours of inactivity\n")
  cat("Exclude dead animals:", exclude_dead, "\n\n")

  # Read file with death detection
  data <- read_dam_file_with_death_detection(file_path, start_datetime, end_datetime,
                                             death_inactivity_hours, exclude_dead)

  # Get death information
  death_info <- attr(data, "death_info")
  alive_channels <- attr(data, "alive_channels")

  # Filter channels based on death status
  if(exclude_dead && nrow(death_info) > 0) {
    # Only analyze alive channels
    condition1_channels_alive <- intersect(condition1_channels, alive_channels)
    condition2_channels_alive <- intersect(condition2_channels, alive_channels)

    cat("\nAfter excluding dead animals:\n")
    cat("  ", condition_names[1], "alive channels:", paste(condition1_channels_alive, collapse = ", "),
        "(", length(condition1_channels_alive), "/", length(condition1_channels), ")\n")
    cat("  ", condition_names[2], "alive channels:", paste(condition2_channels_alive, collapse = ", "),
        "(", length(condition2_channels_alive), "/", length(condition2_channels), ")\n\n")

    # Update channel lists
    condition1_channels <- condition1_channels_alive
    condition2_channels <- condition2_channels_alive
  }

  # Combine results
  all_results <- data.table()

  # Process Condition 1 channels
  if(length(condition1_channels) > 0) {
    cat("Processing", condition_names[1], "channels:", paste(condition1_channels, collapse = ", "), "\n")
    for(ch in condition1_channels) {
      activity_col <- paste0("activity_", ch)

      if(activity_col %in% names(data)) {
        sleep_analysis <- detect_sleep_bouts_simple(data[[activity_col]], sleep_threshold_min)

        animal_data <- data.table(
          file = basename(file_path),
          file_number = 1,
          condition = condition_names[1],
          channel = ch,
          animal_id = paste(condition_names[1], "Ch", sprintf("%02d", ch), sep = "_"),
          datetime = data$datetime,
          activity = data[[activity_col]],
          asleep = sleep_analysis$asleep,
          sleep_bout = sleep_analysis$sleep_bout,
          bout_duration = sleep_analysis$bout_duration
        )

        # Add time variables
        animal_data[, hour := hour(datetime)]
        animal_data[, day_num := as.numeric(as.Date(datetime) - min(as.Date(datetime)))]
        animal_data[, is_light := hour >= lights_on_hour & hour < lights_off_hour]

        all_results <- rbind(all_results, animal_data)
      }
    }
  }

  # Process Condition 2 channels
  if(length(condition2_channels) > 0) {
    cat("Processing", condition_names[2], "channels:", paste(condition2_channels, collapse = ", "), "\n")
    for(ch in condition2_channels) {
      activity_col <- paste0("activity_", ch)

      if(activity_col %in% names(data)) {
        sleep_analysis <- detect_sleep_bouts_simple(data[[activity_col]], sleep_threshold_min)

        animal_data <- data.table(
          file = basename(file_path),
          file_number = 1,
          condition = condition_names[2],
          channel = ch,
          animal_id = paste(condition_names[2], "Ch", sprintf("%02d", ch), sep = "_"),
          datetime = data$datetime,
          activity = data[[activity_col]],
          asleep = sleep_analysis$asleep,
          sleep_bout = sleep_analysis$sleep_bout,
          bout_duration = sleep_analysis$bout_duration
        )

        # Add time variables
        animal_data[, hour := hour(datetime)]
        animal_data[, day_num := as.numeric(as.Date(datetime) - min(as.Date(datetime)))]
        animal_data[, is_light := hour >= lights_on_hour & hour < lights_off_hour]

        all_results <- rbind(all_results, animal_data)
      }
    }
  }

  # Process results and create Excel file
  result <- process_and_export_results_1file(
    all_results, condition_names, sleep_threshold_min, lights_on_time, lights_off_time,
    file_path, output_dir, death_info, alive_channels
  )

  return(result)
}

################################ RESULT PROCESSING ################################

process_and_export_results_1file <- function(all_results, condition_names, sleep_threshold_min,
                                             lights_on_time, lights_off_time,
                                             file_path, output_dir, death_info, alive_channels) {

  # Show time range
  time_range <- range(all_results$datetime)
  total_duration <- as.numeric(difftime(time_range[2], time_range[1], units = "days"))

  cat("\nOverall analysis period:\n")
  cat("  Start:", format(time_range[1], "%Y-%m-%d %H:%M:%S"), "\n")
  cat("  End:  ", format(time_range[2], "%Y-%m-%d %H:%M:%S"), "\n")
  cat("  Duration:", round(total_duration, 2), "days\n")

  # Calculate sleep bout counts
  cat("\nCalculating sleep bout counts...\n")

  # Light vs Dark counts
  lightdark_counts <- all_results[, .(count = sum(sleep_bout)), by = .(animal_id, condition, is_light)]
  lightdark_counts[, phase := ifelse(is_light, "Light", "Dark")]
  lightdark_counts[, is_light := NULL]

  # Reshape to wide format
  lightdark_wide <- dcast(lightdark_counts, animal_id + condition ~ phase, value.var = "count")

  # Ensure both Light and Dark columns exist
  if(!"Light" %in% names(lightdark_wide)) lightdark_wide[, Light := 0]
  if(!"Dark" %in% names(lightdark_wide)) lightdark_wide[, Dark := 0]

  # Add calculations
  lightdark_wide[, Total_Sleep_Bouts := Light + Dark]
  lightdark_wide[, Light_Dark_Ratio := ifelse(Dark > 0, round(Light/Dark, 3), NA)]
  lightdark_wide[, Percent_Light := ifelse(Total_Sleep_Bouts > 0, round(100 * Light/Total_Sleep_Bouts, 1), NA)]
  lightdark_wide[, Percent_Dark := ifelse(Total_Sleep_Bouts > 0, round(100 * Dark/Total_Sleep_Bouts, 1), NA)]

  # Summary statistics by condition
  summary_stats <- lightdark_wide[, .(
    Animals = .N,
    Mean_Light_Bouts = round(mean(Light, na.rm = TRUE), 2),
    SD_Light_Bouts = round(sd(Light, na.rm = TRUE), 2),
    SEM_Light_Bouts = round(sd(Light, na.rm = TRUE)/sqrt(.N), 2),
    Mean_Dark_Bouts = round(mean(Dark, na.rm = TRUE), 2),
    SD_Dark_Bouts = round(sd(Dark, na.rm = TRUE), 2),
    SEM_Dark_Bouts = round(sd(Dark, na.rm = TRUE)/sqrt(.N), 2),
    Mean_Total_Bouts = round(mean(Total_Sleep_Bouts, na.rm = TRUE), 2),
    Mean_Light_Dark_Ratio = round(mean(Light_Dark_Ratio, na.rm = TRUE), 3)
  ), by = condition]

  # Print summaries
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("=== 1-FILE SPLIT ANALYSIS SUMMARY ===\n")
  cat("Total animals analyzed:", length(unique(all_results$animal_id)), "\n")
  cat("Dead animals excluded:", nrow(death_info), "\n")
  cat("Total sleep bouts detected:", sum(all_results$sleep_bout), "\n\n")

  cat("Summary by Condition:\n")
  print(summary_stats)

  # Create Excel file with experiment date range in filename
  avg_duration_hours <- round(total_duration * 24, 1)

  # Extract start and end dates from the actual data
  start_date <- format(time_range[1], "%Y%m%d")
  end_date <- format(time_range[2], "%Y%m%d")

  # Create filename with experiment dates
  if(start_date == end_date) {
    date_string <- start_date
  } else {
    date_string <- paste0(start_date, "_to_", end_date)
  }

  excel_file <- file.path(output_dir, paste0("Sleep_Bout_Analysis_",
                                             paste(condition_names, collapse = "_vs_"), "_",
                                             date_string, "_",
                                             avg_duration_hours, "h_DeathFiltered.xlsx"))

  # Create Excel file using openxlsx (no Java required!)
  tryCatch({
    wb <- createWorkbook()

    # Sheet 1: Individual Results
    addWorksheet(wb, "Individual_Results")
    writeData(wb, "Individual_Results", lightdark_wide)

    # Sheet 2: Summary by Condition
    addWorksheet(wb, "Summary_by_Condition")
    writeData(wb, "Summary_by_Condition", summary_stats)

    # Sheet 3: Death Information
    if(nrow(death_info) > 0) {
      addWorksheet(wb, "Dead_Animals")
      writeData(wb, "Dead_Animals", death_info)
    }

    # Sheet 4: Analysis Info
    analysis_info <- data.table(
      Parameter = c("Analysis Type", "Total Animals Analyzed", "Dead Animals Excluded",
                    "Alive Animals", "File", "Conditions",
                    "Sleep Threshold (min)", "Light Phase", "Duration (hours)",
                    "Death Detection Threshold (hours)"),
      Value = c("1-File Split Conditions with Death Detection",
                length(unique(all_results$animal_id)),
                nrow(death_info),
                length(alive_channels),
                basename(file_path),
                paste(condition_names, collapse = " vs "),
                sleep_threshold_min,
                paste(lights_on_time, "to", lights_off_time),
                avg_duration_hours,
                ifelse(nrow(death_info) > 0, death_info$max_inactivity_hours[1], "12"))
    )

    addWorksheet(wb, "Analysis_Info")
    writeData(wb, "Analysis_Info", analysis_info)

    saveWorkbook(wb, excel_file, overwrite = TRUE)
    cat("\n>>> Excel file created:", excel_file, "\n")

  }, error = function(e) {
    cat("ERROR creating Excel file:", e$message, "\n")
  })

  cat("\n=== ANALYSIS COMPLETE ===\n")

  return(list(
    excel_file = excel_file,
    data = all_results,
    individual_results = lightdark_wide,
    summary_by_condition = summary_stats,
    death_info = death_info,
    alive_channels = alive_channels
  ))
}

################################ EXAMPLE USAGE ################################

# Uncomment and modify the following to run your analysis:

# results <- analyze_one_file_split_conditions(
#   # File path - MODIFY THIS
#   file_path = "/Users/cchuang/Desktop/Locomotion activity monitor data/Sleep/hTauV337M elav-GS-gal4 AdoRi kTry/50D/3rd/Sleep 50D hTauV337M elav-GS-gal4 AdoRi kTry Monitor2.txt",
#
#   # Time period - MODIFY THIS
#   start_date = "2025-11-25", start_time = "05:00",
#   end_date = "2025-11-26", end_time = "04:59",
#
#   # Channel settings
#   condition1_channels = 1:16,   # Left side
#   condition2_channels = 17:32,  # Right side
#
#   # Condition names - MODIFY THIS
#   condition_names = c("mCherry-RNAi 50D EtOH", "mCherry-RNAi 50D RU"),
#
#   # Analysis settings
#   sleep_threshold_min = 5,
#   lights_on_time = "05:00",
#   lights_off_time = "17:00",
#
#   # Death detection settings
#   death_inactivity_hours = 12,
#   exclude_dead = TRUE,
#
#   # Output directory - MODIFY THIS
#   output_dir = "/Users/cchuang/Desktop/hTauV337M elav-GS-gal4 AdoRi kTry"
# )

cat("\n")
cat("=======================================================\n")
cat("  SLEEP BOUT ANALYSIS SCRIPT LOADED SUCCESSFULLY\n")
cat("=======================================================\n")
cat("\n")
cat("To run an analysis, use the analyze_one_file_split_conditions() function.\n")
cat("\n")
cat("Example:\n")
cat("  results <- analyze_one_file_split_conditions(\n")
cat("    file_path = 'path/to/your/DAM_file.txt',\n")
cat("    start_date = '2025-01-01', start_time = '05:00',\n")
cat("    end_date = '2025-01-02', end_time = '04:59',\n")
cat("    condition1_channels = 1:16,\n")
cat("    condition2_channels = 17:32,\n")
cat("    condition_names = c('Control', 'Treatment'),\n")
cat("    output_dir = 'path/to/output/folder'\n")
cat("  )\n")
cat("\n")

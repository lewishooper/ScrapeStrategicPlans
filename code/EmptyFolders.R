# Set the path to your target folder
target_folder <- "E:/Hospital_Strategic_Plans/strategic_plans"  # Replace with your folder path

# Get list of all subfolders
subfolders <- list.dirs(target_folder, full.names = TRUE, recursive = FALSE)

# Initialize empty vectors to store results
folder_names <- character()
file_counts <- numeric()

# Loop through each subfolder
for (folder in subfolders) {
  # Get folder name (without full path)
  folder_name <- basename(folder)
  
  # List all files in the folder (excluding subfolders)
  files <- list.files(folder, recursive = FALSE, include.dirs = FALSE)
  
  # Count files
  file_count <- length(files)
  
  # Store results
  folder_names <- c(folder_names, folder_name)
  file_counts <- c(file_counts, file_count)
}

# Create dataframe
folder_info <- data.frame(
  folder_name = folder_names,
  file_count = file_counts,
  stringsAsFactors = FALSE
)

# Sort by file count (optional)
folder_info <- folder_info[order(folder_info$file_count), ]

# View the results
print(folder_info)

# Find empty folders
empty_folders <- folder_info[folder_info$file_count == 0, ]
print("\nEmpty folders:")
print(empty_folders)

# You can also save the results to a CSV file
# write.csv(folder_info, "folder_file_counts.csv", row.names = FALSE)


duplicated(folder_info)

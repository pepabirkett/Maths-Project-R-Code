combine_chapters <- function(input_folder, output_folder, batch_size = 5) {
  # Get the list of .txt files in the folder
  files <- list.files(input_folder, pattern = "\\.txt$", full.names = TRUE)
  
  # Sort files to maintain chapter order
  files <- sort(files) 
  
  # Create the output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Loop through files in batches
  num_batches <- ceiling(length(files) / batch_size)
  
  for (i in seq_len(num_batches)) {
    # Get the range of files for this batch
    start <- ((i - 1) * batch_size) + 1
    end <- min(i * batch_size, length(files))
    batch_files <- files[start:end]
    
    # Read and combine text
    combined_text <- sapply(batch_files, readLines, warn = FALSE, USE.NAMES = FALSE)
    combined_text <- unlist(combined_text)  # Flatten the list to a character vector
    
    # Define output filename
    output_file <- file.path(output_folder, paste0("ch_batch_", i, ".txt"))
    
    # Write to new file
    writeLines(combined_text, output_file)
    
    cat("Created:", output_file, "\n")
  }
}

# Example usage:
combine_chapters("C:/Users/cobet/Documents/corpus_files/ChaptersOfLeVicomte/", "C:/Users/cobet/Documents/corpus_files/CombinedChaptersOfLeVicomte/")

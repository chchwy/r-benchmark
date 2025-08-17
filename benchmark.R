# benchmark.R - Main benchmark script for GitHub Actions
# R 2D Array Traversal Benchmark

cat("=== R ARRAY TRAVERSAL BENCHMARK ===\n")
cat("Starting benchmark at:", as.character(Sys.time()), "\n")
cat("R Version:", R.version.string, "\n")
cat("Platform:", Sys.info()["sysname"], Sys.info()["release"], "\n\n")

# Install and load required packages
tryCatch({
  if (!require(microbenchmark, quietly = TRUE)) {
    install.packages("microbenchmark", repos = "https://cran.rstudio.com/")
    library(microbenchmark)
  }
  
  if (!require(ggplot2, quietly = TRUE)) {
    install.packages("ggplot2", repos = "https://cran.rstudio.com/")
    library(ggplot2)
  }
}, error = function(e) {
  cat("Error installing packages:", e$message, "\n")
  quit(status = 1)
})

cat("Required packages loaded successfully.\n\n")

# Function for Row Major Traversal
row_major_traversal <- function(matrix_data) {
  total_sum <- 0
  for (i in 1:nrow(matrix_data)) {
    for (j in 1:ncol(matrix_data)) {
      total_sum <- total_sum + matrix_data[i, j]
    }
  }
  return(total_sum)
}

# Function for Column Major Traversal  
column_major_traversal <- function(matrix_data) {
  total_sum <- 0
  for (j in 1:ncol(matrix_data)) {
    for (i in 1:nrow(matrix_data)) {
      total_sum <- total_sum + matrix_data[i, j]
    }
  }
  return(total_sum)
}

# Create test matrices of different sizes
cat("=== BENCHMARK: Row Major vs Column Major Traversal ===\n\n")

# Reduced sizes for CI environment to avoid timeouts
matrix_sizes <- list(
  small = c(100, 100),
  medium = c(300, 300),  # Reduced from 500x500
  large = c(500, 500)    # Reduced from 1000x1000
)

results_summary <- data.frame()
all_results <- list()

for (size_name in names(matrix_sizes)) {
  rows <- matrix_sizes[[size_name]][1]
  cols <- matrix_sizes[[size_name]][2]
  
  cat("Testing with", size_name, "matrix (", rows, "x", cols, "):\n")
  
  # Create test matrix
  set.seed(42)  # For reproducible results
  test_matrix <- matrix(runif(rows * cols), nrow = rows, ncol = cols)
  
  # Benchmark both methods with fewer repetitions for CI
  tryCatch({
    benchmark_result <- microbenchmark(
      row_major = row_major_traversal(test_matrix),
      column_major = column_major_traversal(test_matrix),
      times = 30,  # Reduced from 100 for faster CI
      unit = "ms"
    )
    
    # Display results
    print(benchmark_result)
    cat("\n")
    
    # Store results
    all_results[[size_name]] <- benchmark_result
    
    # Store summary statistics
    summary_stats <- summary(benchmark_result)
    for (i in 1:nrow(summary_stats)) {
      results_summary <- rbind(results_summary, data.frame(
        matrix_size = size_name,
        method = as.character(summary_stats$expr[i]),
        mean_time = summary_stats$mean[i],
        median_time = summary_stats$median[i],
        min_time = summary_stats$min[i],
        max_time = summary_stats$max[i],
        stringsAsFactors = FALSE
      ))
    }
    
  }, error = function(e) {
    cat("Error benchmarking", size_name, "matrix:", e$message, "\n")
  })
}

# Overall comparison summary
cat("=== SUMMARY COMPARISON ===\n")
if (nrow(results_summary) > 0) {
  print(results_summary)
  cat("\n")
  
  # Performance ratio analysis
  cat("=== PERFORMANCE RATIO ANALYSIS ===\n")
  for (size_name in names(matrix_sizes)) {
    size_data <- results_summary[results_summary$matrix_size == size_name, ]
    if (nrow(size_data) >= 2) {
      row_major_time <- size_data[size_data$method == "row_major", "mean_time"]
      column_major_time <- size_data[size_data$method == "column_major", "mean_time"]
      
      if (length(row_major_time) > 0 && length(column_major_time) > 0) {
        if (row_major_time < column_major_time) {
          ratio <- column_major_time / row_major_time
          faster_method <- "Row Major"
        } else {
          ratio <- row_major_time / column_major_time
          faster_method <- "Column Major"
        }
        
        cat(size_name, "matrix:", faster_method, "is", sprintf("%.2f", ratio), "times faster\n")
      }
    }
  }
} else {
  cat("No benchmark results available.\n")
}

# Memory layout explanation
cat("\n=== WHY THE DIFFERENCE? ===\n")
cat("R stores matrices in COLUMN-MAJOR order in memory.\n")
cat("This means elements in the same column are stored contiguously.\n")
cat("Column-major traversal should generally be faster due to:\n")
cat("1. Better cache locality (accessing adjacent memory locations)\n")
cat("2. Fewer cache misses when accessing sequential elements\n")
cat("3. More efficient memory access patterns\n\n")

# Verify both methods produce same results
test_matrix_small <- matrix(1:20, nrow = 4, ncol = 5)
cat("=== CORRECTNESS VERIFICATION ===\n")
cat("Test matrix:\n")
print(test_matrix_small)
cat("\nRow Major Sum:", row_major_traversal(test_matrix_small), "\n")
cat("Column Major Sum:", column_major_traversal(test_matrix_small), "\n")
cat("Built-in Sum:", sum(test_matrix_small), "\n")

results_match <- (row_major_traversal(test_matrix_small) == column_major_traversal(test_matrix_small) &&
                  row_major_traversal(test_matrix_small) == sum(test_matrix_small))
cat("All methods produce the same result:", results_match, "\n")

if (!results_match) {
  cat("ERROR: Results do not match!\n")
  quit(status = 1)
}

# System information
cat("\n=== SYSTEM INFORMATION ===\n")
cat("CPU cores:", parallel::detectCores(), "\n")
cat("Memory info:\n")
if (Sys.info()["sysname"] == "Linux") {
  system("free -h", ignore.stderr = TRUE)
}

cat("\n=== BENCHMARK COMPLETED ===\n")
cat("Finished at:", as.character(Sys.time()), "\n")

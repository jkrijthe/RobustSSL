detectBatchCPUs <- function() { 
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) 
  if (is.na(ncores)) { 
    ncores <- as.integer(Sys.getenv("SLURM_JOB_CPUS_PER_NODE")) 
  } 
  if (is.na(ncores)) {
    ncores <- parallel::detectCores()
  } 
  if (is.na(ncores)) {
    stop("Can't detect number of cores.")
  } 
  return(ncores) 
}

print(detectBatchCPUs)
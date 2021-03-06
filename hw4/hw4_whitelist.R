allowed_files = c("hw4.Rmd",
                  "README.md",
                  "wercker.yml",
                  "hw4.Rproj",
                  "Makefile",
                  "get_lq.R",
                  "parse_lq.R",
                  "get_dennys.R",
                  "parse_dennys.R")

files = dir()
disallowed_files = files[!(files %in% allowed_files)]

if (length(disallowed_files != 0))
{
  cat("Disallowed files found:\n")
  cat("  (remove the following files from your repo)\n\n")

  for(file in disallowed_files)
    cat("*",file,"\n")

  quit("no",1,FALSE)
}

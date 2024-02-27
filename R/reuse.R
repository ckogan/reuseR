#' @export
reuseR <- function(fcall, repo = ".", verbose = F, debugFile = F, verify = F,
                   ignore = NULL, depends = NULL, capture_output = F,
                   attach = F) {
  call <- substitute(fcall)

  if(class(call) == "call") {
    d <- list()
    for(i in 1:length(call)) {
      if(!(any(i == ignore)))
        d[[length(d) + 1L]] <- eval(call[[i]], parent.frame())
    }

    if(!is.null(depends)) {
      for(i in 1:length(depends)) {
        d[[length(d) + 1L]] <- depends[[i]]
      }
    }

    for(i in 1:length(d)) {
      ## Formula
      if(rlang::is_formula(d[[i]])) d[[i]] <- as.character(d[[i]])
      ## Stanmodel
      if(class(d[[i]])[1] == "stanmodel") {
        d[[i]] <- d[[i]]@model_code
      }
      if(is.function(d[[i]])) d[[i]] <- as.character(body(d[[i]]))
    }

    dig_list <- sapply(d, digest)
    rnk <- rank(dig_list)
    dig_list <- sort(dig_list) # So argument order does not matter
    dig_str <- paste(dig_list, collapse = "")
    dig <- digest(dig_str, serialize = F)
  } else { #If just an object is passed, not a call
    if(!is.null(depends)) print("Depends not implemented for single object reuseR")
    d <- eval(call, parent.frame())
    dig <- digest(d)
  }

  name <- paste0("md5_", dig, ".RDS")
  name_output <- paste0("md5_", dig,"_output.txt")
  files <- list.files(repo)
  fmatch <- match(name, files)
  if(verify || is.na(fmatch)) {
    if(!capture_output) {
      obj <- fcall
    }
    else {
      captured_output <- capture.output(obj <- fcall)
      cat(captured_output, sep = "\n")
    }

    if(is.na(fmatch)) {
      saveRDS(obj, file = paste0(repo, "/", name))
      if(capture_output)
        cat(captured_output, file = paste0(repo, "/", name_output), sep = "\n", append = F)
    }
  }
  if(!is.na(fmatch))  {
    obj_loaded <- readRDS(paste0(repo, "/", files[fmatch]))
    if(capture_output) {
      captured_output <- readLines(paste0(repo, "/", name_output))
      cat(captured_output, sep = "\n")
    }
    if(!verify) {
      obj <- obj_loaded
    } else {
      is_identical <- all.equal(obj, obj_loaded)
      if(is_identical) {
        print(paste0("Verified generated object is same as ", files[fmatch]))
      } else {
        print(paste0("Generated object is NOT the same as ", files[fmatch], ". Returning NEW object, but not saving."))
      }
    }
  }

  if(verbose & debugFile & is.na(fmatch)) sink(paste0(repo, "/", paste0("md5_", dig, ".txt")))
  if(verbose) {
    if(is.na(fmatch))
      print(paste0("Writing to: ", repo, "/", name))
    else
      print(paste0("Match found for ", name))
    print(call)
    cat("Overall list: \n")
    print(dig_list[rnk])
    # cat("Overall string: \n")
    # print(dig_str)
    cat("Overall digest\n")
    print(dig)
  }
  if(verbose & debugFile & is.na(fmatch)) sink()

  if (attach) {
    list2env(obj, envir = .GlobalEnv)
  } else {
    return(obj)
  }
}

reuse_match <- function(dig, repo) {
  name <- paste0("md5_", dig, ".RDS")
  files <- list.files(repo)
  fmatch <- match(name, files)
}
#' @export
reuseR2 <- function(...) {
  reuseR(..., collapse = T)
}

#' @export
reuseRrepo_cleanup <- function(repo = ".", days_keep) {
  files <- list.files(repo)
  info <- file.info(files)
  dtime <- Sys.time() - days(days_keep)
  mtime <- info$mtime < dtime
  ctime <- info$ctime < dtime
  atime <- info$atime < dtime
  ix <- mtime & ctime & atime
  print(files[ix])
  dec <- readline("Ok to delete all above files in repo? (y/n)")
  if(dec == "y") {
    unlink(paste0(repo, "/", dec))
    cat(paste0(sum(ix), " files deleted from ", repo))
  }
}
# mtcars2 <- mtcars
# (out <- reuseR(
#   lm(mpg ~ wt, data = mtcars)
# ))
# (out <- reuseR(
#   lm(mpg ~ wt, data = mtcars2)
# ))

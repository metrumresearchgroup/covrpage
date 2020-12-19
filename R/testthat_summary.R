sprea <- function(x) {
  rhs <- x[1, c(1, 3, 4)]
  lhs <- x[[5]]
  names(lhs) <- x[[2]]
  lhs <- data.frame(t(lhs))
  data.frame(cbind(rhs, lhs), stringsAsFactors = FALSE)
}

gath <- function(i, x) {
  y <- x[, c(1:3, i)]
  nm <- names(y)[ncol(y)]
  y$status_type <- nm
  y$status <- y[[nm]]
  y <- y[, -4]
  y
}

sum_func <- function(x) {
  x <- do.call("rbind", lapply(4:7, gath, x))

  x <- data.frame(stats::aggregate(. ~ file + status_type, x, sum), stringsAsFactors = FALSE)

  x <- sprea(x)

  x$file <- sprintf("[%s](testthat/%s)", x$file, x$file)

  x
}

testthat_sum_short <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }

  x1 <- data.frame(x, stringsAsFactors = FALSE)

  x1 <- x1[, c("file", "nb", "real", "failed", "skipped", "error", "warning")]

  names(x1)[c(2, 3)] <- c("n", "time")

  for (i in names(x1)[-1])
    x1[[i]] <- as.numeric(x1[[i]])

  ret <- do.call("rbind", lapply(split(x1, x1$file), sum_func))

  ret <- emo_result(ret, "FAILED", type = "short")
  ret <- emo_result(ret, "SKIPPED", type = "short")
  ret <- emo_result(ret, "WARNING", type = "short")

  rownames(ret) <- NULL

  ret
}

testthat_sum_long <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }

  x1 <- data.frame(x, stringsAsFactors = FALSE)

  for (i in seq(1:ncol(x1)))
    if (is.logical(x1[[i]])) {
      x1[[i]] <- as.numeric(x1[[i]])
    }

  x1$pass <- as.numeric((apply(x1[, c("failed", "skipped", "warning", "error")], 1, sum)) == 0)

  x1$status <- NA

  for (i in 1:nrow(x1)) {
    if (x1$pass[i] == 1) {
      x1$status[i] <- "PASS"
    }

    if (x1$failed[i] != 0) {
      x1$status[i] <- "FAILED"
    }

    if (x1$error[i] != 0) {
      x1$status[i] <- "ERROR"
    }

    if (x1$skipped[i] != 0) {
      x1$status[i] <- "SKIPPED"
    }

    if (x1$warning[i] != 0) {
      x1$status[i] <- "WARNING"
    }
  }

  ret <- x1[, c("file", "context", "test", "status", "nb", "real")]

  names(ret)[c(5, 6)] <- c("n", "time")

  lines <- sapply(x, function(x) {
    ret <- sprintf("L%s", unique(c(x$results[[1]]$src[1], x$results[[1]]$src[3])))
    paste0(ret, collapse = "_")
  })

  ret$file <- sprintf("[%s](testthat/%s#%s)", ret$file, ret$file, lines)

  ret <- emo_result(ret, "FAILED", type = "long")
  ret <- emo_result(ret, "SKIPPED", type = "long")
  ret <- emo_result(ret, "WARNING", type = "long")

  ret
}

emo_result <- function(dat, status, type = "short") {
  if (type == "short") {
    idx <- dat[[tolower(status)]] > 0
  }

  if (type == "long") {
    idx <- dat$status == status
  }

  if (any(idx)) {
    if (!"icon" %in% names(dat)) {
      dat[["icon"]] <- ""
    }

    dat$icon[idx] <- paste0(dat$icon[idx], emos[[platform()]][[status]])
  }

  dat
}

enfram <- function(x, name = "name", value = "value") {
  nm <- names(x)
  names(x) <- NULL
  ret <- data.frame(name = nm, value = x, stringsAsFactors = FALSE)
  names(ret) <- c(name, value)
  ret
}

#' importFrom utils sessionInfo packageVersion
sinfo <- function() {
  
  x <- utils::sessionInfo()

  sinfo <- c(
    Version = x$R.version$version.string,
    Platform = x$platform,
    Running = x$running,
    Language = gsub("\\.(.*?)$", "", Sys.getlocale(category = "LC_COLLATE")),
    Timezone = Sys.timezone()
  )

  pkgs <- sapply(c("testthat", "covr", "covrpage"), 
                 FUN = function(x){
                   as.character(utils::packageVersion(x))
                 })

  sinfo <- enfram(sinfo, name = "Field", value = "Value")
  
  sinfo <- build_icon(sinfo)
  
  pkgs <- enfram(pkgs, name = "Package", value = "Version")

  list(info = sinfo, pkgs = pkgs)
}

#' @title Re-export magrittr pipe operators
#' @description magrittr pipe operators
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

platform <- function() {
  if (.Platform[["OS.type"]] == "windows") {
    "windows"
  } else {
    "unix"
  }
}

is_travis <- function() {
  identical(Sys.getenv("TRAVIS"), "true")
}

travis_image <- function() {
  sprintf(
    '<a href="%s" target="_blank"><span title="Built on Travis">![](%s)</span></a>',
    Sys.getenv("TRAVIS_JOB_WEB_URL"),
    "https://github.com/yonicd/covrpage/blob/master/inst/logo/travis.png?raw=true"
  )
}

is_git <- function(){
  system('git rev-parse --is-inside-work-tree',intern = TRUE)=='true'
}
is_travis <- function() {
  identical(Sys.getenv("TRAVIS"), "true")
}

is_gh <- function() {
  nzchar(Sys.getenv("GITHUB_ACTION"))
}

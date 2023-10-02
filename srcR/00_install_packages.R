#TODO: create load package fn. using system.file

is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}

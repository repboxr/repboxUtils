
#' A function that deals with failures depending on the on_fail action
make_fail_fun = function(fail_action) {
  if (fail_action=="error") {
    fail_fun = function(...) stop(paste0(...),call. = FALSE)
  } else if (fail_action=="warn") {
    fail_fun = function(...) warning(paste0(...), call. = FALSE)
  } else if (fail_action=="msg") {
    fail_fun = function(...) cat(paste0("\n",...,"\n"))
  } else {
    fail_fun = function(...) {}
  }
  fail_fun
}

repbox_set_current_project_dir = function(project_dir) {
  options(repbox.current.project.dir = project_dir)
}

repbox_get_current_project_dir = function() {
  getOption("repbox.current.project.dir")
}

#' A function that deals with failures depending on the on_fail action
repbox_problem = function(msg, type, fail_action, project_dir=repbox_get_current_project_dir(), extra=list()) {
  restore.point("repbox_problem")

  if (is.null(project_dir)) {
    stop("project_dir not specfied")
  }
  prob = list(type=type,msg=msg, extra=extra)

  problem_dir = file.path(project_dir,"problems")
  if (!dir.exists(problem_dir)) {
    dir.create(problem_dir,recursive = TRUE)
  }
  num_files = length(list.files(problem_dir))
  prob_num = num_files +1
  saveRDS(prob, paste0(problem_dir,"/problem_", prob_num,"__", type,".Rds"))

  if (fail_action=="error") {
    stop(msg,call. = FALSE)
  } else if (fail_action=="warn") {
    warning(msg, call. = FALSE)
  } else if (fail_action=="msg") {
    cat(paste0("\n",msg,"\n"))
  }
  invisible(prob)
}


try_catch_repbox_problems <- function(expr,project_dir, warn_action="msg", err_action="msg") {
  warn_li = list()
  err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- paste0(as.character(e), collapse="\n")#captureOutput(print(e))#
      NULL
    }), warning=function(w) {
      warn_li[[length(warn_li)+1]] <<- captureOutput(print(w), collapse="\n")#as.character(w)
      invokeRestart("muffleWarning")
    })

  if (!is.null(err)) {
    repbox_problem(err,"error", fail_action=err_action, project_dir=project_dir)
  }
  for (w in warn_li) {
    repbox_problem(w,"warning", fail_action=warn_action, project_dir=project_dir)
  }

  list(value=value, warnings=warn_li, error=err)
}


tryCatchWarningsAndError <- function(expr) {
  warn_li = list()
  err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn_li[[length(warn_li)+1]] <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warnings=warn_li, error=err)
}

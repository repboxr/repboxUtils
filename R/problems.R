
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
    dir.create(problem_dir)
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

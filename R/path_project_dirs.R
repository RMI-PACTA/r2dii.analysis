#' Create paths to common project directories
#'
#' This function creates paths that represent the structure of a common
#' "analysis project".
#'
#' @param project String giving the name of a project directory.
#' @param parent String giving the path or the parent directory of the
#'   `project`.
#'
#' @seealso [fs::dir_create()]
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' path_project_dirs("a-project")
#'
#' # Use `fs::dir_crete() to create all directories in one step
#' path_to_project_dirs <- path_project_dirs("a-project", parent = tempdir())
#' fs::dir_create(path_to_project_dirs)
#'
#' all(fs::dir_exists(path_to_project_dirs))
#'
#' # Cleanup
#' fs::dir_delete(path_to_project_dirs)
path_project_dirs <- function(project, parent = NULL) {
  path_dir <- path_proj(project, parent)
  fs::path(path_dir, get_nested_dirs())
}

get_nested_dirs <- function() {
  path <- r2dii.utils::path_dropbox_2dii(
    "PortCheck_v2", "00_Administration", "10_Folder_Structures", "StartFolders"
  )
  fs::path_file(fs::dir_ls(path))
}

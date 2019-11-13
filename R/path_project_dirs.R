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
#' project_paths <- path_project_dirs("a-project", parent = tempdir())
#' fs::dir_create(project_paths)
#'
#' all(fs::dir_exists(project_paths))
#'
#' # Cleanup
#' fs::dir_delete(project_paths)
path_project_dirs <- function(project, parent = NULL) {
  project_path <- path_proj(project = project, parent = parent)
  nested_dirs <- fs::path_file(fs::dir_ls(get_nested_dirs()))
  fs::path(project_path, nested_dirs)
}

get_nested_dirs <- function() {
  r2dii.utils::path_dropbox_2dii(
    "PortCheck_v2", "00_Administration", "10_Folder_Structures", "StartFolders"
  )
}

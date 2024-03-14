external get_executable_path : unit -> string option = "fl_executable_path"

let executable_path = lazy (get_executable_path ())

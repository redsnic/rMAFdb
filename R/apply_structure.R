

import_table <- function (table_path, structure_path){
  table_reader <- chunk_reader(table_path)
  structure <- load_structure(structure_path, from.file = TRUE)

  raw_df <- table_reader$read(1) # skip header
  raw_df <- table_reader$read()
  raw_df <- tibble(raw = raw_df)
  table_reader$close()

  run_commands(raw_df, structure)
}


run_commands <- function(tab, struct){
  print(paste("INFO:",struct$type))
  if(struct$type == "init"){
    eval(parse(text=struct$Rcode))
    run_commands(tab, struct$content)
  }else if(struct$type == "table"){
    tab <- separate(tab, raw, map_chr(struct$content, ~.$name), sep=struct$sep)
    print(tab)
    purrr::walk(struct$content, ~ run_commands(tab, .))
  }else{

  }

}

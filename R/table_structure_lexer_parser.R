library(rly)
library(purrr)

# parser and lexer to read the structure of the given MAF (or csv/tsv file)
# implementation is based on the rly package

TOKENS <- c('TABLE', 'LIST', 'DESCRIPTOR', 'SEPARATOR', 'NAME', 'KEY', 'ANYTHING', 'DEFAULT', 'PROCESS', 'RCODE')
LITERALS <- c('{', '}', ':', ',', '[', ']')

Lexer <- R6::R6Class(
  "Lexer",
  public = list(
    tokens = TOKENS,
    t_RCODE = function(re='@begin@(?<=@begin@)[\\s\\S]*?(?=@end@)@end@', t){
      # match a string between "", supports escaping
      t$value <- gsub('^@begin@\\{', '', t$value)
      t$value <- gsub('\\}@end@$', '', t$value)
      return(t)
    },
    t_ANYTHING = function(re='"([^"]|(?<=\\\\)")*"', t){
      # match a string between "", supports escaping
      vec_t <- strsplit(t$value, "")[[1]]
      t$value <- paste( vec_t[c(-1,-length(vec_t))], collapse = "" )
      t$value <- gsub('\\"', '"', t$value) # manage escaping
      return(t)
    },
    literals = LITERALS,
    t_TABLE = "table(?![a-zA-Z])",
    t_LIST = "list(?![a-zA-Z])",
    t_DESCRIPTOR = "(indexed(?![a-zA-Z])|basic(?![a-zA-Z]))",
    t_KEY = "key(?![a-zA-Z])",
    t_DEFAULT = "default(?![a-zA-Z])",
    t_PROCESS = "process(?![a-zA-Z])",
    t_SEPARATOR = function(re="\\(.\\)", t){
      vec_t <- strsplit(t$value, "")[[1]]
      if(length(vec_t) > 3){ # useless
        stop(paste("ERROR at line", t$lexer$lineno, ":", "separator", t$value, "must be a single character"))
      }
      t$value <- paste( vec_t[c(-1,-length(vec_t))], collapse = "" )
      return(t)
    },
    t_NAME = '[a-zA-Z_][a-zA-Z0-9]*',
    # comments start with '##'
    t_comment = function(re='##[^\\n]*', t) {
      return(NULL)
    },
    t_ignore = " \t",
    # keep track of line number for errors
    t_newline = function(re='\\n+', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    # lexical error
    t_error = function(t){
      stop(paste("ERROR at line", t$lexer$lineno, ": illegal character", t$value[1]))
    }
  )
)

Parser <- R6::R6Class(
  "Parser",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    # Parsing rules
    precedence = list(c('left', ',')),
    p_init = function(
      doc='init : RCODE main
                | main', p){
      tryCatch({
        p$set(1, list(type = "init", Rcode = p$get(2), content = p$get(3)))
      }, error = function(e){
        p$set(1, list(type = "init", Rcode = "", content = p$get(2)))
      })

    },
    p_main = function(
      doc='main : TABLE SEPARATOR NAME \'{\' content \'}\'', p){
      p$set(1, list(name = p$get(4), sep = p$get(3), type = p$get(2), content = p$get(6)))
    },
    p_content = function(
      doc="content : element \',\' content
                   | element",
      p
    ){
      tryCatch({
        p$set(1, c(list(p$get(2)), p$get(4)))
      }, error = function(e){
        p$set(1, list(p$get(2)))
      })
    },
    p_element = function(
      doc = "element : value
                     | table
                     | list
                     | key
                     | process",
      p
    ){
      p$set(1, p$get(2))
    },
    p_value = function(
      doc = "value : DESCRIPTOR NAME \':\' NAME
                   | NAME \':\' NAME",
      p
    ){
      if(p$get(2) %in% c("indexed", "basic")){
        p$set(1, list(name = p$get(3), type = p$get(2), vartype = p$get(5)))
      }else{
        p$set(1, list(name = p$get(2), type = "basic", vartype = p$get(4)))
      }
    },
    p_table = function(
      doc = "table : TABLE SEPARATOR NAME \'{\' content \'}\'",
      p
    ){
      p$set(1, list(name = p$get(4), sep = p$get(3), type = p$get(2), content = p$get(6)))
    },
    p_list = function(
      doc = "list : LIST SEPARATOR NAME \'{\' content \'}\'",
      p
    ){
      p$set(1, list(name = p$get(4), sep = p$get(3), type = p$get(2), content = p$get(6)))
    },
    p_key = function(
      doc = "key : KEY \':\' NAME SEPARATOR NAME \'[\' keysub \']\' keydefault
                 | KEY \':\' NAME SEPARATOR NAME keydefault",
      p
    ){
      if(p$get(7) == "["){
        p$set(1, list(name = p$get(6), sep = p$get(5), type = p$get(2), content = p$get(8), default=p$get(10), keytype=p$get(4)))
      }else{
        p$set(1, list(name = p$get(6), sep = p$get(5), type = p$get(2), content = list(), default=p$get(7), keytype=p$get(4)))
      }
    },
    p_keysub = function(
      doc = "keysub : keysubelement
                    | keysubelement \',\' keysub",
      p
    ){
      tryCatch({
        p$set(1, c(list(p$get(2)), p$get(4)))
      }, error = function(e){
        p$set(1, list(p$get(2))) # empty list
      })
    },
    p_keysubelement = function(
      doc = "keysubelement : ANYTHING NAME \'{\' content \'}\'
                           | ANYTHING SEPARATOR NAME \'{\' content \'}\'",
      p
    ){
      tryCatch({
        test <- p$get(7) # raise error if no separator is given (and use the other rule)
        p$set(1, list(keyID = p$get(2), name = p$get(4), content = p$get(6), separator = p$get(3) ))
      }, error = function(e){
        p$set(1, list(keyID = p$get(2), name = p$get(3), content = p$get(5), separator = ""))
      })
    },
    p_keydefault = function(
      doc = "keydefault : DEFAULT ANYTHING",
      p
    ){
      p$set(1, p$get(3))
    },
    p_process = function(
      # apply an R function to the rows
      doc = "process : PROCESS RCODE NAME \'{\' content \'}\'",
      p
    ){
      p$set(1, list(type = p$get(2), name = p$get(4), content = p$get(6), Rfunction = p$get(3)))
    },
    p_error = function(p) {
      # manage syntax errors
      if(is.null(p)) stop("ERROR: Syntax error at EOF")
      else           stop(paste("ERROR: Syntax error at", p$value))
    }
  )
)

#' Lex and parse table structure
#'
#' Creates a nested lists representing the structure the complex table
#' that we want to simplify and load into a database.
#'
#' @param structure the text describing the table
#' @param from.file if TRUE, input is interpretes as a path
#'
#' @return nested list describing the table
#'
#' @examples
#' load_structure('./inst/extdata/test.treatments')
#'
#' @export load_structure
load_structure <- function(structure, from.file = FALSE){
  lexer <- rly::lex(Lexer)
  parser <- rly::yacc(Parser)
  if(from.file){
    txt <- paste(readLines(structure), collapse="\n")
  }else{
    txt <- structure
  }
  parser$parse(txt, lexer)
}


print_tree <- function(tree, level=0){
  # base case
  if(tree$type == "init"){
    cat(
      paste(
        "@begin@{",
        tree$Rcode,
        "}@end@\n\n",
        sep="")
    )
    print_tree(tree$content)
  }else if(tree$type %in% c("indexed","basic")){
    cat(
      paste(
        paste(rep("  ", times=level), collapse =""),
        tree$type, " ",
        tree$name, ":",
        tree$vartype,
        sep = ""
      )
    )
  }else if(tree$type == "key"){
    # open
    cat(
      paste(
        paste(rep("  ", times=level), collapse=""),
        tree$type, ":", tree$keytype, " ",
        "(", gsub('"', '', (capture.output(str(tree$sep)) %>% strsplit(" "))[[1]][3]), ")",
        " ",
        tree$name,
        sep = ""
      )
    )
    # print subtables
    if(length(tree$content) > 0){
      # open
      cat("\n",
        paste(
          paste(rep("  ", times=level), collapse=""),
          "[\n"
        )
      )
      for(i in seq(1,length(tree$content))){
        # open
        cat(
          paste(
            paste(rep("  ", times=level + 2), collapse=""),
            tree$content[[i]]$keyID %>% print_quoted, " ",
            ifelse(tree$content[[i]]$sep == "",
                   "",
                   paste("(", gsub('"', '', (capture.output(str(tree$content[[i]]$sep)) %>% strsplit(" "))[[1]][3]),
                         ") ",
                         sep="")),
            tree$content[[i]]$name, " ",
            ifelse(length(tree$content[[i]])>0,"{\n",""),
            sep = ""
          )
        )
        if(length(tree$content[[i]])>0){
          for(j in seq(1, length(tree$content[[i]]$content))){
            print_tree(tree$content[[i]]$content[[j]], level = level + 3)
            if(j != length(tree$content[[i]]$content)){
              cat(",\n")
            }else{
              cat("\n")
            }
          }
        }
        # close
        cat(
          paste(
            paste(rep("  ", times=level + 2), collapse=""),
            ifelse(length(tree$content[[i]])>0,"}",""),
            ifelse(i == length(tree$content), "\n", ",\n"),
            sep = ""
          )
        )
      }
      # close
      cat(
        paste(
          paste(rep("  ", times=level), collapse=""),
          "]"
        )
      )
    }
    # close
    cat(
      paste(
        " default",  tree$default %>% print_quoted
      )
    )
  }else if(tree$type == "process"){
    # open
    cat(
      paste(
        paste(rep("  ", times=level), collapse=""),
        tree$type, " ",
        "@begin@{",
        tree$Rfunction,
        "}@end@ ",
        tree$name, " ",
        "{",
        "\n",
        sep = ""
      )
    )
    # print content
    for(i in seq(1, length(tree$content))){
      print_tree(tree$content[[i]], level = level + 1)
      if(i != length(tree$content)){
        cat(",\n")
      }else{
        cat("\n")
      }
    }
    # close
    cat(
      paste(
        paste(rep("  ", times=level), collapse=""),
        "}",
        sep = ""
      )
    )
  }else{
    # open
    cat(
      paste(
        paste(rep("  ", times=level), collapse=""),
        tree$type, " ",
        "(", gsub('"', '', (capture.output(str(tree$sep)) %>% strsplit(" "))[[1]][3]), ") ",
        tree$name, " ",
        "{",
        "\n",
        sep = ""
      )
    )
    # print content
    for(i in seq(1, length(tree$content))){
      print_tree(tree$content[[i]], level = level + 1)
      if(i != length(tree$content)){
        cat(",\n")
      }else{
        cat("\n")
      }
    }
    # close
    cat(
      paste(
        paste(rep("  ", times=level), collapse=""),
        "}",
        sep = ""
      )
    )
  }
}


print_quoted <- function(string){
  temp <- string %>%
    cat %>%
    capture.output() %>%
    print() %>%
    capture.output()
  temp <- gsub("^\\[1\\] ", "", temp)
  cat(temp) %>% capture.output()
}




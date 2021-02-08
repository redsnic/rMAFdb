// Reader.h

#ifndef MAF_READER_READER
#define MAF_READER_READER

#include <Rcpp.h>
using namespace Rcpp;
#include <strings.h>

#include "Table.h" 
#include "Utils.h"


CharacterVector maf_db_reader(CharacterVector table_name, CharacterVector text, CharacterVector header, 
IntegerVector rules, int starting_point); 
void separe_and_get_query(text_table& main_table, std::string& output_query, std::string colname, int rule);
void add_priority_index(text_table* table);

#endif 
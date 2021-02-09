#ifndef MAF_READER_UTILS
#define MAF_READER_UTILS

#include <Rcpp.h>
#include "Table.h" 
using namespace Rcpp;

std::vector<field*>* tokenize(field* text, char sep);
std::vector<field*>* tokenize_bracket(field* text);
bool locate_and_test(std::string column, int rule, std::vector<std::string>* columns, std::vector<int>* rules);

#endif  
// Table.h

#ifndef MAF_READER_TABLE
#define MAF_READER_TABLE
 
#include <Rcpp.h>
#include <stdexcept>
#include "Field.h"
#include "Utils.h"
using namespace Rcpp;
 
class text_table{
public: 
  text_table(std::vector<std::string>* header, std::vector<int>* rules, std::string name, int starting_point); 
  int nrow(){return this->content->size();}
  int ncol(){return this->header->size();}
  int getDBindex(int i){return this->index[i] + 1 + this->starting_point;}
  void add(field* next_field);
  std::string echo();
  ~text_table(); //TODO manage header and such...
  field* at(int row, int col);
  text_table* separe_rows(std::string colname, std::string name, char sep);
  std::vector<int> index; 
  std::vector<int> extra_index;
  bool use_extra_index = false;
  text_table* separe_cols(std::string colname, std::vector<std::string>* new_header, std::vector<int>* new_rules, std::string name, char sep);
  text_table* kv_separe(std::string colname, int key_rule, int value_rule, std::string name, char sep);
  text_table* kv_merge(std::string colname1, std::string colname2, int key_rule, int value_rule, std::string name, char sep1, char sep2);
  text_table* separe_vcf_info_field(std::string colname, int key_rule, int value_rule, std::string name, char sep);
  text_table* separe_cols_brackets(std::string colname, std::vector<std::string>* new_header, std::vector<int>* new_rules, std::string name);
  text_table* brackets_separe(std::string colname, std::string name);
private: 
  std::vector<std::vector<field*>*>* content;
  std::vector<std::string>* header;
  std::vector<int>* rules;
  std::string name;
  int line;
  int col;
  int starting_point;
};

#endif 
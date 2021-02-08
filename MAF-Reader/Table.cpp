#include "Table.h"

/*
 * RULES:
 * 1: quote
 * 2: do not quote
 */

/**
 * Text table constructor 
 * 
 * this class is useful to manage a fixed column number table 
 * based on an original text file, like a csv or a tsv.
 * Can also be used to operate recursively on character separated fields.
 * 
 * @param header sequence of column names
 * @param rules sequence of rules (quoting etc.)
 * @param name table name
 * @param starting_point starting index for the db_index additional column
 * 
 */
text_table::text_table(std::vector<std::string>* header, std::vector<int>* rules, std::string name, int starting_point){
  assert(header->size() == rules->size()); 
  this->header = header;
  this->rules = rules;
  this->content = new std::vector<std::vector<field*>*>();  
  this->line = -1; /* no line exist */ 
  this->col = 0;  
  this->name = name;
  this->starting_point = starting_point;
  this->index = std::vector<int>();
  this->extra_index = std::vector<int>();
  this->use_extra_index = false;
}  

/**
 * add a field to the table
 * 
 * also manages the passage to the next row 
 * 
 * @param next field to be added
 */
void text_table::add(field* next){
  if(this->content->size() == 0 || this->content->at(this->line)->size() >= this->ncol()){
    this->content->push_back(new std::vector<field*>());
    this->col = 0;
    this->line++;
  }
  if(this->col == 0){ // set up index when at the beginning of a row
    if(starting_point>=0){
      this->index.push_back(starting_point+this->index.size()+1);
      this->extra_index.push_back(0);
    }else{
      this->index.push_back(0); // something else will set the indexes
      this->extra_index.push_back(0);
    }
  }
  this->content->at(this->line)->push_back(next); 
  /* apply rules (quoting in this case)*/
  if(this->rules->at(this->col) == 1){
    this->content->at(this->line)->at(this->col)->quote(); 
  }else if(this->rules->at(this->col) == 2){
    this->content->at(this->line)->at(this->col)->unquote();
  } 
  this->col++;
}

/**
 * distructor 
 */
text_table::~text_table(){ 
  for(int i = 0; i<this->nrow(); i++){
    for(int j = 0; j<this->ncol(); j++){
      delete(this->content->at(i)->at(j));
    }
    delete(this->content->at(i));
  }
  delete(this->content);
}


/**
 * Prepare inserion query
 * 
 * The query is tested for PostgeSQL
 * 
 * @return insertion query for this table
 */
std::string text_table::echo(){
  if(content->size() == 0) return ""; /* empty table */
  std::string out = "INSERT INTO ";
  out.append(this->name);

  out.append(" VALUES \n");
  for(int i = 0; i<this->nrow(); i++){
    out.push_back('(');
    out.append(std::to_string(this->index.at(i))); // add DB_INDEX
    out.push_back(',');
    if(this->use_extra_index){
      out.append(std::to_string(this->extra_index.at(i))); // add EXTRA_INDEX
      out.push_back(',');
    }
    for(int j = 0; j<this->ncol(); j++){
      if(this->rules->at(j) == 0) continue; // skip masked (rule 0) columns
      out.append(this->content->at(i)->at(j)->echo());
      if(j+1!=this->ncol()){ // not last column
        out.push_back(',');
      }
    }
    out.push_back(')');
    if(i+1 == this->nrow()){ // last line
      out.push_back(';');
      out.push_back('\n');
    }else{
      out.push_back(',');
    }
  }
  return out;
} 

/**
 * get field at given position 
 * 
 * @param row
 * @param col
 * 
 */
field* text_table::at(int row, int col){
  return this->content->at(row)->at(col);
}


/**
 * 
 */
text_table* text_table::separe_rows(std::string colname, std::string name, char sep){ // int rule
  auto pos_it = std::find(this->header->begin(),
                          this->header->end(), colname);
  if(pos_it != this->header->end()){
    int col = std::distance(this->header->begin(), pos_it);
    
    auto new_header = new std::vector<std::string>();
    new_header->push_back(colname);
    
    auto new_rules = new std::vector<int>();
    new_rules->push_back(1); // int rule
    
    auto new_subtable = new text_table(
      new_header,
      new_rules,
      name,
      -1
    );
    
    // prepare table adding elements from the separation of each row
    int new_row_pos = 0;
    for(int i = 0; i<this->nrow(); i++){
      if(this->at(i,col)->length() == 0) continue;  /* skip null elements */
      auto row = tokenize(this->at(i,col), sep);
      for(auto el : *row){
        new_subtable->add(el);
        new_subtable->index.at(new_row_pos) = this->index.at(i); 
        new_row_pos++;
      }
      delete(row); // remember to delete pointer array
    }
    
    return new_subtable;
    
  }else{
    Rcerr << "Cannot separe_rows on column " << colname << ". Column not found.\n";
    throw 1;
  }
}



/**
 * 
 */
text_table* text_table::separe_cols(std::string colname, std::vector<std::string>* new_header, std::vector<int>* new_rules, std::string name, char sep){ // int rule
  auto pos_it = std::find(this->header->begin(),
                          this->header->end(), colname);
  if(pos_it != this->header->end()){
    int col = std::distance(this->header->begin(), pos_it);
    
    auto new_subtable = new text_table(
      new_header,
      new_rules,
      name,
      -1
    ); 
    
    // prepare table adding elements from the separation of each row
    int new_row_pos = 0;
    for(int i = 0; i<this->nrow(); i++){
      auto row = tokenize(this->at(i,col), sep);
      for(auto el : *row){
        new_subtable->add(el);
      }
      new_subtable->index.at(i) = this->index.at(i); 
      delete(row); // remember to delete pointer array
    }
    
    return new_subtable;
    
  }else{
    Rcerr << "Cannot separe_rows on column " << colname << ". Column not found.\n";
    throw 1;
  }
}


text_table* text_table::kv_separe(std::string colname, int key_rule, int value_rule, std::string name, char sep){ 
  auto header_splitted_cols = new std::vector<std::string>();
  header_splitted_cols->push_back("key");
  header_splitted_cols->push_back("value");
  auto rules_splitted_cols = new std::vector<int>();
  rules_splitted_cols->push_back(key_rule);
  rules_splitted_cols->push_back(value_rule);
  return this->separe_cols(colname, header_splitted_cols, rules_splitted_cols, name, sep);
}


/**
 * 
 */ 
text_table* text_table::kv_merge(std::string colname1, std::string colname2, int key_rule, int value_rule, std::string name, char sep1, char sep2){ 
  auto pos_it1 = std::find(this->header->begin(),
                          this->header->end(), colname1);
  auto pos_it2 = std::find(this->header->begin(),
                          this->header->end(), colname2);
  if((pos_it1 != this->header->end()) && (pos_it2 != this->header->end())){
    int col1 = std::distance(this->header->begin(), pos_it1);
    int col2 = std::distance(this->header->begin(), pos_it2);
    
    auto new_header = new std::vector<std::string>();
    new_header->push_back("key");
    new_header->push_back("value");
    
    auto new_rules = new std::vector<int>();
    new_rules->push_back(key_rule);
    new_rules->push_back(value_rule);
    
    auto new_subtable = new text_table(
      new_header, 
      new_rules,
      name,
      -1
    );

    // prepare table adding elements from the separation of each row
    int new_row_pos = 0;
    for(int i = 0; i<this->nrow(); i++){
      
      auto row1 = tokenize(this->at(i,col1), sep1);
      auto row2 = tokenize(this->at(i,col2), sep2);
      
      for(int j=0; j<row1->size(); j++){
        new_subtable->add(row1->at(j));
        new_subtable->add(row2->at(j));
        new_subtable->index.at(new_row_pos) = this->index.at(i);
        new_row_pos++;
      
      }
      
      delete(row1); // remember to delete pointer array
      delete(row2); // remember to delete pointer array
    }
    
    return new_subtable;
    
  }else{
    Rcerr << "Cannot kv_merge on columns " << colname1 <<", "<< colname2<< ". A column was not found.\n";
    throw 1;
  }
}

std::string TRUE_STR = "true";

/**
 * 
 */
text_table* text_table::separe_vcf_info_field(std::string colname, int key_rule, int value_rule, std::string name, char sep){ 
  
  auto header_splitted_cols = new std::vector<std::string>();
  header_splitted_cols->push_back("key");
  header_splitted_cols->push_back("value");
  auto rules_splitted_cols = new std::vector<int>();
  rules_splitted_cols->push_back(key_rule);
  rules_splitted_cols->push_back(value_rule);
  
  auto pos_it = std::find(this->header->begin(),
                          this->header->end(), colname);
  if(pos_it != this->header->end()){ 
    int col = std::distance(this->header->begin(), pos_it);
    
    auto new_subtable = new text_table(
      header_splitted_cols,
      rules_splitted_cols,
      name,
      -1
    ); 

    // prepare table adding elements from the separation of each row
    int new_row_pos = 0;
    for(int i = 0; i<this->nrow(); i++){
      auto row = tokenize(this->at(i,col), sep);
      for(auto el : *row){
        new_subtable->add(el);
      }
      if(row->size() == 1){ // is a flag, add true!
        new_subtable->add(new field(0,4,&TRUE_STR));
      }
      new_subtable->index.at(i) = this->index.at(i); 
      delete(row); // remember to delete pointer array
    }
    
    return new_subtable;
    
  }else{
    Rcerr << "Cannot separe_rows on column " << colname << ". Column not found.\n";
    throw 1;
  }
}

/* --------------------- */
 


text_table* text_table::separe_cols_brackets(std::string colname, std::vector<std::string>* new_header, std::vector<int>* new_rules, std::string name){ // int rule
  auto pos_it = std::find(this->header->begin(),
                          this->header->end(), colname);
  if(pos_it != this->header->end()){
    int col = std::distance(this->header->begin(), pos_it);
     
    auto new_subtable = new text_table(
      new_header,
      new_rules,
      name,
      -1
    ); 
     
    // prepare table adding elements from the separation of each row
    int new_row_pos = 0; // do not keep null rows 
    for(int i = 0; i<this->nrow(); i++){
      auto row = tokenize_bracket(this->at(i,col));
      
      if(row->size() == 0){
        delete(row);
        continue;
      }
      for(auto el : *row){
        new_subtable->add(el);
      } 
      new_subtable->index.at(new_row_pos) = this->index.at(i); 
      new_row_pos++;
      delete(row); // remember to delete pointer array
    }
    
    return new_subtable;
     
  }else{
    Rcerr << "Cannot separe_rows on column " << colname << ". Column not found.\n";
    throw 1;
  }
}


text_table* text_table::brackets_separe(std::string colname, std::string name){ 
  auto header_splitted_cols = new std::vector<std::string>();
  header_splitted_cols->push_back("classification");
  header_splitted_cols->push_back("score");
  auto rules_splitted_cols = new std::vector<int>();
  rules_splitted_cols->push_back(1);
  rules_splitted_cols->push_back(2);
  return this->separe_cols_brackets(colname, header_splitted_cols, rules_splitted_cols, name);
}



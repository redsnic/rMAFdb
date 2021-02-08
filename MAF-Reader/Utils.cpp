#include "Utils.h"

/**
 * tokenize a string
 * 
 * this function creates a list of fields from another field 
 * using a given separator to determine segments.
 *  
 * @param text a text field (create a text field for the first line)
 * @param sep separator
 * 
 */  
std::vector<field*>* tokenize(field* text, char sep){
  int ini = 0;
  int cursor = 0;  
  std::vector<field*>* out_line = new std::vector<field*>();
  while(true){
    /* search separators and cut */
    if(cursor >= text->length() || text->at(cursor) == sep){
      field* new_field = new field(text->begin()+ini, text->begin()+cursor, text->source());
      out_line->push_back(new_field);
      ini = cursor+1;
    }
    /* exit at field end */
    if(cursor >= text->length()){ 
      break;
    }
    cursor++; 
  } 
  return out_line;
}
 
 
/**
 * locate (column) and test (rule equality)
 * 
 * Auxiliary function to check if a column has a certain rule by column name
 * 
 * @param column
 * @param rule 
 * @param columns (list of columns)
 * @param rules (list of rules)
 * 
 * @return true if target column has the target rule
 */
bool locate_and_test(std::string column, int rule, std::vector<std::string>* columns, std::vector<int>* rules){
  auto pos_it = std::find(columns->begin(),
                          columns->end(), column);
  if(pos_it != columns->end()){
    int col = std::distance(columns->begin(), pos_it); 
    if(rule == rules->at(col)){
      return true;
    }else{
      return false;
    }
  }else{
    return false;
  }
}
 
 
/**
* tokenize a string of the kind w1(w2)
* 
* this function creates a list of fields from another field 
* using a given separator to determine segments.
*  
* @param text a text field (create a text field for the first line)
* @param sep separator
* 
*/  
std::vector<field*>* tokenize_bracket(field* text){
  std::vector<field*>* out_line = new std::vector<field*>();
  int pos_open = -1;
  int pos_closed = -1;
  for(int i = 0; i<text->length(); i++){
    if(text->at(i) == '('){
      pos_open = i;
      break;
    }
  }
  for(int i = 0; i<text->length(); i++){
    if(text->at(i) == ')'){
      pos_closed = i;
      break;
    }
  }
  
  /* HIT */
  if(pos_open != -1 && pos_closed != -1){
    out_line->push_back(new  field(text->begin(), text->begin()+pos_open, text->source()));
    out_line->push_back(new  field(text->begin()+pos_open+1, text->begin()+pos_closed, text->source()));
  }
  
  return out_line;
} 
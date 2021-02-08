#include "Field.h"

// implemented in header file

/**
 * print this field in SQL format
 * 
 * also manages NAs
 */
std::string field::echo(){
  if(this->length()==0){
    return "NULL";
  }
  if(!to_quote){
    return this->reference->substr(this->start, this->length());
  } else{
    std::string out = "";
    out.push_back('\'');
    for(int i = 0; i<this->length(); i++){
      char c = this->at(i);
      out.push_back(c);
      if(c == '\''){ // escape ' 
        out.push_back('\'');
      }
    }
    out.push_back('\'');
    return out; 
  }
}

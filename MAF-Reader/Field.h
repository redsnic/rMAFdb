// Field.h

#ifndef MAF_READER_FIELD
#define MAF_READER_FIELD

#include <Rcpp.h>
#include <string.h>   
using namespace Rcpp;

// a field of a table (a text table)

class field{
public: 
  field(int start, int stop, std::string* reference){
    this->start = start; this->stop = stop; this->reference = reference;
    this->to_quote=false;
  }
  int begin(){return this->start;}
  int end(){return this->stop;}
  int length(){return this->stop-this->start;} // stop is excluded
  char at(int i){return this->reference->at(this->start+i);}
  void quote(){this->to_quote=true;}
  void unquote(){this->to_quote=false;};
  std::string* source(){return this->reference;};
  std::string echo();
private:
  int start,stop;
  std::string* reference;
  bool to_quote;
};

#endif 
#include "Reader.h"

/*
 * RULES:
 * 1: quote
 * 2: do not quote
 * 3: special (quote in main table)
 */

/**
 * Prepare queries to store a maf file in a database
 * 
 * @param table_name name of the db table
 * @param text group of maf lines (original text)
 * @param header names of the columns
 * @param rules list of actions to manage fields (quoting)
 * @param filter_header header for the "filter" columns
 * @param starting_point starting index for "db_index" column (primary key)
 * 
 * Function exported for R 
 */
//[[Rcpp::export]]
CharacterVector maf_db_reader(CharacterVector table_name, CharacterVector text, CharacterVector header, 
                              IntegerVector rules, int starting_point){
  
  /* manage R types */
  std::vector<std::string> _header = as<std::vector<std::string>>(header);  
  std::vector<int> _rules = as<std::vector<int>>(rules); 
  std::vector<std::string> _text = as<std::vector<std::string>>(text); 
  std::string _table_name = as<std::string>(table_name);   
  
  /* main table */
  text_table main_table = text_table(&_header, &_rules, _table_name, starting_point);
 
  // prepare main table
  for(int i = 0; i<_text.size(); i++){
    /* read it line by line */
    field* line_field = new field(0,_text[i].length(), &(_text[i]));

    int col_position = 0;
    auto line_tok = tokenize(line_field, '\t');
    for(field* cell : *(line_tok) ){
      if(_rules.at(col_position) == 1 || _rules.at(col_position) == 3){ /* quote if necessary */
        cell->quote();
      }
      main_table.add(cell);
      col_position++;
    }
    /* cleanup */
    delete(line_tok);
    delete(line_field);
  }
  
  //--------------------------------------------------------------------------------
  
  /* output */
  std::string output_query = ""; 
  output_query.append(main_table.echo());
  
  //--------------------------------------------------------------------------------

  /* separe rows, manage lists */ 
  
  if(locate_and_test("dbsnp_val_status", 3, &_header, &_rules)){
    auto dbsnp_val_status = main_table.separe_rows("dbsnp_val_status", "dbsnp_val_status", ';');
    output_query.append(dbsnp_val_status->echo());
    delete(dbsnp_val_status);
  }
    
  if(locate_and_test("consequence", 3, &_header, &_rules)){
    auto consequence = main_table.separe_rows("consequence", "consequence", ';');
    output_query.append(consequence->echo());
    delete(consequence);
  }

  if(locate_and_test("existing_variation", 3, &_header, &_rules)){
    auto existing_variation = main_table.separe_rows("existing_variation", "existing_variation", ';');
    output_query.append(existing_variation->echo());
    delete(existing_variation);
  }

  if(locate_and_test("refseq", 3, &_header, &_rules)){
    auto refseq = main_table.separe_rows("refseq", "refseq", ';');
    output_query.append(refseq->echo());
    delete(refseq);
  }

  if(locate_and_test("pubmed", 3, &_header, &_rules)){
    auto pubmed = main_table.separe_rows("pubmed", "pubmed", ';');
    output_query.append(pubmed->echo());
    delete(pubmed);
  }

  if(locate_and_test("filter", 3, &_header, &_rules)){
    auto filter = main_table.separe_rows("filter", "filter", ';');
    output_query.append(filter->echo());
    delete(filter);
  }
  
  if(locate_and_test("gdc_filter", 3, &_header, &_rules)){
    auto gdc_filter = main_table.separe_rows("gdc_filter", "gdc_filter", ';');
    output_query.append(gdc_filter->echo());
    delete(gdc_filter);
  }
  
  /* domains */
  if(locate_and_test("domains", 3, &_header, &_rules)){
    auto domains = main_table.separe_rows("domains", "domains", ';');
    auto domains_kv = domains->kv_separe("domains", 1, 1, "domains", ':');
    output_query.append(domains_kv->echo());
    delete(domains_kv);
    delete(domains);
  }
  
  /* vcf_info */
  if(locate_and_test("vcf_info", 3, &_header, &_rules)){
    auto vcf_info = main_table.separe_rows("vcf_info", "vcf_info", ';');
    auto vcf_info_kv = vcf_info->separe_vcf_info_field("vcf_info", 1, 1, "vcf_info", '=');
    output_query.append(vcf_info_kv->echo());
    delete(vcf_info_kv);
    delete(vcf_info);
  }
  
  /* kv_merge for tumor and normal genotypes */
  if(locate_and_test("vcf_tumor_gt", 3, &_header, &_rules)){
    auto vcf_tumor_gt = main_table.kv_merge("vcf_format", "vcf_tumor_gt", 1, 1, "vcf_tumor_gt", ':', ':'); 
    output_query.append(vcf_tumor_gt->echo());
    delete(vcf_tumor_gt);
  }
  
  if(locate_and_test("vcf_normal_gt", 3, &_header, &_rules)){
    auto vcf_normal_gt = main_table.kv_merge("vcf_format", "vcf_normal_gt", 1, 1, "vcf_normal_gt", ':', ':'); 
    output_query.append(vcf_normal_gt->echo());
    delete(vcf_normal_gt);
  }
  
  /* --- VEP TABLE --- */
  
  if(locate_and_test("all_effects", 3, &_header, &_rules)){
    auto all_effects = main_table.separe_rows("all_effects", "all_effects", ';'); 
    std::vector<std::string>* vep_header =
      new std::vector<std::string>({"symbol","consequence","hgvsp_short","transcript_id",
                                    "refseq","hgvsc","impact","canonical","sift","polyphen","strand"});
    auto vep_rules = new std::vector<int>({1,1,1,1,1,1,1,1,0,0,2});
    auto all_effects_table = all_effects->separe_cols("all_effects", vep_header, vep_rules, "all_effects", ',');
    add_priority_index(all_effects_table);
    output_query.append(all_effects_table->echo());
    // SIFT
    auto sift_vep = all_effects_table->brackets_separe("sift", "sift_vep");
    add_priority_index(sift_vep);
    output_query.append(sift_vep->echo());
    delete(sift_vep);
    // PolyPhen
    auto polyphen = all_effects_table->brackets_separe("polyphen", "polyphen_vep");
    add_priority_index(polyphen);
    output_query.append(polyphen->echo());
    delete(polyphen);
    delete(all_effects_table);
    delete(vep_header);
    delete(vep_rules);
    delete(all_effects);
  }
  
  /* OUTPUT */
  return wrap(output_query);
}

/**
 * Add priority index
 * 
 * Auxiliary function to add a "Priority Index". 
 * A priority index follows the db_index number (normally, the MAF line) 
 * position and keeps track of the order of the element in the field 
 * considered for the split in that row.
 * 
 * This is useful for example to maintain priority order of VEP annotations 
 * in the MAF file.
 * 
 * @param table table on which to add this priority index
 * 
 * @return
 */
void add_priority_index(text_table* table){
  table->use_extra_index = true;
   int current_extra_index = 1;
   int current_db_index = 0;
   for(int i = 0; i<table->nrow(); i++){
     if(table->getDBindex(i) == current_db_index){
       table->extra_index[i] = current_extra_index;
       current_extra_index++;
     }else{
       current_extra_index=1;
       current_db_index=table->getDBindex(i);
       table->extra_index[i] = current_extra_index;
       current_extra_index++;
     }
   }
}

/**
 * Simple testing procedure used for a small table and to show functionalities
 * (see .Rmd file)
 */
//[[Rcpp::export]]
CharacterVector test_MAFdb(CharacterVector table_name, CharacterVector text, CharacterVector header, 
                              IntegerVector rules, int starting_point){
  
  /* manage R types */
  std::vector<std::string> _header = as<std::vector<std::string>>(header);  
  std::vector<int> _rules = as<std::vector<int>>(rules); 
  std::vector<std::string> _text = as<std::vector<std::string>>(text); 
  std::string _table_name = as<std::string>(table_name);  
  
  /* main table */
  text_table main_table = text_table(&_header, &_rules, _table_name, starting_point);
  
  // prepare main table
  for(int i = 0; i<_text.size(); i++){
    /* read it line by line */
    field* line_field = new field(0,_text[i].length(), &(_text[i]));
    
    int col_position = 0;
    auto line_tok = tokenize(line_field, '\t');
    for(field* cell : *(line_tok) ){
      if(_rules.at(col_position) == 1 || _rules.at(col_position) == 3){ /* quote if necessary */
        cell->quote();
      }
      main_table.add(cell);
      col_position++;
    }
    /* cleanup */
    delete(line_tok);
    delete(line_field);
  }
  
  // /* output */
  std::string ouput_query = "";
  ouput_query.append(main_table.echo());
  
  auto splitted = main_table.separe_rows("list", "cosa", ';');

  ouput_query.append(splitted->echo());

  delete(splitted); 
  
  auto header_splitted_cols = new std::vector<std::string>();
  header_splitted_cols->push_back("C1");
  header_splitted_cols->push_back("C2");
  auto rules_splitted_cols = new std::vector<int>();
  rules_splitted_cols->push_back(1);
  rules_splitted_cols->push_back(2);
  auto splitted_cols = main_table.separe_cols("list_cols", header_splitted_cols, rules_splitted_cols,"nani", ';');
  
  ouput_query.append(splitted_cols->echo());
  
  delete(header_splitted_cols);
  delete(rules_splitted_cols);
  delete(splitted_cols);

  auto kv_test = main_table.kv_separe("kv_test", 1, 2, "kv1", '=');
  ouput_query.append(kv_test->echo());
  delete(kv_test);
  
  auto kv_test2 = main_table.kv_merge("keys", "values", 1, 2, "kv2", ':', ',');
  ouput_query.append(kv_test2->echo());
  // delete(kv_test2);
   
  /* OUTPUT */
  return wrap(ouput_query);
}


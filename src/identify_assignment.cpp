#include <Rcpp.h>


// [[Rcpp::export]]
bool is_assign(std::vector<std::string> code) {
  
  bool ret = false;
  
  if(code[0] == "`<-`") {
    ret = true;
  }
  
  return ret;
}

// [[Rcpp::export]]
std::string stor_var(std::vector<std::string> code) {
  return code[1];
}



int count_letters(const std::string& s)
{
  return std::count_if(s.begin(), s.end(), 
                       [](unsigned char c){ return std::isalpha(c); } 
  );
}

// [[Rcpp::export]]
std::vector<std::string> find_var_at_rhs(std::vector<std::string> code) {
  
  std::vector<std::string> functions{"`+`", "`-`", "`*`", "`/`"};
  
  std::vector<std::string> res(code.size());
  
  for(int i = 0; i < code.size(); i++) {
    if(count_letters(code[i]) > 0) {
      res[i] = code[i]; 
    } else{
      res[i] = "NA";
    }
  }
  
  return res;
}

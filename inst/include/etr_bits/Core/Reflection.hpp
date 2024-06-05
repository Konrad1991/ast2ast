#ifndef REFLECTION_H
#define REFLECTION_H

#include "Header.hpp"

namespace etr {
inline std::string demangle(const char *mangledName) {
  int status;
  char *demangled = abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);
  std::string result(demangled);
  std::free(demangled);
  return result;
}

template <typename T> inline void printType(T inp) {
  std::cout << demangle(typeid(inp).name()) << std::endl;
}

template <typename T> void inline printT() {
  std::cout << demangle(typeid(T).name()) << std::endl;
}

inline std::string convertIndentation(std::size_t idx) {
  std::string ret = "";
  for (std::size_t i = 0; i < idx; i++) {
    ret += "\t";
  }
  return ret;
}

template <typename T> inline void printAST(T inp) {
  std::string s = demangle(typeid(inp).name());
  std::vector<std::string> v;
  v.push_back("");
  std::size_t counter = 0;
  std::vector<int> indentationLevels(1, 0);
  for (std::size_t i = 0; i < s.length(); i++) {
    v[counter].push_back(s[i]);
    if (s[i] == '<') {
      indentationLevels.push_back(indentationLevels[counter] + 1);
      counter++;
      v.push_back("");
    } else if (s[i] == '>') {
      indentationLevels.push_back(indentationLevels[counter] - 1);
      counter++;
      v.push_back("");
    }
  }
  for (std::size_t i = 0; i < v.size(); i++) {
    std::string indentation = convertIndentation(indentationLevels[i]);
    std::cout << indentation << v[i] << std::endl;
  }
}

template <typename T> inline void printTAST() {
  std::string s = demangle(typeid(T).name());
  std::vector<std::string> v;
  v.push_back("");
  std::size_t counter = 0;
  std::vector<int> indentationLevels(1, 0);
  for (std::size_t i = 0; i < s.length(); i++) {
    v[counter].push_back(s[i]);
    if (s[i] == '<') {
      indentationLevels.push_back(indentationLevels[counter] + 1);
      counter++;
      v.push_back("");
    } else if (s[i] == '>') {
      indentationLevels.push_back(indentationLevels[counter] - 1);
      counter++;
      v.push_back("");
    }
  }
  for (std::size_t i = 0; i < v.size(); i++) {
    std::string indentation = convertIndentation(indentationLevels[i]);
    std::cout << indentation << v[i] << std::endl;
  }
}
} // namespace etr

#endif

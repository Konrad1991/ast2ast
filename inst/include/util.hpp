#ifndef UTIL
#define UTIL

#include <cmath>

int compare(int size, int index) {
  int ret = 0;
  int times = floor(index/size);
  if(times == 0) {
    return index;
  } else if(times > 0) {
    ret = index -times*size;
  }
  return ret;
}

#endif

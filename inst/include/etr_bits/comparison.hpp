/*
R package etr
Copyright (C) 2021 Konrad Krämer

This file is part of R package etr


etr is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with etr
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/


#ifndef COMPARISON
#define COMPARISON

#include "vec.hpp"

namespace etr {

bool cd(double a, double b) {
      return fabs(a - b) < 1E-3;
}

VEC<bool> operator==(VEC<double>& a, double b) {
  
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = cd(a[i], b);
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator==(const VEC<T2, R2>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = cd(a[i], b);
  }

  return res;
}

VEC<bool> operator==(VEC<double>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = cd(a[i], static_cast<double>(b));
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator==(const VEC<T2, R2>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = cd(a[i], static_cast<double>(b));
  }

  return res;
}



VEC<bool> operator>=(VEC<double>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] >= b;
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator>=(const VEC<T2, R2>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] >= b;
  }

  return res;
}

VEC<bool> operator>=(VEC<double>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] >= static_cast<double>(b);
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator>=(const VEC<T2, R2>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] >= static_cast<double>(b);
  }

  return res;
}



VEC<bool> operator<=(VEC<double>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] <= b;
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator<=(const VEC<T2, R2>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] <= b;
  }

  return res;
}

VEC<bool> operator<=(VEC<double>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] <= static_cast<double>(b);
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator<=(const VEC<T2, R2>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] <= static_cast<double>(b);
  }

  return res;
}


VEC<bool> operator<(VEC<double>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] < b;
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator<(const VEC<T2, R2>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] < b;
  }

  return res;
}

VEC<bool> operator<(VEC<double>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] < static_cast<double>(b);
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator<(const VEC<T2, R2>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] < static_cast<double>(b);
  }

  return res;
}



VEC<bool> operator>(VEC<double>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < res.size(); i++) {
    res[i] = a[i] > b;
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator>(const VEC<T2, R2>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] > b;
  }

  return res;
}

VEC<bool> operator>(VEC<double>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i < a.size(); i++) {
    res[i] = a[i] > static_cast<double>(b);
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator>(const VEC<T2, R2>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i > a.size(); i++) {
    res[i] = a[i] > static_cast<double>(b);
  }

  return res;
}



VEC<bool> operator!=(VEC<double>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i != a.size(); i++) {
    res[i] = a[i] != b;
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator!=(const VEC<T2, R2>& a, double b) {
  VEC<bool> res(a.size());
  for(int i = 0; i != a.size(); i++) {
    res[i] = a[i] != b;
  }

  return res;
}

VEC<bool> operator!=(VEC<double>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i != a.size(); i++) {
    res[i] = a[i] != static_cast<double>(b);
  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator!=(const VEC<T2, R2>& a, int b) {
  VEC<bool> res(a.size());
  for(int i = 0; i != a.size(); i++) {
    res[i] = a[i] != static_cast<double>(b);
  }

  return res;
}



VEC<bool> operator==(VEC<double>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = cd(a[i], b[i]);
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = cd(a[i], b[i -counter*b.size()]);
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = cd(a[i - counter*a.size()], b[i]);
      }

    }

  }

  return res;
}



VEC<bool> operator>=(VEC<double>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] >= b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] >= b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] >= b[i];
      }

    }

  }

  return res;
}



VEC<bool> operator<=(VEC<double>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] <= b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] <= b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] <= b[i];
      }

    }

  }

  return res;
}




VEC<bool> operator>(VEC<double>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] > b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] > b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] > b[i];
      }

    }

  }

  return res;
}




VEC<bool> operator<(VEC<double>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] < b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] < b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] < b[i];
      }

    }

  }

  return res;
}



VEC<bool> operator!=(VEC<double>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] != b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] != b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] != b[i];
      }

    }

  }

  return res;
}




































template<typename T2, typename R2, typename T3, typename R3>
VEC<bool> operator==(const VEC<T2, R2>& a, const VEC<T3, R3>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = cd(a[i], b[i]);
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = cd(a[i], b[i -counter*b.size()]);
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = cd(a[i - counter*a.size()], b[i]);
      }

    }

  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator==(VEC<double>& a, const VEC<T2, R2>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = cd(a[i], b[i]);
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = cd(a[i], b[i -counter*b.size()]);
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = cd(a[i - counter*a.size()], b[i]);
      }

    }

  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator==(const VEC<T2, R2>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = cd(a[i], b[i]);
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = cd(a[i], b[i -counter*b.size()]);
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = cd(a[i - counter*a.size()], b[i]);
      }

    }

  }

  return res;
}



template<typename T2, typename R2, typename T3, typename R3>
VEC<bool> operator>=(const VEC<T2, R2>& a, const VEC<T3, R3>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] >= b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] >= b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] >= b[i];
      }

    }

  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator>=(VEC<double>& a, const VEC<T2, R2>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] >= b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] >= b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] >= b[i];
      }

    }

  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator>=(const VEC<T2, R2>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] >= b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] >= b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] >= b[i];
      }

    }

  }

  return res;
}


template<typename T2, typename R2, typename T3, typename R3>
VEC<bool> operator<=(const VEC<T2, R2>& a, const VEC<T3, R3>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] <= b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] <= b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] <= b[i];
      }

    }

  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator<=(VEC<double>& a, const VEC<T2, R2>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] <= b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] <= b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] <= b[i];
      }

    }

  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator<=(const VEC<T2, R2>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] <= b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] <= b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] <= b[i];
      }

    }

  }

  return res;
}




template<typename T2, typename R2, typename T3, typename R3>
VEC<bool> operator>(const VEC<T2, R2>& a, const VEC<T3, R3>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] > b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] > b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] > b[i];
      }

    }

  }

  return res;
}


template<typename T2, typename R2>
VEC<bool> operator>(VEC<double>& a, const VEC<T2, R2>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] > b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] > b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] > b[i];
      }

    }

  }

  return res;
}


template<typename T2, typename R2>
VEC<bool> operator>(const VEC<T2, R2>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] > b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] > b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] > b[i];
      }

    }

  }

  return res;
}



template<typename T2, typename R2, typename T3, typename R3>
VEC<bool> operator<(const VEC<T2, R2>& a, const VEC<T3, R3>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] < b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] < b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] < b[i];
      }

    }

  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator<(VEC<double>& a, const VEC<T2, R2>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] < b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] < b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] < b[i];
      }

    }

  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator<(const VEC<T2, R2>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] < b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] < b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] < b[i];
      }

    }

  }

  return res;
}





template<typename T2, typename R2, typename T3, typename R3>
VEC<bool> operator!=(const VEC<T2, R2>& a, const VEC<T3, R3>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] != b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] != b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] != b[i];
      }

    }

  }

  return res;
}


template<typename T2, typename R2>
VEC<bool> operator!=(VEC<double>& a, const VEC<T2, R2>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] != b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] != b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] != b[i];
      }

    }

  }

  return res;
}

template<typename T2, typename R2>
VEC<bool> operator!=(const VEC<T2, R2>& a, VEC<double>& b) {

  int size = (a.size() >= b.size()) ? a.size() : b.size();
  int counter = 0;
  VEC<bool> res(size);
  res.ismatrix = false;
  res.subsetted = false;

  if(a.size() == b.size()) {
    for(int i = 0; i < a.size(); i++) {
      res[i] = a[i] != b[i];
    }
    return res;
  } else {

    if(a.size() > b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % b.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i] != b[i -counter*b.size()];
      }

    } else if(a.size() < b.size()) {

      for(int i = 0; i < size; i++) {
        if( (i % a.size() == 0) && i != 0) {
          counter++;
        }
        res[i] = a[i - counter*a.size()] != b[i];
      }

    }

  }

  return res;
}


}

#endif

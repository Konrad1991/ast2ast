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

#ifndef SUBSETTING
#define SUBSETTING

namespace etr {

VEC<double> subset(VEC<double>& inp) {
  VEC<double> ret;
  ret = inp;
  return ret;
}

VEC<double> subset(VEC<double>& inp, int pos) {
  VEC<double> ret(1);
  pos--;
  ret[0] = inp[pos];
  return ret;
}

VEC<double> subset(VEC<double>& inp, double pos_) {
  int pos = d2i(pos_);
  VEC<double> ret(1);
  pos--;
  ret[0] = inp[pos];
  return ret;
}

VEC<double> subset(VEC<double>& inp, bool p) {

  VEC<double> ret;
  if(p == false) {
    return ret;
  }
  ret = inp;
  return ret;
}

VEC<double> subset(VEC<double>& inp, void* p) {

  VEC<double> ret;
  ret = inp;
  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>& pos) {

  VEC<double> ret(pos.size());

  for(int i = 0; i < ret.size(); i++) {
    ret[i] = inp[( d2i(pos[i]) - 1)];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>& pos) {

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.size()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.size();
      counter++;
    }
  }

  VEC<double> ret(positions.size());

  for(int i = 0; i < ret.size(); i++) {
    ret[i] = inp[positions[i]];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, int r, int c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(1);
  r--;
  c--;
  int pos = c*inp.nr() + r;

  ret[0] = inp[pos];

  return ret;
}

VEC<double> subset(VEC<double>& inp, int r, double c_) {

  int c = d2i(c_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(1);
  r--;
  c--;
  int pos = c*inp.nr() + r;

  ret[0] = inp[pos];

  return ret;
}

VEC<double> subset(VEC<double>& inp, int row, bool c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(c == false) {
    VEC<double> empty;
    return empty;
  }

  VEC<double> ret(inp.nc());
  ret.ismatrix = false;
  int pst = 0;

  for(int j = 0; j < inp.nc(); j++) {
    pst = j*inp.nr() + (row - 1);
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, int row, void* nptr) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(inp.nc());
  ret.ismatrix = false;
  int pst = 0;

  for(int j = 0; j < inp.nc(); j++) {
    pst = j*inp.nr() + (row - 1);
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, int row, VEC<double>& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size());
  int pst = 0;
  row--;
  for(int j = 0; j < pos.size(); j++) {
    pst = (d2i(pos[j])-1)*inp.nr() + row;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, int row, VEC<bool>& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(positions.size());
  int pst = 0;
  row--;
  for(int j = 0; j < positions.size(); j++) {
    pst = (d2i(positions[j]))*inp.nr() + row;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, double r_, int c) {

  int r = d2i(r_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(1);
  r--;
  c--;
  int pos = c*inp.nr() + r;

  ret[0] = inp[pos];

  return ret;
}


VEC<double> subset(VEC<double>& inp, bool r, int col) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(r == false) {
    VEC<double> empty;
    return empty;
  }

  VEC<double> ret(inp.nr());
  ret.ismatrix = false;
  int pst = 0;

  for(int j = 0; j < inp.nr(); j++) {
    pst = (col-1)*inp.nr() + j;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, void* ptr, int col) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(inp.nr());
  ret.ismatrix = false;
  int pst = 0;

  for(int j = 0; j < inp.nr(); j++) {
    pst = (col-1)*inp.nr() + j;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>& pos, int col) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size());
  int pst = 0;
  col--;

  for(int j = 0; j < pos.size(); j++) {
    pst =  col*inp.nr() + (d2i(pos[j]) - 1);
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>& pos, int col) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size());
  int pst = 0;
  col--;
  for(int j = 0; j < positions.size(); j++) {
    pst = col*inp.nr() + positions[j];
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, double r_, double c_) {
  int r = static_cast<int>(r_);
  int c = static_cast<int>(c_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(1);
  r--;
  c--;
  int pos = c*inp.nr() + r;

  ret[0] = inp[pos];

  return ret;
}

VEC<double> subset(VEC<double>& inp, double row_, bool c) {

  int row = d2i(row_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(c == false) {
    VEC<double> empty;
    return empty;
  }

  VEC<double> ret(inp.nc());
  ret.ismatrix = false;
  int pst = 0;

  for(int j = 0; j < inp.nc(); j++) {
    pst = j*inp.nr() + (row - 1);
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, double row_, void* nptr) {
  int row = d2i(row_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(inp.nc());
  ret.ismatrix = false;
  int pst = 0;

  for(int j = 0; j < inp.nc(); j++) {
    pst = j*inp.nr() + (row - 1);
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, double row_, VEC<double>& pos) {
  int row = d2i(row_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size());
  int pst = 0;
  row--;
  for(int j = 0; j < pos.size(); j++) {
    pst = (d2i(pos[j])-1)*inp.nr() + row;
    ret[j] = inp[pst];
  }

  return ret;
}


VEC<double> subset(VEC<double>& inp, double row_, VEC<bool>& pos) {
  int row = d2i(row_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(positions.size());
  int pst = 0;
  row--;
  for(int j = 0; j < positions.size(); j++) {
    pst = (d2i(positions[j]))*inp.nr() + row;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, bool r, double col_) {
  int col = d2i(col_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(r == false) {
    VEC<double> empty;
    return empty;
  }

  VEC<double> ret(inp.nr());
  ret.ismatrix = false;
  int pst = 0;

  for(int j = 0; j < inp.nr(); j++) {
    pst = (col-1)*inp.nr() + j;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, void* ptr, double col_) {
  int col = d2i(col_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(inp.nr());
  ret.ismatrix = false;
  int pst = 0;

  for(int j = 0; j < inp.nr(); j++) {
    pst = (col-1)*inp.nr() + j;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>& pos, double col_) {
  int col = d2i(col_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size());
  int pst = 0;
  col--;

  for(int j = 0; j < pos.size(); j++) {
    pst =  col*inp.nr() + (d2i(pos[j]) - 1);
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>& pos, double col_) {
  int col = d2i(col_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size());
  int pst = 0;
  col--;
  for(int j = 0; j < positions.size(); j++) {
    pst = col*inp.nr() + positions[j];
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, bool r, bool c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret;
  if( (r == true) && (c == true)) {
    ret = inp;
    return ret;
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, bool r, void* c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret;
  if( r == true) {
    ret = inp;
    return ret;
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, bool r, VEC<double>& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(r == false) {
    VEC<double> empty;
    return empty;
  }

  VEC<double> ret(inp.nr() * pos.size());
  ret.ismatrix = true;
  ret.ncols = pos.size();
  ret.nrows = inp.nr();

  int pst = 0;
  int counter = 0;
  for(int j = 0; j < pos.size(); j++) {
    for(int i = 0; i < inp.nr(); i++) {
      pst = (d2i(pos[j])-1)*inp.nr() + i;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, bool r, VEC<bool>& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(r == false) {
    VEC<double> empty;
    return empty;
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(inp.nr() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = inp.nr();

  int pst = 0;
  counter = 0;
  for(int j = 0; j < positions.size(); j++) {
    for(int i = 0; i < inp.nr(); i++) {
      pst = (positions[j])*inp.nr() + i;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, void* r, bool c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret;
  if( c == true) {
    ret = inp;
    return ret;
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>& pos, bool c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(c == false) {
    VEC<double> empty;
    return empty;
  }

  VEC<double> ret(pos.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = pos.size();

  int pst = 0;
  int counter = 0;
  for(int i = 0; i < inp.nc(); i++) {
    for(int j = 0; j < pos.size(); j++) {
      pst =  i*inp.nr() + (d2i(pos[j]) - 1);
      ret[counter] = inp[pst];
      counter++;
    }
 }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>& pos, bool c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(c == false) {
    VEC<double> empty;
    return empty;
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;
  for(int i = 0; i < inp.nc(); i++) {
    for(int j = 0; j < positions.size(); j++) {
      pst =  i*inp.nr() + positions[j];
      ret[counter] = inp[pst];
      counter++;
    }
 }

  return ret;
}

VEC<double> subset(VEC<double>& inp, void* r, void* c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret;
  ret = inp;

  return ret;
}

VEC<double> subset(VEC<double>& inp, void* r, VEC<double>& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(inp.nr() * pos.size());
  ret.ismatrix = true;
  ret.ncols = pos.size();
  ret.nrows = inp.nr();

  int pst = 0;
  int counter = 0;
  for(int j = 0; j < pos.size(); j++) {
    for(int i = 0; i < inp.nr(); i++) {
      pst = (d2i(pos[j])-1)*inp.nr() + i;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, void* r, VEC<bool>& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(inp.nr() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = inp.nr();

  int pst = 0;
  counter = 0;
  for(int j = 0; j < positions.size(); j++) {
    for(int i = 0; i < inp.nr(); i++) {
      pst = (positions[j])*inp.nr() + i;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>& pos, void* c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = pos.size();

  int pst = 0;
  int counter = 0;
  for(int i = 0; i < inp.nc(); i++) {
    for(int j = 0; j < pos.size(); j++) {
      pst =  i*inp.nr() + (d2i(pos[j]) - 1);
      ret[counter] = inp[pst];
      counter++;
    }
 }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>& pos, void* c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;
  for(int i = 0; i < inp.nc(); i++) {
    for(int j = 0; j < positions.size(); j++) {
      pst =  i*inp.nr() + positions[j];
      ret[counter] = inp[pst];
      counter++;
    }
 }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>& rpos, VEC<double>& cpos) {
  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(rpos.size() * cpos.size());
  ret.ismatrix = true;
  ret.ncols = cpos.size();
  ret.nrows = rpos.size();

  int pst = 0;
  int counter = 0;

  for(int i = 0; i < ret.nc(); i++) {
    for(int j = 0; j < ret.nr(); j++) {
      pst = (d2i(cpos[i]) - 1)*inp.nr() + (d2i(rpos[j]) - 1);
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>& rows, VEC<bool>& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(rows.size() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = rows.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < positions.size(); j++) {
    for(int i = 0; i < rows.size(); i++) {
      pst = (positions[j])*inp.nr() + d2i(rows[i]) - 1;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>& pos, VEC<double>& cols) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * cols.size());
  ret.ismatrix = true;
  ret.ncols = cols.size();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < cols.size(); j++) {
    for(int i = 0; i < positions.size(); i++) {
      pst = (d2i(cols[j]) - 1)*inp.nr() + positions[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>& rows, VEC<bool>& cols) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < rows.size(); i++) {
    if(rows[i] == true) {
      counter++;
    }
  }
  VEC<int> positions_rows(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < rows.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(rows[i] == true) {
      positions_rows[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  counter = 0;
  for(int i = 0; i < cols.size(); i++) {
    if(cols[i] == true) {
      counter++;
    }
  }
  VEC<int> positions_cols(counter);
  counter = 0;
  counter2 = 0;
  for(int i = 0; i < cols.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(cols[i] == true) {
      positions_cols[counter] = i -counter2*inp.nc();
      counter++;
    }
  }


  VEC<double> ret(positions_rows.size() * positions_cols.size());
  ret.ismatrix = true;
  ret.ncols = positions_cols.size();
  ret.nrows = positions_rows.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < positions_cols.size(); j++) {
    for(int i = 0; i < positions_rows.size(); i++) {
      pst = positions_cols[j]*inp.nr() + positions_rows[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}





// rvalue functions
VEC<double> subset(VEC<double>& inp, VEC<double>&& pos) {

  VEC<double> ret(pos.size());

  for(int i = 0; i < ret.size(); i++) {
    ret[i] = inp[( d2i(pos[i]) - 1)];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>&& pos) {

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.size()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.size();
      counter++;
    }
  }

  VEC<double> ret(positions.size());

  for(int i = 0; i < ret.size(); i++) {
    ret[i] = inp[positions[i]];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, int row, VEC<double>&& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size());
  int pst = 0;
  row--;
  for(int j = 0; j < pos.size(); j++) {
    pst = (d2i(pos[j])-1)*inp.nr() + row;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, int row, VEC<bool>&& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(positions.size());
  int pst = 0;
  row--;
  for(int j = 0; j < positions.size(); j++) {
    pst = (d2i(positions[j]))*inp.nr() + row;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>&& pos, int col) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size());
  int pst = 0;
  col--;

  for(int j = 0; j < pos.size(); j++) {
    pst =  col*inp.nr() + (d2i(pos[j]) - 1);
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>&& pos, int col) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size());
  int pst = 0;
  col--;
  for(int j = 0; j < positions.size(); j++) {
    pst = col*inp.nr() + positions[j];
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, double row_, VEC<double>&& pos) {
  int row = d2i(row_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size());
  int pst = 0;
  row--;
  for(int j = 0; j < pos.size(); j++) {
    pst = (d2i(pos[j])-1)*inp.nr() + row;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, double row_, VEC<bool>&& pos) {
  int row = d2i(row_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(positions.size());
  int pst = 0;
  row--;
  for(int j = 0; j < positions.size(); j++) {
    pst = (d2i(positions[j]))*inp.nr() + row;
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>&& pos, double col_) {
  int col = d2i(col_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size());
  int pst = 0;
  col--;

  for(int j = 0; j < pos.size(); j++) {
    pst =  col*inp.nr() + (d2i(pos[j]) - 1);
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>&& pos, double col_) {
  int col = d2i(col_);

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size());
  int pst = 0;
  col--;
  for(int j = 0; j < positions.size(); j++) {
    pst = col*inp.nr() + positions[j];
    ret[j] = inp[pst];
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, bool r, VEC<double>&& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(r == false) {
    VEC<double> empty;
    return empty;
  }

  VEC<double> ret(inp.nr() * pos.size());
  ret.ismatrix = true;
  ret.ncols = pos.size();
  ret.nrows = inp.nr();

  int pst = 0;
  int counter = 0;
  for(int j = 0; j < pos.size(); j++) {
    for(int i = 0; i < inp.nr(); i++) {
      pst = (d2i(pos[j])-1)*inp.nr() + i;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, bool r, VEC<bool>&& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(r == false) {
    VEC<double> empty;
    return empty;
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(inp.nr() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = inp.nr();

  int pst = 0;
  counter = 0;
  for(int j = 0; j < positions.size(); j++) {
    for(int i = 0; i < inp.nr(); i++) {
      pst = (positions[j])*inp.nr() + i;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>&& pos, bool c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(c == false) {
    VEC<double> empty;
    return empty;
  }

  VEC<double> ret(pos.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = pos.size();

  int pst = 0;
  int counter = 0;
  for(int i = 0; i < inp.nc(); i++) {
    for(int j = 0; j < pos.size(); j++) {
      pst =  i*inp.nr() + (d2i(pos[j]) - 1);
      ret[counter] = inp[pst];
      counter++;
    }
 }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>&& pos, bool c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  if(c == false) {
    VEC<double> empty;
    return empty;
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;
  for(int i = 0; i < inp.nc(); i++) {
    for(int j = 0; j < positions.size(); j++) {
      pst =  i*inp.nr() + positions[j];
      ret[counter] = inp[pst];
      counter++;
    }
 }

  return ret;
}

VEC<double> subset(VEC<double>& inp, void* r, VEC<double>&& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(inp.nr() * pos.size());
  ret.ismatrix = true;
  ret.ncols = pos.size();
  ret.nrows = inp.nr();

  int pst = 0;
  int counter = 0;
  for(int j = 0; j < pos.size(); j++) {
    for(int i = 0; i < inp.nr(); i++) {
      pst = (d2i(pos[j])-1)*inp.nr() + i;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, void* r, VEC<bool>&& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(inp.nr() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = inp.nr();

  int pst = 0;
  counter = 0;
  for(int j = 0; j < positions.size(); j++) {
    for(int i = 0; i < inp.nr(); i++) {
      pst = (positions[j])*inp.nr() + i;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>&& pos, void* c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(pos.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = pos.size();

  int pst = 0;
  int counter = 0;
  for(int i = 0; i < inp.nc(); i++) {
    for(int j = 0; j < pos.size(); j++) {
      pst =  i*inp.nr() + (d2i(pos[j]) - 1);
      ret[counter] = inp[pst];
      counter++;
    }
 }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>&& pos, void* c) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;
  for(int i = 0; i < inp.nc(); i++) {
    for(int j = 0; j < positions.size(); j++) {
      pst =  i*inp.nr() + positions[j];
      ret[counter] = inp[pst];
      counter++;
    }
 }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>&& rpos, VEC<double>&& cpos) {
  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(rpos.size() * cpos.size());
  ret.ismatrix = true;
  ret.ncols = cpos.size();
  ret.nrows = rpos.size();

  int pst = 0;
  int counter = 0;

  for(int i = 0; i < ret.nc(); i++) {
    for(int j = 0; j < ret.nr(); j++) {
      pst = (d2i(cpos[i]) - 1)*inp.nr() + (d2i(rpos[j]) - 1);
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>& rpos, VEC<double>&& cpos) {
  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(rpos.size() * cpos.size());
  ret.ismatrix = true;
  ret.ncols = cpos.size();
  ret.nrows = rpos.size();

  int pst = 0;
  int counter = 0;

  for(int i = 0; i < ret.nc(); i++) {
    for(int j = 0; j < ret.nr(); j++) {
      pst = (d2i(cpos[i]) - 1)*inp.nr() + (d2i(rpos[j]) - 1);
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>&& rpos, VEC<double>& cpos) {
  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  VEC<double> ret(rpos.size() * cpos.size());
  ret.ismatrix = true;
  ret.ncols = cpos.size();
  ret.nrows = rpos.size();

  int pst = 0;
  int counter = 0;

  for(int i = 0; i < ret.nc(); i++) {
    for(int j = 0; j < ret.nr(); j++) {
      pst = (d2i(cpos[i]) - 1)*inp.nr() + (d2i(rpos[j]) - 1);
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>&& rows, VEC<bool>&& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(rows.size() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = rows.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < positions.size(); j++) {
    for(int i = 0; i < rows.size(); i++) {
      pst = (positions[j])*inp.nr() + d2i(rows[i]) - 1;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>& rows, VEC<bool>&& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(rows.size() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = rows.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < positions.size(); j++) {
    for(int i = 0; i < rows.size(); i++) {
      pst = (positions[j])*inp.nr() + d2i(rows[i]) - 1;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<double>&& rows, VEC<bool>& pos) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nc();
      counter++;
    }
  }

  VEC<double> ret(rows.size() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = rows.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < positions.size(); j++) {
    for(int i = 0; i < rows.size(); i++) {
      pst = (positions[j])*inp.nr() + d2i(rows[i]) - 1;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>&& pos, VEC<double>&& cols) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * cols.size());
  ret.ismatrix = true;
  ret.ncols = cols.size();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < cols.size(); j++) {
    for(int i = 0; i < positions.size(); i++) {
      pst = (d2i(cols[j]) - 1)*inp.nr() + positions[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>& pos, VEC<double>&& cols) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * cols.size());
  ret.ismatrix = true;
  ret.ncols = cols.size();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < cols.size(); j++) {
    for(int i = 0; i < positions.size(); i++) {
      pst = (d2i(cols[j]) - 1)*inp.nr() + positions[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>&& pos, VEC<double>& cols) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < pos.size(); i++) {
    if(pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < pos.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(pos[i] == true) {
      positions[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * cols.size());
  ret.ismatrix = true;
  ret.ncols = cols.size();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < cols.size(); j++) {
    for(int i = 0; i < positions.size(); i++) {
      pst = (d2i(cols[j]) - 1)*inp.nr() + positions[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>&& rows, VEC<bool>&& cols) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < rows.size(); i++) {
    if(rows[i] == true) {
      counter++;
    }
  }
  VEC<int> positions_rows(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < rows.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(rows[i] == true) {
      positions_rows[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  counter = 0;
  for(int i = 0; i < cols.size(); i++) {
    if(cols[i] == true) {
      counter++;
    }
  }
  VEC<int> positions_cols(counter);
  counter = 0;
  counter2 = 0;
  for(int i = 0; i < cols.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(cols[i] == true) {
      positions_cols[counter] = i -counter2*inp.nc();
      counter++;
    }
  }


  VEC<double> ret(positions_rows.size() * positions_cols.size());
  ret.ismatrix = true;
  ret.ncols = positions_cols.size();
  ret.nrows = positions_rows.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < positions_cols.size(); j++) {
    for(int i = 0; i < positions_rows.size(); i++) {
      pst = positions_cols[j]*inp.nr() + positions_rows[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>& rows, VEC<bool>&& cols) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < rows.size(); i++) {
    if(rows[i] == true) {
      counter++;
    }
  }
  VEC<int> positions_rows(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < rows.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(rows[i] == true) {
      positions_rows[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  counter = 0;
  for(int i = 0; i < cols.size(); i++) {
    if(cols[i] == true) {
      counter++;
    }
  }
  VEC<int> positions_cols(counter);
  counter = 0;
  counter2 = 0;
  for(int i = 0; i < cols.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(cols[i] == true) {
      positions_cols[counter] = i -counter2*inp.nc();
      counter++;
    }
  }


  VEC<double> ret(positions_rows.size() * positions_cols.size());
  ret.ismatrix = true;
  ret.ncols = positions_cols.size();
  ret.nrows = positions_rows.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < positions_cols.size(); j++) {
    for(int i = 0; i < positions_rows.size(); i++) {
      pst = positions_cols[j]*inp.nr() + positions_rows[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

VEC<double> subset(VEC<double>& inp, VEC<bool>&& rows, VEC<bool>& cols) {

  if(inp.ismatrix == false) {
    std::cerr << "incorrect number of dimensions" << std::endl;
    exit(0);
  }

  int counter = 0;
  for(int i = 0; i < rows.size(); i++) {
    if(rows[i] == true) {
      counter++;
    }
  }
  VEC<int> positions_rows(counter);
  counter = 0;
  int counter2 = 0;
  for(int i = 0; i < rows.size(); i++) {
    if( ( (i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if(rows[i] == true) {
      positions_rows[counter] = i -counter2*inp.nr();
      counter++;
    }
  }

  counter = 0;
  for(int i = 0; i < cols.size(); i++) {
    if(cols[i] == true) {
      counter++;
    }
  }
  VEC<int> positions_cols(counter);
  counter = 0;
  counter2 = 0;
  for(int i = 0; i < cols.size(); i++) {
    if( ( (i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if(cols[i] == true) {
      positions_cols[counter] = i -counter2*inp.nc();
      counter++;
    }
  }


  VEC<double> ret(positions_rows.size() * positions_cols.size());
  ret.ismatrix = true;
  ret.ncols = positions_cols.size();
  ret.nrows = positions_rows.size();

  int pst = 0;
  counter = 0;

  for(int j = 0; j < positions_cols.size(); j++) {
    for(int i = 0; i < positions_rows.size(); i++) {
      pst = positions_cols[j]*inp.nr() + positions_rows[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

}

#endif

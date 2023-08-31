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

#ifndef DISTRI
#define DISTRI

#include "vec.hpp"

namespace etr {

// =======================================================================================================================
// uniform
// =======================================================================================================================
inline VEC<double> dunif_etr(const VEC<double> &x, const VEC<double> &min_,
                             const VEC<double> &max_, const VEC<double> &lg) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::dunif(x[0], min_[0], max_[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{min_.size(), max_.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::dunif(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> runif_etr(const VEC<double> &x, const VEC<double> &min_,
                             const VEC<double> &max_) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1)) {
    VEC<double> res(x[0], 0.0);
    int size = static_cast<int>(x[0]);
    for (int i = 0; i < size; i++) {
      GetRNGstate();
      res[i] = R::runif(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
    }
    return res;
  } else {
    std::vector<int> sizes{min_.size(), max_.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::runif(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> punif_etr(const VEC<double> &x, const VEC<double> &min_,
                             const VEC<double> &max_, const VEC<double> &lower,
                             const VEC<double> &lg) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::punif(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{min_.size(), max_.size(), lower.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::punif(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lower[i % lower.size()],
                        lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> qunif_etr(const VEC<double> &x, const VEC<double> &min_,
                             const VEC<double> &max_, const VEC<double> &lower,
                             const VEC<double> &lg) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::qunif(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{min_.size(), max_.size(), lower.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::qunif(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lower[i % lower.size()],
                        lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

// =======================================================================================================================
// normal
// =======================================================================================================================
inline VEC<double> dnorm_etr(const VEC<double> &x, const VEC<double> &min_,
                             const VEC<double> &max_, const VEC<double> &lg) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::dnorm(x[0], min_[0], max_[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{min_.size(), max_.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::dnorm(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> rnorm_etr(const VEC<double> &x, const VEC<double> &min_,
                             const VEC<double> &max_) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1)) {
    VEC<double> res(x[0], 0.0);
    int size = static_cast<int>(x[0]);
    for (int i = 0; i < size; i++) {
      GetRNGstate();
      res[i] = R::rnorm(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
    }
    return res;
  } else {
    std::vector<int> sizes{min_.size(), max_.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::rnorm(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> pnorm_etr(const VEC<double> &x, const VEC<double> &min_,
                             const VEC<double> &max_, const VEC<double> &lower,
                             const VEC<double> &lg) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::pnorm(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{min_.size(), max_.size(), lower.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::pnorm(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lower[i % lower.size()],
                        lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> qnorm_etr(const VEC<double> &x, const VEC<double> &min_,
                             const VEC<double> &max_, const VEC<double> &lower,
                             const VEC<double> &lg) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::qnorm(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{min_.size(), max_.size(), lower.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::qnorm(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lower[i % lower.size()],
                        lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

// =======================================================================================================================
// l normal
// =======================================================================================================================
inline VEC<double> dlnorm_etr(const VEC<double> &x, const VEC<double> &min_,
                              const VEC<double> &max_, const VEC<double> &lg) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::dlnorm(x[0], min_[0], max_[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{min_.size(), max_.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::dlnorm(x[i % x.size()], min_[i % min_.size()],
                         max_[i % max_.size()], lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> rlnorm_etr(const VEC<double> &x, const VEC<double> &min_,
                              const VEC<double> &max_) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1)) {
    VEC<double> res(x[0], 0.0);
    int size = static_cast<int>(x[0]);
    for (int i = 0; i < size; i++) {
      GetRNGstate();
      res[i] = R::rlnorm(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
    }
    return res;
  } else {
    std::vector<int> sizes{min_.size(), max_.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::rlnorm(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> plnorm_etr(const VEC<double> &x, const VEC<double> &min_,
                              const VEC<double> &max_, const VEC<double> &lower,
                              const VEC<double> &lg) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::plnorm(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{min_.size(), max_.size(), lower.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::plnorm(x[i % x.size()], min_[i % min_.size()],
                         max_[i % max_.size()], lower[i % lower.size()],
                         lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> qlnorm_etr(const VEC<double> &x, const VEC<double> &min_,
                              const VEC<double> &max_, const VEC<double> &lower,
                              const VEC<double> &lg) {

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::qlnorm(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{min_.size(), max_.size(), lower.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::qlnorm(x[i % x.size()], min_[i % min_.size()],
                         max_[i % max_.size()], lower[i % lower.size()],
                         lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

// =======================================================================================================================
// gamma
// =======================================================================================================================
inline VEC<double> dgamma_etr(const VEC<double> &x, const VEC<double> &shape,
                              const VEC<double> &rate, const VEC<double> &lg) {
  std::vector<double> scale(rate.size());
  for (int i = 0; i < scale.size(); i++) {
    scale[i] = 1.0 / rate[i];
  }
  // const VEC<double> &scale = 1 / rate;
  if ((x.size() == 1) && (shape.size() == 1) && (scale.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::dgamma(x[0], shape[0], scale[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{shape.size(), static_cast<int>(scale.size()),
                           lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::dgamma(x[i % x.size()], shape[i % shape.size()],
                         scale[i % scale.size()], lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> rgamma_etr(const VEC<double> &x, const VEC<double> &shape,
                              const VEC<double> &rate) {

  std::vector<double> scale(rate.size());
  for (int i = 0; i < scale.size(); i++) {
    scale[i] = 1.0 / rate[i];
  }

  // const VEC<double> &scale = 1 / rate;
  if ((x.size() == 1) && (shape.size() == 1) && (scale.size() == 1)) {
    VEC<double> res(x[0], 0.0);
    int size = static_cast<int>(x[0]);
    for (int i = 0; i < size; i++) {
      GetRNGstate();
      res[i] = R::rgamma(shape[i % shape.size()], scale[i % scale.size()]);
      PutRNGstate();
    }
    return res;
  } else {
    std::vector<int> sizes{shape.size(), static_cast<int>(scale.size())};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::rgamma(shape[i % shape.size()], scale[i % scale.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> pgamma_etr(const VEC<double> &x, const VEC<double> &shape,
                              const VEC<double> &rate, const VEC<double> &lower,
                              const VEC<double> &lg) {
  std::vector<double> scale(rate.size());
  for (int i = 0; i < scale.size(); i++) {
    scale[i] = 1.0 / rate[i];
  }
  // const VEC<double> &scale = 1 / rate;
  if ((x.size() == 1) && (shape.size() == 1) && (scale.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::pgamma(x[0], shape[0], scale[0], lower[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{shape.size(), static_cast<int>(scale.size()),
                           lower.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::pgamma(x[i % x.size()], shape[i % shape.size()],
                         scale[i % scale.size()], lower[i % lower.size()],
                         lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

inline VEC<double> qgamma_etr(const VEC<double> &x, const VEC<double> &shape,
                              const VEC<double> &rate, const VEC<double> &lower,
                              const VEC<double> &lg) {
  std::vector<double> scale(rate.size());
  for (int i = 0; i < scale.size(); i++) {
    scale[i] = 1.0 / rate[i];
  }
  // const VEC<double> &scale = 1 / rate;
  if ((x.size() == 1) && (shape.size() == 1) && (scale.size() == 1) &&
      (lg.size() == 1)) {
    GetRNGstate();
    return R::qgamma(x[0], shape[0], scale[0], lower[0], lg[0]);
    PutRNGstate();
  } else {
    std::vector<int> sizes{shape.size(), static_cast<int>(scale.size()),
                           lower.size(), lg.size()};
    int max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    VEC<double> res(max, 0.0);
    for (int i = 0; i < res.size(); i++) {
      GetRNGstate();
      res[i] = R::qgamma(x[i % x.size()], shape[i % shape.size()],
                         scale[i % scale.size()], lower[i % lower.size()],
                         lg[i % lg.size()]);
      PutRNGstate();
    }
    return res;
  }

  return VEC<double>(R_NaN);
}

} // namespace etr

#endif
#ifndef DISTRI
#define DISTRI

#include "UtilsTraits.hpp"

namespace etr {

// =======================================================================================================================
// uniform
// =======================================================================================================================
inline Vec<BaseType> dunif_etr(const Vec<BaseType> &x,
                               const Vec<BaseType> &min_,
                               const Vec<BaseType> &max_,
                               const Vec<BaseType> &lg) {
  std::mutex m;
  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::dunif(x[0], min_[0], max_[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size(), lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::dunif(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }
  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> runif_etr(const Vec<BaseType> &x,
                               const Vec<BaseType> &min_,
                               const Vec<BaseType> &max_) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1)) {
    Vec<BaseType> res(static_cast<int>(x[0]));
    size_t size = static_cast<size_t>(x[0]);
    for (size_t i = 0; i < size; i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::runif(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::runif(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> punif_etr(const Vec<BaseType> &x,
                               const Vec<BaseType> &min_,
                               const Vec<BaseType> &max_,
                               const Vec<BaseType> &lower,
                               const Vec<BaseType> &lg) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::punif(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size(), lower.size(),
                              lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::punif(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lower[i % lower.size()],
                        lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> qunif_etr(const Vec<BaseType> &x,
                               const Vec<BaseType> &min_,
                               const Vec<BaseType> &max_,
                               const Vec<BaseType> &lower,
                               const Vec<BaseType> &lg) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::qunif(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size(), lower.size(),
                              lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::qunif(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lower[i % lower.size()],
                        lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

// =======================================================================================================================
// normal
// =======================================================================================================================
inline Vec<BaseType> dnorm_etr(const Vec<BaseType> &x,
                               const Vec<BaseType> &min_,
                               const Vec<BaseType> &max_,
                               const Vec<BaseType> &lg) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::dnorm(x[0], min_[0], max_[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size(), lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::dnorm(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> rnorm_etr(const Vec<BaseType> &x,
                               const Vec<BaseType> &min_,
                               const Vec<BaseType> &max_) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1)) {
    size_t size = static_cast<size_t>(x[0]);
    Vec<BaseType> res(size);
    for (size_t i = 0; i < size; i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::rnorm(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::rnorm(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> pnorm_etr(const Vec<BaseType> &x,
                               const Vec<BaseType> &min_,
                               const Vec<BaseType> &max_,
                               const Vec<BaseType> &lower,
                               const Vec<BaseType> &lg) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::pnorm(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size(), lower.size(),
                              lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::pnorm(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lower[i % lower.size()],
                        lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> qnorm_etr(const Vec<BaseType> &x,
                               const Vec<BaseType> &min_,
                               const Vec<BaseType> &max_,
                               const Vec<BaseType> &lower,
                               const Vec<BaseType> &lg) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::qnorm(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size(), lower.size(),
                              lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::qnorm(x[i % x.size()], min_[i % min_.size()],
                        max_[i % max_.size()], lower[i % lower.size()],
                        lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

// =======================================================================================================================
// l normal
// =======================================================================================================================
inline Vec<BaseType> dlnorm_etr(const Vec<BaseType> &x,
                                const Vec<BaseType> &min_,
                                const Vec<BaseType> &max_,
                                const Vec<BaseType> &lg) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::dlnorm(x[0], min_[0], max_[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size(), lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::dlnorm(x[i % x.size()], min_[i % min_.size()],
                         max_[i % max_.size()], lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> rlnorm_etr(const Vec<BaseType> &x,
                                const Vec<BaseType> &min_,
                                const Vec<BaseType> &max_) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1)) {
    size_t size = static_cast<size_t>(x[0]);
    Vec<BaseType> res(size);
    for (size_t i = 0; i < size; i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::rlnorm(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::rlnorm(min_[i % min_.size()], max_[i % max_.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> plnorm_etr(const Vec<BaseType> &x,
                                const Vec<BaseType> &min_,
                                const Vec<BaseType> &max_,
                                const Vec<BaseType> &lower,
                                const Vec<BaseType> &lg) {
  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::plnorm(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size(), lower.size(),
                              lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::plnorm(x[i % x.size()], min_[i % min_.size()],
                         max_[i % max_.size()], lower[i % lower.size()],
                         lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> qlnorm_etr(const Vec<BaseType> &x,
                                const Vec<BaseType> &min_,
                                const Vec<BaseType> &max_,
                                const Vec<BaseType> &lower,
                                const Vec<BaseType> &lg) {

  std::mutex m;

  if ((x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::qlnorm(x[0], min_[0], max_[0], lower[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{min_.size(), max_.size(), lower.size(),
                              lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::qlnorm(x[i % x.size()], min_[i % min_.size()],
                         max_[i % max_.size()], lower[i % lower.size()],
                         lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

// =======================================================================================================================
// gamma
// =======================================================================================================================
inline Vec<BaseType> dgamma_etr(const Vec<BaseType> &x,
                                const Vec<BaseType> &shape,
                                const Vec<BaseType> &rate,
                                const Vec<BaseType> &lg) {
  std::mutex m;
  std::vector<double> scale(rate.size());
  for (size_t i = 0; i < scale.size(); i++) {
    scale[i] = 1.0 / rate[i];
  }
  // const Vec<BaseType> &scale = 1 / rate;
  if ((x.size() == 1) && (shape.size() == 1) && (scale.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::dgamma(x[0], shape[0], scale[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{shape.size(), static_cast<size_t>(scale.size()),
                              lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::dgamma(x[i % x.size()], shape[i % shape.size()],
                         scale[i % scale.size()], lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> rgamma_etr(const Vec<BaseType> &x,
                                const Vec<BaseType> &shape,
                                const Vec<BaseType> &rate) {
  std::mutex m;

  std::vector<double> scale(rate.size());
  for (size_t i = 0; i < scale.size(); i++) {
    scale[i] = 1.0 / rate[i];
  }

  // const Vec<BaseType> &scale = 1 / rate;
  if ((x.size() == 1) && (shape.size() == 1) && (scale.size() == 1)) {
    size_t size = static_cast<size_t>(x[0]);
    Vec<BaseType> res(size);
    for (size_t i = 0; i < size; i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::rgamma(shape[i % shape.size()], scale[i % scale.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  } else {
    std::vector<size_t> sizes{shape.size(), static_cast<size_t>(scale.size())};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::rgamma(shape[i % shape.size()], scale[i % scale.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> pgamma_etr(const Vec<BaseType> &x,
                                const Vec<BaseType> &shape,
                                const Vec<BaseType> &rate,
                                const Vec<BaseType> &lower,
                                const Vec<BaseType> &lg) {
  std::mutex m;
  std::vector<double> scale(rate.size());
  for (size_t i = 0; i < scale.size(); i++) {
    scale[i] = 1.0 / rate[i];
  }
  // const Vec<BaseType> &scale = 1 / rate;
  if ((x.size() == 1) && (shape.size() == 1) && (scale.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::pgamma(x[0], shape[0], scale[0], lower[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{shape.size(), static_cast<size_t>(scale.size()),
                              lower.size(), lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::pgamma(x[i % x.size()], shape[i % shape.size()],
                         scale[i % scale.size()], lower[i % lower.size()],
                         lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

inline Vec<BaseType> qgamma_etr(const Vec<BaseType> &x,
                                const Vec<BaseType> &shape,
                                const Vec<BaseType> &rate,
                                const Vec<BaseType> &lower,
                                const Vec<BaseType> &lg) {
  std::mutex m;
  std::vector<double> scale(rate.size());
  for (size_t i = 0; i < scale.size(); i++) {
    scale[i] = 1.0 / rate[i];
  }
  // const Vec<BaseType> &scale = 1 / rate;
  if ((x.size() == 1) && (shape.size() == 1) && (scale.size() == 1) &&
      (lg.size() == 1)) {
    Vec<BaseType> ret(1);
    m.lock();
    GetRNGstate();
    ret[0] = R::qgamma(x[0], shape[0], scale[0], lower[0], lg[0]);
    PutRNGstate();
    m.unlock();
    return ret;
  } else {
    std::vector<size_t> sizes{shape.size(), static_cast<size_t>(scale.size()),
                              lower.size(), lg.size()};
    size_t max = x.size();
    for (size_t i = 1; i < sizes.size(); i++) {
      if (sizes[i] > max) {
        max = sizes[i];
      }
    }
    Vec<BaseType> res(max);
    for (size_t i = 0; i < res.size(); i++) {
      m.lock();
      GetRNGstate();
      res[i] = R::qgamma(x[i % x.size()], shape[i % shape.size()],
                         scale[i % scale.size()], lower[i % lower.size()],
                         lg[i % lg.size()]);
      PutRNGstate();
      m.unlock();
    }
    return res;
  }

  Vec<BaseType> ret(1);
  ret[0] = R_NaN;
  return ret;
}

} // namespace etr

#endif
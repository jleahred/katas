//  generated automatically

#include "fsm_login_base.h"

namespace login {
std::ostream &operator<<(std::ostream &os, State state) {
  switch (state) {
  case State::init:
    os << "init";
  case State::w_login:
    os << "w_login";
  case State::login:
    os << "login";
  case State::logout:
    os << "logout";
  }
  return os;
}

void BaseFsm::in(const rq_key &in) {
  switch (state) {
  case State::init: {
    state = State::w_login;
    send_key(in);
    break;
  }
  case State::w_login: {
    state = State::logout;
    log_error(in);
    break;
  }
  case State::login: {
    state = State::logout;
    log_error(in);
    break;
  }
  case State::logout: {
    state = State::logout;
    log_error(in);
    break;
  }
  }
}

void BaseFsm::in(const rq_login &in) {
  switch (state) {
  case State::init: {
    state = State::logout;
    log_error(in);
    break;
  }
  case State::w_login: {
    if (valid(in)) {
      state = State::login;
      send_login(in);
    } else {
      state = State::logout;
      log_error(in);
    }
    break;
  }
  case State::login: {
    state = State::logout;
    log_error(in);
    break;
  }
  case State::logout: {
    state = State::logout;
    log_error(in);
    break;
  }
  }
}

} // namespace login

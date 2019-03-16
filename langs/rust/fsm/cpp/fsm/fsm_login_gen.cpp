//  generated automatically

#include "fsm_login_gen.h"
#include "fsm_login.h"

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

Fsm::Fsm() : state(State::init) {}

void Fsm::in(const rq_key_t &in) {
  switch (state) {
  case State::init: {
    state = State::w_login;
    gen_key(in);
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

void Fsm::in(const rq_login_t &in) {
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

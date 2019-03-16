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

void Fsm::delete_state_info(void) { this->w_login_info.reset(); }

Fsm::Fsm() : state(State::init) {}

void Fsm::in(const rq_key_t &in) {
  switch (state) {
  case State::init: {
    delete_state_info();
    state = State::w_login;
    auto key = gen_key(in);
    send_key(in, key);
    w_login_info = std::make_unique<key_t>(key);
    break;
  }
  case State::w_login: {
    delete_state_info();
    state = State::logout;
    log_error(in);
    break;
  }
  case State::login: {
    delete_state_info();
    state = State::logout;
    log_error(in);
    break;
  }
  case State::logout: {
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
    if (valid(in, *this->w_login_info)) {
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

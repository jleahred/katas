#ifndef FSM_LOGIN_H
#define FSM_LOGIN_H

#include <string>

#include "fsm_login_gen.h"

namespace login {

struct init_info_t {};
struct w_login_info_t {};
struct login_info_t {};
struct logout_info_t {};

struct rq_key_t {};
struct rq_login_t {
  std::string key;
  std::string login;
};

} // namespace login
#endif // FSM_LOGIN_H

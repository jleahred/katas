#ifndef FSM_LOGIN_H
#define FSM_LOGIN_H

#include "fsm_login_base.h"

namespace login {

struct rq_key {};
struct rq_login {
  std::string key;
  std::string login;
};

} // namespace login

#endif // FSM_LOGIN_H

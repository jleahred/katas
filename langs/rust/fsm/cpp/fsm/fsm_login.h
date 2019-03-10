//  generated automatically

#ifndef FSM_LOGIN_H
#define FSM_LOGIN_H

#include "fsm_login_base.h"

namespace login {

struct rq_key {};
struct rq_login {
  std::string key;
  std::string login;
};

class Fsm : public BaseFsm {
public:
  Fsm() : BaseFsm() {}

protected:
  //    actions
  void log_error(const rq_key &in) override;
  void log_error(const rq_login &in) override;
  void log_error(const rq_logout &in) override;
  void log_error(const heartbeat &in) override;
  void log_error(const timer &in) override;
  void send_key(const rq_key &in) override;
  void send_login(const rq_login &in) override;
  void send_logout(const rq_logout &in) override;

  //    guards
  bool valid(const rq_login &in) override;
};
} // namespace login

#endif // FSM_LOGIN_H

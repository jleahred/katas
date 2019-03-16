//  generated automatically

#ifndef FSM_LOGIN_GENERATED_H
#define FSM_LOGIN_GENERATED_H

#include <iostream>
#include <memory>

namespace login {

enum class State { init, w_login, login, logout };
std::ostream &operator<<(std::ostream &os, State state);

//  ----------------------------------
//  forward declaration
//  define by hand on .h

//  in
struct rq_key_t;
struct rq_login_t;
struct rq_logout_t;
struct heartbeat_t;
struct timer_t;

//  generators
struct key_t;

//  forward declaration
//  ----------------------------------

class Fsm {
public:
  Fsm();
  State state;

public:
  //    automatic generated methods ---------
  //    defined on fsm_login_generated.cpp
  //
  void in(const rq_key_t &in);
  void in(const rq_login_t &in);
  void in(const rq_logout_t &in);
  void in(const heartbeat_t &in);
  void in(const timer_t &in);

protected:
  //    hand written methods    --------------
  //    to be defined on fsm_login.cpp
  //
  //    generators
  void gen_key(const rq_key_t &in);
  std::unique_ptr<key_t> key;

  //    actions
  void log_error(const rq_key_t &in);
  void log_error(const rq_login_t &in);
  void log_error(const rq_logout_t &in);
  void log_error(const heartbeat_t &in);
  void log_error(const timer_t &in);
  void send_key(const rq_key_t &in);
  void send_login(const rq_login_t &in);
  void send_logout(const rq_logout_t &in);

  //    guards
  bool valid(const rq_login_t &in);
};

} // namespace login

#endif // FSM_LOGIN_GENERATED_H

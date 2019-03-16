//  generated automatically  2019-03-16 19:26:16

#ifndef FSM_DOOR_BASE_H
#define FSM_DOOR_BASE_H

#include <iostream>

namespace door {

enum class State { init, w_login, login, logout };
std::ostream &operator<<(std::ostream &os, State state);

//  forward declaration
//  define by hand on .h
struct rq_key;
struct rq_login;
struct rq_logout;
struct heartbeat;
struct timer;

class BaseFsm {
protected:
  BaseFsm() : state(State::init) {}
  State state;

public:
  void in(const rq_key &in);
  void in(const rq_login &in);
  void in(const rq_logout &in);
  void in(const heartbeat &in);
  void in(const timer &in);

protected:
  //    actions
  virtual void log_error(const rq_key &in) = 0;
  virtual void log_error(const rq_login &in) = 0;
  virtual void log_error(const rq_logout &in) = 0;
  virtual void log_error(const heartbeat &in) = 0;
  virtual void log_error(const timer &in) = 0;
  virtual void send_key(const rq_key &in) = 0;
  virtual void send_login(const rq_login &in) = 0;
  virtual void send_logout(const rq_logout &in) = 0;

  //    guards
  virtual bool valid(const rq_login &in) = 0;
};

//  override by hand on cpp
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

#endif // FSM_LOGIN_BASE_H

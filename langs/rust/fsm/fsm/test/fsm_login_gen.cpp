
//  generated automatically  2019-03-17 22:02:08
//  do not modify it manually

#include "fsm_login_gen.h"
#include "fsm_login.h"

namespace login {

class BaseState {
public:
  BaseState() {}
  ~BaseState() {}

public:
  virtual SState in(const heartbeat_t& in) = 0;
  virtual SState in(const rq_key_t& in) = 0;
  virtual SState in(const rq_login_t& in) = 0;
  virtual SState in(const rq_logout_t& in) = 0;
  virtual SState in(const timer_t& in) = 0;

};


class init : public BaseState {
public:
    init(const init_info_t& i) : info(i) {}
    virtual ~init(){}

private:
    init_info_t info;

  SState in(const heartbeat_t& in) override;
  SState in(const rq_key_t& in) override;
  SState in(const rq_login_t& in) override;
  SState in(const rq_logout_t& in) override;
  SState in(const timer_t& in) override;

};

class w_login : public BaseState {
public:
    w_login(const w_login_info_t& i) : info(i) {}
    virtual ~w_login(){}

private:
    w_login_info_t info;

  SState in(const heartbeat_t& in) override;
  SState in(const rq_key_t& in) override;
  SState in(const rq_login_t& in) override;
  SState in(const rq_logout_t& in) override;
  SState in(const timer_t& in) override;

};

class login : public BaseState {
public:
    login(const login_info_t& i) : info(i) {}
    virtual ~login(){}

private:
    login_info_t info;

  SState in(const heartbeat_t& in) override;
  SState in(const rq_key_t& in) override;
  SState in(const rq_login_t& in) override;
  SState in(const rq_logout_t& in) override;
  SState in(const timer_t& in) override;

};

class logout : public BaseState {
public:
    logout(const logout_info_t& i) : info(i) {}
    virtual ~logout(){}

private:
    logout_info_t info;

  SState in(const heartbeat_t& in) override;
  SState in(const rq_key_t& in) override;
  SState in(const rq_login_t& in) override;
  SState in(const rq_logout_t& in) override;
  SState in(const timer_t& in) override;

};



Fsm::Fsm() {}
Fsm::~Fsm() {}

void Fsm::in(const heartbeat_t& in) { state ->in(in); }
void Fsm::in(const rq_key_t& in) { state ->in(in); }
void Fsm::in(const rq_login_t& in) { state ->in(in); }
void Fsm::in(const rq_logout_t& in) { state ->in(in); }
void Fsm::in(const timer_t& in) { state ->in(in); }



  SState init::in(const heartbeat_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState init::in(const rq_key_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState init::in(const rq_login_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState init::in(const rq_logout_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState init::in(const timer_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState w_login::in(const heartbeat_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState w_login::in(const rq_key_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState w_login::in(const rq_login_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState w_login::in(const rq_logout_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState w_login::in(const timer_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState login::in(const heartbeat_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState login::in(const rq_key_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState login::in(const rq_login_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState login::in(const rq_logout_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState login::in(const timer_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState logout::in(const heartbeat_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState logout::in(const rq_key_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState logout::in(const rq_login_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState logout::in(const rq_logout_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    
  SState logout::in(const timer_t& in) {
    return std::make_shared<_>(__info_t{}); heartbeat
    return std::make_shared<_>(__info_t{}); rq_key
    return std::make_shared<_>(__info_t{}); rq_login
    return std::make_shared<_>(__info_t{}); rq_logout
    return std::make_shared<_>(__info_t{}); timer
}
    

} // namespace login


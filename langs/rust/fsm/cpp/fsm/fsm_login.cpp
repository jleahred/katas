#include "fsm_login.h"
#include "fsm_login_gen.h"

namespace login {

//  generators  --------------------------------------------
void Fsm::gen_key(const rq_key_t & /*in*/) {
  key = std::make_unique<key_t>(key_t{"key"});
}

//  actions ------------------------------------------------
void Fsm::log_error(const rq_key_t &in) {
  std::cout << "error received rq_key  on state " << this->state << std::endl;
}
void Fsm::log_error(const rq_login_t &in) {
  std::cout << "error received rq_login  on state " << this->state << std::endl;
}
void Fsm::log_error(const rq_logout_t &in) {}
void Fsm::log_error(const heartbeat_t &in) {}
void Fsm::log_error(const timer_t &in) {}
void Fsm::send_key(const rq_key_t &in) {}
void Fsm::send_login(const rq_login_t &in) {}
void Fsm::send_logout(const rq_logout_t &in) {}

//  guards  -------------------------------------------------
bool Fsm::valid(const rq_login_t &in) { return true; }

} // namespace login

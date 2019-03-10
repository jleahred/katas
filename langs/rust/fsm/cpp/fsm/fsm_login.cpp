#include "fsm_login.h"

namespace login {

void Fsm::log_error(const rq_key &in) {
  std::cout << "error received rq_key  on state " << this->state << std::endl;
}
void Fsm::log_error(const rq_login &in) {
  std::cout << "error received rq_login  on state " << this->state << std::endl;
}
void Fsm::log_error(const rq_logout &in) {}
void Fsm::log_error(const heartbeat &in) {}
void Fsm::log_error(const timer &in) {}
void Fsm::send_key(const rq_key &in) {}
void Fsm::send_login(const rq_login &in) {}
void Fsm::send_logout(const rq_logout &in) {}

bool Fsm::valid(const rq_login &in) { return true; }

} // namespace login

#include "fsm_login.h"
#include "fsm_login_gen.h"

namespace login {

logout_info_t Fsm::in2logout(const heartbeat_t &i) {}
logout_info_t Fsm::in2logout(const rq_key_t &i) {}
w_login_info_t Fsm::in2w_login(const rq_key_t &i) {}
login_info_t Fsm::in2login(const rq_login_t &i) {}
logout_info_t Fsm::in2logout(const rq_login_t &i) {}
logout_info_t Fsm::in2logout(const rq_logout_t &i) {}
login_info_t Fsm::in2login(const timer_t &i) {}
logout_info_t Fsm::in2logout(const timer_t &i) {}

} // namespace login

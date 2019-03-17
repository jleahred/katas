#include "fsm_login.h"
#include "fsm_login_gen.h"

namespace login {

w_login_info_t in2w_login(const rq_key_t &i) { return w_login_info_t{}; }
logout_info_t in2logout(const rq_key_t &i) { return logout_info_t{}; }

logout_info_t in2logout(const rq_login_t &i) { return logout_info_t{}; }

} // namespace login

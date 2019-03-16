/*
 *
 *  [init]
 *      rq_key                  -   key     | send_key  ->  w_login(key)
 *      rq_login                -   log_err             ->  logout
 *      rq_logout               -   log_err             ->  logout
 *      heartbeat               -   log_err             ->  logout
 *      timer                   -   log_err             ->  logout
 *
 *
 *  [w_login, key_t]
 *      rq_key                  -   log_err             ->  logout
 *      rq_login    &   valid   -   send_login          ->  login
 *      rq_login                -   log_err             ->  logout
 *      rq_logout               -   log_err             ->  logout
 *      heartbeat               -   log_err             ->  logout
 *      timer                   -                       ->  logout
 *
 *
 *  [login]
 *      rq_key                  -   log_err             ->  logout
 *      rq_login                -   log_err             ->  logout
 *      rq_logout               -   send_logout         ->  logout
 *      heartbeat               -   log_err             ->  logout
 *      timer       &   on_time -                       ->  login
 *      timer                   -                       ->  logout
 *
 *
 *  [logout]
 *      rq_key                  -   log_err             ->  logout
 *      rq_login                -   log_err             ->  logout
 *      rq_logout               -   logout              ->  logout
 *      heartbeat               -   log_err             ->  logout
 *      timer                   -   log_err             ->  logout
 *
 */

#include <iostream>

#include "fsm_login.h"
//#include "fsm_login_gen.h"

int main() {
  login::Fsm login;
  login.in(login::rq_key_t{});
  login.in(login::rq_login_t{"key", "login"});
  login.in(login::rq_login_t{"key", "login"});
  return 0;
}

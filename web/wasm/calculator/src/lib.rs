// extern crate stdweb;
use std::str::FromStr;

#[macro_use]
extern crate yew;
// use yew::services::ConsoleService;

extern crate idata;

mod calculator;
use calculator::Calculator;

use yew::prelude::*;

#[derive(Debug)]
pub struct Model {
    calc_num: usize,
    // console: ConsoleService,
}

pub enum Msg {
    NumCalcs(usize),
}

//  ----------

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Model {
            calc_num: 2,
            // console: ConsoleService::new(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::NumCalcs(nc) => self.calc_num = nc,
        };
        true
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
        let ncalcs = || {
            html! {
            <>
                {"Num Calcs: "}
                <input type="number",
                        value={self.calc_num},
                        name="quantity", min="1", max="9",
                        onchange=|event| {
                            use std::cmp::{min, max};
                            match event {
                                ChangeData::Value(s) => {
                                    let v = usize::from_str(&s).unwrap_or_else(|_|1);
                                    let v = min(9, max(1, v));
                                    Msg::NumCalcs(v)
                                }
                                _ => {
                                    Msg::NumCalcs(1)
                                }
                            }
                        },/>
            </>
            }
        };

        let nav_bar = || {
            html! {
            <>
            <nav class="navbar sticky-top navbar-light bg-light",>
                <a class="navbar-brand", href="#",>{"Calculator"}</a>

                <button class="navbar-toggler", type="button", data-toggle="collapse", data-target="#nav_menu", aria-controls="nav_menu", aria-expanded="false", aria-label="Toggle navigation",>
                <span class="navbar-toggler-icon",></span>
                </button>
                <div class="collapse navbar-collapse", id="nav_menu",>
                    <a class="navbar-brand", href="https://github.com/jleahred/katas/tree/master/web/wasm/calculator",>{"Repository"}</a>
                    <br/>{ncalcs()}
                </div>
            </nav>
            </>
            }
        };

        let status = || {
            html!{
                <>
                    // {format!("{:#?}", self)}
                </>
            }
        };

        html! {
            <div>{nav_bar()}</div>

            <div class="container",><div class="row",>

            {for (1..=self.calc_num).map(|_| html!{<div class="col",><Calculator:/></div>})}

            </div></div>

            <div>{status()}</div>
        }
    }
}

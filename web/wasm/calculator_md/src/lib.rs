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
                {format!("Num Calcs: {}", self.calc_num)}
                <input type="range",
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

        let md_header = || {
            html!{
                <>
                    <header class="mdl-layout__header",>
                        <div class="mdl-layout__header-row",>
                        // <!-- Title -->
                        <span class="mdl-layout-title",>{"Calculator md"}</span>
                    //     // <!-- Add spacer, to align navigation to the right -->
                    //     <div class="mdl-layout-spacer",></div>
                    //     // <!-- Navigation -->
                    //     <nav class="mdl-navigation",>
                    //         <a class="mdl-navigation__link", href="https://github.com/jleahred/katas/tree/master/web/wasm/calculator",>{"Repository"}</a>
                    //     </nav>
                        </div>
                    </header>
                </>
            }
        };

        let md_drawer = || {
            html!{
            <>
                <div class="mdl-layout__drawer",>
                    <span class="mdl-layout-title",>{"Calculator md"}</span>
                    <nav class="mdl-navigation",>
                    <a class="mdl-navigation__link", href="https://github.com/jleahred/katas/tree/master/web/wasm/calculator_md",>{"Repository"}</a>
                    <div class="mdl-layout-spacer",></div>
                    {ncalcs()}
                    </nav>
                </div>
            </>
            }
        };

        let md_main = || {
            html!{
                <>
                <div class="section--center mdl-grid",>
                {for (1..=self.calc_num).map(|_| html!{ 
                     <div class="mdl-layout-spacer",></div>
                     <div class="mdl-cell mdl-cell--4-col",>
                    <Calculator:/>
                    </div>})}
                </div>
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
                <div class="mdl-layout mdl-js-layout mdl-layout--fixed-header",>
                {md_header()}
                {md_drawer()}

                <main class="mdl-layout__content",>
                {md_main()}
                <div>{status()}</div>
                </main>
                </div>
        }
    }
}

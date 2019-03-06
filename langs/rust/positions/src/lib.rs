// extern crate stdweb;

#[macro_use]
extern crate stdweb;

#[macro_use]
extern crate yew;
// use yew::services::ConsoleService;

extern crate failure;

// extern crate idata;

mod agregator;
mod json_api;
mod position;
mod wposition;

use wposition::Model as WPositions;

use yew::prelude::*;

#[derive(Debug)]
pub struct Model {
    // console: ConsoleService,
}

pub enum Msg {}

//  ----------

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Model {
            // console: ConsoleService::new(),
        }
    }

    fn update(&mut self, _msg: Self::Message) -> ShouldRender {
        // match msg {};
        true
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
        let nav_bar = || {
            html! {
            <>
            // <nav class="navbar sticky-top navbar-light bg-light",>
            //     <a class="navbar-brand", href="#",>{"Positions"}</a>

                // <button class="navbar-toggler", type="button", data-toggle="collapse", data-target="#nav_menu", aria-controls="nav_menu", aria-expanded="false", aria-label="Toggle navigation",>
                // <span class="navbar-toggler-icon",></span>
                // </button>
                // <div class="collapse navbar-collapse", id="nav_menu",>
                //     <a class="navbar-brand", href="https://",>{"Repository"}</a>
                // </div>
            // </nav>
            </>
            }
        };

        let status = || {
            html! {
                <>
                    // {format!("{:#?}", self)}
                </>
            }
        };

        html! {
            <div>{nav_bar()}</div>

            <div class="container",><div class="row",>

            <WPositions:/>

            </div></div>

            <div>{status()}</div>
        }
    }
}

// extern crate stdweb;

pub mod engine;

#[macro_use]
extern crate yew;
// use yew::services::ConsoleService;

extern crate idata;

mod html;
use crate::html::board::Board as HBoard;

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
        true
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
        html! {
            <div><h1>{"Connect 4"}</h1></div>
            <HBoard:/>
        }
    }
}

// extern crate stdweb;

#[macro_use]
extern crate yew;
// use yew::services::ConsoleService;

extern crate idata;

mod board;
use board::Board;

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

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        // match msg {
        //     Msg::NumCalcs(nc) => self.calc_num = nc,
        // };
        true
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
        html! {
            <h1>{"Connect 4"}</h1>
            <Board:/>
        }
    }
}

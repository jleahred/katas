// use std::result::Result;

use yew::prelude::*;
// use yew::services::ConsoleService;

pub struct Board {
    // console: ConsoleService,
}

pub enum Msg {}

//  ----------

impl Board {}

impl Component for Board {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Board {}
    }

    fn update(&mut self, _msg: Self::Message) -> ShouldRender {
        true
    }
    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        true
    }
}

impl Renderable<Board> for Board {
    fn view(&self) -> Html<Self> {
        let row = || {
            html! {
                <tr>
                    <td class="red",></td>
                    <td class="yellow",></td>
                    <td></td>
                    <td></td>
                    <td></td>
                    <td></td>
                    <td></td>
                </tr>
            }
        };
        html! {
            <div class="game",>
                <table class="board",>
                    {row()}
                    {row()}
                    {row()}
                    {row()}
                    {row()}
                    {row()}
                </table>
            </div>
        }
    }
}

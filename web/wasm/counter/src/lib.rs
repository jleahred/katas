extern crate stdweb;
#[macro_use]
extern crate yew;

// use stdweb::web::Date;
use yew::prelude::*;
use yew::services::ConsoleService;

pub struct Model {
    console: ConsoleService,
    value: i64,
}

pub enum Msg {
    Increment,
    Decrement,
    Bulk(Vec<Msg>),
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Model {
            console: ConsoleService::new(),
            value: 0,
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::Increment => {
                self.value += 1;
                self.console.log("plus one");
            }
            Msg::Decrement => {
                self.value += -1;
                self.console.log("minus one");
            }
            Msg::Bulk(list) => for msg in list {
                self.update(msg);
                self.console.log("Bulk action");
            },
        }
        true
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
        let nav_bar = || {
            html! {
            <nav class="navbar sticky-top navbar-light bg-light",>
                <a class="navbar-brand", href="#",>{"Counter"}</a>

                <button class="navbar-toggler", type="button", data-toggle="collapse", data-target="#nav_menu", aria-controls="nav_menu", aria-expanded="false", aria-label="Toggle navigation",>
                <span class="navbar-toggler-icon",></span>
                </button>
                <div class="collapse navbar-collapse", id="nav_menu",>
                    <a class="navbar-brand", href="https://github.com/jleahred/katas/tree/master/web/wasm/counter",>{"Repository"}</a>
                </div>
            </nav>
            }
        };
        let buttons_view = || {
            html! {
                <div class="row my-3",>
                <div class="mx-auto",>
                    <button type="button", class="btn btn-outline-primary mr-1", onclick=|_| Msg::Increment,>{ "Increment" }</button>
                    <button type="button", class="btn btn-outline-primary mr-1", onclick=|_| Msg::Decrement,>{ "Decrement" }</button>
                    <button type="button", class="btn btn-outline-info mr-1", onclick=|_| Msg::Bulk(vec![Msg::Increment, Msg::Increment]),>{ "Increment Twice" }</button>
                </div>
                </div>
            }
        };
        html! {
            <div>
            {nav_bar()}
            </div>
            <div class="container",>
            {buttons_view()}
            <div class="row",>
                <p class="row my-3 mx-auto",>{"Counter: "}{ self.value }</p>
            </div>
            // <div>
            //     <p class="row my-5 mx-auto",>{ Date::new().to_string() }</p>
            // </div>
            </div>
        }
    }
}

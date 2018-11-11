extern crate stdweb;
#[macro_use]
extern crate yew;
extern crate idata;

use idata::IString;
// use stdweb::web::Date;
use yew::prelude::*;
// use yew::services::ConsoleService;

pub struct Model {
    // console: ConsoleService,
    content: ModelContent,
}

#[derive(Debug)]
pub enum ModelContent {
    Error,
    Emtpy,
    Status(ModelStatus),
}

#[derive(Debug, Clone)]
pub struct ModelStatus {
    // console: ConsoleService,
    accumulator: f64,
    display: String,
    operator: Option<Operator>,
}

pub enum Msg {
    PressedDigit(char),
    Op(Operator),
    Clear,
    Reset,
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Subs,
    Mult,
    Div,
}

fn try_add_char2display(display: &mut String, ch: char) {
    let ignore_zero = |display: &str, ch| display == "" && ch == '0';
    if display.len() < 14 && !ignore_zero(&display, ch) {
        use std::str::FromStr;
        let new_display = display.clone().ipush(ch);
        let conv = f64::from_str(&new_display);
        if conv.is_ok() {
            display.push(ch);
        }
    }
}

impl ModelContent {
    fn display(&self) -> String {
        match self {
            ModelContent::Emtpy => "".to_string(),
            ModelContent::Error => "error".to_string(),
            ModelContent::Status(st) => st.display.clone(),
        }
    }
}

impl ModelStatus {
    fn new() -> Self {
        Self {
            display: "".to_string(),
            accumulator: 0.,
            operator: None,
        }
    }

    fn add_digit_to_display(mut self, d: char) -> Self {
        try_add_char2display(&mut self.display, d);
        self
    }

    fn process_operator(mut self, op: Operator) -> Self {
        use std::str::FromStr;
        let val_display = f64::from_str(&self.display);

        self.display.clear();
        self.operator = Some(op);
        self
    }

    fn clear(mut self) -> Self {
        self.display.clear();
        self
    }
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Model {
            content: ModelContent::Emtpy,
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        fn update_model_status(st: ModelStatus, msg: &Msg) -> ModelContent {
            match *msg {
                Msg::PressedDigit(d) => ModelContent::Status(st.add_digit_to_display(d)),
                Msg::Op(op) => ModelContent::Status(st.process_operator(op)),
                Msg::Clear => ModelContent::Status(st.clear()),
                Msg::Reset => ModelContent::Emtpy,
            }
        }
        fn update_model_error(msg: &Msg) -> ModelContent {
            match *msg {
                Msg::Reset => ModelContent::Emtpy,
                _ => ModelContent::Error,
            }
        }
        fn update_model_empty(msg: &Msg) -> ModelContent {
            update_model_status(ModelStatus::new(), msg)
        }
        //  ------------------------------------

        self.content = match self.content {
            ModelContent::Status(ref st) => update_model_status(st.clone(), &msg),
            ModelContent::Error => update_model_error(&msg),
            ModelContent::Emtpy => update_model_empty(&msg),
        };
        true
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
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
                </div>
            </nav>
            </>
            }
        };
        let display = || {
            html! {
                <>
                <input class="form-control my-3", id="display", placeholder="0.", disabled=true, value=&self.content.display(),>
                </input>
                </>
            }
        };

        let digit_button = |digit: char| {
            html! {
                <>
                <input class="form-group btn btn-outline-secondary butt", type="button", value=digit.to_string(), onclick=|_| Msg::PressedDigit(digit),>
                </input>
                </>
            }
        };

        let op_button = |op: Operator| {
            let sop = match op {
                Operator::Add => "+",
                Operator::Subs => "-",
                Operator::Mult => "*",
                Operator::Div => "/",
            };
            html! {
                <>
                <input class="form-group btn btn-outline-secondary butt", type="button", value=sop.to_string(), onclick=|_| Msg::Op(op),>
                </input>
                </>
            }
        };

        let buttons = || {
            html! {
                <>

                <div class="row mx-auto",>
                {digit_button('7')}  {digit_button('8')} {digit_button('9')} {op_button(Operator::Add)}
                </div>
                <div class="row mx-auto",>
                {digit_button('4')}  {digit_button('5')} {digit_button('6')} {op_button(Operator::Subs)}
                </div>
                <div class="row mx-auto",>
                {digit_button('1')}  {digit_button('2')} {digit_button('3')} {op_button(Operator::Mult)}
                </div>
                <div class="row mx-auto",>
                {digit_button('0')}  /*{op_button(".")} {op_button("=")}*/ {op_button(Operator::Div)}
                </div>
                <div class="row mx-auto",>
                    <input class="form-group btn btn-outline-info buttbig", type="button", value="Clear", onclick=|_| Msg::Clear, >
                    </input>
                    <input class="form-group btn btn-outline-danger buttbig", type="button", value="Reset", onclick=|_| Msg::Reset,>
                    </input>
                </div>

                </>
            }
        };

        let status = || {
            html!{
                <>
                    {format!("{:#?}", self.content)}
                </>
            }
        };

        html! {
            <div>{nav_bar()}</div>
            <div class="container",><div class="row my-3",><div class="mx-auto",>
                <form name="form",>
                    {display()}
                    {buttons()}
                </form>
            </div></div></div>

            <div>{status()}</div>
        }
    }
}

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
    st: MStatus,
}

#[derive(Debug)]
pub enum MStatus {
    Error,
    Emtpy,
    Calculating(ModelCalc),
    Display(ModelCalc),
}

#[derive(Debug, Clone)]
pub struct ModelCalc {
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

fn operation_from_operator(op: Option<Operator>) -> Option<Box<Fn(f64, f64) -> f64>> {
    let add = Box::new(move |l: f64, r: f64| l + r);
    let sub = Box::new(move |l: f64, r: f64| l - r);
    let mult = Box::new(move |l: f64, r: f64| l * r);
    let div = Box::new(move |l: f64, r: f64| l / r);
    match op {
        Some(Operator::Add) => Some(add),
        Some(Operator::Subs) => Some(sub),
        Some(Operator::Mult) => Some(mult),
        Some(Operator::Div) => Some(div),
        None => None,
    }
}

fn try_add_char2display(display: String, ch: char) -> String {
    let ignore_zero = |display: &str, ch| display == "" && ch == '0';

    use std::str::FromStr;
    let can_add_display = display.len() < 14 && !ignore_zero(&display, ch);
    let new_display = display.clone().ipush(ch);
    match (can_add_display, f64::from_str(&new_display).ok()) {
        (true, Some(_)) => new_display,
        _ => display,
    }
}

impl MStatus {
    fn display(&self) -> String {
        match self {
            MStatus::Emtpy => "".to_string(),
            MStatus::Error => "error".to_string(),
            MStatus::Calculating(st) => st.display.clone(),
            MStatus::Display(st) => st.display.clone(),
        }
    }
}

impl ModelCalc {
    fn new() -> Self {
        Self {
            display: "".to_string(),
            accumulator: 0.,
            operator: None,
        }
    }

    fn add_digit_to_display(mut self, d: char) -> Self {
        self.display = try_add_char2display(self.display, d);
        self
    }

    fn calc_accumulator(&self) -> Option<f64> {
        let operation = operation_from_operator(self.operator);

        match (operation, self.num_from_display()) {
            (Some(op), Some(ndisplay)) => Some(op(ndisplay, self.accumulator)),
            (None, Some(ndisplay)) => Some(ndisplay),
            _ => Some(self.accumulator),
        }
    }

    fn num_from_display(&self) -> Option<f64> {
        use std::str::FromStr;
        f64::from_str(&self.display).ok()
    }

    fn process_operator(self, op: Operator) -> MStatus {
        match self.calc_accumulator() {
            None => MStatus::Error,
            Some(new_acc) => MStatus::Display(ModelCalc {
                display: new_acc.to_string(),
                accumulator: new_acc,
                operator: Some(op),
            }),
        }
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
        Model { st: MStatus::Emtpy }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        fn update_model_status(st: ModelCalc, msg: &Msg) -> MStatus {
            match *msg {
                Msg::PressedDigit(d) => MStatus::Calculating(st.add_digit_to_display(d)),
                Msg::Op(op) => st.process_operator(op),
                Msg::Clear => MStatus::Calculating(st.clear()),
                Msg::Reset => MStatus::Emtpy,
            }
        }
        fn update_model_error(msg: &Msg) -> MStatus {
            match *msg {
                Msg::Reset => MStatus::Emtpy,
                _ => MStatus::Error,
            }
        }
        fn update_model_empty(msg: &Msg) -> MStatus {
            update_model_status(ModelCalc::new(), msg)
        }
        fn update_model_display(st: &ModelCalc, msg: &Msg) -> MStatus {
            match *msg {
                Msg::PressedDigit(d) => {
                    let st = st.clone().clear().add_digit_to_display(d);
                    MStatus::Calculating(st)
                }
                Msg::Op(op) => ModelCalc {
                    accumulator: 0.,
                    display: st.display.clone(),
                    operator: None,
                }.process_operator(op),
                Msg::Clear => MStatus::Emtpy,
                Msg::Reset => MStatus::Emtpy,
            }
        }
        //  ------------------------------------

        self.st = match self.st {
            MStatus::Calculating(ref st) => update_model_status(st.clone(), &msg),
            MStatus::Error => update_model_error(&msg),
            MStatus::Emtpy => update_model_empty(&msg),
            MStatus::Display(ref st) => update_model_display(&st, &msg),
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
                <input class="form-control my-3", id="display", placeholder="0.", disabled=true, value=&self.st.display(),>
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
                    {format!("{:#?}", self.st)}
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

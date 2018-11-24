use std::result::Result;

use idata::IString;
use std::str::FromStr;
// use stdweb::web::Date;
use yew::prelude::*;
// use yew::services::ConsoleService;

type ModelStatus = Result<MOptions, String>;

pub struct Calculator {
    // console: ConsoleService,
    st: ModelStatus,
}

#[derive(Debug)]
pub enum MOptions {
    Emtpy,
    Num(NumSt),
    Op(OpSt),
    OpNum(OpNumSt),
    Res(ResSt),
}

pub enum Msg {
    Digit(char),
    Op(Operator),
    Equal,
    Clear,
    Reset,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Add,
    Subs,
    Mult,
    Div,
}

#[derive(Debug, Clone)]
pub struct NumSt(String);

#[derive(Debug, Clone)]
pub struct OpSt {
    display: String,
    accumulator: f64,
    operator: Operator,
}

#[derive(Debug, Clone)]
pub struct OpNumSt {
    display: String,
    accumulator: f64,
    operator: Operator,
}

#[derive(Debug, Clone)]
pub struct ResSt {
    display: String,
    accumulator: f64,
    last_input: f64,
    operator: Operator,
}

//  ----------

impl Calculator {
    fn display(&self) -> String {
        match self.st {
            Ok(ref st) => st.display(),
            Err(ref descr) => format!("err {}", descr),
        }
    }
}

impl MOptions {
    fn display(&self) -> String {
        match self {
            MOptions::Emtpy => "".to_string(),
            MOptions::Num(ref snum) => snum.0.clone(),
            MOptions::Op(ref st) => st.display.clone(),
            MOptions::OpNum(ref st) => st.display.clone(),
            MOptions::Res(ref st) => st.display.clone(),
        }
    }
}

impl NumSt {
    fn process_op(&self, op: Operator) -> ModelStatus {
        Ok(MOptions::Op(OpSt {
            display: self.0.clone(),
            accumulator: f64::from_str(&self.0).map_err(|_| "conv")?,
            operator: op,
        }))
    }

    fn process_digit(&self, ch: char) -> ModelStatus {
        Ok(MOptions::Num(NumSt(try_add_char2display(&self.0, ch))))
    }
}

impl OpSt {
    fn process_digit(&self, ch: char) -> ModelStatus {
        let display = try_add_char2display("", ch);
        Ok(MOptions::OpNum(OpNumSt {
            display,
            accumulator: self.accumulator,
            operator: self.operator,
        }))
    }
}

impl OpNumSt {
    fn clear(&self) -> ModelStatus {
        Ok(MOptions::OpNum(OpNumSt {
            display: "".to_string(),
            accumulator: self.accumulator,
            operator: self.operator,
        }))
    }

    fn process_op(self, op: Operator) -> ModelStatus {
        let new_acc = self.calc()?;
        Ok(MOptions::Op(OpSt {
            display: new_acc.to_string(),
            accumulator: new_acc,
            operator: op,
        }))
    }

    fn process_digit(mut self, ch: char) -> ModelStatus {
        self.display = try_add_char2display(&self.display, ch);

        Ok(MOptions::OpNum(self))
    }

    fn process_equal(self) -> ModelStatus {
        let new_acc = self.calc()?;
        match f64::from_str(&self.display).ok() {
            Some(last_input) => Ok(MOptions::Res(ResSt {
                display: new_acc.to_string(),
                accumulator: new_acc,
                operator: self.operator,
                last_input,
            })),
            None => Err("conv displ".to_string()),
        }
    }

    fn calc(&self) -> Result<f64, String> {
        let operation = operation_from_operator(self.operator);
        let onum_display = f64::from_str(&self.display).ok();
        let result = match onum_display {
            Some(ndispl) => operation(self.accumulator, ndispl),
            None => Err("conv displ".to_string()),
        }?;
        Ok(result)
    }
}

impl ResSt {
    fn process_op(self, op: Operator) -> ModelStatus {
        NumSt(self.display).process_op(op)
    }

    fn process_digit(self, ch: char) -> ModelStatus {
        NumSt("".to_string()).process_digit(ch)
    }

    fn process_equal(self) -> ModelStatus {
        let operation = operation_from_operator(self.operator);
        let accumulator = operation(self.accumulator, self.last_input)?;
        Ok(MOptions::Res(ResSt {
            display: accumulator.to_string(),
            accumulator,
            last_input: self.last_input,
            operator: self.operator,
        }))
    }
}

fn try_add_char2display(display: &str, ch: char) -> String {
    let ignore_zero = |display: &str, ch| display == "0" && ch == '0';

    let can_add_display = display.len() < 14 && !ignore_zero(&display, ch);
    let new_display = if display == "0" {
        "".to_string()
    } else {
        display.to_string()
    }.ipush(ch);
    match (can_add_display, f64::from_str(&new_display).ok()) {
        (true, Some(_)) => new_display,
        _ => display.to_string(),
    }
}

fn operation_from_operator(op: Operator) -> Box<Fn(f64, f64) -> Result<f64, String>> {
    let add = Box::new(move |l: f64, r: f64| Ok(l + r));
    let sub = Box::new(move |l: f64, r: f64| Ok(l - r));
    let mult = Box::new(move |l: f64, r: f64| Ok(l * r));
    let div = Box::new(move |l: f64, r: f64| {
        if r != 0. {
            Ok(l / r)
        } else {
            Err("div by zero".to_string())
        }
    });
    match op {
        Operator::Add => add,
        Operator::Subs => sub,
        Operator::Mult => mult,
        Operator::Div => div,
    }
}

fn update_model_empty(msg: &Msg) -> Option<ModelStatus> {
    match msg {
        Msg::Reset => None,
        Msg::Clear => None,
        Msg::Equal => None,
        Msg::Op(_) => Some(Err("op on empty".to_string())),
        Msg::Digit(d) => Some(Ok(MOptions::Num(NumSt(d.to_string())))),
    }
}

fn update_model_error(msg: &Msg) -> Option<ModelStatus> {
    match msg {
        Msg::Reset => Some(Ok(MOptions::Emtpy)),
        Msg::Clear => None,
        Msg::Equal => None,
        Msg::Op(_) => None,
        Msg::Digit(_) => None,
    }
}

fn update_model_num(snum: &NumSt, msg: &Msg) -> Option<ModelStatus> {
    match msg {
        Msg::Reset => Some(Ok(MOptions::Emtpy)),
        Msg::Clear => Some(Ok(MOptions::Num(NumSt("".to_string())))),
        Msg::Equal => None,
        Msg::Op(ref op) => Some(snum.process_op(*op)),
        Msg::Digit(d) => Some(snum.process_digit(*d)),
    }
}

fn update_model_op(op_st: &OpSt, msg: &Msg) -> Option<ModelStatus> {
    match msg {
        Msg::Reset => Some(Ok(MOptions::Emtpy)),
        Msg::Clear => None,
        Msg::Equal => Some(Err("incompl".to_string())),
        Msg::Op(_) => Some(Err("rec op w num".to_string())),
        Msg::Digit(d) => Some(op_st.process_digit(*d)),
    }
}

fn update_model_op_num(opn_st: OpNumSt, msg: &Msg) -> Option<ModelStatus> {
    Some(match msg {
        Msg::Reset => Ok(MOptions::Emtpy),
        Msg::Clear => opn_st.clear(),
        Msg::Equal => opn_st.process_equal(),
        Msg::Op(op) => opn_st.process_op(*op),
        Msg::Digit(d) => opn_st.process_digit(*d),
    })
}

fn update_model_res(st: ResSt, msg: &Msg) -> Option<ModelStatus> {
    match msg {
        Msg::Reset => Some(Ok(MOptions::Emtpy)),
        Msg::Clear => None,
        Msg::Equal => Some(st.process_equal()),
        Msg::Op(op) => Some(st.process_op(*op)),
        Msg::Digit(d) => Some(st.process_digit(*d)),
    }
}

impl Component for Calculator {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Calculator {
            st: Ok(MOptions::Emtpy),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        let new_st = match self.st {
            Ok(MOptions::Emtpy) => update_model_empty(&msg),
            Ok(MOptions::Num(ref snum)) => update_model_num(snum, &msg),
            Ok(MOptions::Op(ref st)) => update_model_op(st, &msg),
            Ok(MOptions::OpNum(ref st)) => update_model_op_num(st.clone(), &msg),
            Ok(MOptions::Res(ref st)) => update_model_res(st.clone(), &msg),
            Err(_) => update_model_error(&msg),
        };
        if let Some(st) = new_st {
            self.st = st
        }
        true
    }
    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        true
    }
}

impl Renderable<Calculator> for Calculator {
    fn view(&self) -> Html<Self> {
        let display = || {
            html! {
                <>
                <input class="form-control my-3", id="display", placeholder="0.", disabled=true, value=&self.display(),>
                </input>
                </>
            }
        };

        let digit_button = |digit: char| {
            html! {
                <>
                <input class="form-group btn btn-outline-secondary butt", type="button", value=digit.to_string(), onclick=|_| Msg::Digit(digit),>
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

        let dot_button = || {
            html!{
                <>
                <input class="form-group btn btn-outline-secondary butt", type="button", value=".", onclick=|_| Msg::Digit('.'),>
                </input>
                </>
            }
        };

        let equal_button = || {
            html! {
                <>
                <input class="form-group btn btn-outline-primary butt", type="button", value="=", onclick=|_| Msg::Equal,>
                </input>
                </>
            }
        };

        let clear_reset_buttons = || {
            html!{
                <>
                <div class="row mx-auto",>
                    <input class="form-group btn btn-outline-info buttbig", type="button", value="Clear", onclick=|_| Msg::Clear, >
                    </input>
                    <input class="form-group btn btn-outline-danger buttbig", type="button", value="Reset", onclick=|_| Msg::Reset,>
                    </input>
                </div>
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
                {digit_button('0')}
                {dot_button()}
                {equal_button()}
                {op_button(Operator::Div)}
                </div>

                </>
            }
        };

        let status = || {
            html!{
                <>
                    // {format!("{:#?}", self.st)}
                </>
            }
        };

        // html! {
        //     <button>{ "&self.title" }</button>
        // }
        html! {
            <div class="container",><div class="row my-3",><div class="mx-auto",>
                <form name="form",>
                    {display()}
                    {clear_reset_buttons()}
                    {buttons()}
                </form>
            </div></div></div>

            <div>{status()}</div>
        }
    }
}

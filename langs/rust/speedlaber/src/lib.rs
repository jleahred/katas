//! [Web-sys docs](https://rustwasm.github.io/wasm-bindgen/api/web_sys/struct.CanvasRenderingContext2d.html)
//! [Web-sys example](https://rustwasm.github.io/wasm-bindgen/examples/2d-canvas_info.html)
//! [MDN](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/drawWindow)

use seed::{prelude::*, *};
use web_sys::HtmlCanvasElement;

mod map;
mod matrix;
// use map::Matrix;
use map::Map;
mod canvas;

const DEF_SIZE_S: &str = "500";
const DEF_SIZE: usize = 500;
const MATRIX_SIZE_S: &str = "10";
const MATRIX_SIZE: usize = 10;

// ------ ------
//     Init
// ------ ------

fn init(_: Url, orders: &mut impl Orders<Msg>) -> Model {
    // orders.after_next_render(|_| Msg::Rendered);
    Model::default()
}

// ------ ------
//     Model
// ------ ------

struct Model {
    canvas_info: CanvasInfo,
    edit: EditInfo,
    map: Map,
}

struct CanvasInfo {
    size: usize,
    canvas: ElRef<HtmlCanvasElement>,
}

struct EditInfo {
    canvas_size: String,
    matrix_size: String,
}

impl Default for Model {
    fn default() -> Self {
        Self {
            canvas_info: CanvasInfo::default(),
            edit: EditInfo::default(),
            map: map::new_zero(MATRIX_SIZE),
        }
    }
}

impl Default for CanvasInfo {
    fn default() -> Self {
        Self {
            size: DEF_SIZE,
            canvas: Default::default(),
        }
    }
}

impl Default for EditInfo {
    fn default() -> Self {
        Self {
            canvas_size: DEF_SIZE_S.to_string(),
            matrix_size: MATRIX_SIZE_S.to_string(),
        }
    }
}

// ------ ------
//    Update
// ------ ------

#[derive(Clone)]
enum Msg {
    Rendered,
    ClickRender,
    EditChange(MsgEditChange),
}

#[derive(Clone)]
enum MsgEditChange {
    Size(String),
    RowsCols(String),
}

fn update(msg: Msg, model: &mut Model, orders: &mut impl Orders<Msg>) {
    match msg {
        Msg::Rendered => {
            // We want to call `.skip` to prevent infinite loop.
            // (However infinite loops are useful for animations.)
            // orders.after_next_render(|_| Msg::Rendered).skip();
        }
        Msg::ClickRender => {
            model.map = map::new_zigzag(model.edit.matrix_size.parse::<usize>().unwrap_or(10));
            let canvas_ctx = seed::canvas_context_2d(
                &model.canvas_info.canvas.get().expect("get canvas element"),
            );
            canvas::draw(canvas_ctx, model.canvas_info.size, &model.map);
        }
        Msg::EditChange(ec) => {
            update_edit_change(ec, model, orders);
            // let canvas_size = model.edit.canvas_size.parse::<usize>().unwrap_or(100);
            // let cell_size = (canvas_size as f32 / model.map.size as f32 + 0.4999) as usize;
            // model.canvas_info.size = model.map.size * cell_size;
            // model.edit.canvas_size = format!("{}", model.canvas_info.size);
        }
    }
}

fn update_edit_change(msg: MsgEditChange, model: &mut Model, _orders: &mut impl Orders<Msg>) {
    match msg {
        MsgEditChange::Size(s) => model.edit.canvas_size = s,
        MsgEditChange::RowsCols(s) => model.edit.matrix_size = s,
    }
}

macro_rules! msg_edit_change {
    ($t:ident) => {
        |v| Msg::EditChange(MsgEditChange::$t(v))
    };
}

// ------ ------
//     View
// ------ ------

fn view(model: &Model) -> impl IntoNodes<Msg> {
    div![
        label![
            "canvas size (nxn):",
            input![
                attrs! {At::Value => model.edit.canvas_size.to_string(), At::Type => "number",
                At::Style => background_edit_color(model.canvas_info.size, &model.edit.canvas_size)},
                // input_ev(Ev::Input, |s| Msg::EditChange(MsgEditChange::Size(s))),
                input_ev(Ev::Input, msg_edit_change!(Size)),
            ],
        ],
        label![
            "map: (nxn)",
            input![
                attrs! {At::Value => model.edit.matrix_size.to_string(), At::Type => "number",
                At::Style => background_edit_color(model.map.matrix.size, &model.edit.matrix_size)},
                input_ev(Ev::Input, msg_edit_change!(RowsCols)),
            ]
        ],
        p![],
        label!["Load map: ", attrs! {At::For => "cars"}],
        select![
            attrs! {At::Name => "cars", At::Id => "cars",}
                option![attrs! {At::Value => "random",}, "random"]
        ],
        // <label for="cars">Choose a car:</label>

        // <select name="cars" id="cars">
        //   <option value="volvo">Volvo</option>
        //   <option value="saab">Saab</option>
        //   <option value="mercedes">Mercedes</option>
        //   <option value="audi">Audi</option>
        // </select>
        p![],
        button!["render lab", ev(Ev::Click, |_| Msg::ClickRender)],
        p![],
        div![
            // style! {St::Display => "flex"},
            canvas![
                el_ref(&model.canvas_info.canvas),
                attrs![
                    At::Width => px(model.canvas_info.size),
                    At::Height => px(model.canvas_info.size),
                ],
                // style![
                //     St::Border => "1px solid gray",
                // ],
            ],
        ]
    ]
}

fn background_edit_color(s1: usize, s2: &str) -> &str {
    if s1 == s2.parse::<usize>().unwrap_or(1) {
        ""
    } else {
        "background: yellow"
    }
}

// ------ ------
//     Start
// ------ ------

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}

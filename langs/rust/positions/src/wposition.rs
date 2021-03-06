// use std::result::Result;
use std::collections::HashSet;
use std::time::Duration;

use stdweb::Value;

use crate::position::*;
use crate::json_api::{JsonApiAdd, JsonApiDel};

extern crate failure;
extern crate yew;
use yew::format::Format;
use yew::format::Json;
use yew::prelude::*;
use yew::services::websocket::{WebSocketService, WebSocketStatus, WebSocketTask};
use yew::services::{ConsoleService, IntervalService, Task};

use crate::agregator;
// use stdweb::web::Date;

pub struct Model {
    config: ModelConfig,
    ws: Option<WebSocketTask>,
    wss: WebSocketService,
    link: ComponentLink<Model>,
    console: ConsoleService,
    group_pos: GroupsPos,
    api_posisions: Box<Positions>,
    expanded: HashSet<(String, String)>, 
    connect2: String,

    timer_interval_connect: IntervalService,
    timer_callback_connect: Callback<()>,
    timer_job_connect: Option<Box<Task>>,

    timer_interval_update: IntervalService,
    timer_callback_update: Callback<()>,
    timer_job_update: Option<Box<Task>>,

    conn_status: ConnStatus,
    update_pending: bool,
}

struct ModelConfig {
    show_address_input: bool,
}

pub enum Msg {
    ClickConnect,
    ClickFakeData(u8),
    Connection(ConnStatus),
    Received(Format<serde_json::value::Value>),
    UpdatedUrl(String),
    ExpandCollapse((String, String)),
    TickConnect,
    TickUpdate,
}

#[derive(PartialEq, Clone, Copy)]
pub enum ConnStatus {
    Ok,
    Connecting,
    Disconnected,
}

impl Model {
    fn connect(&mut self) {
        self.conn_status = ConnStatus::Connecting;        
        self.console.log("trying to connect");
        let cbout = self.link.send_back(|Json(data)| Msg::Received(data));
        let cbnot = self.link.send_back(|input| {
            // ConsoleService::new().log(&format!("Notification: {:?}", input));
            match input {
                WebSocketStatus::Opened => Msg::Connection(ConnStatus::Ok),
                WebSocketStatus::Closed | WebSocketStatus::Error => Msg::Connection(ConnStatus::Disconnected),
            }
        });
        let task = self.wss.connect(&self.connect2, cbout, cbnot);
        self.ws = Some(task);
    }
}



impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, mut link: ComponentLink<Self>) -> Self {
        let mut console = ConsoleService::new();

        let value : stdweb::Value = js!( return show_address_input(); );
        let show_address_input = match value {
            stdweb::Value::Bool(b) => b,
            _ => {
                 console.log("invalid show_address_input() in config.js");
                 false
            }
        };
        // console.log(&format!("invalid show_address_input() alue: {}", show_address_input));
        
        let timer_callback_connect =link.send_back(|_| Msg::TickConnect); 
        let mut timer_interval_connect = IntervalService::new(); 
        let timer_handle_connect = timer_interval_connect.spawn(Duration::from_secs(2), timer_callback_connect.clone());

        let timer_callback_update =link.send_back(|_| Msg::TickUpdate); 
        let mut timer_interval_update = IntervalService::new(); 
        let timer_handle_update = timer_interval_update.spawn(Duration::from_millis(200), timer_callback_update.clone());

        Model {
            config: ModelConfig{show_address_input},
            update_pending: false,
            conn_status: ConnStatus::Disconnected,
            console,
            ws: None,
            wss: WebSocketService::new(),
            link,

            timer_callback_connect,
            timer_interval_connect,
            timer_job_connect: Some(Box::new(timer_handle_connect)),

            timer_callback_update,
            timer_interval_update,
            timer_job_update: Some(Box::new(timer_handle_update)),

            api_posisions: Box::new(vec![]),
            group_pos: vec![], 
            expanded: HashSet::new(),
            connect2: match stdweb::web::window().location() {
                Some(location) => {
                    match location.host() {
                        Ok(href) => format!("ws://{}/positions/trayport", href.to_string()),
                        _ => "".to_string()
                    }
                },
                _ => "".to_string()
            }
            // connect2: "ws://127.0.0.1:9006/hi/".to_string(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::ClickFakeData(n) => {
                self.group_pos = test_fill_fake(n);
                true
            }
            Msg::ClickConnect => {
                self.connect();
                true
            }
            Msg::Connection(conn_status) => 
            {
                match conn_status {
                    ConnStatus::Disconnected => self.ws = None,
                    ConnStatus::Ok => { 
                        self.api_posisions.clear();
                        self.update_pending = true;
                    }
                    _ => (),
                }
                self.conn_status = conn_status;
                true
            }
            Msg::ExpandCollapse(ex) => {
                if self.expanded.contains(&ex) {
                    self.expanded.remove(&ex);
                } else {
                    self.expanded.insert(ex);
                }
                true
            }
            Msg::Received(Err(e)) => {
                self.console.log(&format!("received Error {:?}", e));
                true
            }
            Msg::Received(Ok(data)) => {
                self.console.log(&format!("received OK {}", data));
                self.console.log(&format!("received OK {}", data["_msg_type"].to_string()));
                if  let Some(msg_type) = data["_msg_type"].as_str() {
                    let result = 
                    match &msg_type as &str {
                        "PubNewBookPosition" => process_add(self, &data),
                        "PubDelBookPosition" => process_del(self, &data),
                        _ => Err(format!("_msg_type unknown {}", data))
                    };
                    if let Err(e) = result {
                        self.console.log(&e);
                    }
                } else {
                    self.console.log(&format!("missing _msg_typoe {}", data))
                }
                self.update_pending = true;
                false
            }
            Msg::UpdatedUrl(url) => {
                self.connect2 = url;
                true
            }
            Msg::TickConnect => {
                if self.ws.is_none() {
                    self.connect();
                }
                false
            }
            Msg::TickUpdate => {
                if self.update_pending {
                    self.group_pos = agregator::gen_group_positions(&self.api_posisions);
                    self.update_pending = false;
                    true
                } else {
                    false
                }
            }
        }
    }
    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        true
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
        // let status = || {
        //     html! {
        //         <>
        //             // {format!("{:#?}", self.st)}
        //         </>
        //     }
        // };


        let connect_params = |msg : &str| {
            if self.config.show_address_input {
                html! {
                    <>
                        <span onclick=|_| Msg::ClickConnect,>
                        {msg}
                        </span>
                        <input type="text",
                            value={self.connect2.to_string()},
                            oninput=|input| Msg::UpdatedUrl(input.value), >
                        </input>
                    </>
                }
            } else {html!{<></>}}
        };

        let test_fake = || {
            html! {
                <>
                    <p>
                    // <span onclick=|_| Msg::ClickFakeData(0),>{"fake0 "}</span>
                    // <span onclick=|_| Msg::ClickFakeData(1),>{"fake1 "}</span>
                    // <span onclick=|_| Msg::ClickFakeData(2),>{"fake2 "}</span>
                    // <span onclick=|_| Msg::ClickFakeData(3),>{"fake3 "}</span>
                    // <span onclick=|_| Msg::ClickFakeData(4),>{"fake4 "}</span>
                    // <span onclick=|_| Msg::ClickFakeData(5),>{"fake5 "}</span>
                    </p>
                </>
            }
        };

        let connected = || {
            html! {
                <>
                <p>
                {
                    match self.conn_status {
                        ConnStatus::Ok => html! {<>{if self.config.show_address_input { "connected" } else {""}  }</>},
                        ConnStatus::Connecting => html! {<>{connect_params("connecting")}</>},
                        ConnStatus::Disconnected => html! {<>{connect_params("connecting")}</>},
                    }
                }
                </p>
                </>
            }
        };


        let header = || {
            html! {
                <>
            <thead>
                <tr>
                <th scope="col", class="uk-width-small header",>{"id"}</th>
                <th scope="col", class="uk-width-medium header",>{"descr"}</th>
                <th scope="col", class="uk-width-small header center",>{"qty"}</th>
                <th scope="col", class="uk-width-small header center",>{"bid"}</th>
                <th scope="col", class="uk-width-small header center",>{"ask"}</th>
                <th scope="col", class="uk-width-small header center",>{"qty"}</th>
                <th scope="col", class="uk-width-medium header",>{"updated"}</th>
                </tr>
            </thead>
                </>
            }
        };

        let leveln = |n: usize, bids: &Levels, asks: &Levels, bordertop: bool| {
            let price_quantity = |levels: &Levels| {
                match levels.get(n) {
                    Some(l) => (format!("{:.3}", l.price), format!("{}", l.qty)),
                    _ => ("".to_string(), "".to_string())
                }
            };
            let (pbid, qbid) = price_quantity(bids);
            let (pask, qask) = price_quantity(asks);
            let tdclass = || if bordertop {
                "center bordertop"
            } else {"center celldepth"};
            html! {
                <>
                <td class=tdclass(),> { qbid } </td> <td class=tdclass(),> { pbid } </td>
                <td class=tdclass(),> { pask } </td> <td class=tdclass(),> { qask } </td>
                </>
            }
        };

        let depth_levels = |pos_by_prod: &PositionsByProduct| {
            html! {
                <>{
                for (1..=5).map(|i| 
                    html!{
                        <tr><td></td><td></td>
                        {leveln(i, &pos_by_prod.bids, &pos_by_prod.asks, false)}
                        <td class="transparent",>{"_"}</td></tr>
                })
                }</>
            }
        };

        let pos_by_prod_view = |group: &str, pos_by_prod: &PositionsByProduct| {
            let g = group.to_string();
            let id = pos_by_prod.isin.to_string();
            html! {
                <>
                <tr onclick=|_| Msg::ExpandCollapse((g.clone(), id.clone())),>
                // <th scope="row",>{1}</th>
                // <td>{&pos_by_prod.id}</td>
                <td class="bordertop",>{&pos_by_prod.isin}</td>
                <td class="uk-text-truncate bordertop",>{&pos_by_prod.desc}</td>
                {leveln(0, &pos_by_prod.bids, &pos_by_prod.asks, true)}
                <td class="uk-text-truncate bordertop",>{&pos_by_prod.updated}</td>
                </tr>
                {
                    if self.expanded.contains(&(group.to_string(), pos_by_prod.isin.to_string())) {
                        depth_levels(pos_by_prod)
                    } else {
                        html!{<></>}
                    }
                }
                </>
            }
        };

        let rows = |group: &str, pos_by_prod: &PosByProds| {
            html! {
                <>
                    {for (pos_by_prod.iter()).map(|pos_by_prod| {
                        pos_by_prod_view(group, &pos_by_prod)
                    })}
                </>
            }
        };

        let table = |group:&str, pos_prods: &PosByProds| {
            let style_disconected = if self.conn_status==ConnStatus::Ok {
                ""
            } else {
                "disconected"
            };
            html! {
                <>
                <h3 class=style_disconected.to_string(),>{group}</h3>
                <div class="uk-overflow-auto table_pading",>
                    <table class="uk-table uk-table-small",>
                    {header()}
                    <tbody class=style_disconected,>
                        {rows(group, pos_prods)}
                    </tbody>
                    </table>
                </div>            
                </>
            }
        };

        html! {
            <>
            {connected()}
            {test_fake()}

            {
                for (self.group_pos.iter()).map(|positions_group| {
                    table(&positions_group.group, &positions_group.pos_prods)
                })
            }
            </>
        }
    }
}

fn process_add(model: &mut Model, data: &serde_json::value::Value) -> Result<(), String> {
    match serde_json::from_str::<JsonApiAdd>(&data.to_string()) {
        Ok(pos) => {
        let mut fake = Box::new(vec![]);
        std::mem::swap(&mut fake, &mut model.api_posisions);
        let mut fake = match agregator::add(fake, pos) {
            Ok(aps) => aps,
            Err((e, aps)) =>{
                model.console.log(&e.to_string());
                aps
            },
        };
        std::mem::swap(&mut fake, &mut model.api_posisions);
        // model.group_pos = agregator::gen_group_positions(&model.api_posisions);
        Ok(())
        }
        Err(e) => Err(e.to_string())
    }
    // if let Ok(pos) =
    //     serde_json::from_str::<JsonApiAdd>(&data.to_string())
    // {
    //     let mut fake = Box::new(vec![]);
    //     std::mem::swap(&mut fake, &mut model.api_posisions);
    //     let mut fake = match agregator::add(fake, pos) {
    //         Ok(aps) => aps,
    //         Err((e, aps)) =>{
    //             model.console.log(&e.to_string());
    //             aps
    //         },
    //     };
    //     std::mem::swap(&mut fake, &mut model.api_posisions);
    // } 
}

fn process_del(model: &mut Model, data: &serde_json::value::Value)  -> Result<(), String> {
    match serde_json::from_str::<JsonApiDel>(&data.to_string()) {
        Ok(pos) => {
        let mut fake = Box::new(vec![]);
        std::mem::swap(&mut fake, &mut model.api_posisions);
        let mut fake = match agregator::del(fake, pos) {
            Ok(aps) => aps,
            Err((e, aps)) =>{
                model.console.log(&e.to_string());
                aps
            },
        };
        std::mem::swap(&mut fake, &mut model.api_posisions);
        // model.group_pos = agregator::gen_group_positions(&model.api_posisions);
        Ok(())
        }
        Err(e) => Err(e.to_string())
    }

    // if let Ok(pos) =
    //     serde_json::from_str::<JsonApiDel>(&data.to_string())
    // {
    //     let mut fake = Box::new(vec![]);
    //     std::mem::swap(&mut fake, &mut model.api_posisions);
    //     let mut fake = match agregator::del(fake, pos) {
    //         Ok(aps) => aps,
    //         Err((e, aps)) =>{
    //             model.console.log(&e.to_string());
    //             aps
    //         },
    //     };
    //     std::mem::swap(&mut fake, &mut model.api_posisions);
    // }
    // model.group_pos = agregator::gen_group_positions(&model.api_posisions);
}


//  --------------------------------------------------------
//  --------------------------------------------------------
//  --------------------------------------------------------

fn test_fill_fake(i: u8) -> GroupsPos {
    match i {
        0 => vec![PositionsGroup {
        group: "SPAIN".to_string(),
        pos_prods: vec![
        PositionsByProduct {
            isin: "ISINSADFASDF".to_string(),
            desc: "description sad fsad f".to_string(),
            updated: "2019-02-15 17:30:29.123".to_string(),
            bids: vec![Level {
            price: 99.1111,
            qty: 3,
        }],
            asks: vec![],
        }
    ]
    }]
,
        1 =>  vec![PositionsGroup {
        group: "SPAIN".to_string(),
        pos_prods: vec![
        PositionsByProduct {
            isin: "ISINSADFASDF".to_string(),
            desc: "description sad fsad f".to_string(),
            updated: "2019-02-15 17:30:29.123".to_string(),
            asks: vec![Level {
            price: 99.1111,
            qty: 3,
        }],
            bids: vec![],
        }
    ]}],
        2 =>  vec![PositionsGroup {
        group: "SPAIN".to_string(),
        pos_prods: vec![
        PositionsByProduct {
            isin: "ISINSADFASDF".to_string(),
            desc: "description sad fsad f".to_string(),
            updated: "2019-02-15 17:30:29.123".to_string(),
            bids: vec![Level {
            price: 99.1111,
            qty: 3,
        },
        Level {
            price: 97.1111,
            qty: 4,
        },
        Level {
            price: 95.1111,
            qty: 5,
        }],
            asks: vec![Level {
            price: 99.1111,
            qty: 3,
        }],
        }
    ]}],
        3 =>  vec![PositionsGroup {
        group: "SPAIN".to_string(),
        pos_prods: vec![
        PositionsByProduct {
            isin: "ISINSADFASDF".to_string(),
            desc: "description sad fsad f".to_string(),
            updated: "2019-02-15 17:30:29.123".to_string(),
            asks: vec![Level {
            price: 99.1111,
            qty: 3,
        },
        Level {
            price: 97.1111,
            qty: 4,
        },
        Level {
            price: 95.1111,
            qty: 5,
        }],
            bids: vec![],
        }
    ]}],    
        4 =>  vec![PositionsGroup {
        group: "SPAIN".to_string(),
        pos_prods: vec![
        PositionsByProduct {
            isin: "ISINSADFASDF".to_string(),
            desc: "description sad fsad f".to_string(),
            updated: "2019-02-15 17:30:29.123".to_string(),
            asks: vec![Level {
            price: 99.1111,
            qty: 3,
        },
        Level {
            price: 97.1111,
            qty: 4,
        },
        Level {
            price: 95.1111,
            qty: 5,
        }],
            bids: vec![],
        },
        PositionsByProduct {
            isin: "SADFASDFASDFASDF".to_string(),
            desc: "ASDFASDFSADF".to_string(),
            updated: "2019-02-15 17:33:29.123".to_string(),
            asks: vec![Level {
            price: 89.1111,
            qty: 38,
        },
        Level {
            price: 87.1111,
            qty: 48,
        },
        Level {
            price: 85.1111,
            qty: 58,
        }],
            bids: vec![],
        }
    ]}],

        5 =>  vec![PositionsGroup {
        group: "SPAIN".to_string(),
        pos_prods: vec![
        PositionsByProduct {
            isin: "ISINSADFASDF".to_string(),
            desc: "description sad fsad f".to_string(),
            updated: "2019-02-15 17:30:29.123".to_string(),
            asks: vec![Level {
            price: 99.1111,
            qty: 3,
        },
        Level {
            price: 97.1111,
            qty: 4,
        },
        Level {
            price: 95.1111,
            qty: 5,
        }],
            bids: vec![],
        },
        PositionsByProduct {
            isin: "SADFASDFASDFASDF".to_string(),
            desc: "ASDFASDFSADF".to_string(),
            updated: "2019-02-15 17:33:29.123".to_string(),
            asks: vec![Level {
            price: 89.1111,
            qty: 38,
        },
        Level {
            price: 87.1111,
            qty: 48,
        },
        Level {
            price: 85.1111,
            qty: 58,
        }],
            bids: vec![],
        },
        PositionsByProduct {
            isin: "ISINSADFASDF".to_string(),
            desc: "description sad fsad f".to_string(),
            updated: "2019-02-15 17:30:29.123".to_string(),
            asks: vec![Level {
            price: 99.1111,
            qty: 3,
        },
        Level {
            price: 97.1111,
            qty: 4,
        },
        Level {
            price: 95.1111,
            qty: 5,
        }],
            bids: vec![],
        },
        PositionsByProduct {
            isin: "SADFASDFASDFASDF".to_string(),
            desc: "ASDFASDFSADF".to_string(),
            updated: "2019-02-15 17:33:29.123".to_string(),
            asks: vec![Level {
            price: 89.1111,
            qty: 38,
        },
        Level {
            price: 87.1111,
            qty: 48,
        },
        Level {
            price: 85.1111,
            qty: 58,
        }],
            bids: vec![],
        }

    ]},
    PositionsGroup {
        group: "ITALY".to_string(),
        pos_prods: vec![
        PositionsByProduct {
            isin: "ISINSADFASDF".to_string(),
            desc: "description sad fsad f".to_string(),
            updated: "2019-02-15 17:30:29.123".to_string(),
            asks: vec![Level {
            price: 99.1111,
            qty: 3,
        },
        Level {
            price: 97.1111,
            qty: 4,
        },
        Level {
            price: 95.1111,
            qty: 5,
        }],
            bids: vec![],
        },
        PositionsByProduct {
            isin: "SADFASDFASDFASDF".to_string(),
            desc: "ASDFASDFSADF".to_string(),
            updated: "2019-02-15 17:33:29.123".to_string(),
            asks: vec![Level {
            price: 89.1111,
            qty: 38,
        },
        Level {
            price: 87.1111,
            qty: 48,
        },
        Level {
            price: 85.1111,
            qty: 58,
        }],
            bids: vec![],
        },
        PositionsByProduct {
            isin: "ISINSADFASDF".to_string(),
            desc: "description sad fsad f".to_string(),
            updated: "2019-02-15 17:30:29.123".to_string(),
            asks: vec![Level {
            price: 99.1111,
            qty: 3,
        },
        Level {
            price: 97.1111,
            qty: 4,
        },
        Level {
            price: 95.1111,
            qty: 5,
        }],
            bids: vec![],
        },
        PositionsByProduct {
            isin: "SADFASDFASDFASDF".to_string(),
            desc: "ASDFASDFSADF".to_string(),
            updated: "2019-02-15 17:33:29.123".to_string(),
            asks: vec![Level {
            price: 89.1111,
            qty: 38,
        },
        Level {
            price: 87.1111,
            qty: 48,
        },
        Level {
            price: 85.1111,
            qty: 58,
        }],
            bids: vec![],
        }

    ]}],    

    _ => vec![]
    }
}

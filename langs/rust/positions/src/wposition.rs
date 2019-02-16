// use std::result::Result;
use crate::position::*;
use crate::json_api::{JsonApiAdd, JsonApiDel};

extern crate failure;
extern crate yew;
use yew::format::Format;
use yew::format::Json;
use yew::prelude::*;
use yew::services::websocket::{WebSocketService, WebSocketStatus, WebSocketTask};
use yew::services::ConsoleService;

use crate::agregator;
// use stdweb::web::Date;

pub struct Model {
    ws: Option<WebSocketTask>,
    wss: WebSocketService,
    link: ComponentLink<Model>,
    console: ConsoleService,
    group_pos: GroupsPos,
    api_posisions: Box<Positions>,
    connect2: String,
}

pub enum Msg {
    ClickConnect,
    ClickFakeData(u8),
    Ignore,
    Disconnected,
    Received(Format<serde_json::value::Value>),
    UpdatedUrl(String),
}

impl Model {
    fn connect(&mut self) {
        // self.console.log("Connecting");
        let cbout = self.link.send_back(|Json(data)| Msg::Received(data));
        let cbnot = self.link.send_back(|input| {
            // ConsoleService::new().log(&format!("Notification: {:?}", input));
            match input {
                WebSocketStatus::Closed | WebSocketStatus::Error => Msg::Disconnected,
                _ => Msg::Ignore,
            }
        });
        if self.ws.is_none() {
            let task = self.wss.connect(&self.connect2, cbout, cbnot);
            self.ws = Some(task);
        }
    }
}



impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
        Model {
            console: ConsoleService::new(),
            ws: None,
            wss: WebSocketService::new(),
            link,
            api_posisions: Box::new(vec![]),
            group_pos: vec![], 
            connect2: match stdweb::web::window().location() {
                Some(location) => {
                    match location.host() {
                        Ok(href) => format!("ws://{}", href.to_string()),
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
            Msg::Ignore => false,
            Msg::Disconnected => {
                self.ws = None;
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
                    match &msg_type as &str {
                        "PubBookPosition" => process_add(self, &data),
                        "PubDelPosition" => process_del(self, &data),
                        _ => self.console.log(&format!("_msg_type unknown {}", data))
                    };
                } else {
                    self.console.log(&format!("missing _msg_typoe {}", data))
                }
                true
            }
            Msg::UpdatedUrl(url) => {
                self.connect2 = url;
                true
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


        let connect_params = || {
            html! {
                <>
                    <span onclick=|_| Msg::ClickConnect,>
                    {"disconected"}
                    </span>
                    <input type="text",
                        value={self.connect2.to_string()},
                        oninput=|input| Msg::UpdatedUrl(input.value), >
                    </input>
                </>
            }
        };

        let test_fake = || {
            html! {
                <>
                    <p>
                    <span onclick=|_| Msg::ClickFakeData(0),>{"fake0 "}</span>
                    <span onclick=|_| Msg::ClickFakeData(1),>{"fake1 "}</span>
                    <span onclick=|_| Msg::ClickFakeData(2),>{"fake2 "}</span>
                    <span onclick=|_| Msg::ClickFakeData(3),>{"fake3 "}</span>
                    <span onclick=|_| Msg::ClickFakeData(4),>{"fake4 "}</span>
                    <span onclick=|_| Msg::ClickFakeData(5),>{"fake5 "}</span>
                    </p>
                </>
            }
        };

        let connected = || {
            html! {
                <>
                <p>
                {
                    if self.ws.is_some() {
                        html! {<>{"connected"}</>}
                    } else {
                        connect_params()
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
                <th scope="col", class="uk-width-medium black",>{"id"}</th>
                <th scope="col", class="uk-width-medium black",>{"descr"}</th>
                <th scope="col", class="uk-width-small center",>{"qty"}</th>
                <th scope="col", class="uk-width-small center",>{"bid"}</th>
                <th scope="col", class="uk-width-small center",>{"ask"}</th>
                <th scope="col", class="uk-width-small center",>{"qty"}</th>
                <th scope="col",>{"updated"}</th>
                </tr>
            </thead>
                </>
            }
        };

        let leveln = |n: usize, bids: &Levels, asks: &Levels, bordertop: bool| {
            let price_quantity = |levels: &Levels| {
                match levels.get(n) {
                    Some(l) => (format!("{}", l.price), format!("{}", l.qty)),
                    _ => (" ".to_string(), "".to_string())
                }
            };
            let (pbid, qbid) = price_quantity(bids);
            let (pask, qask) = price_quantity(asks);
            let tdclass = || if bordertop {
                "center , bordertop"
            } else {"center"};
            html! {
                <>
                <td class=tdclass(),> { qbid } </td> <td class=tdclass(),> { pbid } </td>
                <td class=tdclass(),> { pask } </td> <td class=tdclass(),> { qask } </td>
                </>
            }
        };

        let pos_by_prod_view = |pos_by_prod: &PositionsByProduct| {
            html! {
                <>
                <tr>
                // <th scope="row",>{1}</th>
                // <td>{&pos_by_prod.id}</td>
                <td class="bordertop",>{&pos_by_prod.isin}</td>
                <td class="uk-text-truncate bordertop",>{&pos_by_prod.desc}</td>
                {leveln(0, &pos_by_prod.bids, &pos_by_prod.asks, true)}
                <td class="uk-text-truncate bordertop",>{&pos_by_prod.updated}</td>
                </tr>
                {for (1..5).map(|i| 
                    html!{<tr><td></td><td></td>{leveln(i, &pos_by_prod.bids, &pos_by_prod.asks, false)}<td></td></tr>})
                }
                </>
            }
        };

        let rows = |pos_by_prod: &PosByProds| {
            html! {
                <>
                    {for (pos_by_prod.iter()).map(|pos_by_prod| {
                        pos_by_prod_view(&pos_by_prod)
                    })}
                </>
            }
        };

        let table = |group:&String, pos_prods: &PosByProds| {
            html! {
                <>
                <h3>{group}</h3>
                <div class="uk-overflow-auto",>
                    <table class="uk-table uk-table-small",>
                    {header()}
                    <tbody>
                        {rows(pos_prods)}
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

fn process_add(model: &mut Model, data: &serde_json::value::Value) {
    if let Ok(pos) =
        serde_json::from_str::<JsonApiAdd>(&data.to_string())
    {
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
    }
    model.group_pos = agregator::gen_group_positions(&model.api_posisions);
}

fn process_del(model: &mut Model, data: &serde_json::value::Value) {
    if let Ok(pos) =
        serde_json::from_str::<JsonApiDel>(&data.to_string())
    {
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
    }
    model.group_pos = agregator::gen_group_positions(&model.api_posisions);
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

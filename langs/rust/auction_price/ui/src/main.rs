extern crate auction;

use yew::prelude::*;

enum Msg {
    SwitchLimitMarket,
    SwitchSide,

    UpdateEditPrice(String),
    UpdateEditQuantity(String),
    AddOrder,
}

#[derive(Clone, Copy)]
struct Order {
    side: auction::Side,
    price: Option<i64>,
    quantity: u64,
}

enum OrderType {
    Limit,
    Market,
}

struct Model {
    // `ComponentLink` is like a reference to a component.
    // It can be used to send messages to the component
    link: ComponentLink<Self>,
    order_type: OrderType,
    side: auction::Side,
    edit_state: EditState,
    orders: Vec<Order>,

    auction_engine: auction::OrderBookAcc,
}

struct EditState {
    editing_price: String,
    editing_quantity: String,
}

impl EditState {
    fn new() -> Self {
        EditState {
            editing_price: "".to_string(),
            editing_quantity: "".to_string(),
        }
    }
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link,
            order_type: OrderType::Limit,
            side: auction::Side::Bid,
            edit_state: EditState::new(),
            orders: vec![],
            auction_engine: auction::OrderBookAcc::new(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        let get_text_edit_number = |new_txt: &str, old_txt: &str| {
            if !new_txt.is_empty() {
                match new_txt.parse::<i32>() {
                    Ok(_) => new_txt.to_owned(),
                    Err(_) => old_txt.to_owned(),
                }
            } else {
                "".to_owned()
            }
        };
        match msg {
            Msg::SwitchLimitMarket => {
                match self.order_type {
                    OrderType::Limit => self.order_type = OrderType::Market,
                    OrderType::Market => self.order_type = OrderType::Limit,
                };
                true
            }
            Msg::SwitchSide => {
                self.side = match self.side {
                    auction::Side::Bid => auction::Side::Ask,
                    auction::Side::Ask => auction::Side::Bid,
                };
                true
            }
            Msg::UpdateEditPrice(new_txt) => {
                self.edit_state.editing_price =
                    get_text_edit_number(&new_txt, &self.edit_state.editing_price);
                true
            }
            Msg::UpdateEditQuantity(new_txt) => {
                self.edit_state.editing_quantity =
                    get_text_edit_number(&new_txt, &self.edit_state.editing_quantity);
                true
            }
            Msg::AddOrder => {
                let order = self.get_current_order();
                self.clear();
                self.orders.push(order);
                match order.price {
                    None => {
                        self.auction_engine = self
                            .auction_engine
                            .clone()
                            .add_market_order(order.side, auction::Qty(order.quantity));
                    }
                    Some(price) => {
                        self.auction_engine = self.auction_engine.clone().add_limit_order(
                            order.side,
                            auction::Price(price),
                            auction::Qty(order.quantity),
                        );
                    }
                };

                true
            }
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        // Should only return "true" if new properties are different to
        // previously received properties.
        // This component has no properties so we will always return "false".
        false
    }

    fn view(&self) -> Html {
        html! {
            <>
            <div class="container is-max-desktop">
            <h3 class="title">
            {"Auction price calculator"}
            </h3>

            <div class="block">
            <a href="https://github.com/jleahred/katas/tree/master/langs/rust/auction_price">{"repository source"}</a>
            </div>

            <div class="columns">
                <div class="column">
                {self.view_enter_order()}
                </div>

                <div class="column">
                { self.view_auction_price() }
                {self.view_orders()}
                </div>
                </div>
            </div>
            </>
        }
    }
}

impl Model {
    fn view_enter_order(&self) -> Html {
        html! {
            <>
            {self.view_order_type()}

            {self.view_side()}

            {self.view_price()}

            <div class="field">
                <label class="label">{"Quantity"}</label>
                <input class="input" min="1" step="0.01" placeholder="quantity"
                    value = {self.edit_state.editing_quantity.clone()}
                    oninput=self.link.callback(|e: InputData| Msg::UpdateEditQuantity(e.value))
                />
            </div>

            {self.view_add_order_button()}
            </>
        }
    }
    fn view_auction_price(&self) -> Html {
        let auction_price = self
            .auction_engine
            .get_more_qty_exec_levels()
            .get_min_diff_levels()
            .get_max_qty_levels()
            .get_proxim2_levels(auction::Price(0))
            .get_first_level();
        let price = match auction_price {
            None => "no price".to_owned(),
            Some((price, qty)) => format!("{}@{}", price.0, qty.0),
        };
        html! {
            <>
            <h2 class="title">
            {price}
            </h2>
            </>
        }
    }
    fn view_orders(&self) -> Html {
        let price_txt = |oprice| match oprice {
            Some(price) => format!("{}", price),
            None => "".to_owned(),
        };
        let side_txt_color = |side: &auction::Side| match side {
            auction::Side::Bid => ("buy", "has-background-success"),
            auction::Side::Ask => ("sell", "has-background-danger"),
        };
        let oview = |order: &Order| {
            let (side_txt, side_color) = side_txt_color(&order.side);
            html! {
                <>
                <tr>
                <td>
                    {price_txt(order.price)}
                    </td>
                    <td>
                    {order.quantity}
                    </td>
                    <td class=classes!({side_color}) >
                    {side_txt}
                </td>
                </tr>
                </>
            }
        };
        html! {
            <table class="table">
            <thead>
            <td>{"Price"}</td>
            <td>{"Quantity"}</td>
            <td>{"Side"}</td>
            </thead>
            <tbody>
                { for self.orders.iter().map(|order| oview(order))}
            </tbody>
          </table>
        }
    }

    fn get_current_order(&self) -> Order {
        let price = match self.order_type {
            OrderType::Limit => Some(self.edit_state.editing_price.parse::<i64>().unwrap()),
            OrderType::Market => None,
        };
        Order {
            side: self.side,
            price: price,
            quantity: self.edit_state.editing_quantity.parse::<u64>().unwrap(),
        }
    }

    fn clear(&mut self) {
        self.edit_state.editing_price.clear();
        self.edit_state.editing_quantity.clear();
    }

    fn is_valid_input(&self) -> bool {
        match self.order_type {
            OrderType::Limit => {
                !(self.edit_state.editing_price.is_empty()
                    || self.edit_state.editing_quantity.is_empty())
            }
            OrderType::Market => !(self.edit_state.editing_quantity.is_empty()),
        }
    }

    fn view_add_order_button(&self) -> Html {
        let (color, enabled) = match self.is_valid_input() {
            true => ("is-primary", true),
            false => ("", false),
        };
        html! {
            <>
            <div class="field">
                <button class=classes!("button", color, "is-fullwidth")
                    disabled={!enabled}
                     onclick=self.link.callback(|_| Msg::AddOrder)
                >{ "Add order" }
                </button>
            </div>
            </>
        }
    }

    fn view_side(&self) -> Html {
        let button_text_color = match self.side {
            auction::Side::Bid => ("Buy", "is-success"),
            auction::Side::Ask => ("Sell", "is-danger"),
        };
        html! {
            <>
            <div class="field">
                <button
                    class=classes!("button",  "is-fullwidth",  button_text_color.1)
                    onclick=self.link.callback(|_| Msg::SwitchSide)
                >
                { button_text_color.0 }
                </button>
            </div>
            </>
        }
    }

    fn view_order_type(&self) -> Html {
        let button_text = match self.order_type {
            OrderType::Limit => "Limit order",
            OrderType::Market => "Market order",
        };
        html! {
            <>
            <div class="field">
                <button class="button is-fullwidth" onclick=self.link.callback(|_| Msg::SwitchLimitMarket)>{ button_text }</button>
            </div>
            </>
        }
    }

    fn view_price(&self) -> Html {
        let html_price = html! {
            <>
            <div class="field">
            <label class="label">{"Price"}</label>
            <input class="input" min="1" step="0.01" placeholder="price"
                value = {self.edit_state.editing_price.clone()}
                oninput=self.link.callback(|e: InputData| Msg::UpdateEditPrice(e.value))
            />
            //<p class="help">{"This is a help text"}</p>
            </div>
            </>
        };
        match self.order_type {
            OrderType::Limit => html_price,
            OrderType::Market => html! {},
        }
    }
}

fn main() {
    yew::start_app::<Model>();
}

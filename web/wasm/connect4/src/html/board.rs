use crate::engine;
use yew::prelude::*;
// use yew::services::ConsoleService;

pub struct Board {
    // console: ConsoleService,
    game: Box<engine::Game>,
}

pub enum Msg {
    Click(u8),
}

//  ----------

impl Board {}

impl Component for Board {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        let game = engine::Game::new(engine::Player::O);
        Board {
            game: Box::new(game),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        let try_play = |game: &mut engine::Game, c| {
            if let Some(col) = engine::Col::b(c) {
                idata::steal_borrow(game, &|s: engine::Game| s.try_play(col))
            }
        };

        match msg {
            Msg::Click(col) => try_play(&mut self.game, col),
        };
        true
    }
    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        true
    }
}

impl Renderable<Board> for Board {
    fn view(&self) -> Html<Self> {
        let get_color_cell = |r, c| -> &str {
            let get_color_rc = |r, c| match self.game.board.get_cell(c, r) {
                engine::Cell::P(engine::Player::O) => "yellow",
                engine::Cell::P(engine::Player::X) => "red",
                engine::Cell::Empty => "",
            };

            match (engine::Row::b(r), engine::Col::b(c)) {
                (Some(r), Some(c)) => get_color_rc(r, c),
                _ => "",
            }
        };

        let td = |r, c| {
            html! {
                <>
                <td class=get_color_cell(r,c), onclick=|_| Msg::Click(c),></td>
                </>
            }
        };

        let row = |r| {
            html! {
                <tr>
                    {td(r,0)}
                    {td(r,1)}
                    {td(r,2)}
                    {td(r,3)}
                    {td(r,4)}
                    {td(r,5)}
                    {td(r,6)}
                </tr>
            }
        };

        let board = || {
            html! {
                <table class="board",>
                    {row(0)}
                    {row(1)}
                    {row(2)}
                    {row(3)}
                    {row(4)}
                    {row(5)}
                </table>
            }
        };

        let pattern_count = || {
            let get_pos_eval = || match &self.game.patterns {
                crate::engine::patterns::Patterns::P(pcp) => format!("{:?}", pcp.eval(self.game.turn)),
                _ => "we have a winner".to_string(),
            };
            let get_pattern_debug_txt = || match &self.game.patterns {
                crate::engine::patterns::Patterns::P(pcp) => {
                    (format!("{:?}", pcp.player_o), format!("{:?}", pcp.player_x))
                }
                _ => ("".to_string(), "".to_string()),
            };
            html! {
                <>
                <p>{get_pos_eval()}</p>
                <p>{get_pattern_debug_txt().0}</p>
                <p>{get_pattern_debug_txt().1}</p>
                </>
            }
        };

        html! {
            <div class="game",>
                {board()}
            </div>
            {pattern_count()}
            </div>
            </div>
        }
    }
}

use engine;
use yew::prelude::*;
// use yew::services::ConsoleService;

pub struct Board {
    // console: ConsoleService,
    game: engine::Game,
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
        Board { game }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        let play_mut_game = |game: &mut engine::Game, c| {
            if let Some(col) = engine::Col::new(c) {
                let g = game.clone();
                std::mem::swap(&mut g.try_play(col), game);
            }
        };

        match msg {
            Msg::Click(col) => play_mut_game(&mut self.game, col),
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

            match (engine::Row::new(r), engine::Col::new(c)) {
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
        html! {
            <div class="game",>
                <table class="board",>
                    {row(0)}
                    {row(1)}
                    {row(2)}
                    {row(3)}
                    {row(4)}
                    {row(5)}
                </table>
            </div>
        }
    }
}

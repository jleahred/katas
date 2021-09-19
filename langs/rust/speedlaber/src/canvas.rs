// use crate::map::Matrix;
use crate::map;
use crate::map::Map;

use seed::prelude::*;

struct Context {
    size: usize,
    cell_size: usize,
}

struct Color(u8, u8, u8);

struct CellInfo {
    row: usize,
    col: usize,
    color: Color,
}

pub(crate) fn draw(canvas: web_sys::CanvasRenderingContext2d, canvas_size: usize, map: &Map) {
    canvas.begin_path();
    canvas.clear_rect(0., 0., canvas_size as f64, canvas_size as f64);
    // canvas.fill_rect(0., 0., canvas_size as f64, canvas_size as f64);

    // canvas.set_fill_style(&JsValue::from_str("#fffffF"));
    // canvas.fill_rect(0., 0., canvas_size as f64, canvas_size as f64);
    let ctx = Context {
        size: map.matrix.size,
        cell_size: canvas_size / map.matrix.size,
    };
    let real_canvas_size = ctx.cell_size * map.matrix.size;
    // canvas.save();
    // canvas.restore();
    // let _ = canvas.scale(
    //     canvas_size as f64 / real_canvas_size as f64,
    //     canvas_size as f64 / real_canvas_size as f64,
    // );
    // canvas.save();
    canvas.set_fill_style(&JsValue::from_str("rgb(255, 255, 255)"));
    canvas.rect(0., 0., real_canvas_size as f64, real_canvas_size as f64);

    for row in 0..ctx.size {
        for col in 0..ctx.size {
            let light = map.matrix.get_rc(row, col);
            let light = if light > 0 { 255 - light } else { 0 };
            let cell_info = CellInfo {
                row,
                col,
                color: Color(light, light, light),
            };
            draw_cell(&canvas, &ctx, &cell_info);
        }
    }

    let cell_info_point = |p: &map::Position, color| CellInfo {
        row: p.row,
        col: p.col,
        color,
    };
    let cell_enter = cell_info_point(&map.enter, Color(0, 255, 0));
    let cell_exit = cell_info_point(&map.exit, Color(0, 180, 255));
    draw_cell(&canvas, &ctx, &cell_enter);
    draw_cell(&canvas, &ctx, &cell_exit);
    canvas.stroke();
}

fn draw_cell(ctx: &web_sys::CanvasRenderingContext2d, context: &Context, ci: &CellInfo) {
    let x = (context.cell_size * ci.col) as f64;
    let y = (context.cell_size * ci.row) as f64;

    ctx.set_fill_style(&JsValue::from_str(&format!(
        "rgb({}, {}, {})",
        ci.color.0, ci.color.1, ci.color.2
    )));
    ctx.fill_rect(
        x,
        y,
        (context.cell_size + 1) as f64,
        (context.cell_size + 1) as f64,
    );
    // ctx.stroke();
}

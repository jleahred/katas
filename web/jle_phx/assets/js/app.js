// We need to import the CSS so that webpack will load it.
// The MiniCssExtractPlugin is used to separate it out into
// its own CSS file.
import css from "../css/app.css"

// webpack automatically bundles all modules in your
// entry points. Those entry points can be configured
// in "webpack.config.js".
//
// Import dependencies
//
import "phoenix_html"

//import "echarts"
//var echarts = require('echarts');

// Import local files
//
// Local files can be imported directly using relative paths, for example:
// import socket from "./socket"

//  bootstrap
$(function () {
    $('[data-toggle="tooltip"]').tooltip()
})

jQuery(document).ready(function ($) {
    $(".clickable-row").click(function () {
        window.location = $(this).data("href");
    });
});

// assets/js/app.js
import LiveSocket from "phoenix_live_view"

let liveSocket = new LiveSocket("/live")
liveSocket.connect()